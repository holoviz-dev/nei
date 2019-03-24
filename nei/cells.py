import os
import logging
import json
import sys
import re
import webbrowser

try:
    import nbconvert
except:
    nbconvert = None

import nbformat

from .styles import process_css

BROWSER = 'firefox' # E.g 'chrome' or 'firefox'

# TODO
# [ ] Move scratch/learning content to a branch
# [ ] Could add a kill buffer hook to shutdown server when all nei buffers closed.


# [ ] Add Yank hook.
# [ ] Hide/Show cells inputs/outputs etc
# [ ] Scroll to point in client. Maybe use an idle timer?
# [ ] Comment prompts in client


def serialize_binary_message(msg):
    """serialize a message as a binary blob
    Header:
    4 bytes: number of msg parts (nbufs) as 32b int
    4 * nbufs bytes: offset for each buffer as integer as 32b int
    Offsets are from the start of the buffer, including the header.
    Returns
    -------
    The message serialized to bytes.
    """
    from jupyter_client.jsonutil import date_default
    import struct
    # don't modify msg or buffer list in-place
    msg = msg.copy()
    buffers = list(msg.pop('buffers'))
    if sys.version_info < (3, 4):
        buffers = [x.tobytes() for x in buffers]
    bmsg = json.dumps(msg, default=date_default).encode('utf8')
    buffers.insert(0, bmsg)
    nbufs = len(buffers)
    offsets = [4 * (nbufs + 1)]
    for buf in buffers[:-1]:
        offsets.append(offsets[-1] + len(buf))
    offsets_buf = struct.pack('!' + 'I' * (nbufs + 1), nbufs, *offsets)
    buffers.insert(0, offsets_buf)
    return b''.join(buffers)



class Cell(object):
    "Object representing a code or markdown cell"

    code_pretty_formatter = "# In[{prompt}]\n{source}\n\n"
    md_pretty_formatter = '\"\"\"\n{source}\n\"\"\"\n\n'

    code_prompt_formatter = "# In[{prompt}]\n{source}"
    md_prompt_formatter = '\"\"\"\n{source}\n\"\"\"'

    # For quick startswith line matching
    code_startswith = "# In["
    md_startswith = '\"\"\"'

    # To extract the prompt number (if any)
    code_prompt_regexp = re.compile("^# In\[(\d*| )\]")

    def __init__(self, mode, source, input=None, prompt=None, outputs=[]):
        self.mode = mode
        self.source = source
        self.prompt = prompt
        self.input = input

        self.outputs = outputs # List of nbformat output nodes

    @property
    def data(self):
        return {'mode':self.mode,
                'source':self.source,
                'prompt':self.prompt}

    @property
    def text(self):
        if self.mode == 'code':
            prompt = ' ' if self.prompt is None else self.prompt
            return self.code_prompt_formatter.format(source=self.source, prompt=prompt)
        elif self.mode == 'markdown':
            return self.md_prompt_formatter.format(source=self.source)

    @property
    def hash(self):
        return hash((self.prompt, self.mode,self.source.strip()))

    def node(self, cleared=False):
        "Return cell as a notebook node"
        out_nodes = []
        if not cleared:
            for output in self.outputs: # TODO: Support other outputs
                out_nodes.append(output)

        if self.mode == 'code':
            return nbformat.v4.new_code_cell(self.source, outputs=out_nodes)
        elif self.mode == 'markdown':
            return nbformat.v4.new_markdown_cell(self.source)

    def clear_output(self):
        self.outputs = []

    def __repr__(self):
        return "%scell(%s)" % (self.mode, self.source[:50])


class Cells(object):
    "Notebook state is represented as a list of Cell objects"

    def __init__(self, text=None, cells=[]):
        if text is None:
            self.cells = cells
        else:
            self.load(ParseNotebook.extract_cells(text))

    @property
    def hashes(self):
        return [cell.hash for cell in self.cells]

    @property
    def text(self):
        "Return the textual representation of the cells"
        return ''.join([cell.text for cell in self.cells])

    @property
    def pretty(self):
        print("PRETTY NOT IMPLEMENTED")
        return self.text

    def load(self, dicts):
        "Load cells from cell dictionaries"
        if dicts is None: return
        self.cells = [Cell(**cdict) for cdict in dicts]

    def by_line(self, line_number, offset=False):
        "Given a line number find the corresponding cell number with offset if requested"
        md_closed, cell_count, boundary = True, -1, 0
        for lineno, line in enumerate(str(self.mirrorbuffer).splitlines(keepends=True)):
            if line.startswith(Cell.code_startswith):
                cell_count += 1
                boundary = lineno
            if line.startswith(Cell.md_startswith) and md_closed:
                cell_count += 1
                boundary = lineno
                md_closed = not md_closed
            elif line.startswith(Cell.md_startswith):
                md_closed = not md_closed
            if line_number == lineno+1:
                cell_no = cell_count if cell_count >= 0 else None
                delta = lineno - boundary
                return (cell_no, delta) if offset else cell_no

        logging.info('WARNING: Line number out of bounds')

    def find_cell(self, prompt):
        "Given a prompt number, find the corresponding cell, if any"
        for cell in self.cells:
            if cell.prompt == prompt:
                return cell
        logging.info("WARNING: Could not find cell with prompt %s" % prompt)

    def cell_position(self, cell):
        "Given a cell, return its position"
        for pos, candidate in enumerate(self.cells):
            if candidate == cell:
                return pos

    # Commands (that don't need to talk to the client)

    def write_notebook(self, connection, mode, filename):
        assert mode in ['python', 'cleared', 'full-notebook', 'html']
        if mode == 'python':
            data = self.pretty
        else:
            cleared = (mode == 'cleared')
            nb = nbformat.v4.new_notebook()
            nb['cells'] = [cell.node(cleared=cleared) for cell in self.cells]
            nb['metadata'] = self.metadata

        if mode == 'html':
            if nbconvert is None:
                logging.info("nbconvert not available for HTML conversion")
                return
            (data, resources) = nbconvert.exporters.export(nbconvert.HTMLExporter, nb)
        with open(filename, 'w') as f:
            if mode in ['cleared', 'full-notebook']:
                nbformat.write(nb, f)
            else:
                f.write(data)

    def view_notebook(self, connection):
        filename = os.path.join(self.STATIC_PATH, 'view.html')
        self.write_notebook(connection, 'html', filename)
        webbrowser.get(self.config['browser']).open("http://localhost:8000/view.html")

    def view_browser(self, connection):
        if connection is None:
            webbrowser.get(self.config['browser']).open("http://localhost:8000/index.html")


class OutputMessage(object):
    """
    Class to process and filter output messages sent to the frontend.
    """

    @classmethod
    def process(cls, node):
        out_msgs = cls._output_messages(node)
        return cls._filter_outputs(out_msgs)

    @classmethod
    def _filter_outputs(cls, outputs):
        "Remove plaintext mimetype if html mimetype available"
        if outputs == []: return []
        mimes, data = zip(*outputs)
        exclude = all(m in mimes for m in ['text/plain', 'text/html'])
        return [(mime, val) for (mime, val) in zip(mimes, data)
                if mime != ('text/plain'if exclude else None)]


    @classmethod
    def _output_messages(cls, node):
        "Given a notebook node, generate output messages"
        mime_types = ['text/plain', 'text/html', 'text/ansi',
                      'application/javascript']
        output_type = node['output_type']
        # Assuming notebook node is iterated in order.
        if output_type == 'stream':
            text = node['text']
            if node['name'] == 'stderr':
                text = "<p style='color:red'>{text}</p>".format(text=text)
            return [('text/html', text)]
        elif output_type == 'error': # Handle warnings
            return[('text/ansi', node['traceback'])]

        outputs = []
        for mtype, data in node['data'].items():
            if mtype not in mime_types: continue
            outputs.append((mtype, data))
        return outputs



class Notebook(Cells):
    """
    Extends cells with methods that operate on them and communicate with the client

    PROTOCOL

    In browser, 'cmd' can be:
    'add_cell','update_cell', 'remove_cells', 'clear_cell_output'

    In editor, 'cmd' can *also* be:
    'exec_cell_by_line','exec_cell_by_line'
    which then updates the display
    """

    STATIC_PATH = None

    def __init__(self, name=None, **kwargs):
        super(Notebook, self).__init__(**kwargs)
        self.name = name
        self.metadata = None
        self._last_dispatch = None # Used for debugging and testing
        self.mirrorbuffer = MirrorBuffer()
        self.commands = {'view_browser': self.view_browser, # Opens browser connection
                         # Commands that do no need a browser connection
                         'update_config':    self.update_config,
                         'scroll_by' :       self.scroll_by,
                         'scroll_to_line' :  self.scroll_to_line,

                         # Used for debugging
                         'terminate' :       self.terminate,
                         'server_info' :     self.server_info,

                         'write_notebook':   self.write_notebook,
                         'start_mirror':     self.start_mirror,
                         'mirror':           self.mirror,
                         'hold_mode':        self.hold_mode,
                         # Cell commands
                         'add_cell':            self.add_cell,
                         'update_cell_outputs': self.update_cell_outputs,
                         'update_cell_input':   self.update_cell_input,
                         'clear_cell_output':   self.clear_cell_output,
                         # Multi-cell commands
                         'load_from_file':         self.load_from_file,
                         'insert_file_at_point':   self.insert_file_at_point,
                         'reorder_cells':          self.reorder_cells,
                         'clear_notebook':         self.clear_notebook,
                         'clear_all_cell_outputs': self.clear_all_cell_outputs,
                         # IO commands
                         'download_python_notebook':  self.download_python_notebook,
                         'download_cleared_notebook': self.download_cleared_notebook,
                         'download_full_notebook':    self.download_full_notebook,
                         'view_notebook':             self.view_notebook,
                         'update_style':              self.update_style,
                         # Derived 'by_line' methods
                         'clear_cell_output_by_line': self.clear_cell_output_by_line,
                         }
        self.css = "" # Last CSS sent to browser
        self.config = {'browser':'firefox'}


    def message(self, connection, command, args, buffers=[]):
        if connection is not None:
            if buffers == []:
                connection.write_message({'cmd':command, 'args':args, 'name':self.name})
            else:
                binary = serialize_binary_message(
                    {'cmd':'comm_msg', 'args':args, 'name':self.name, 'buffers': buffers})
                connection.write_message(binary, binary=True)
        else:
            logging.info("WARNING: Command %s sent to non-connection" % command)

    def dispatch(self, connection, payload):
        cmd, args = payload['cmd'], payload['args']
        if cmd != 'server_info':
            self._last_dispatch = payload # Used for debugging/testing
        self.commands[cmd](connection, **(args if args is not None else {}))

    def update_config(self, connection, config):
        self.config = config

    def terminate(self, connection): # Used for debugging
        logging.info("ERROR: Termination requested")

    def server_info(self, connection):
        # Used for debugging and testing by outputting info on browser connection
        self.message(connection, 'server_info',
                     {'text': self.text,
                      'last_dispatch': self._last_dispatch})

    def scroll_by(self, connection, offset):
        self.message(connection, 'scroll_by', {'offset': offset})

    def scroll_to_line(self, connection, line):
        position, offset = self.by_line(line, offset=True)
        self.message(connection, 'scroll_to', {'position':position, 'line':offset})

    def clear_cell_output(self, connection, position):
        self.cells[position].clear_output()
        self.message(connection, 'clear_cell_output', {'position':position})

    def clear_cell_output_by_line(self, connection, line_number):
        position = self.by_line(line_number)
        if position is None: return
        self.clear_cell_output(connection, position)


    def remove_cell(self, connection, position):
        del self.cells[position]
        self.message(connection, 'remove_cell', {'position':position})

    def add_cell(self, connection, source, input=None,
                 mode='code', prompt=None, position=None, outputs=[]):
        cell = Cell(mode=mode, source=source, input=input, prompt=prompt, outputs=outputs)
        if position is None:
            self.cells.append(cell)
        else:
            self.cells.insert(position, cell)

        args = {'mode':mode, 'source':source, 'input':input,
                'position':position, 'outputs':[]}
        if outputs is not None:
            filtered_outputs = []
            for cell_output in outputs:
                filtered_outputs.extend(OutputMessage.process(cell_output))
            args['outputs'] = filtered_outputs

        self.message(connection, 'add_cell', args)


    def update_cell_outputs(self, connection, position, outputs):
        "Call with outputs of None to simply update the prompt"
        cell = self.cells[position]
        if outputs is not None:
            cell.outputs.append(outputs)
        if not isinstance(outputs, (list, type(None))): # I.e an output node
            # Filtering output messages
            outputs =  OutputMessage.process(outputs)

        self.message(connection, 'update_cell_outputs',{'position':position,
                                                        'outputs': outputs,
                                                        'prompt': cell.prompt})



    def update_cell_input(self, connection, source, position, input=None):
        self.cells[position].source = source
        self.cells[position].input = input
        self.message(connection, 'update_cell_input',{'position':position,
                                                      'source':source,
                                                      'input': input})

    #=================================================#
    # Commands that operate on multiple cells at once #
    #=================================================#

    def reload(self, connection):
        """
        Called when the browser tab is initially opened or reloaded and
        the notebook has cell state.
        """
        self.message(connection, 'clear_notebook', {})
        for pos, cell in enumerate(self.cells):
            # TODO: Pass prompt to JS
            self.message(connection, 'add_cell', {'mode':cell.mode,
                                                  'source':cell.source,
                                                  'input':cell.input})
            if cell.mode == 'code' and cell.outputs:
                for output in cell.outputs:
                    filtered = OutputMessage.process(output)
                    self.message(connection,'update_cell_outputs', {'position':pos,
                                                                    'outputs':filtered})


    def mirror(self, connection, start, end, length, added, size):
        self.mirrorbuffer(start, end, length, added, size)
        if not self.mirrorbuffer.hold:
            cells = ParseNotebook.extract_cells(str(self.mirrorbuffer))
            src = Notebook(cells=list())
            src.load(cells)
            SyncNotebooks.sync(connection, src, self)


    def start_mirror(self, connection, text):
        self.mirrorbuffer.clear()
        self.mirror(connection, 0, len(text), len(text), text, len(text))

    def hold_mode(self, connection, mode):
        assert mode in ['on','off','auto']
        self.mirrorbuffer.hold_mode = mode

    def insert_file_at_point(self, connection, filename, text, line_number):
        insert_cell_pos = self.by_line(line_number)
        # Mirror buffer expected to be already updated by mirror insertion from editor
        buffer_text = str(self.mirrorbuffer)
        cells = ParseNotebook.extract_cells(buffer_text)
        src = Notebook(cells=list())
        src.load(cells)

        with open(filename, 'r') as f:
            nb = nbformat.v4.reads(f.read())

        positions = range(insert_cell_pos, insert_cell_pos + len(nb.cells))
        assert len(positions) == len(nb.cells)
        for insert_pos, nb_cell in zip(positions, nb.cells):
            if nb_cell.cell_type == 'code':
                src.cells[insert_pos].outputs = nb_cell.outputs

        SyncNotebooks.sync(connection, src, self)

    def load_from_file(self, connection, json_string, filename):
        "Clears any existing cells and replaces them with ones loaded from file"
        self.clear_notebook(connection)

        with open(filename, 'r') as f:
            nb = nbformat.v4.reads(f.read())

        self.metadata = nb.metadata
        dict_cells = json.loads(json_string)
        if len(nb.cells) != len(dict_cells):
            raise Exception('Notebook length does not match provided JSON spec')

        for (pos, (dict_cell, nb_cell)) in enumerate(zip(dict_cells, nb.cells)):
            if nb_cell.cell_type == 'code':
                dict_cell['outputs'] = nb_cell.outputs

            dict_cell['position'] = pos
            self.add_cell(connection, **dict_cell)

    def reorder_cells(self, connection, positions):
        assert len(positions) == len(self.cells)
        reordered = []
        for _, target in sorted([(pos,ind) for (ind,pos) in enumerate(positions)]):
            reordered.append(self.cells[target])
        self.cells = reordered
        self.message(connection,'reorder_cells', {'positions':positions})


    def clear_all_cell_outputs(self, connection):
        for pos in range(len(self.cells)):
            self.clear_cell_output(connection, pos)


    def clear_notebook(self, connection):
        self.cells = []
        self.message(connection,'clear_notebook', {})


    #=============#
    # IO Commands #
    #=============#

    # TODO: Can simplify/unify the download commands
    def download_python_notebook(self, connection):
        self.message(connection, 'download_python_notebook',
                     {'filename':'python_notebook.py',
                      'data':self.pretty})

    def download_cleared_notebook(self, connection):
        nb = nbformat.v4.new_notebook()
        nb['cells'] = [cell.node(cleared=True) for cell in self.cells]
        self.message(connection, 'download_python_notebook',
                     {'filename':'cleared_notebook.ipynb',
                      'data':nbformat.writes(nb)})

    def download_full_notebook(self, connection):
        nb = nbformat.v4.new_notebook()
        nb['cells'] = [cell.node(cleared=False) for cell in self.cells]
        self.message(connection, 'download_python_notebook',
                     {'filename':'full_notebook.ipynb',
                      'data':nbformat.writes(nb)})


    def update_style(self, connection, css):
        if css is None: # Use stored CSS if the tab is reloaded
            css = self.css
        else:
            self.css = css
        lines = css.splitlines()

        self.message(connection, 'update_style', {'css':"\n".join(lines[1:-1])})



class ExecutableNotebook(Notebook):
    """
    ExecutableNotebook is a notebook with executable state i.e a Python
    kernel along with methods for executing code.
    """
    def __init__(self, executor_init, name=None, **kwargs):
        self.executor_init = executor_init
        self.executor = None # Initialized by start_kernel
        self.completion_info = None
        super(ExecutableNotebook, self).__init__(name=name, **kwargs)
        self.exec_commands = {
            'exec_silently':    self.exec_silently,
            'exec_cell':        self.exec_cell,
            'exec_cell_by_line':self.exec_cell_by_line,
            # For JS testing only...
            'add_cell_exec':    self.add_cell_exec,
            'comm_open':        self.comm_open,    # Request from JS to open comm
            'comm_msg' :        self.comm_msg, # Message send from JS
            'start_kernel':     self.start_kernel,
            'interrupt_kernel': self.interrupt_kernel,
            'restart_kernel':   self.restart_kernel,
            'complete':         self.complete
        }
        self.commands.update(self.exec_commands)

    def complete(self, connection, line_number, line_context, position):
        cell_index =  self.by_line(line_number)
        cell_source = self.cells[cell_index].source

        relative_position = 0
        for line in cell_source.split("\n"):
            if line.startswith(line_context):
                relative_position += len(line_context)
                break
            relative_position += len(line) +1 # +1 to compensate for newlines

        self.completion_info = {'cell_source':cell_source,
                                'position': position,
                                'relative_position':relative_position}
        self.executor.complete(cell_source, relative_position)

    def dispatch(self, connection, payload):
        kernel_required = [cmd for cmd in self.exec_commands if cmd != 'start_kernel']
        if self.executor is None and (payload['cmd'] in kernel_required):
            logging.info("WARNING: Kernel needs to be started before execution can occur")
            return
        super(ExecutableNotebook, self).dispatch(connection, payload)

    def exec_silently(self, connection, code):
        self.executor(code, stop_on_error=False, cell=None, silent=True)

    def comm_open(self, connection, target_name, comm_id, data=None, metadata=None):
        # Request to open a comm from JS
        self.executor.comms.comm_open(comm_id=comm_id,
                                      target_name=target_name,
                                      data=data, metadata=metadata)

    def comm_msg(self, connection, target_name, comm_id, data, metadata=None):
        # Message from JS comm to Python
        self.executor.comms.comm_msg(comm_id=comm_id,
                                     target_name=target_name,
                                     data=data, metadata=metadata)

    def exec_cell(self, connection, position):
        if position >= len(self.cells):
            logging.info("WARNING: Position %d is out of bounds" % position)
            return
        cell = self.cells[position]
        if cell.mode == 'markdown': return
        if cell.outputs:
            self.clear_cell_output(connection, position)

        self.executor(cell.source, stop_on_error=True, cell=cell)


    def add_cell_exec(self, connection, source, input=None,
                      mode='code', prompt=None, position=None):
        """
        Needed to solve concurrency issues in JS where add_cell messages arrive
        before the corresponding executions. Should only be used for JS tests.
        """
        position = len(self.cells) if position is None else position
        self.add_cell(connection, source, input, mode, prompt, position)
        self.exec_cell(connection, position)


    def exec_cell_by_line(self, connection, line_number):
        position =  self.by_line(line_number)
        if position is None: return
        self.exec_cell(connection, position)

    def start_kernel(self, connection, cwd):
        exec_cls, name, queue = self.executor_init
        self.executor  = exec_cls(name, queue)
        self.executor.start(cwd)

    def interrupt_kernel(self, connection):
        self.executor.interrupt_kernel()

    def restart_kernel(self, connection):
        self.executor.restart_kernel()


    def update_style(self, connection, css):
        if self.executor and css:
            lines = css.splitlines()
            process_css(self, connection, "\n".join(lines[1:-1]))
        super(ExecutableNotebook, self).update_style(connection, css)




class MirrorBuffer(object):

    def __init__(self, hold_mode='off'):
        self.buff = ''
        self._hold = False
        self.hold_mode = hold_mode # May be 'on', 'off' or 'auto'

    def clear(self):
        self.buff = ''

    @property
    def hold(self):
        """
        Sync can't distinguish a cut and paste (i.e a move) from an
        initial deletion without the concept of a 'hold'. The hold sapplies
        if a cell prompt initially appears to be deleted
        """
        return self._hold


    def _check_hold(self, start, length, added):
        if self.hold_mode in ['on','off']:
            self._hold = (self.hold_mode == 'on')
        elif added != "" or length < 5:
            self._hold = False
        else:
            deletion = self.buff[start-1:start+length-1]
            self._hold = deletion.strip().startswith(Cell.code_startswith)

    def __call__(self, start, end, length, added, size):
        self._check_hold(start, length, added)
        if length-1 >= 0:
            self.buff = self.buff[:start-1] + self.buff[start+length-1:]
        self.buff = self.buff[:start-1] + added + self.buff[start-1:]

        if len(self.buff) != size:
            logging.warning("Mirror buffer size is mismatched by %d chars" %
                            abs(len(self.buff) - size))
    def __str__(self):
        return self.buff



class SyncNotebooks(object):
    """
    The SyncNotebooks utility class has a single entrypoint, namely the
    'sync' classmethod used to compute the commands needed to make two
    notebooks consistent.

    The sync method calls commands on the 'dst' notebook needed to make
    it match the 'src' notebook. The commands it uses includes add_cell,
    remove_cell and reorder_cells.

    This utility is used to sync the state of the notebook as declared
    by the editor (i.e emacs) with the state of the notebook in the
    client.
    """


    @classmethod
    def deduplicate_hashes(cls, src, dst):
        """
        Everything mostly works if this simply returns (src.hashes,
        dst.hashes) although cells without prompts and the same source
        will be collapsed into a single cell.

        The solution used here is to deduplicate the hashes in this
        situation as cells without prompts have no output so it does not
        matter that the hashes don't match as there is no output to preserve.
        """
        src_hashes, dst_hashes = src.hashes, dst.hashes
        dedupe_src_hashes, dedupe_dst_hashes = [], []

        src_dupes = set([h for h in src_hashes if src_hashes.count(h) > 1])
        dst_dupes = set([h for h in dst_hashes if dst_hashes.count(h) > 1])

        duplicates = src_dupes | dst_dupes

        for count, (hsh, cell) in enumerate(zip(src_hashes, src.cells)):
            dedupe_src_hashes.append(hsh + count if
                                     (hsh in duplicates and cell.prompt is None) else hsh)

        for count, (hsh, cell) in enumerate(zip(dst_hashes, dst.cells)):
            dedupe_dst_hashes.append(hsh + count if
                                     (hsh in duplicates and cell.prompt is None) else hsh)
        return dedupe_src_hashes, dedupe_dst_hashes


    @classmethod
    def sync(cls, connection, src, dst):
        hashes = cls.deduplicate_hashes(src, dst)
        src_hashes, dst_hashes = hashes

        src_set, dst_set = set(src_hashes), set(dst_hashes)
        additions = src_set - dst_set
        deletions =  dst_set - src_set

        if src_hashes == dst_hashes: # Same cells, same order : no change
            pass
        elif src_set == dst_set: # Same cells, different order
            cls.reorderings(connection, src, dst, hashes)
        elif len(src_set) == len(dst_set):
            cls.updates(connection, src, dst, additions, deletions, hashes)
        elif additions and not deletions:
            cls.additions(connection, src, dst, additions, hashes)
        elif deletions and not additions:
            cls.deletions(connection, dst, deletions, hashes)

        # As this all works via hashes on the stripped source, we need to
        # transfer the exact source lines across to preserve newlines and
        # to correctly infer cell positions from line numbers.
        if len(src.cells) != len(dst.cells):
            logging.info('WARNING: Cell length mismatch %r vs %r' % (src.cells, dst.cells))
        for src_cell, dst_cell in zip(src.cells, dst.cells):
            dst_cell.source = src_cell.source


    @classmethod
    def updates(cls, connection, src, dst, additions, deletions, hashes):
        src_hashes, dst_hashes = hashes

        add_set = {src_hashes.index(h) for h in additions}
        del_set = {dst_hashes.index(h) for h in deletions}
        if add_set.symmetric_difference(del_set):
            logging.info("Sync cannot resolve all cell updates")

        for pos in sorted(add_set.intersection(del_set)):
            dst.update_cell_input(connection,  src.cells[pos].source,
                                  pos, src.cells[pos].input)

    @classmethod
    def reorderings(cls, connection, src, dst, hashes):
        src_hashes, dst_hashes = hashes
        positions = []
        for hsh in dst_hashes:
            position = src_hashes.index(hsh)
            positions.append(position)
        dst.reorder_cells(connection, positions)

    @classmethod
    def additions(cls, connection, src, dst, additions, hashes):
        "When src is the same as dst with some new cells"
        src_hashes, dst_hashes = hashes
        added = []
        for addition in additions:
            ind = src_hashes.index(addition)
            cell = src.cells[ind]
            added.append((ind, cell))

        for ind, cell in sorted(added):
            dst.add_cell(connection, cell.source,
                         cell.input, mode=cell.mode,
                         position=ind, outputs=cell.outputs)

    @classmethod
    def deletions(cls, connection, dst, deletions, hashes):
        "When src is the same as dst with some cells deleted"
        src_hashes, dst_hashes = hashes
        for deletion in deletions:
            ind = dst_hashes.index(deletion)
            dst.remove_cell(connection, ind)


class ParseNotebook(object):
    """
    Given the plaintext notebook format, parse out the markdown and code
    cells e.g:

    python cells.py ../emacs/tests/splitting.py
    """

    @classmethod
    def normalize(cls, code):
        "Strip newlines so parsed code matches contents of JSON notebooks"
        if code.startswith('\n'):
            code = code[1:]
        if code.endswith('\n\n'):
            code = code[:-2]
        elif code.endswith('\n'):
            code = code[:-1]
        return code

    @classmethod
    def extract_markdown_cells(cls, source):
        accumulator = ''
        extracted = []
        opened = False
        for line in source.splitlines(keepends=True):
            if line.startswith(Cell.md_startswith):
                if opened and accumulator.strip():
                    extracted.append({'mode':'markdown',
                                      # Remove newlines on quote lines
                                      'source':accumulator[1:-1]})
                elif accumulator.strip():
                    extracted.append({'mode':'code',
                                      'source': cls.normalize(accumulator)})
                accumulator = line[3:]
                opened = not opened
            else:
                accumulator += line
        if accumulator.strip() and opened:
            extracted.append({'mode':'markdown', 'source':accumulator})
        elif accumulator.strip():
            extracted.append({'mode':'code',
                              'source' : cls.normalize(accumulator)})
        return extracted

    @classmethod
    def extract_code_cells(cls, source):
        extracted = []
        accumulator = ''
        count, started = None, False
        for line in source.splitlines(keepends=True):
            match = re.match(Cell.code_prompt_regexp, line)
            if match:
                # Don't accumulate till the first cell prompt appears
                if started is False: accumulator=''
                started = True
                if accumulator.strip():
                    extracted.append({'mode':'code',
                                      'source' : cls.normalize(accumulator),
                                      'prompt':count})

                accumulator = re.sub(Cell.code_prompt_regexp, '', line)
                if len(accumulator) > 1 and accumulator[0] == '\n':
                    accumulator = accumulator[1:]  # Remove newline after prompt
                raw_count = match.groups()[0]
                try:
                    count = int(raw_count) if raw_count not in [None, ' '] else None
                except Exception as e:
                    logging.info('WARNING: Parse failure for prompt count %r' % str(e))

            elif started:
                accumulator += line

        if accumulator and started:
            extracted.append({'mode':'code', 'prompt':count,
                              'source' : cls.normalize(accumulator)})
        return extracted


    @classmethod
    def unique_prompts(cls, cells):
        "Ensures prompts are either unique or None"
        unique, seen = [], []
        for cell in cells:
            if cell['mode'] != 'code':
                unique.append(cell)
                continue
            elif cell['prompt'] in seen:
                cell['prompt'] = None
            else:
                seen.append(cell['prompt'])
            unique.append(cell)
        return unique

    @classmethod
    def extract_cells(cls, source, unique_prompts=True):
        cells = cls.extract_markdown_cells(source)
        extracted = []
        for cell in cells:
            if cell['mode'] == 'markdown':
                extracted.append(cell)
            if cell['mode'] == 'code':
                code_cells = cls.extract_code_cells(cell['source'])
                for code_cell in code_cells:
                    extracted.append(code_cell)

        return cls.unique_prompts(extracted) if unique_prompts else extracted
