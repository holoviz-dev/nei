import re
import logging

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
    def extract_markdown_cells(cls, source, md_startswith):
        accumulator = ''
        extracted = []
        opened = False
        for line in source.splitlines(keepends=True):
            if line.startswith(md_startswith):
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
    def extract_code_cells(cls, source, code_prompt_regexp):
        extracted = []
        accumulator = ''
        count, started = None, False
        for line in source.splitlines(keepends=True):
            match = re.match(code_prompt_regexp, line)
            if match:
                # Don't accumulate till the first cell prompt appears
                if started is False: accumulator=''
                started = True
                if accumulator.strip():
                    extracted.append({'mode':'code',
                                      'source' : cls.normalize(accumulator),
                                      'prompt':count})

                accumulator = re.sub(code_prompt_regexp, '', line)
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
    def extract_cells(cls, source, cell_cls, unique_prompts=True):
        cells = cls.extract_markdown_cells(source, cell_cls.md_startswith)
        extracted = []
        for cell in cells:
            if cell['mode'] == 'markdown':
                extracted.append(cell)
            if cell['mode'] == 'code':
                code_cells = cls.extract_code_cells(cell['source'],
                                                    cell_cls.code_prompt_regexp)
                for code_cell in code_cells:
                    extracted.append(code_cell)

        return cls.unique_prompts(extracted) if unique_prompts else extracted
