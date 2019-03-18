import os, time, logging, json, uuid, weakref
import atexit

from collections import OrderedDict

import warnings

class VisibleDeprecationWarning(UserWarning):
    """Warning issued by jupyter_client 5.2.4 about future deprecations in tornado"""
    pass

# Hopefully jupyter_client will keep working with latest tornado
with warnings.catch_warnings():
    warnings.filterwarnings("ignore", category=VisibleDeprecationWarning)
    warnings.simplefilter("ignore")
    from jupyter_client import KernelManager
    from jupyter_client.threaded import ThreadedZMQSocketChannel, ThreadedKernelClient


from nbformat.v4 import output_from_msg
from queue import Queue

class Channel(ThreadedZMQSocketChannel):

    executions = None
    queue = None

    channels = weakref.WeakSet()

    def __init__(self, *args, **kwargs):
        super(Channel, self).__init__(*args, **kwargs)
        self.channels.add(self)

    @classmethod
    def execution_number(cls):
        "Gets the execution count if available on any of the channels"
        counts = [c.executions for c in cls.channels if c.executions]
        return counts[0] if len(counts)==1 else None


    def call_handlers(self, msg):
        msg_type = msg.get('msg_type',None)
        content = msg.get('content', None)
        buffers = msg.get('buffers', [])
        metadata = msg.get('metadata', None)

        if msg_type == 'complete_reply':
            filtered = {}
            for field in ['matches', 'cursor_start', 'cursor_end']:
                if field == 'matches':
                    value = [el for el in content["matches"] if el.endswith("=")]
                else:
                    value = content[field]
                filtered[field] = value

            self.queue.put((filtered, "completion"))
            return

        node = None
        if msg_type == 'execute_input':
            assert 'execution_count' in content
            try:
                executions = int(content['execution_count'])
                self.executions = executions
            except:
                logging.info('ERROR: execution_count is not an integer %r' %
                      content['execution_count'])
                self.executions = None

        elif msg_type == 'error':
            node = output_from_msg(msg)
        # Capture print output
        elif msg_type == 'stream':
            node = output_from_msg(msg)
        elif msg_type == 'status':
            pass # e.g content['execution_state'] == 'idle'

        elif msg_type == "comm_open":
            self.queue.put(({'content':content,
                             'buffers':buffers,
                             'metadata':metadata}, 'comm_open'))
            return
        elif msg_type == "comm_msg":
            self.queue.put(({'content':content,
                             'buffers':buffers,
                             'metadata':metadata}, 'comm_msg'))
            return
        elif msg_type.startswith('comm'):
            logging.info("Unhandled 'comm' message of type {msg_type} with {content}".format(
                msg_type=msg_type,
                content=content))

        if msg_type in {'execute_result', 'display_data', 'update_display_data'}:
            if msg_type == 'update_display_data':
                logging.info("Unhandled 'update_display_data' message")
            # Should filter text/plain and text/html at this level
            # print(output_from_msg(msg)['data'])

            node = output_from_msg(msg)

        if node:
            self.queue.put((node, self.executions))
        elif msg_type == 'execute_reply':
            self.queue.put((None, self.execution_number()))


class Client(ThreadedKernelClient):
    iopub_channel_class = Channel
    shell_channel_class = Channel
    stdin_channel_class = Channel


class ThreadedExecutor(object):

    def __init__(self, name, queue=Queue()):
        self.name = name

        self.comms = Comms(self)
        self.km = None
        self.kc = None

        self.execution_count = 0

        Channel.queue = queue

    def start(self, cwd=None):
        self.km = KernelManager(kernel_name='python',
                                client_class='nei.execute.Client')
        self.km.start_kernel(**({} if cwd is None else {'cwd':cwd}))
        self.kc = self.km.client()
        self.kc.start_channels()
        time.sleep(2)
        atexit.register(self.shutdown_kernel)

    def reset(self):
        Channel.executions = 0

    def restart_kernel(self):
        self.execution_count = 0
        self.km.restart_kernel()
        Channel.executions = 0

    def interrupt_kernel(self):
        self.km.interrupt_kernel()

    def shutdown_kernel(self): # TODO: Shutdown kernel but keep nei running
        self.execution_count = 0
        self.km.request_shutdown()
        self.km.cleanup()
        self.km.finish_shutdown()
        logging.info('Shutting down')

    def kernel_info(self):
        self.kc.kernel_info()

    def complete(self, code, position):
        self.kc.complete(code, position)

    def __call__(self, code, stop_on_error=True, cell=None, silent=False):
        "If stop_on_error is True, execution may stop on exceptions"

        if not silent:
            self.execution_count += 1

        if cell:
            cell.prompt = self.execution_count
        self.kc.execute(code,
                        silent=silent,
                        store_history=True, # Has to be true to make execution counter work
                        allow_stdin=False,
                        stop_on_error=stop_on_error)


"""
NOTES

A RemoteExecute kernel could connect via a port to a remote kernel instance:

http://www.giantflyingsaucer.com/blog/?p=4602
http://www.tornadoweb.org/en/stable/websocket.html#tornado.websocket.websocket_connect
https://github.com/ilkerkesen/tornado-websocket-client-example/blob/master/client.py

Is get_iopub_msg a useful method?
"""


class Comms(object):

    def __init__(self, executor):
        self.executor = executor
        self.opened = set()

    def _publish_msg(self, cmd, comm_id, data=None, metadata=None, buffers=None, **keys):
        target_module = '' # requirejs module from which to load comm target
        content = dict(data        = data if data else {},
                       comm_id     = comm_id,
                       buffers     = buffers, # TODO: Needed for Bokeh
                       **keys)  # Includes target_name

        session = self.executor.km.session
        shell_channel_socket = self.executor.kc.shell_channel.socket
        session.send(shell_channel_socket, cmd, json.dumps(content))

    def comm_open(self, comm_id, target_name, data=None, metadata=None,
                  buffers=None, **keys):
        self.opened.add(comm_id)
        self._publish_msg('comm_open', comm_id, target_name=target_name,
                          data=data, metadata=metadata, buffers=buffers)

    def comm_msg(self, comm_id, target_name, data=None, metadata=None
                 , buffers=None, **keys):
        logging.info("Sending message to Python comm %s." % comm_id)
        self._publish_msg('comm_msg', comm_id, target_name=target_name,
                          data=data, metadata=metadata, buffers=buffers)
