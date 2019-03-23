#!/usr/bin/env python

import logging
import tornado
import tornado.web
from tornado import httpserver
from tornado import ioloop
from tornado import websocket

import os
import sys
import json
import webbrowser
import nbformat
from queue import Queue

from .execute import ThreadedExecutor
from .cells import ExecutableNotebook


STATIC_PATH = os.path.join(os.path.split(__file__)[0], 'client')
ExecutableNotebook.STATIC_PATH = STATIC_PATH

class PeriodicOutputCallback(object):
    """
    Sets up a periodic callback to push output to cells by polling from
    the queue pushed to by the ThreadedExecutor.
    """

    def __init__(self, server, period=20):
        self.server = server
        self.notebook = None
        self.period = period

    def switch_notebook(self, notebook):
        self.notebook = notebook

    def start(self):
        self.callback = ioloop.PeriodicCallback(self.__call__, self.period)
        self.callback.start()

    def stop(self):
        self.callback.stop()

    def __call__(self):
        "Processes queue pushed to by ThreadedExecutor"
        try:
            val = self.server.queue.get_nowait()
            self.server.queue.task_done()
            result, status  = val
        except:
            return

        connection = (self.server.BROWSER_CONNECTIONS[0]
                      if self.server.BROWSER_CONNECTIONS else None)

        if status == 'completion':
            editor_connection = self.server.EDITOR_CONNECTIONS[0]
            position = self.notebook.completion_info['position']
            relative_position = self.notebook.completion_info['relative_position']

            # Adjusted for emacs point position
            start_delta = relative_position - result['cursor_start']
            end_delta = relative_position - result['cursor_end']
            result['cursor_start'] = position - start_delta
            result['cursor_end'] = position - end_delta

            editor_connection.write_message(json.dumps(result))
            self.notebook.completion_info = None
            return

        if connection and (status == 'comm_open'):
            logging.info("REQUEST TO OPEN COMM FOR JS: %s" % result)
            self.notebook.message(connection, 'comm_open', result['content'])
            # e.g:
            # {'data': {}, 'comm_id': 'ee0a39d3728945cdb4ad30848b7856fc',
            #  'target_name': 'ZOO', 'target_module': None}
            return
        elif connection and (status == 'comm_msg'):
            buffers = result['buffers']
            metadata = result.get('metadata', {})
            self.notebook.message(connection, 'comm_msg', # FIXME: redundant 'comm_msg'
                                  {'msg_type': 'comm_msg',
                                   'metadata': metadata,
                                   'content': result['content']},
                                  buffers=buffers)
            return


        else:
            outnode, execution_count = result, status

        if connection:
            cell = self.notebook.find_cell(execution_count)
            if cell is None: return # There may be no cell if running a silent execution
            position = self.notebook.cell_position(cell)

            if execution_count is None:
                # For silent execution before *any* output
                return
            # What about silent execution after start?
            self.notebook.update_cell_outputs(connection, position, outnode)


class Server(websocket.WebSocketHandler):

    BROWSER_CONNECTIONS = []
    EDITOR_CONNECTIONS = []

    NOTEBOOK = None
    NOTEBOOKS = {}

    def open(self):
        self.queue = Queue()
        self.output_callback = PeriodicOutputCallback(self)
        self.output_callback.start()
        logging.info("Connection opened")


    def toggle_notebook(self, name):
        notebook = self.NOTEBOOKS.get(name, None)

        if notebook is None:  # Create notebook
            # Note that there are multiple Server instances and we want only one notebook!
            # (for now)
            notebook = ExecutableNotebook(
                (ThreadedExecutor, "threaded-kernel", self.queue),
                name=name, cells=list())
            self.NOTEBOOKS[name] = notebook

        Server.NOTEBOOK = notebook
        self.output_callback.switch_notebook(notebook)


    def on_message(self, message):
        "Websocket on_message handler. Tracks connection type."
        try:
            payload = json.loads(message)
        except Exception as e:
            logging.info('JSON parse exception: %s' % str(e))
            return

        if 'cmd' in payload:
            if payload['cmd'] in ['start_mirror']: # Verbose commands
                logging.info(u"Received %s command" % payload['cmd'])
            else:
                logging.info(u"Received message: {0}".format(message))

        if payload.get('init', False) == 'editor':
            self.EDITOR_CONNECTIONS.append(self)
            assert len(self.EDITOR_CONNECTIONS) == 1, "Only one editor connection expected"
            return

        if payload.get('init', False) == 'browser':
            self.BROWSER_CONNECTIONS.append(self)
            logging.info('Added browser client connection')
            if len(Server.NOTEBOOK.cells) > 0: # TODO: Needs updating
                logging.info("Restart with previously opened notebook")
                Server.NOTEBOOK.reload(self)
                # If you hit reload in the browser, the CSS needs to be re-sent
                Server.NOTEBOOK.update_style(self, css=None)
            return

        # SOME COMMANDS (e.g mirroring) should happen even without a browser tab open!
        connection = self.BROWSER_CONNECTIONS[0] if len(self.BROWSER_CONNECTIONS) else None
        self.toggle_notebook(payload['name'])

        if payload.get('cmd', False) == 'reload_page':
            # Reload over the browser connection (currently assuming only one)
            if len(self.BROWSER_CONNECTIONS) > 0:
                Server.NOTEBOOK.reload(self.BROWSER_CONNECTIONS[0])
            return

        Server.NOTEBOOK.dispatch(connection, payload)


    def check_origin(self, origin):
        return True

    def on_close(self):
        logging.info("ON_CLOSE")
        if self in self.BROWSER_CONNECTIONS:
            self.BROWSER_CONNECTIONS.remove(self)

        self.output_callback.stop()

def serve(ws_port=9999, html_port=8000):
    import tornado.options
    tornado.options.parse_command_line()
    html_handler = (r'/(.*)', tornado.web.StaticFileHandler,
                    {'path': STATIC_PATH})
    tornado.web.Application([html_handler]).listen(html_port)
    ws_server = httpserver.HTTPServer(tornado.web.Application([(r"/", Server)]))
    ws_server.listen(ws_port, "127.0.0.1")
    logging.info("STARTED: Server start listening")
    ioloop.IOLoop.instance().start()

if __name__ == "__main__":
    serve()
