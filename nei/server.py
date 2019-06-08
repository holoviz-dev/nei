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

class Session(object):
    """
    Global state of the server that doesn't belong to any single
    websocket connection but to the whole server session.
    """
    def __init__(self):
        self._browser = None
        self._editor = None
        self.notebook = None
        self.buffers = {}

    def reset(self):
        self._browser = None
        self.editors = []
        self.notebook = None
        self.buffers = {}

    @property
    def browser(self):
        return self._browser

    @browser.setter
    def browser(self, connection):
        if self._browser is not None:
            logging.info("WARNING: Only one browser connection expected")
        self._browser = connection

    @property
    def editor(self):
        return self._editor

    @editor.setter
    def editor(self, connection):
        if self._editor is not None:
            logging.info("WARNING: Only editor browser connection expected")
        self._editor = connection


session = Session()

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

        if status == 'completion':
            position = self.notebook.completion_info['position']
            relative_position = self.notebook.completion_info['relative_position']

            # Adjusted for emacs point position
            start_delta = relative_position - result['cursor_start']
            end_delta = relative_position - result['cursor_end']
            result['cursor_start'] = position - start_delta
            result['cursor_end'] = position - end_delta

            session.editor.write_message(json.dumps({'cmd':'completion',
                                                     'data': result}))
            self.notebook.completion_info = None
            return

        if session.browser and (status == 'comm_open'):
            logging.info("REQUEST TO OPEN COMM FOR JS: %s" % result)
            self.notebook.message(session.browser, 'comm_open', result['content'])
            # e.g:
            # {'data': {}, 'comm_id': 'ee0a39d3728945cdb4ad30848b7856fc',
            #  'target_name': 'ZOO', 'target_module': None}
            return
        elif session.browser and (status == 'comm_msg'):
            buffers = result['buffers']
            metadata = result.get('metadata', {})
            self.notebook.message(session.browser, 'comm_msg', # FIXME: redundant 'comm_msg'
                                  {'msg_type': 'comm_msg',
                                   'metadata': metadata,
                                   'content': result['content']},
                                  buffers=buffers)
            return


        else:
            outnode, execution_count = result, status

        if session.browser:
            cell = self.notebook.find_cell(execution_count)
            if cell is None: return # There may be no cell if running a silent execution
            position = self.notebook.cell_position(cell)

            if execution_count is None:
                # For silent execution before *any* output
                return
            # What about silent execution after start?
            self.notebook.update_cell_outputs(session.browser, position, outnode)


class WS(websocket.WebSocketHandler):

    def open(self):
        self.queue = Queue()
        self.output_callback = PeriodicOutputCallback(self)
        self.output_callback.start()
        logging.info("Connection opened")


    def toggle_notebook(self, name):
        notebook = session.buffers.get(name, None)

        if notebook is None:  # Create notebook
            # Note that there are multiple connections and we want only one notebook!
            # (for now)
            notebook = ExecutableNotebook(
                (ThreadedExecutor, "threaded-kernel", self.queue),
                name=name, cells=list())
            session.buffers[name] = notebook

        session.notebook = notebook
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
                logging.info(u"Received message: {0:<.100}".format(message))

        if payload.get('cmd') == 'reset_server':
            self.output_callback.stop()
            session.reset()
            return

        if payload.get('init', False) == 'editor':
            logging.info('Added editor client connection')
            session.editor = self
            return

        if payload.get('init', False) == 'browser':
            session.browser = self
            logging.info('Added browser client connection')
            if session.notebook and len(session.notebook.cells) > 0: # TODO: Needs updating
                logging.info("Restart with previously opened notebook")
                session.notebook.reload(self)
                # If you hit reload in the browser, the CSS needs to be re-sent
                session.notebook.update_theme(self, css=None)
            return

        # SOME COMMANDS (e.g mirroring) should happen even without a browser tab open!
        self.toggle_notebook(payload['name'])

        if payload.get('cmd', False) == 'reload_page':
            # Reload over the browser connection (currently assuming only one)
            if session.browser is not None:
                session.notebook.reload(session.browser)
            return

        editor_msg = session.notebook.dispatch(session.browser, payload)
        if (editor_msg is not None) and (session.editor is not None):
            session.editor.write_message(json.dumps(editor_msg))


    def check_origin(self, origin):
        return True


    def on_close(self):
        logging.info("ON_CLOSE")
        if self is session.browser:
            session.browser = None

        self.output_callback.stop()

def serve(ws_port=9999, html_port=8000, host='127.0.0.1'):
    import logging
    logging.basicConfig(level=logging.INFO)
    html_handler = (r'/(.*)', tornado.web.StaticFileHandler,
                    {'path': STATIC_PATH})
    tornado.web.Application([html_handler]).listen(html_port)
    ws_server = httpserver.HTTPServer(tornado.web.Application([(r"/", WS)]))
    ws_server.listen(ws_port, host)
    logging.info("STARTED: Server started and listening")
    ioloop.IOLoop.instance().start()
