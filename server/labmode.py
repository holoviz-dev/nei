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

from execute import ThreadedExecutor
from cells import Notebook


STATIC_PATH = os.path.join(os.path.split(__file__)[0], '..', 'client')

class PeriodicOutputCallback(object):
    """
    Sets up a periodic callback to push output to cells by polling from
    the queue pushed to by the ThreadedExecutor.
    """

    def __init__(self, server, notebook, period=20):
        self.server = server
        self.notebook = notebook
        self.period = period


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
            outnode, execution_count  = val
        except:
            return

        connection = (self.server.BROWSER_CONNECTIONS[0]
                      if self.server.BROWSER_CONNECTIONS else None)
        if connection:
            cell = self.notebook.find_cell(execution_count)
            if cell is None: return # There may be no cell if running a silent execution
            position = self.notebook.cell_position(cell)

            if execution_count is None:
                return # silent execution before *any* output
            if outnode is None and (cell.prompt == execution_count):
                return # no need to update prompt for silent execution
            self.notebook.update_cell_outputs(connection, position, outnode)


class LabServer(websocket.WebSocketHandler):

    BROWSER_CONNECTIONS = []

    NOTEBOOK = None

    def open(self):
        self.queue = Queue()

        # Note that there are multiple LabServer instances and we want only one notebook!
        # (for now)
        if LabServer.NOTEBOOK is None:
            LabServer.NOTEBOOK = Notebook(ThreadedExecutor("threaded-kernel", self.queue))
            LabServer.NOTEBOOK.STATIC_PATH = STATIC_PATH

        self.output_callback = PeriodicOutputCallback(self, LabServer.NOTEBOOK)
        self.output_callback.start()
        logging.info("Connection opened")


    def on_message(self, message):
        "Websocket on_message handler. Tracks connection type."
        logging.info(u"Received message: {0}".format(message))
        try:
            payload = json.loads(message)
        except Exception as e:
            logging.info('JSON parse exception: %s' % str(e))
            return

        if payload.get('init',False):
            if payload['init'] == 'browser':
                self.BROWSER_CONNECTIONS.append(self)
                logging.info('Added browser client connection')
                if len(LabServer.NOTEBOOK.cells) > 0:
                    logging.info("Restart with previously opened notebook")
                    LabServer.NOTEBOOK.reload(self)
                    # If you hit reload in the browser, the CSS needs to be re-sent
                    LabServer.NOTEBOOK.update_style(self, css=None)
                return

        # SOME COMMANDS (e.g mirroring) should happen even without a browser tab open!
        connection = self.BROWSER_CONNECTIONS[0] if len(self.BROWSER_CONNECTIONS) else None
        LabServer.NOTEBOOK.dispatch(connection, payload)


    def check_origin(self, origin):
        return True

    def on_close(self):
        logging.info("ON_CLOSE")
        if self in self.BROWSER_CONNECTIONS:
            self.BROWSER_CONNECTIONS.remove(self)

        self.output_callback.stop()

if __name__ == "__main__":
    import tornado.options
    tornado.options.parse_command_line()


    html_handler = (r'/(.*)', tornado.web.StaticFileHandler,
                    {'path': STATIC_PATH})


    tornado.web.Application([html_handler]).listen(8000)
    ws_server = httpserver.HTTPServer(tornado.web.Application([(r"/", LabServer)]))
    ws_server.listen(9999, "127.0.0.1")
    logging.info("STARTED: Server start listening")
    ioloop.IOLoop.instance().start()
