"""
Python interface from Vim to the NEI server
"""

reselect = False            # reselect lines after sending from Visual mode


class Stub(object):
    def __getattribute__(self, key):
        return lambda *args: '0'
try:
    import vim
except ImportError:
    vim = Stub()
    print("Error: Needs to be run inside vim")

import sys
import json
from tornado.ioloop import IOLoop
from tornado import gen
from tornado.websocket import websocket_connect


# get around unicode problems when interfacing with vim
if isinstance(vim, Stub):
    vim_encoding=vim.eval('&encoding') or 'utf-8'


class Client(object):
    def __init__(self, url):
        self.url = url
        self.ioloop = IOLoop.instance()
        self.ws = None
        self.connect()
        self.ioloop.start()

    @gen.coroutine
    def connect(self):
        try:
            self.ws = yield websocket_connect(self.url)
            self.ws.write_message(json.dumps({'init':'editor'}))
            self.ioloop.stop()
        except Exception as e:
            print("Error: Could not connect")


    @gen.coroutine
    def message(self, cmd, **args):
        self.ws.write_message(json.dumps({'cmd':cmd, 'args':args, 'name':'vim'}))

client = None
import time

def nei_connect_to_server(args=""):
    global client
    client = Client("ws://localhost:9995")

def echo(arg,style="Question"):
    "Useful for sending information back to vim"
    try:
        vim.command("echohl %s" % style)
        vim.command("echom \"%s\"" % arg.replace('\"','\\\"'))
        vim.command("echohl None")
    except vim.error:
        print("-- %s" % arg)


def view_browser():
    global client
    client.message('view_browser', ws_port=9995)

def run_these_lines(dedent=False):
    global client
    r = vim.current.range
    if dedent:
        lines = list(vim.current.buffer[r.start:r.end+1])
        nonempty_lines = [x for x in lines if x.strip()]
        if not nonempty_lines:
            return
        first_nonempty = nonempty_lines[0]
        leading = len(first_nonempty) - len(first_nonempty.lstrip())
        lines = "\n".join(x[leading:] for x in lines)
    else:
        lines = "\n".join(vim.current.buffer[r.start:r.end+1])
    #echo("%s" % lines)
    client.message("start_mirror", text=lines)
    #reselect the previously highlighted block
    vim.command("normal! gv")
    if not reselect:
        vim.command("normal! ")
