import os, sys
from .server import serve, WS, session

__version__ = 'v0.0.7'


def server_status(port, host='localhost'):
    import socket
    from contextlib import closing
    with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as sock:
        if sock.connect_ex((host, port)) == 0:
            print("port unavailable")
