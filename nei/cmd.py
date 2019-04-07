import argparse
from .server import serve

parser = argparse.ArgumentParser()


parser.add_argument('--ws-port', action='store', default=9999, type=int,
                    help='Websocket port for editor and client connections.')

parser.add_argument('--html-port', action='store', default=8000, type=int,
                    help='Port used to serve the HTML content.')

parser.add_argument('--host',
                    action='store',
                    default='127.0.0.1',
                    help='IP address the application should listen on.')

args = parser.parse_args()


def main():
    serve(args.ws_port, args.html_port, args.host)
