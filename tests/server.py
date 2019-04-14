import tornado
from tornado import testing
import json
from nei import WS, session
import logging

class TestServer(tornado.testing.AsyncHTTPTestCase):

    def setUp(self):
        super(TestServer,self).setUp()

    def tearDown(self):
        super(TestServer,self).tearDown()

    def get_app(self): # Set up a dummy server per test
        return tornado.web.Application([(r"/", WS)])

    @tornado.testing.gen_test
    def test_one(self):

        ws_url = "ws://localhost:%s" % str(self.get_http_port())
        ws_client = yield tornado.websocket.websocket_connect(ws_url)
        ws_client.write_message(json.dumps({'cmd':'reset_server'}))
        ws_client.write_message(json.dumps({'init':'browser'}))
        msg = {'cmd':'add_cell',
               'name':'test-buffer',
               'args': {'source':"This is a code cell", "mode":"code"}}
        ws_client.write_message(json.dumps(msg))

        response = yield ws_client.read_message()
        self.assertEqual(json.loads(response)['args']['source'], msg['args']['source'])
        str_val = json.dumps({'cmd':'server_info', 'args':{}, 'name':'test-buffer'})
        ws_client.write_message(str_val)
        response = yield ws_client.read_message()

        self.assertEqual(json.loads(response)['args']['text'],
                         "# In[ ]\nThis is a code cell")

    @tornado.testing.gen_test
    def test_two(self):

        ws_url = "ws://localhost:%s" % str(self.get_http_port())
        ws_client = yield tornado.websocket.websocket_connect(ws_url)
        ws_client.write_message(json.dumps({'cmd':'reset_server'}))
        ws_client.write_message(json.dumps({'init':'browser'}))
        msg = {'cmd':'add_cell',
               'name':'test-buffer',
               'args': {'source':"This is a different code cell", "mode":"code"}}
        ws_client.write_message(json.dumps(msg))
        ws_client.write_message(json.dumps({'cmd':'server_info', 'args':{},
                                            'name':'test-buffer'}))
        response = yield ws_client.read_message()
        response = yield ws_client.read_message()

        self.assertEqual(json.loads(response)['args']['text'],
                         "# In[ ]\nThis is a different code cell")


    @tornado.testing.gen_test
    def test_three(self):
        ws_url = "ws://localhost:%s" % str(self.get_http_port())
        ws_client = yield tornado.websocket.websocket_connect(ws_url)
        ws_client.write_message(json.dumps({'cmd':'reset_server'}))
        ws_client.write_message(json.dumps({'init':'browser'}))
        ws_client.write_message(json.dumps({'cmd':'start_mirror',
                                            'args':dict(text=""),
                                            'name':'test-buffer'}))
        ws_client.write_message(json.dumps({'cmd':'mirror',
                                            'args':dict(start=1,end=1, length=0,
                                                        added="", size=0),
                                            'name':'test-buffer'}))

        ws_client.write_message(json.dumps({'cmd':'server_info', 'args':{},
                                            'name':'test-buffer'}))
        response = yield ws_client.read_message()
        self.assertEqual(json.loads(response)['args']['text'], "")
