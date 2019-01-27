'use strict';
import {range} from './util.js';

export class ScriptTest {

  constructor(commlink) {
    this.commlink = commlink
  }

  _onmessage(cmd, args) {
    this.commlink.onmessage({'cmd':cmd, 'args':args});
  }

  test_simple_console_script() {
    let script = '<script>console.log(3)</script>';
    this._onmessage('add_cell', {mode: 'code',
                                 source:'test3:src',
                                 input:'<i>test3:in</i>',
                                 outputs:[['text/html',
                                           script + '<b>test3:output</b>']]});
  }

  test_simple_function_script1() {
    let script = '<script>function fn(){console.log(42)}</script>';
    this._onmessage('add_cell', {mode: 'code',
                                 source:'fntest:src',
                                 outputs: [['text/html',
                                            script + '<b>fntest1</b>']]});
  }

  test_simple_function_script2() {
    let script = '<script>console.log(fn)</script>';
    this._onmessage('add_cell', {mode: 'code',
                                 source:'fntest2:src',
                                 outputs: [['text/html',
                                            script + '<b>fntest2</b>']]});
  }


  test_simple_function_script_combined() {
    let script1 = '<script>function fn2(){console.log(-42)}</script>';
    let script2 = '<script>console.log(fn2)</script>';
    this._onmessage('add_cell', {mode: 'code',
                                 source:'fntestC:src',
                                 outputs: [['text/html',
                                            script1 + '<b>script1</b>'],
                                           ['text/html',
                                            script2 + '<b>script2</b>']]});
  }

  run() {
    this.test_simple_console_script();
    this.test_simple_function_script1();
    this.test_simple_function_script2();
    this.test_simple_function_script_combined();
  }
}


export class ReceiveTest {
  // Fakes messages from the server

  constructor(commlink) {
    this.commlink = commlink
  }

  _onmessage(cmd, args) {
    this.commlink.onmessage({'cmd':cmd, 'args':args});
  }

  test_additions() {
    this._onmessage('add_cell', {mode: 'code', source:'test1:src'});

    this._onmessage('add_cell', {mode: 'code',
                                 source:'test2:src',
                                 input:'<i>test2:in</i>'});

    this._onmessage('add_cell', {mode: 'code',
                                 source:'test3:src',
                                 input:'<i>test3:in</i>',
                                 outputs:[['text/html',
                                           '<b>test3: HTML</b>']]});

    this._onmessage('add_cell', {mode: 'code',
                                 source:'test4:src',
                                 input:'<i>test4:in</i>',
                                 outputs:[['text/plain',
                                           'test4: <plaintext>'],
                                             ['text/html',
                                              '<b>test4: HTML</b>']]});
  }

  test_deletions() {
    this._onmessage('remove_cells', {positions:[0,1,2,3]});
  }

  test_clear() {
    this._onmessage('clear_notebook', {});
  }

  test_update() {
    this._onmessage('update_cell', {position:2,
                                    source:'test3:NEWsrc',
                                    input: '<i>test3:NEWin</i>',
                                    outputs:[['text/html',
                                              '<b>test3: NEW HTML</b>']]});
  }

  test_update_outputs() {
    this._onmessage('update_cell_outputs', {position:2,
                                            outputs:[['text/html',
                                                      '<b>test3: APPENDED HTML</b>']]});
  }

  test_updates() {
    // console.log('NOT QUITE WORKING - NEED TO BATCH THE UPDATES');
    this._onmessage('update_cell', {position:0, source:'test1:NEWsrc'});
    this._onmessage('update_cell', {position:2,
                                    source:'test3:NEWsrc',
                                    input: '<i>test3:NEWin</i>',
                                    outputs:[['text/html',
                                              '<b>test3: NEW HTML</b>']]});
  }

  test_reorder() {
    this._onmessage('reorder_cells', {positions:[3,2,1,0]});
  }


  test_typing(delay=10) {
    this._onmessage('clear_notebook', {});
    if (notebook.uuids.length == 0) {
      notebook.add_cells([new Cell('code', '')], [0]);
    }
    let string = 'abcdefghijklmnopqrstuvwxyz';
    for (let i of range(string.length)) {
      setTimeout(function(){
        notebook.update_cell_input(0, string.slice(0,i)) }, i*delay)
    }
  }

  run(delay=1000) {
    setTimeout(() => this.test_clear(), delay*0);
    setTimeout(() => this.test_additions(), delay*1);
    setTimeout(() => this.test_deletions(), delay*2);
    setTimeout(() => this.test_additions(), delay*3);
    setTimeout(() => this.test_clear(), delay*4);
    setTimeout(() => this.test_additions(), delay*5);
    setTimeout(() => this.test_update(), delay*6);
    setTimeout(() => this.test_reorder(), delay*7);
    setTimeout(() => this.test_typing(), delay*8);
  }

}



export class MarkdownTest {

  constructor(commlink) {
    this.commlink = commlink
  }

  add_md_cell(source) {
    this._onmessage('add_cell', { source:source, mode: 'markdown'});
  }

  _onmessage(cmd, args) {
    this.commlink.onmessage({'cmd':cmd, 'args':args});
  }

  test_simple_markdown_cell() {
    this.add_md_cell('*some italic text*');
  }

  test_long_markdown_cell() {
    this.add_md_cell(`*italic ...*
... now **bold**

* an item
* another item


1. itemized 1
2. itemized 2

And a link: [holoviews](http://holoviews.org)`);
  }

  test_three_markdown_cells() {
    this.add_md_cell('**some items again**');
    this.add_md_cell(`* item1
* item2
* item3`);
    this.add_md_cell('Link again: [holoviews](http://holoviews.org)');
  }


  test_interleaved_cells() {
    this.add_md_cell('**Here is a code cell:**');
    this._onmessage('add_cell', { mode: 'code',
                                  source: "['code']*3",
                                  outputs:[['text/html',
                                            "['code', 'code', 'code']"]]});
    this.add_md_cell(`And markdown once again:

1. list item 1
2. list item 2
`);
  }

  run(delay=1000) {
    setTimeout(() => this.test_simple_markdown_cell(), delay*0);
    setTimeout(() => this.test_long_markdown_cell(), delay*1);
    setTimeout(() => this.test_three_markdown_cells(), delay*2);
    setTimeout(() => this.test_interleaved_cells(), delay*3);
  }


}


export class ExecutionTest {
  // Fakes messages from the server originating with the editor

  constructor(commlink) {
    this.commlink = commlink
  }

  test_exec_add_cell(mode, args) {
    args["mode"] = mode;
    let json_args = JSON.stringify(args);
    if (this.commlink.socket.readyState === 1) {
      this.commlink.send_message("add_cell_exec", json_args);
    }
  }

  test_simple_execution() { // Sends messages like the editor would which get echoed back
    this.test_exec_add_cell("code", {source:"40+2"});
    this.test_exec_add_cell("code", { source:"42+2",
                                      input:"In[1]: <i>42+2</i>"});
  }


  test_holoviews() {
    this.test_exec_add_cell("code", {"source":"import holoviews as hv"});
  }

  test_holoviews_ext() {
    this.test_exec_add_cell("code", {"source":"hv.extension('bokeh')"});
  }

  test_bokeh_plot() {
    this.test_exec_add_cell("code",  {"source":"hv.Curve([1,2,3,4])"});
  }

  test_print() {
    this.test_exec_add_cell("code",  {"source":"print('hello world')"});
  }

  test_exception() {
    this.test_exec_add_cell("code",  {"source":"1/0"});
  }

  test_holoviews_bokeh() {
    this.test_exec_add_cell("code",  {"source":"import holoviews as hv"});
    this.test_exec_add_cell("code",  {"source":"hv.extension('bokeh')"});
    this.test_exec_add_cell("code",  {"source":"hv.Curve([1,2,3,4])"});
  }


  test_holoviews_bokeh_opts() {
    this.test_exec_add_cell("code",  {"source":"import holoviews as hv"});
    this.test_exec_add_cell("code",  {"source":"hv.extension('bokeh')"});
    this.test_exec_add_cell("code",  {"source":`%%opts Curve (color='red')
hv.Curve([1,2,3,4])`});
  }

  test_holomap() {
    this.test_exec_add_cell("code",  {"source":"import holoviews as hv"});
    this.test_exec_add_cell("code",  {"source":"hv.extension('bokeh')"});
    this.test_exec_add_cell("code",  {"source":"hv.HoloMap({i:hv.Curve([1,2,3*i,4]) for i in range(10)})"});
  }

  test_execution(code) {
    this.test_exec_add_cell("code",  {"source":code});
  }

  test_save_notebook(filename='/tmp/foo.ipynb') {
    let json_args = JSON.stringify({'filename':filename});
    this.commlink.send_message("save_notebook", json_args);
  }

  test_print_loop() {
    let code = `import time
for i in range(7):
   time.sleep(1)
   print(i)`;
    this.test_exec_add_cell("code",  {"source":code});
  }

  test_html_repr() {
    this.test_exec_add_cell("code",
                            {"source":"from IPython.display import HTML; HTML('<b>BOLD</b>')"});
  }

  test_html_display() {
    this.test_exec_add_cell("code",  {"source":"from IPython.display import HTML, display; display(HTML('<i>ITALIC</i>'))"});
  }


  test_html_loop() {
    let code = `import time
from IPython.display import HTML, display

for i in range(4):
   time.sleep(1)
   display(HTML('<i>ITALIC %d</i>' % i))`;
    this.test_exec_add_cell("code",  {"source":code});
  }

  test_interleaved_markdown_execution() {
    this.test_exec_add_cell("code", {"source":"'Markdown next'"});
    this.test_exec_add_cell("markdown", {"source":"***Markdown***"})
    this.test_exec_add_cell("code", {"source":"1-10"});
  }

}


export class KernelTest {

  constructor(commlink) {
    this.commlink = commlink
  }

  test_kernel_restart() {
    this.commlink.send_message("restart_kernel", {});
  }

  test_kernel_interrupt() {
    this.commlink.send_message("interrupt_kernel", {});
  }

  test_comm_open(data, metadata=null) {
    this.commlink.send_message("comm_open", {data: data, metadata : metadata});
  }



}
