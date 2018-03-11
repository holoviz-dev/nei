'use strict';


import {download_file, update_style} from './util.js'

export class CommLink {
  constructor(app, notebook, server, port=9999) {
    this.app = app;
    this.socket = new WebSocket("ws://localhost:9999");
    // this.socket = new WebSocket("ws://"+server+":"+port+"/ws");
    this.setup(this.socket)

    this.notebook = notebook;
    }

    setup(socket) {
        socket.onopen =    (e) => this.socket_onopen(e);
        socket.onerror =   (e) => this.socket_onerror(e);
        socket.onmessage = (e) => this.socket_onmessage(e);
        socket.onclose =   (e) => this.socket_onclose(e);
    }

    socket_onopen(e) {
     console.log('Socket opened');
     this.socket.send('{"init":"browser"}');
    }

    socket_onclose(e) {
        console.log('Socket closed');
    }

    socket_onerror(e) {
        console.log('Socket error');
    }

  socket_onmessage(e) {
    this.onmessage(JSON.parse(e.data));
  }

  send_message(cmd, args) {
    let json_args = JSON.stringify(args);
    if (this.socket.readyState === 1) {
      this.socket.send(`{"cmd":"${cmd}", "args":${json_args}}`);
    }
  }

  onmessage(json) {
    // Remove later
    let deprecated = ['append_cell', 'append_md_cell', 'swap_cells'];
    if (deprecated.includes(json.cmd)) {
      console.log(`Command ${json.cmd} is deprecated.`);
      return
    }

    // Dispatch commands
    let commands = ['add_cell', 'remove_cell', 'remove_cells',
                    'update_cell', 'update_cell_outputs', 'update_cell_input',
                    'clear_notebook', 'reorder_cells', 'clear_cell_output',
                    'update_style',
                    // Download
                    'download_python_notebook',
                    'download_cleared_notebook',
                    'download_full_notebook',
                    // Comms
                    'comm_msg'];
    // TODO: 'insert_cell', 'append_cells'
    if (!commands.includes(json.cmd)) {
      console.log(`Command ${json.cmd} is not one of ${commands}`);
      return
    }
    let message = `Running command ${json.cmd} with args ${JSON.stringify(json.args)}`;
    console.log( (message.length < 200) ? message : message.slice(0,200));

    if (json.cmd == 'comm_msg') {
      let {data, comm_id} = json.args;
      console.log(`COMM ${comm_id} SAYS ${data}`);
    }

    if (json.cmd == 'add_cell') {
        let {mode, source, input, outputs, position} = json.args;
        let cell = new Cell(mode, source, input, outputs);
        this.notebook.add_cell(cell, position);
    }
    if (json.cmd == 'remove_cell') {
        this.notebook.remove_cell(json.args.position);
    }
    else if (json.cmd == 'remove_cells') {
      this.notebook.remove_cells(json.args.positions);
    }
    else if (json.cmd == 'reorder_cells') {
      this.notebook.reorder_cells(json.args.positions);
    }
    else if (json.cmd == 'update_cell') {
      let {position, source, input, outputs, prompt} = json.args;
      this.notebook.update_cell(position, source, input, outputs, prompt);
    }
    else if (json.cmd == 'update_cell_outputs') {
      let {position, outputs, prompt} = json.args;
      this.notebook.update_cell_outputs(position, outputs, prompt);
    }
    else if (json.cmd == 'clear_cell_output') {
      let {position} = json.args;
      this.notebook.clear_cell_output(position);
    }
    else if (json.cmd == 'update_cell_input') {
      let {position, source, input} = json.args;
      this.notebook.update_cell_input(position, source, input);
    }
    else if (json.cmd == 'clear_notebook') {
      this.notebook.clear_notebook();
    }
    // Download commands
    else if (['download_python_notebook',
              'download_cleared_notebook',
              'download_full_notebook'].includes(json.cmd)) {
      let {filename, data} = json.args;
      download_file(filename, data);
    }
    else if (json.cmd == 'update_style') {
      let {css} = json.args;
      update_style(css);
    }
  }
}
