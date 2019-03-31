'use strict';


import {download_file, update_theme} from './util.js'
import {UUID} from './util.js';
import {serialize, deserialize} from './buffer.js';


import {Notebook} from './cell.js';

export class Comm {
  constructor(manager, target_name, callback=null, comm_id=null) {
    this.manager = manager;
    this.target_name = target_name;
    this.comm_id =  (comm_id === null) ? UUID() : comm_id;

    if (callback != null) {
      this.callback = callback;
    }
    this.get_callback(target_name);
  }

  get_callback(target_name) {
    if (this.manager.targets[target_name] != undefined) {
      this.manager.targets[target_name](this)
    }
  }

  on_msg(callback) {
    this.callback = callback;
  }

  trigger(msg) {  // Trigger the on_msg callback with msg
    this.get_callback(this.target_name);
    if (this.callback != null) {
      this.callback(msg)
    }
  }

  send(data) {
    this.manager.send_message(this.target_name, this.comm_id, data);
  }
}

export class CommManager {
  // Commlink messages sent:
  //   'comm_open' from new_comm
  //   'comm_msg'  from send_message
  // Commlink message received:
  //    'comm_msg' triggers dispatch_message method
  //    'comm_open' triggers the comm_open method
  //
  // TODO: comm_close
  constructor(commlink) {
    this.commlink = commlink;
    this.comms = {};
    this.targets = {};
  }

  register_target(target_name, callback, data={}, metadata={}, comm_id=null) {
    // Registers a comm target on the JS side
    this.targets[target_name] = callback;
  }

  send_message(target_name, comm_id, data, metadata={}) {
    this.commlink.send_message("comm_msg", {target_name:target_name,
                                            comm_id: comm_id,
                                            data: data,
                                            metadata:metadata});
  }

  comm_open(target_name, comm_id, data={}, on_msg=null, metadata={}) {
    let comm = new Comm(this, target_name, on_msg, comm_id);
    this.comms[comm.comm_id] = comm;
    return comm
  }

  // new_comm(target_name, comm_id, data={}, on_msg=null, metadata={}) {
  new_comm(target_name, data, callbacks, metadata, comm_id, buffers=[]) {
    // Note: data, metadata, callbacks and buffers untested
    let comm = this.comm_open(target_name, comm_id, data, callbacks, metadata);
    this.commlink.send_message("comm_open", {target_name: target_name,
                                             comm_id  : comm.comm_id,
                                             data     : data,
                                             metadata : metadata});
    return comm
  }

  dispatch_message(msg, comm_id) { // Commlink broadcasts message to appropriate comm
    let comm = this.comms[comm_id];
    comm.trigger(msg);
  }
}


function notebook_name_from_URL() {
  // e.g for http://localhost:8000/index.html?name=foo
  let url = new URL(window.location.href)
  return url.searchParams.get("name");
}

export class CommLink {
  constructor(app, server, port=9999) {
    let ws_port = port == null ? 9999 : port
    this.app = app;
    this.socket = new WebSocket(`ws://localhost:${ws_port}`);
    // this.socket = new WebSocket("ws://"+server+":"+port+"/ws");
    this.setup(this.socket)

    this.notebooks = {}
    this.notebook = null
    this.comm_manager = new CommManager(this);
    }


    toggle_notebook(name) {
      // Switch to an existing notebook or create a new one as necessary
      if (!(name in this.notebooks)) {
        let notebook = new Notebook(name, this.app, eval)
        this.notebooks[name] = notebook
      }
      if ((this.notebook != null)  && (name == this.notebook.name)) {return}

      this.app.set('uuids',         [])
      this.app.set('removed',       [])
      this.app.set('refresh_uuids', [])
      this.notebook = this.notebooks[name]
      window.notebook = this.notebooks[name] // Used by Moon app
      this.notebook.refresh()
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
    let resolved = deserialize(e.data);
    resolved.then((value) => {
      this.onmessage(value);
    });
  }

  send_message(cmd, args) {
    let json_args = JSON.stringify(args);
    if (this.socket.readyState === 1) {
      this.socket.send(`{"cmd":"${cmd}", "args":${json_args}, "name":"${this.notebook.name}"}`);
    }
  }

  onmessage(json) {
    // Remove later
    let deprecated = ['append_cell', 'append_md_cell', 'swap_cells'];
    if (deprecated.includes(json.cmd)) {
      console.log(`Command ${json.cmd} is deprecated.`);
      return
    }

    let url_name = notebook_name_from_URL()
    let name =  (url_name != null) ? url_name : json.name
    this.toggle_notebook(name)

    // Dispatch commands
    let commands = ['add_cell', 'remove_cell', 'remove_cells',
                    'update_cell', 'update_cell_outputs', 'update_cell_input',
                    'clear_notebook', 'reorder_cells', 'clear_cell_output',
                    'update_theme', 'display_code',
                    // Window management
                    'scroll_by',
                    'scroll_to',
                    // Download
                    'download_python_notebook',
                    'download_cleared_notebook',
                    'download_full_notebook',
                    // Comms
                    'comm_open',
                    'comm_msg']; // TODO: comm_close

    // TODO: 'insert_cell', 'append_cells'
    if (!commands.includes(json.cmd)) {
      console.log(`Command ${json.cmd} is not one of ${commands}`);
      return
    }
    let message = `Running command ${json.cmd} with args ${JSON.stringify(json.args)}`;
    console.log( (message.length < 200) ? message : message.slice(0,200));

    if (json.cmd == 'comm_open') {
      let {comm_id, target_name} = json.args;
      this.comm_manager.comm_open(target_name, comm_id)
    }
    if (json.cmd == 'comm_msg') {
      let {content, msg_type, metadata} = json.args;
      let buffers = json.buffers ? json.buffers : [];
      this.comm_manager.dispatch_message({content:content,
                                          metadata: metadata,
                                          msg_type:msg_type,
                                          buffers:buffers}, content.comm_id);
    }
    if (json.cmd == 'scroll_by') {
      let {offset} = json.args;
      this.notebook.scroll_by(offset);
    }
    if (json.cmd == 'scroll_to') {
      let {position, line} = json.args;
      this.notebook.scroll_to(position, line);
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
    else if (json.cmd == 'update_theme') {
      let {css} = json.args;
      update_theme(css);
    }
    else if (json.cmd == 'display_code') {
      let {pos, visible} = json.args;
      this.notebook.display_code(pos, visible);
    }
  }
}
