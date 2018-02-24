'use strict';

import {escapeHtml, syntax_highlight, UUID, range, zip, reorder} from './util.js';
import {ansi_solarized_xterm} from './util.js';


export class Notebook {
  // Notebook model constituting the Javascript API
  // Handles interfacing with comms and the moon application.

  constructor(app, evaluate) {
    // If eval is used in a module, the result gets trapped
    this.app = app;
    this.evaluate = evaluate;

    this.uuids = [];
    this.cell = {};
    this.removed = [];                // uuids sadly no longer with us
    this.refresh_uuids = [];          // uuids of cells needing update
  }

  visible_uuids() {
    return this.uuids.filter(v => !this.removed.includes(v));
  }

  map_pos(pos) {
    // Given the visible position, find the actual position in the uuids array
    let skipped = 0;
    for (let i=0; i < this.uuids.length; i++) {
      if (this.removed.includes(this.uuids[i])) {
        skipped += 1;
      }
      if (i == (pos + skipped)) { break}
    }
    return pos + skipped
  }

  uuid_at_pos(pos) {
    // Returns the uuid by the position of visible cells. If pos is null, return last cell.
    let visible = this.visible_uuids();
    return visible[pos == null ? (filtered.length - 1) : pos]
  }

  get_cell(pos) { return this.cell[this.uuid_at_pos(pos)] }

  text() { // Useful for debugging
    for (let uuid of this.visible_uuids()) {
      console.log(`${this.cell[uuid].source}`);
    }
  }


  refresh() {
    this.app.set('uuids',         this.uuids);
    this.app.set('removed',       this.removed);
    this.app.set('refresh_uuids', this.refresh_uuids);
  }

  _set_refresh(uuid, types) {
    for (let type of types) {
      this.refresh_uuids.push(uuid + type);
    }
    this.refresh();
  }

  refreshed(uuid, type) {
    this.refresh_uuids = this.refresh_uuids.map(v => v != (uuid + type));
  }

  add_cells(cells, positions) {
    for (let pair of zip([cells, positions])) {
      let [cell, pos] = pair;
      cell.notebook = this;
      this.uuids.splice( this.map_pos(pos), 0, cell.uuid);
      this.cell[cell.uuid] = cell;
    }
    this.refresh();
  }

  add_cell(cell, position) { // append if position not supplied
    this.add_cells([cell], (position===undefined) ? [this.uuids.length] : [position]);
  }

  remove_cells(positions) {
    let uuids = positions.map(pos => this.uuid_at_pos(pos));
    for (let uuid of uuids) {
      delete this.cell[uuid];
      this.removed.push(uuid);
    }
    this.refresh();
  }

  remove_cell(position) {
    this.remove_cells([position]);
  }

  update_cell(position, source=null, input=null, outputs=null, prompt=null) {
    let cell = this.get_cell(position);
    if (source || input) {
      cell.update_input(source, input);
    }
    if (outputs) {
      cell.update_output(outputs, false);
    }
    cell.prompt = prompt;
    this._set_refresh(this.uuid_at_pos(position), ['input', 'output']);
  }

  update_cell_input(position, source, input) {
    this.get_cell(position).update_input(source, input);
    this._set_refresh(this.uuid_at_pos(position), ['input']);
  }

  update_cell_outputs(position, outputs, prompt=null) {
    if (outputs != null) {
      this.get_cell(position).update_output(outputs, true);
    }
    this.get_cell(position).prompt = prompt;
    this._set_refresh(this.uuid_at_pos(position), ['output']);
  }

  clear_cell_output(position) {
    this.get_cell(position).update_output([]);
    this._set_refresh(this.uuid_at_pos(position), ['output']);
  }

  clear_notebook() {
    this.cell = {};
    this.uuids = [];
    this.removed = [];
    this.refresh();
  }

  reorder_cells(positions) { // Index array as long as cells.
    this.uuids = reorder(this.visible_uuids(), positions).concat(this.removed);
    this.refresh();
  }
}

export class Cell {
  constructor(mode, source, input=null, outputs=[], notebook=null) {

    this.notebook = notebook; // "cell" or "markdown"
    this.mode = mode;
    this.prompt = null;

    this.uuid = UUID();
    this.update_input(source, input);
    this.update_output(outputs);

    this.evaltypes = ["application/javascript",
                      "application/vnd.bokehjs_load.v0+json"]
  }

  update_input(source, input) {
    this.source = source;                              // Input as plaintext
    // Input to display as HTML  (if any)
    if (this.mode == "code") {
      this.input = input ? input : syntax_highlight(source.trim());
    }
    else if (this.mode == "markdown")  {
      this.input = `<div class="markdown-cell">${marked(source)}</div>`;
    }
  }

  update_output(outputs, extend=false) {
    if (this.mode == "markdown")  {
      this.outputs = [];
    }
    else if (extend) {
      this.outputs.push.apply(this.outputs, outputs);
    }
    else {
      this.outputs = outputs
    }
  }

  extract_scripts(output) {
    // For a text/html output, extract JS from script tags
    let parser = new DOMParser();
    let doc = parser.parseFromString(output, "text/html");
    return doc.getElementsByTagName('script');
  }

  completed(val, index) {
    // Used to know when JS execution is completed.
    if (index+1 ==this.outputs.length) {
      this.notebook.refresh();
    }
    return val
  }

  display(index) {
    // Whether a display div is needed
    let [mime, data] = this.outputs[index];
    if (!this.evaltypes.includes(mime)) {
      return true
    }
    this.notebook.evaluate(data);
    return this.completed(false, index);
  }

  output(index) {
    // Get the displayable output data
    let [mime, data] = this.outputs[index];
    if (mime == 'text/html') {
      return data;
    }
   else if (mime == 'text/ansi') {
     let converter = new ansi_to_html.js({colors:ansi_solarized_xterm, escapeXML: true});
     let mapped = data.map((d) => converter.toHtml(d));
     return "<pre>" + mapped.join("<br>") + "</pre>"
   }
    else {  // e.g 'text/plain'
      return escapeHtml(data);
    }
  }

  execute(index) {
    let [mime, data] = this.outputs[index];

    let nextDOMupdate = () => {
      for (let script of this.extract_scripts(data)) {
        console.log(`Script length is ${script.innerHTML.length}`);
        this.notebook.evaluate(script.innerHTML);
        this.completed(null, index);
      }
    };

    if (mime == "text/html") {
      Moon.nextTick(nextDOMupdate);
    }
    else {
      this.completed(null, index);
    }
  }
}
