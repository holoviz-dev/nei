"use strict"

export function render_markdown(source) {
  // Replace zero width space separated triple quotes with regular triple quotes.
  source = source.replace(/"​"​"/g, "\"\"\"")
  return marked(source)
}


export function getURLParameter(name, url) {
        if (!url) {
            url = window.location.href;
        }
        if (!name) {
            return null
        }
        name = name.replace(/[\[\]]/g, "\\$&")
        let regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)")
        let results = regex.exec(url)
        if (!results) return null
        if (!results[2]) return ''
        return decodeURIComponent(results[2].replace(/\+/g, " "))
}


let entityMap = {
  '&': '&amp;',
  '<': '&lt;',
  '>': '&gt;',
  '"': '&quot;',
  "'": '&#39;',
  '/': '&#x2F;',
  '`': '&#x60;',
  '=': '&#x3D;'
};



export let ansi_solarized = ['#073642', '#D30102', '#859900', '#B58900',
                             '#268BD2', '#D33682', '#2AA198', '#EEE8D5',
                             '#002B36', '#CB4B16', '#586E75', '#657B83',
                             '#839496', '#6C71C4', '#93A1A1', '#FDF6E3'];

export let ansi_solarized_xterm = ['#262626', '#AF0000', '#5F8700', '#AF8700',
                                   '#0087FF', '#AF005F', '#00AFAF', '#E4E4E4',
                                   '#1C1C1C', '#D75F00', '#585858', '#626262',
                                   '#808080', '#5F5FAF', '#8A8A8A', '#FFFFD7'];

export let ansi_tango = ['#000000', '#CC0000', '#4E9A06', '#C4A000',
                         '#3465A4', '#75507B', '#06989A', '#D3D7CF',
                         '#555753', '#EF2929', '#8AE234', '#FCE94F',
                         '#729FCF', '#AD7FA8', '#34E2E2', '#EEEEEC'];

export let ansi_xterm = ['#000000', '#CD0000', '#00CD00', '#CDCD00',
                         '#0000EE', '#CD00CD', '#00CDCD', '#E5E5E5',
                         '#7F7F7F', '#FF0000', '#00FF00', '#FFFF00',
                         '#5C5CFF', '#FF00FF', '#00FFFF', '#FFFFFF'];

export let ansi_console = ['#000000', '#AA0000', '#00AA00', '#AA5500',
                           '#0000AA', '#AA00AA', '#00AAAA', '#AAAAAA',
                           '#555555', '#FF5555', '#55FF55', '#FFFF55',
                           '#5555FF', '#FF55FF', '#55FFFF', '#FFFFFF'];


export function zip(rows) {
  return rows[0].map((_,c)=>rows.map(row=>row[c]))
}

export function range(n) {
  return Array.from(Array(n).keys())
}


export function reorder(arr, positions) {
  let result = [];
  let pairs = zip([positions, range(arr.length)]);
  pairs.sort();
  for (let i=0; i < arr.length; i++) {
    let target = pairs[i][1]
     result.push(arr[target]);
  }
  return result
}


export function scroll_position(obj, offset) {
  // The y position of DOM object in pixels used for scrolling
  let curtop = 0;
  if (obj == null) { return 0}
  if (obj.offsetParent) {
    do {
      curtop += obj.offsetTop;
    } while (obj = obj.offsetParent);
    return curtop + offset;
  }
}


export function syntax_highlight(source) {
  let s = hljs.highlight("python", source).value;
  s = s.replace("self", "<span class='hljs-keyword'>self</span>");
  s = s.replace(`<span class="hljs-keyword">class</span> <span class="hljs-title">`,
                `<span class="hljs-keyword">class</span> <span class="hljs-meta">`);
  s = s.replace(`<span class="hljs-keyword">None</span>`,
                `<span class="hljs-meta">None</span>`);
  s = s.replace(`<span class="hljs-keyword">False</span>`,
                `<span class="hljs-meta">False</span>`);
  s = s.replace(`<span class="hljs-keyword">True</span>`,
                `<span class="hljs-meta">True</span>`);
  return "<pre class='nei-code'>" + s + "</pre>"
}

export function update_theme(css) {
  let style_tag = document.getElementById("syntax-highlighting-style");
  style_tag.innerHTML = css;
}


export function newline_breaks(string) {
    return string.replace(/(?:\r\n|\r|\n)/g, '<br />')
}

export function escapeHtml (string) {
  return newline_breaks(String(string).replace(/[&<>"'`=\/]/g, function (s) {
    return entityMap[s];
  }));
}


export function UUID(a) {
  return a ? (a ^ Math.random() * 16 >> a / 4)
    .toString(16) : ([1e7] + -1e3 + -4e3 + -8e3 + -1e11)
    .replace(/[018]/g, UUID).slice(0, 8)
}


export function download_file(filename, data) {
  let a = document.createElement('a');
  a.style = "display: none";
  let blob = new Blob([data], {type: "application/octet-stream"});
  let url = window.URL.createObjectURL(blob);
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  // See https://stackoverflow.com/questions/30694453 for Firefox support
  setTimeout(function(){
    document.body.removeChild(a);
    window.URL.revokeObjectURL(url);
  }, 100);

  document.body.removeChild(a);
  window.URL.revokeObjectURL(url);
}




export function batched_js(mimes) {
  // Given a list of output mime types, figure out which JS outputs indices
  // need to be batched with the preceding HTML.
  // Returns a flat array of batch JS indices and an object mapping html indices
  // to the corresponding batch of JS indices.

  // let mimes = [ "application/javascript",
  //               "text/html",
  //               "application/javascript",
  //               "application/javascript",
  //               "text/ansi",
  //               "text/html",
  //               "application/javascript",
  //               "text/html" ]
  //
  // batch_js(mimes)
  // Should return [ [ 2, 3, 6 ], { 1: [ 2, 3 ], 5: [ 6 ], 7 : [] } ]
  let groups = {}
  let html_mime = 'text/html';
  let js_mime = 'application/javascript';
  let html_inds = mimes.map((e, i) => e === html_mime ? i : '').filter(String);
  for (let ind of html_inds) { groups[ind]=[] }

  let last_mime = undefined;
  let batched = [];
  for (let [key, grp] of groupby( (x)=>x[1] , mimes.entries() )) {
    // console.log(`KEY ${key} LAST ${last_mime}`);
    if (key == js_mime & last_mime== html_mime) {
      let html_ind = html_inds.shift();
      for (let ind of [...grp].map(x=>x[0])) {
        batched.push(ind);
        groups[html_ind].push(ind);
      }
    }
    last_mime = key;
  }
  return [batched, groups]
}

// Snippet below is from https://github.com/aureooms/js-itertools
// This code is AGPL licensed and must therefore be left unchanged
// (otherwise AGPL will also apply to the rest of the code)

function iter ( iterable ) {
  return iterable[Symbol.iterator]( ) ;
}

function* groupby ( key , iterable ) {
  // e.g: for (let [key, grp] of group( (x)=>x ,
  // ["A","A", "A", "B", "B", "A", "B"] )) { console.log([...grp])}
  let iterator = iter( iterable ) ;
  let first = iterator.next() ;
  if ( first.done ) return ;
  let currval = first.value ;
  let currkey = key( currval ) ;

  const grouper = function* ( tgtkey ) {

    while (true) {
      yield currval ;
      let event = iterator.next( ) ;
      if ( event.done ) return ;
      currval = event.value ;
      currkey = key( currval ) ;
      if ( currkey !== tgtkey ) return  }
  };
  while (true) {
    const tgtkey = currkey ;
    const g = grouper( tgtkey ) ;
    yield [ tgtkey , g ] ;
    while ( currkey === tgtkey ) {
      let event = iterator.next( ) ;
      if ( event.done ) return ;
      currval = event.value ;
      currkey = key( currval ) ;
    }
  }
}
