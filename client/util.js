"use strict"

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
  return "<pre class='labmode-code'>" + s + "</pre>"
}

export function update_style(css) {
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
