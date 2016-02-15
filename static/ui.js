//mkText
//mkBox
//mkEditor
//setStyle
//? setAttr
//
//clickHandle
//enterHandle
//
//
//arrow view?
//circles/arrows

var append = function(a, b) {
  b.appendChild(a);
  return a;
}

var mkText = function(str, elem) {
  var t = document.createTextNode(str);
  if (elem)
    append(t, elem);
  return t;
}

var editorHandler = function(sock, id) {
  return function(ev) {
    console.log('mod: ', ev);
    if (ev.shiftKey && ev.keyCode == 13) {
      ev.preventDefault();
      console.log('shift-enter', id);
      var obj = keypress(id, sym('enter'));
      var w = obj.w; // contains 3 messages
      w = w.concat([triple(obj.e, 'modifier', sym('shift'))]); // now 4
      send(sock, put_cmd(w));
    }
  }
}

var mkEditor = function(str, id, sock, other) {
  var el = document.createElement('textarea');
  el.placeholder= str;
  el.addEventListener('keydown', editorHandler(sock, id));

  if (other)
    append(el, other);
  return el;
}

var mkBox = function(str, other) {
  var el = document.createElement('div');
  mkText(str, el);
  if (other)
    append(el, other);
  return el;
}
