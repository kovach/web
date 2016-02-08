//display
//
//  [text div]
//    border
//      color
//    text
//  [arrow]
//    source
//    target
//    label
//  active text area
//    border
//
//events
//
//  enter, text-area
//    [see below]
//  enter, text-div
//    b focus ~ [after text editing done] new a, a after b, a focus.
//  click, text-div
//    a click ~ a focus
//

tag = function(constructor, contents) {
  return {tag: constructor, contents: contents};
}
sym = function(x) {
  return tag('VELit', tag('LSym', x));
}
num = function(x) {
  return tag('VELit', tag('LInt', x));
}
name = function(x) {
  return tag('VESym', x);
}
ref = function(x) {
  return tag('VERef', x);
}
triple = function(a,p,b) {
  return {
    source: a,
    pred: p,
    target: b
  };
}
get_cmd = function() {
  return tag('SCGet', []);
}

keypress = function(elem, key) {
  e = name("e");
  return [
    triple(e, 'type', sym('key')),
    triple(e, 'elem', elem),
    triple(e, 'key', key),
    ]
}

var ex = [
  triple(sym('root'), 'focus', ref(0)),
];

send = function(sock, msg) {
  sock.send(JSON.stringify(msg));
}

parse_init = function(sock) {
  send(sock, get_cmd());
}

parse_handler = function(arrs) {
  _.each(arrs, function(arr) {
    console.log(arr.source, '-->', arr.target);
  });
}
