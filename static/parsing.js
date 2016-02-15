"use strict";
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
//    b focus ~ [after text editing done] new a, a after b, a focus.
//  enter, text-div
//
//  click, text-div
//    a click ~ a focus
//
//  ? text-change
//

var tag = function(constructor, contents) {
  return {tag: constructor, contents: contents};
}
var sym = function(x) {
  return tag('VELit', tag('LSym', x));
}
var _sym = function(t) {
  if (t.tag == 'VELit' && t.contents.tag == 'LSym')
    return t.contents.contents;
}
var num = function(x) {
  return tag('VELit', tag('LInt', x));
}
var binder = function(x) {
  return tag('VESym', x);
}
var ref = function(x) {
  return tag('VERef', x);
}
var triple = function(a,p,b) {
  return {
    source: a,
    pred: p,
    target: b
  };
}

var get_cmd = function() {
  return tag('SCGet', []);
}
var put_cmd = function(arrows) {
  return tag('SCNew', arrows);
}

var keypress = function(elem, key) {
  var e = binder("e");
  return {
    e: e,
    w: [ triple(e, 'type', sym('key')),
         triple(e, 'elem', elem),
         triple(e, 'key', key),
       ]
  };
}

var ex = [
  triple(sym('root'), 'focus', ref(0)),
];

var send = function(sock, msg) {
  sock.send(JSON.stringify(msg));
}

var parse_init = function(sock) {
  send(sock, get_cmd());
  //send(sock, put_cmd([triple(ref(0), 'foo', ref(0))]));
  //send(sock, put_cmd(keypress(ref(0), ref(0))));
}

var parse_handler = function(sock, arrs) {
  console.log('num rows: ', arrs.length);
  _.each(arrs, function(arrow) {
    switch(arrow.pred) {
      case 'ui-elem':
        var s = _sym(arrow.source);
        var id = arrow.target;
        if (s) {
          switch (s) {
            case 'text-box':
              console.log('box!');
              mkBox('box', document.body);
              break;
            case 'text-area':
              console.log('area!');
              mkEditor('editor...', id, sock, document.body);
              break;
          }
        }
        break;
      default:
        console.log('unhandled relation: ', arrow.pred);
        break;
    }
  });
}
