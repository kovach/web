"use strict";

var init = function(fn, handler) {
  var sock = new WebSocket("ws://0.0.0.0:8080");
  sock.onopen = function(event) {
    console.log('open');
    sock.send("hi up there?");
    fn(sock);
  };
  sock.onmessage = function(event) {
    try {
      var obj = JSON.parse(event.data);
    } catch (e) {
      console.log('msg not json: ', event.data);
      return;
    }
    console.log('got object: ', obj);
    handler(sock, obj);
  }
}
