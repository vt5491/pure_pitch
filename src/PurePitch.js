"use strict"

// call from a monad with somehting like:
// let tmp = debugger 1
exports.debugger = function (n) {
  console.log('now in debugger');
  debugger;
  return 1;
};

exports.getOsc = function (val) {
  var s = getState();
  return s.osc
}

exports.setOsc = function (val) {
  var s = getState();
  s.osc = val;
  // debugger
  return s.osc;
}

exports.getIsPlaying = function (val) {
  var s = getState();
  return s.isPlaying
}

exports.setIsPlaying = function (val) {
  var s = getState();
  s.isPlaying = val;
  // debugger
  return s.isPlaying;
}

function getState () {
  if (!document.PP) {
    document.PP = {};
  }

  if (!document.PP.state) {
    document.PP.state = {};
  }

  return document.PP.state
}
