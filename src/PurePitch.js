"use strict"

exports.debugger = function (n) {
  console.log('now in debugger');
  debugger;
  return 1;
};
