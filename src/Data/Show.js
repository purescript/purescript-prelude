/* global exports */
"use strict";

// module Data.Show

exports.showIntImpl = function (n) {
  return n.toString();
};

exports.showNumberImpl = function (n) {
  /* jshint bitwise: false */
  return n === (n | 0) ? n + ".0" : n.toString();
};

exports.showCharImpl = function (c) {
  return c === "'" ? "'\\''" : "'" + c + "'";
};

exports.showStringImpl = function (s) {
  return JSON.stringify(s);
};

exports.showArrayImpl = function (f) {
  return function (xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};
