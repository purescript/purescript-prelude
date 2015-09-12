/* global exports */
"use strict";

// module Data.Eq

exports.arrayEq = function (f) {
  return function (xs) {
    return function (ys) {
      var l = xs.length;
      if (ys.length !== l) return false;
      for (var i = 0; i < l; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
};
