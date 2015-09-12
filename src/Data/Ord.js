/* global exports */
"use strict";

// module Data.Ord

exports.ordArrayImpl = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (f) {
        return function (xs) {
          return function (ys) {
            var xlen = xs.length;
            var ylen = ys.length;
            for (var i = 0; i < xlen && i < ylen; i++) {
              var o = f(xs[i])(ys[i]);
              if (o !== eq) return o;
            }
            if (xlen > ylen) return lt;
            if (xlen < ylen) return gt;
            return eq;
          };
        };
      };
    };
  };
};
