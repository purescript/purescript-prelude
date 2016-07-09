"use strict";
// @flow

// module Data.Ord

// jscs:disable maximumLineLength
exports.ordArrayImpl = function /*:: <A>*/(f/*: (x: A) => (y: A) => number*/)/*: (xs: Array<A>) => (ys: Array<A>) => number*/ {
  return function (xs) {
    return function (ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;
      while (i < xlen && i < ylen) {
        var x = xs[i];
        var y = ys[i];
        var o = f(x)(y);
        if (o !== 0) {
          return o;
        }
        i++;
      }
      if (xlen === ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};
