"use strict";
// @flow

// module Control.Apply

exports.arrayApply = function /*::<A, B>*/(fs/*: Array<(x: A) => B>*/)/*: (xs: Array<A>) => Array<B>*/ {
  return function (xs) {
    var result = [];
    var n = 0;
    for (var i = 0, l = fs.length; i < l; i++) {
      for (var j = 0, k = xs.length; j < k; j++) {
        result[n++] = fs[i](xs[j]);
      }
    }
    return result;
  };
};
