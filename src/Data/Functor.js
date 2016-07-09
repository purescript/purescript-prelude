"use strict";
// @flow

// module Data.Functor

exports.arrayMap = function /*:: <A, B>*/(f/*: (x: A) => B*/)/*: (arr: Array<A>) => Array<B>*/ {
  return function (arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};
