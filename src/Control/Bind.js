"use strict";
// @flow

// module Control.Bind

exports.arrayBind = function /*:: <A, B>*/(arr/*: Array<A>*/)/*: (f: (x: A) => Array<B>) => Array<B>*/ {
  return function (f) {
    var result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      Array.prototype.push.apply(result, f(arr[i]));
    }
    return result;
  };
};
