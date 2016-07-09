"use strict";
// @flow

// module Data.Semigroup

exports.concatString = function (s1/*: string*/)/*: (s2: string) => string*/ {
  return function (s2) {
    return s1 + s2;
  };
};

exports.concatArray = function /*:: <A>*/(xs/*: Array<A>*/)/*: (ys: Array<A>) => Array<A>*/ {
  return function (ys) {
    return xs.concat(ys);
  };
};
