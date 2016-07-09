"use strict";
// @flow

// module Data.Semiring

exports.intAdd = function (x/*: number*/)/*: (y: number) => number*/ {
  return function (y) {
    /* jshint bitwise: false */
    return x + y | 0;
  };
};

exports.intMul = function (x/*: number*/)/*: (y: number) => number*/ {
  return function (y) {
    /* jshint bitwise: false */
    return x * y | 0;
  };
};

exports.numAdd = function (n1/*: number*/)/*: (n2: number) => number*/ {
  return function (n2) {
    return n1 + n2;
  };
};

exports.numMul = function (n1/*: number*/)/*: (n2: number) => number*/ {
  return function (n2) {
    return n1 * n2;
  };
};
