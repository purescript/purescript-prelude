"use strict";
// @flow

// module Data.Ring

exports.intSub = function (x/*: number*/)/*: (y: number) => number*/ {
  return function (y) {
    /* jshint bitwise: false */
    return x - y | 0;
  };
};

exports.numSub = function (n1/*: number*/)/*: (n2: number) => number*/ {
  return function (n2) {
    return n1 - n2;
  };
};
