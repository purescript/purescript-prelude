"use strict";
// @flow

// module Data.EuclideanRing

exports.intDegree = function (x/*: number*/)/*: number*/ {
  return Math.abs(x);
};

exports.intDiv = function (x/*: number*/)/*: (y: number) => number*/ {
  return function (y) {
    /* jshint bitwise: false */
    return x / y | 0;
  };
};

exports.intMod = function (x/*: number*/)/*: (y: number) => number*/ {
  return function (y) {
    return x % y;
  };
};

exports.numDiv = function (n1/*: number*/)/*: (n2: number) => number*/ {
  return function (n2) {
    return n1 / n2;
  };
};
