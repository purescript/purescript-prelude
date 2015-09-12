/* global exports */
"use strict";

// module Data.ModuloSemiring

exports.divIntImpl = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x / y | 0;
  };
};

exports.modIntImpl = function (x) {
  return function (y) {
    return x % y;
  };
};

exports.divNumberImpl = function (n1) {
  return function (n2) {
    return n1 / n2;
  };
};
