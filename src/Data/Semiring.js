"use strict";

export var intAdd = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x + y | 0;
  };
};

export var intMul = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x * y | 0;
  };
};

export var numAdd = function (n1) {
  return function (n2) {
    return n1 + n2;
  };
};

export var numMul = function (n1) {
  return function (n2) {
    return n1 * n2;
  };
};
