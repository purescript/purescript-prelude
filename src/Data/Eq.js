"use strict";

var refEq = function (r1) {
  return function (r2) {
    return r1 === r2;
  };
};

export var eqBooleanImpl = refEq;
export var eqIntImpl = refEq;
export var eqNumberImpl = refEq;
export var eqCharImpl = refEq;
export var eqStringImpl = refEq;

export var eqArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      if (xs.length !== ys.length) return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
};
