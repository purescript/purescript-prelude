"use strict";
// @flow

// module Data.Eq

exports.refEq = function /*:: <A>*/(r1/*: A*/)/*: (r2: A) => boolean*/ {
  return function (r2) {
    return r1 === r2;
  };
};

exports.refIneq = function /*:: <A>*/(r1/*: A*/)/*: (r2: A) => boolean*/ {
  return function (r2) {
    return r1 !== r2;
  };
};

// jscs:disable maximumLineLength
exports.eqArrayImpl = function /*:: <A>*/(f/*: (x: A) => (y: A) => boolean*/)/*: (xs: Array<A>) => (ys: Array<A>) => boolean*/ {
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
