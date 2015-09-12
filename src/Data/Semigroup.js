/* global exports */
"use strict";

// module Data.Semigroup

exports.appendArray = function (xs) {
  return function (ys) {
    return xs.concat(ys);
  };
};

exports.appendString = function (s1) {
  return function (s2) {
    return s1 + s2;
  };
};
