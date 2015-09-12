/* global exports */
"use strict";

// module Data.BooleanAlgebra

exports.conjBooleanImpl = function (b1) {
  return function (b2) {
    return b1 && b2;
  };
};

exports.disjBooleanImpl = function (b1) {
  return function (b2) {
    return b1 || b2;
  };
};

exports.notBooleanImpl = function (b) {
  return !b;
};
