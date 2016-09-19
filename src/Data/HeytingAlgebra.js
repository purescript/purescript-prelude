"use strict";
// @flow

// module Data.HeytingAlgebra

exports.boolConj = function (b1/*: boolean*/)/*: (b2: boolean) => boolean*/ {
  return function (b2) {
    return b1 && b2;
  };
};

exports.boolDisj = function (b1/*: boolean*/)/*: (b2: boolean) => boolean*/ {
  return function (b2) {
    return b1 || b2;
  };
};

exports.boolNot = function (b/*: boolean*/)/*: boolean*/ {
  return !b;
};
