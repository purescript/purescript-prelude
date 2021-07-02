"use strict";

export var boolConj = function (b1) {
  return function (b2) {
    return b1 && b2;
  };
};

export var boolDisj = function (b1) {
  return function (b2) {
    return b1 || b2;
  };
};

export var boolNot = function (b) {
  return !b;
};
