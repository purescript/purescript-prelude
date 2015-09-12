/* global exports */
"use strict";

// module Data.Eq.Unsafe

exports.nativeEq = function (r1) {
  return function (r2) {
    return r1 === r2;
  };
};
