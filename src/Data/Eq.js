"use strict";

exports.refEq = function (r1) {
  return function (r2) {
    return r1 === r2;
  };
};

exports.eqArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      
      /* disclaimer: this check might break referential equality
         for non-lawful instances of `Eq` (e.g. `eq _ _ = false`).
         We strongly discourage unlawful instances, so it shouldn't
         be a problem in practice. */
      if (xs === ys) return true;

      if (xs.length !== ys.length) return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
};
