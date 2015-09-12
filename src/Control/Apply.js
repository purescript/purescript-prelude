/* global exports */
"use strict";

// module Control.Apply

exports.applyArrayImpl = function (fs) {
  return function (xs) {
    var result = [];
    var n = 0;
    for (var j = 0; m = xs.length; j < m; j++) {
      for (var i = 0, l = fs.length; i < l; i++) {
        result[n++] = fs[i](xs[j]);
      }
    }
    return result;
  };
};
