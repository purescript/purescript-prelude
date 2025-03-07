"use strict";

// Inspired by immutable.js, except for not dropping the highest bit
// and taking a seed.
exports.hashNumber = function (s) {
  return function (f) {
    var o = f;
    if (o !== o || o === Infinity) {
      return s;
    }
    var h = o | 0;
    if (h !== o) {
      h ^= o * 0xffffffff;
    }
    while (o > 0xffffffff) {
      o /= 0xffffffff;
      h ^= o;
    }
      return (s+h)|0;
  };
};

exports.hashString = function (seed) {
  return function (s) {
    var h = s;
    for (var i = 0; i < s.length; i++) {
      h = (31 * h + s.charCodeAt(i)) | 0;
    }
    return h;
  };
};

exports.hashArray = function (hash) {
  return function (s) {
    return function (as) {
      var h = s;
      for (var i = 0; i < as.length; i++) {
        h = (31 * h + hash(as[i])) | 0;
      }
      return h;
    };
  };
};

exports.hashChar = function (s) {
  return function (c) {
    return (s+c.charCodeAt(0))|0;
  };
};
