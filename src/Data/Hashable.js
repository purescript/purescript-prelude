"use strict";

// Same as immutable.js, except for not dropping the highest bit.
exports.hashNumber = function (f) {
  var o = f;
  if (o !== o || o === Infinity) {
    return 0;
  }
  var h = o | 0;
  if (h !== o) {
    h ^= o * 0xffffffff;
  }
  while (o > 0xffffffff) {
    o /= 0xffffffff;
    h ^= o;
  }
  return h;
};

// Same as Java. Improvements welcome.
exports.hashString = function (s) {
  var h = 0;
  for (var i = 0; i < s.length; i++) {
    h = (31 * h + s.charCodeAt(i)) | 0;
  }
  return h;
};

// Almost the same as Java. Improvements welcome.
exports.hashArray = function (hash) {
  return function (as) {
    var h = 0;
    for (var i = 0; i < as.length; i++) {
      h = (31 * h + hash(as[i])) | 0;
    }
    return h;
  };
};

exports.hashChar = function (c) {
  return c.charCodeAt(0);
};
