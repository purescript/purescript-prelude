"use strict";

exports.hashNumber = function (o) {
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

exports.hashString = function (s) {
    var h = 0;
    for (var i = 0; i < s.length; i++) {
        h = (31 * h + s.charCodeAt(i)) | 0;
    }
    return h;
};

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
