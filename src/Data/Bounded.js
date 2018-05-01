"use strict";

// Can't use `Number.MIN_SAFE_INTEGER` or `Number.MAX_SAFE_INTEGER` because
//   those apply to double precision floating point, not 32-bit signed integers.
exports.topInt = 2147483647;
exports.bottomInt = -2147483648;

exports.topChar = String.fromCharCode(65535);
exports.bottomChar = String.fromCharCode(0);

exports.topNumber = Number.POSITIVE_INFINITY;
exports.bottomNumber = Number.NEGATIVE_INFINITY;
