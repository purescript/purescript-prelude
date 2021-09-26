"use strict";

exports.intercalate = function (separator) {
  return function (xs) {
    return xs.join(separator);
  };
};
