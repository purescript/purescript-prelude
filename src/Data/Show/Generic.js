export var intercalate = function (separator) {
  return function (xs) {
    return xs.join(separator);
  };
};
