exports.unsafeGet = function (key) {
  return function (xs) {
    return xs[key];
  };
};

exports.unsafeInsert = function (key) {
  return function (value) {
    return function (xs) {
      xs[key] = value;
      return xs;
    };
  };
};
