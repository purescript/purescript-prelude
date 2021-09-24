exports.cons = function (head) {
  return function (tail) {
    return [head].concat(tail);
  };
};
