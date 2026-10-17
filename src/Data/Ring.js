export const intSub = function (x) {
  return function (y) {
    return (x - y) | 0;
  };
};

export const numSub = function (n1) {
  return function (n2) {
    return n1 - n2;
  };
};
