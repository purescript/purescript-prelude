export const arrayBind = function (arr) {
  if (typeof Array.prototype.flatMap === "function") {
    return function (f) {
      return arr.flatMap(f);
    };
  }

  return function (f) {
    const result = [];
    for (let i = 0, l = arr.length; i < l; i++) {
      const xs = f(arr[i]);
      for (let j = 0, m = xs.length; j < m; j++) {
        result.push(xs[j]);
      }
    }
    return result;
  };
};
