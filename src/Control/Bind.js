export const arrayBind = function (arr) {
  if (typeof Array.prototype.flatMap === "function") {
    return function (f) {
      return arr.flatMap(f);
    };
  }

  return function (f) {
    var result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      Array.prototype.push.apply(result, f(arr[i]));
    }
    return result;
  };
};
