export const arrayMap = function (f) {
  return function (arr) {
    var l = arr.length;
    // Allocate the result array at its final size; the loop below fills it,
    // so initializing every slot first would do the work twice.
    // oxlint-disable-next-line unicorn/no-new-array
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};
