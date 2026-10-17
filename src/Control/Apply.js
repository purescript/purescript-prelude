export const arrayApply = function (fs) {
  return function (xs) {
    var l = fs.length;
    var k = xs.length;
    // Allocate the result array at its final size; the loop below fills it,
    // so initializing every slot first would do the work twice.
    // oxlint-disable-next-line unicorn/no-new-array
    var result = new Array(l * k);
    var n = 0;
    for (var i = 0; i < l; i++) {
      var f = fs[i];
      for (var j = 0; j < k; j++) {
        result[n++] = f(xs[j]);
      }
    }
    return result;
  };
};
