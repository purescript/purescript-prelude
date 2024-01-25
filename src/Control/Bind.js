var APPLY_CHUNK_SIZE = 10e3;
var push = Function.prototype.apply.bind(Array.prototype.push);

export const arrayBind = function (arr) {
  return function (f) {
    var result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      var subArr = f(arr[i]);
      while (subArr.length > APPLY_CHUNK_SIZE) {
        push(result, subArr.slice(0, APPLY_CHUNK_SIZE);
        subArr = subArr.slice(APPLY_CHUNK_SIZE);
      }
      push(result, subArr);
    }
    return result;
  };
};
