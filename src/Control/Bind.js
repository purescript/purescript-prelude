export const arrayBind =
  typeof Array.prototype.flatMap === "function"
    ? function (arr) {
        return function (f) {
          return arr.flatMap(f);
        };
      }
    : function (arr) {
        return function (f) {
          var result = [];
          var l = arr.length;
          for (var i = 0; i < l; i++) {
            var xs = f(arr[i]);
            var k = xs.length;
            for (var j = 0; j < k; j++) {
              result.push(xs[j]);
            }
          }
          return result;
        };
      };
