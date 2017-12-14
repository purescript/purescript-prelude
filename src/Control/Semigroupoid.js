"use strict";

// https://medium.com/@safareli/stack-safe-function-composition-85d61feee37e
var runComposition = function(composition, x) {
  var root = composition;
  var val = x;
  var stack = [];
  for (;;) {
    if (root._0 !== undefined){
      stack.push(root._1);
      root = root._0;
    } else {
      val = root(val);
      if (stack.length === 0) {
        return val;
      }
      root = stack.shift();
    }
  }
};

exports.functionCompose = function(f) {
  return function(g) {
    var res = function composition(x) {
      return runComposition(composition, x);
    };
    res._0 = g;
    res._1 = f;
    return res;
  };
};
