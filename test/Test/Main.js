"use strict";
// @flow

// module Test.Main

/*::
type Case = [number, string];
*/

exports.mainImpl = function(showNumber/*: (n: number) => string*/) {
  return function() {
    function testAll(cases/*: Array<Case>*/)/*: void*/ {
      cases.forEach(function(c/*: Case*/)/*: void*/ {
        var expected = c[1];
        var actual = showNumber(c[0]);
        if (expected !== actual) {
          throw new Error(
            "For " + c[0] +
            ", expected " + expected +
            ", got: " + actual + ".");
        }
      });
    }

    testAll([
        // Within Int range
        [0.0, "0.0"],
        [1.0, "1.0"],
        [-1.0, "-1.0"],
        [500.0, "500.0"],

        // Outside Int range
        [1e10, "10000000000.0"],
        [1e10 + 0.5, "10000000000.5"],
        [-1e10, "-10000000000.0"],
        [-1e10 - 0.5, "-10000000000.5"],

        // With exponent
        [1e21, "1e+21"],
        [1e-21, "1e-21"],

        // With decimal and exponent
        [1.5e21, "1.5e+21"],
        [1.5e-10, "1.5e-10"],

        [NaN, "NaN"],
        [Infinity, "Infinity"],
        [-Infinity, "-Infinity"],
      ]);
  };
};
