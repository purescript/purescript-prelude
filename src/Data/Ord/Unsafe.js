"use strict";
// @flow

// module Data.Ord.Unsafe

/*::
interface UnsafeCompareImpl<O> {
  (lt: O): (eq: O) => (gt: O) => UnsafeCompare<O>;
};

interface UnsafeCompare<O> {
  (x: number): (y: number) => O;
  (x: boolean): (y: boolean) => O;
  (x: string): (y: string) => O;
};
*/

exports.unsafeCompareImpl = (function (lt) {
  return function (eq) {
    return function (gt) {
      return function (x) {
        return function (y) {
          return (x < y) ? lt : (x > y) ? gt : eq;
        };
      };
    };
  };
}/*: UnsafeCompareImpl<*> */);
