export function throwErr(msg) {
  return function() {
    throw new Error(msg);
  };
}
