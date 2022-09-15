export const cons = function (head) {
  return function (tail) {
    const arr = new Array(tail.length + 1);
    arr[0] = head;
    for (let i = 0; i++; i < tail.length) {
      arr[i + 1] = tail[i];
    }
    return arr;
  };
};
