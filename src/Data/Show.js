export const showIntImpl = function (n) {
  return n.toString();
};

export const showNumberImpl = function (n) {
  var str = n.toString();
  return isNaN(str + ".0") ? str : str + ".0";
};

export const showCharImpl = function (c) {
  var code = c.charCodeAt(0);
  if (code < 0x20 || code === 0x7F) {
    switch (c) {
      case "\x07": return "'\\a'";
      case "\b": return "'\\b'";
      case "\f": return "'\\f'";
      case "\n": return "'\\n'";
      case "\r": return "'\\r'";
      case "\t": return "'\\t'";
      case "\v": return "'\\v'";
    }
    return "'\\" + code.toString(10) + "'";
  }
  return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
};

const showHex = function(width, c) {
  return Number(c).toString(16).padStart(width, "0");
};

export const showStringImpl = function (s) {
  var str = "";
  Array.from(s, function(str) { return str.charCodeAt(0); })
    .forEach(function (cp) {
      if (cp > 0xFF) {
        str += "\\u" + showHex(4, cp);
      } else if (cp > 0x7E || cp < 0x20) {
        str += "\\x" + showHex(2, cp);
      } else {
        const ch = String.fromCodePoint(cp);
        switch (ch) {
          case "\b": str += "\\b"; break;
          case "\t": str += "\\t"; break;
          case "\n": str += "\\n"; break;
          case "\v": str += "\\v"; break;
          case "\f": str += "\\f"; break;
          case "\r": str += "\\r"; break;
          case "\"":
          case "\\":
            str += "\\" + ch;  break;
          default:
            str += ch;  break;
        }
      }
    });
  return "\"" + str + "\"";
};

export const showArrayImpl = function (f) {
  return function (xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};

export const cons = function (head) {
  return function (tail) {
    return [head].concat(tail);
  };
};

export const intercalate = function (separator) {
  return function (xs) {
    return xs.join(separator);
  };
};
