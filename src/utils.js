// utility functions to replace underscore equivalents

// https://underscorejs.org/docs/modules/uniqueId.html
var idCounter = 0;
function uniqueId(prefix) {
  var id = ++idCounter + "";
  return prefix ? prefix + id : id;
}

// https://underscorejs.org/docs/modules/escape.html
const escapeMap = {
  "&": "&amp;",
  "<": "&lt;",
  ">": "&gt;",
  '"': "&quot;",
  "'": "&#x27;",
  "`": "&#x60;",
};

function escape(string) {
  return Object.entries(escapeMap).reduce(
    (string, [target, sub]) => string.replaceAll(target, sub),
    string
  );
}

export { uniqueId, escape };
