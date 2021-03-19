/*
 * BiwaScheme 0.7.2 - R6RS/R7RS Scheme in JavaScript
 *
 * Copyright (c) 2007-2021 Yutaka HARA (http://www.biwascheme.org/)
 * Licensed under the MIT license.
 */ /*!
 * jQuery JavaScript Library v3.6.0
 * https://jquery.com/
 *
 * Includes Sizzle.js
 * https://sizzlejs.com/
 *
 * Copyright OpenJS Foundation and other contributors
 * Released under the MIT license
 * https://jquery.org/license
 *
 * Date: 2021-03-02T17:08Z
 */
(function (global, factory) {
  "use strict";

  if (typeof module === "object" && typeof module.exports === "object") {
    // For CommonJS and CommonJS-like environments where a proper `window`
    // is present, execute the factory and get jQuery.
    // For environments that do not have a `window` with a `document`
    // (such as Node.js), expose a factory as module.exports.
    // This accentuates the need for the creation of a real `window`.
    // e.g. var jQuery = require("jquery")(window);
    // See ticket #14549 for more info.
    module.exports = global.document
      ? factory(global, true)
      : function (w) {
          if (!w.document) {
            throw new Error("jQuery requires a window with a document");
          }
          return factory(w);
        };
  } else {
    factory(global);
  }

  // Pass this if window is not defined yet
})(typeof window !== "undefined" ? window : this, function (window, noGlobal) {
  // Edge <= 12 - 13+, Firefox <=18 - 45+, IE 10 - 11, Safari 5.1 - 9+, iOS 6 - 9.1
  // throw exceptions when non-strict code (e.g., ASP.NET 4.5) accesses strict mode
  // arguments.callee.caller (trac-13335). But as of jQuery 3.0 (2016), strict mode should be common
  // enough that all such attempts are guarded in a try block.
  "use strict";

  var arr = [];

  var getProto = Object.getPrototypeOf;

  var slice = arr.slice;

  var flat = arr.flat
    ? function (array) {
        return arr.flat.call(array);
      }
    : function (array) {
        return arr.concat.apply([], array);
      };

  var push = arr.push;

  var indexOf = arr.indexOf;

  var class2type = {};

  var toString = class2type.toString;

  var hasOwn = class2type.hasOwnProperty;

  var fnToString = hasOwn.toString;

  var ObjectFunctionString = fnToString.call(Object);

  var support = {};

  var isFunction = function isFunction(obj) {
    // Support: Chrome <=57, Firefox <=52
    // In some browsers, typeof returns "function" for HTML <object> elements
    // (i.e., `typeof document.createElement( "object" ) === "function"`).
    // We don't want to classify *any* DOM node as a function.
    // Support: QtWeb <=3.8.5, WebKit <=534.34, wkhtmltopdf tool <=0.12.5
    // Plus for old WebKit, typeof returns "function" for HTML collections
    // (e.g., `typeof document.getElementsByTagName("div") === "function"`). (gh-4756)
    return (
      typeof obj === "function" &&
      typeof obj.nodeType !== "number" &&
      typeof obj.item !== "function"
    );
  };

  var isWindow = function isWindow(obj) {
    return obj != null && obj === obj.window;
  };

  var document = window.document;

  var preservedScriptAttributes = {
    type: true,
    src: true,
    nonce: true,
    noModule: true,
  };

  function DOMEval(code, node, doc) {
    doc = doc || document;

    var i,
      val,
      script = doc.createElement("script");

    script.text = code;
    if (node) {
      for (i in preservedScriptAttributes) {
        // Support: Firefox 64+, Edge 18+
        // Some browsers don't support the "nonce" property on scripts.
        // On the other hand, just using `getAttribute` is not enough as
        // the `nonce` attribute is reset to an empty string whenever it
        // becomes browsing-context connected.
        // See https://github.com/whatwg/html/issues/2369
        // See https://html.spec.whatwg.org/#nonce-attributes
        // The `node.getAttribute` check was added for the sake of
        // `jQuery.globalEval` so that it can fake a nonce-containing node
        // via an object.
        val = node[i] || (node.getAttribute && node.getAttribute(i));
        if (val) {
          script.setAttribute(i, val);
        }
      }
    }
    doc.head.appendChild(script).parentNode.removeChild(script);
  }

  function toType(obj) {
    if (obj == null) {
      return obj + "";
    }

    // Support: Android <=2.3 only (functionish RegExp)
    return typeof obj === "object" || typeof obj === "function"
      ? class2type[toString.call(obj)] || "object"
      : typeof obj;
  }
  /* global Symbol */
  // Defining this global in .eslintrc.json would create a danger of using the global
  // unguarded in another place, it seems safer to define global only for this module

  var version = "3.6.0",
    // Define a local copy of jQuery
    jQuery = function (selector, context) {
      // The jQuery object is actually just the init constructor 'enhanced'
      // Need init if jQuery is called (just allow error to be thrown if not included)
      return new jQuery.fn.init(selector, context);
    };

  jQuery.fn = jQuery.prototype = {
    // The current version of jQuery being used
    jquery: version,

    constructor: jQuery,

    // The default length of a jQuery object is 0
    length: 0,

    toArray: function () {
      return slice.call(this);
    },

    // Get the Nth element in the matched element set OR
    // Get the whole matched element set as a clean array
    get: function (num) {
      // Return all the elements in a clean array
      if (num == null) {
        return slice.call(this);
      }

      // Return just the one element from the set
      return num < 0 ? this[num + this.length] : this[num];
    },

    // Take an array of elements and push it onto the stack
    // (returning the new matched element set)
    pushStack: function (elems) {
      // Build a new jQuery matched element set
      var ret = jQuery.merge(this.constructor(), elems);

      // Add the old object onto the stack (as a reference)
      ret.prevObject = this;

      // Return the newly-formed element set
      return ret;
    },

    // Execute a callback for every element in the matched set.
    each: function (callback) {
      return jQuery.each(this, callback);
    },

    map: function (callback) {
      return this.pushStack(
        jQuery.map(this, function (elem, i) {
          return callback.call(elem, i, elem);
        })
      );
    },

    slice: function () {
      return this.pushStack(slice.apply(this, arguments));
    },

    first: function () {
      return this.eq(0);
    },

    last: function () {
      return this.eq(-1);
    },

    even: function () {
      return this.pushStack(
        jQuery.grep(this, function (_elem, i) {
          return (i + 1) % 2;
        })
      );
    },

    odd: function () {
      return this.pushStack(
        jQuery.grep(this, function (_elem, i) {
          return i % 2;
        })
      );
    },

    eq: function (i) {
      var len = this.length,
        j = +i + (i < 0 ? len : 0);
      return this.pushStack(j >= 0 && j < len ? [this[j]] : []);
    },

    end: function () {
      return this.prevObject || this.constructor();
    },

    // For internal use only.
    // Behaves like an Array's method, not like a jQuery method.
    push: push,
    sort: arr.sort,
    splice: arr.splice,
  };

  jQuery.extend = jQuery.fn.extend = function () {
    var options,
      name,
      src,
      copy,
      copyIsArray,
      clone,
      target = arguments[0] || {},
      i = 1,
      length = arguments.length,
      deep = false;

    // Handle a deep copy situation
    if (typeof target === "boolean") {
      deep = target;

      // Skip the boolean and the target
      target = arguments[i] || {};
      i++;
    }

    // Handle case when target is a string or something (possible in deep copy)
    if (typeof target !== "object" && !isFunction(target)) {
      target = {};
    }

    // Extend jQuery itself if only one argument is passed
    if (i === length) {
      target = this;
      i--;
    }

    for (; i < length; i++) {
      // Only deal with non-null/undefined values
      if ((options = arguments[i]) != null) {
        // Extend the base object
        for (name in options) {
          copy = options[name];

          // Prevent Object.prototype pollution
          // Prevent never-ending loop
          if (name === "__proto__" || target === copy) {
            continue;
          }

          // Recurse if we're merging plain objects or arrays
          if (
            deep &&
            copy &&
            (jQuery.isPlainObject(copy) || (copyIsArray = Array.isArray(copy)))
          ) {
            src = target[name];

            // Ensure proper type for the source value
            if (copyIsArray && !Array.isArray(src)) {
              clone = [];
            } else if (!copyIsArray && !jQuery.isPlainObject(src)) {
              clone = {};
            } else {
              clone = src;
            }
            copyIsArray = false;

            // Never move original objects, clone them
            target[name] = jQuery.extend(deep, clone, copy);

            // Don't bring in undefined values
          } else if (copy !== undefined) {
            target[name] = copy;
          }
        }
      }
    }

    // Return the modified object
    return target;
  };

  jQuery.extend({
    // Unique for each copy of jQuery on the page
    expando: "jQuery" + (version + Math.random()).replace(/\D/g, ""),

    // Assume jQuery is ready without the ready module
    isReady: true,

    error: function (msg) {
      throw new Error(msg);
    },

    noop: function () {},

    isPlainObject: function (obj) {
      var proto, Ctor;

      // Detect obvious negatives
      // Use toString instead of jQuery.type to catch host objects
      if (!obj || toString.call(obj) !== "[object Object]") {
        return false;
      }

      proto = getProto(obj);

      // Objects with no prototype (e.g., `Object.create( null )`) are plain
      if (!proto) {
        return true;
      }

      // Objects with prototype are plain iff they were constructed by a global Object function
      Ctor = hasOwn.call(proto, "constructor") && proto.constructor;
      return (
        typeof Ctor === "function" &&
        fnToString.call(Ctor) === ObjectFunctionString
      );
    },

    isEmptyObject: function (obj) {
      var name;

      for (name in obj) {
        return false;
      }
      return true;
    },

    // Evaluates a script in a provided context; falls back to the global one
    // if not specified.
    globalEval: function (code, options, doc) {
      DOMEval(code, { nonce: options && options.nonce }, doc);
    },

    each: function (obj, callback) {
      var length,
        i = 0;

      if (isArrayLike(obj)) {
        length = obj.length;
        for (; i < length; i++) {
          if (callback.call(obj[i], i, obj[i]) === false) {
            break;
          }
        }
      } else {
        for (i in obj) {
          if (callback.call(obj[i], i, obj[i]) === false) {
            break;
          }
        }
      }

      return obj;
    },

    // results is for internal usage only
    makeArray: function (arr, results) {
      var ret = results || [];

      if (arr != null) {
        if (isArrayLike(Object(arr))) {
          jQuery.merge(ret, typeof arr === "string" ? [arr] : arr);
        } else {
          push.call(ret, arr);
        }
      }

      return ret;
    },

    inArray: function (elem, arr, i) {
      return arr == null ? -1 : indexOf.call(arr, elem, i);
    },

    // Support: Android <=4.0 only, PhantomJS 1 only
    // push.apply(_, arraylike) throws on ancient WebKit
    merge: function (first, second) {
      var len = +second.length,
        j = 0,
        i = first.length;

      for (; j < len; j++) {
        first[i++] = second[j];
      }

      first.length = i;

      return first;
    },

    grep: function (elems, callback, invert) {
      var callbackInverse,
        matches = [],
        i = 0,
        length = elems.length,
        callbackExpect = !invert;

      // Go through the array, only saving the items
      // that pass the validator function
      for (; i < length; i++) {
        callbackInverse = !callback(elems[i], i);
        if (callbackInverse !== callbackExpect) {
          matches.push(elems[i]);
        }
      }

      return matches;
    },

    // arg is for internal usage only
    map: function (elems, callback, arg) {
      var length,
        value,
        i = 0,
        ret = [];

      // Go through the array, translating each of the items to their new values
      if (isArrayLike(elems)) {
        length = elems.length;
        for (; i < length; i++) {
          value = callback(elems[i], i, arg);

          if (value != null) {
            ret.push(value);
          }
        }

        // Go through every key on the object,
      } else {
        for (i in elems) {
          value = callback(elems[i], i, arg);

          if (value != null) {
            ret.push(value);
          }
        }
      }

      // Flatten any nested arrays
      return flat(ret);
    },

    // A global GUID counter for objects
    guid: 1,

    // jQuery.support is not used in Core but other projects attach their
    // properties to it so it needs to exist.
    support: support,
  });

  if (typeof Symbol === "function") {
    jQuery.fn[Symbol.iterator] = arr[Symbol.iterator];
  }

  // Populate the class2type map
  jQuery.each(
    "Boolean Number String Function Array Date RegExp Object Error Symbol".split(
      " "
    ),
    function (_i, name) {
      class2type["[object " + name + "]"] = name.toLowerCase();
    }
  );

  function isArrayLike(obj) {
    // Support: real iOS 8.2 only (not reproducible in simulator)
    // `in` check used to prevent JIT error (gh-2145)
    // hasOwn isn't used here due to false negatives
    // regarding Nodelist length in IE
    var length = !!obj && "length" in obj && obj.length,
      type = toType(obj);

    if (isFunction(obj) || isWindow(obj)) {
      return false;
    }

    return (
      type === "array" ||
      length === 0 ||
      (typeof length === "number" && length > 0 && length - 1 in obj)
    );
  }
  var Sizzle =
    /*!
     * Sizzle CSS Selector Engine v2.3.6
     * https://sizzlejs.com/
     *
     * Copyright JS Foundation and other contributors
     * Released under the MIT license
     * https://js.foundation/
     *
     * Date: 2021-02-16
     */
    (function (window) {
      var i,
        support,
        Expr,
        getText,
        isXML,
        tokenize,
        compile,
        select,
        outermostContext,
        sortInput,
        hasDuplicate,
        // Local document vars
        setDocument,
        document,
        docElem,
        documentIsHTML,
        rbuggyQSA,
        rbuggyMatches,
        matches,
        contains,
        // Instance-specific data
        expando = "sizzle" + 1 * new Date(),
        preferredDoc = window.document,
        dirruns = 0,
        done = 0,
        classCache = createCache(),
        tokenCache = createCache(),
        compilerCache = createCache(),
        nonnativeSelectorCache = createCache(),
        sortOrder = function (a, b) {
          if (a === b) {
            hasDuplicate = true;
          }
          return 0;
        },
        // Instance methods
        hasOwn = {}.hasOwnProperty,
        arr = [],
        pop = arr.pop,
        pushNative = arr.push,
        push = arr.push,
        slice = arr.slice,
        // Use a stripped-down indexOf as it's faster than native
        // https://jsperf.com/thor-indexof-vs-for/5
        indexOf = function (list, elem) {
          var i = 0,
            len = list.length;
          for (; i < len; i++) {
            if (list[i] === elem) {
              return i;
            }
          }
          return -1;
        },
        booleans =
          "checked|selected|async|autofocus|autoplay|controls|defer|disabled|hidden|" +
          "ismap|loop|multiple|open|readonly|required|scoped",
        // Regular expressions

        // http://www.w3.org/TR/css3-selectors/#whitespace
        whitespace = "[\\x20\\t\\r\\n\\f]",
        // https://www.w3.org/TR/css-syntax-3/#ident-token-diagram
        identifier =
          "(?:\\\\[\\da-fA-F]{1,6}" +
          whitespace +
          "?|\\\\[^\\r\\n\\f]|[\\w-]|[^\0-\\x7f])+",
        // Attribute selectors: http://www.w3.org/TR/selectors/#attribute-selectors
        attributes =
          "\\[" +
          whitespace +
          "*(" +
          identifier +
          ")(?:" +
          whitespace +
          // Operator (capture 2)
          "*([*^$|!~]?=)" +
          whitespace +
          // "Attribute values must be CSS identifiers [capture 5]
          // or strings [capture 3 or capture 4]"
          "*(?:'((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\"|(" +
          identifier +
          "))|)" +
          whitespace +
          "*\\]",
        pseudos =
          ":(" +
          identifier +
          ")(?:\\((" +
          // To reduce the number of selectors needing tokenize in the preFilter, prefer arguments:
          // 1. quoted (capture 3; capture 4 or capture 5)
          "('((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\")|" +
          // 2. simple (capture 6)
          "((?:\\\\.|[^\\\\()[\\]]|" +
          attributes +
          ")*)|" +
          // 3. anything else (capture 2)
          ".*" +
          ")\\)|)",
        // Leading and non-escaped trailing whitespace, capturing some non-whitespace characters preceding the latter
        rwhitespace = new RegExp(whitespace + "+", "g"),
        rtrim = new RegExp(
          "^" + whitespace + "+|((?:^|[^\\\\])(?:\\\\.)*)" + whitespace + "+$",
          "g"
        ),
        rcomma = new RegExp("^" + whitespace + "*," + whitespace + "*"),
        rcombinators = new RegExp(
          "^" + whitespace + "*([>+~]|" + whitespace + ")" + whitespace + "*"
        ),
        rdescend = new RegExp(whitespace + "|>"),
        rpseudo = new RegExp(pseudos),
        ridentifier = new RegExp("^" + identifier + "$"),
        matchExpr = {
          ID: new RegExp("^#(" + identifier + ")"),
          CLASS: new RegExp("^\\.(" + identifier + ")"),
          TAG: new RegExp("^(" + identifier + "|[*])"),
          ATTR: new RegExp("^" + attributes),
          PSEUDO: new RegExp("^" + pseudos),
          CHILD: new RegExp(
            "^:(only|first|last|nth|nth-last)-(child|of-type)(?:\\(" +
              whitespace +
              "*(even|odd|(([+-]|)(\\d*)n|)" +
              whitespace +
              "*(?:([+-]|)" +
              whitespace +
              "*(\\d+)|))" +
              whitespace +
              "*\\)|)",
            "i"
          ),
          bool: new RegExp("^(?:" + booleans + ")$", "i"),

          // For use in libraries implementing .is()
          // We use this for POS matching in `select`
          needsContext: new RegExp(
            "^" +
              whitespace +
              "*[>+~]|:(even|odd|eq|gt|lt|nth|first|last)(?:\\(" +
              whitespace +
              "*((?:-\\d)?\\d*)" +
              whitespace +
              "*\\)|)(?=[^-]|$)",
            "i"
          ),
        },
        rhtml = /HTML$/i,
        rinputs = /^(?:input|select|textarea|button)$/i,
        rheader = /^h\d$/i,
        rnative = /^[^{]+\{\s*\[native \w/,
        // Easily-parseable/retrievable ID or TAG or CLASS selectors
        rquickExpr = /^(?:#([\w-]+)|(\w+)|\.([\w-]+))$/,
        rsibling = /[+~]/,
        // CSS escapes
        // http://www.w3.org/TR/CSS21/syndata.html#escaped-characters
        runescape = new RegExp(
          "\\\\[\\da-fA-F]{1,6}" + whitespace + "?|\\\\([^\\r\\n\\f])",
          "g"
        ),
        funescape = function (escape, nonHex) {
          var high = "0x" + escape.slice(1) - 0x10000;

          return nonHex
            ? // Strip the backslash prefix from a non-hex escape sequence
              nonHex
            : // Replace a hexadecimal escape sequence with the encoded Unicode code point
            // Support: IE <=11+
            // For values outside the Basic Multilingual Plane (BMP), manually construct a
            // surrogate pair
            high < 0
            ? String.fromCharCode(high + 0x10000)
            : String.fromCharCode(
                (high >> 10) | 0xd800,
                (high & 0x3ff) | 0xdc00
              );
        },
        // CSS string/identifier serialization
        // https://drafts.csswg.org/cssom/#common-serializing-idioms
        rcssescape = /([\0-\x1f\x7f]|^-?\d)|^-$|[^\0-\x1f\x7f-\uFFFF\w-]/g,
        fcssescape = function (ch, asCodePoint) {
          if (asCodePoint) {
            // U+0000 NULL becomes U+FFFD REPLACEMENT CHARACTER
            if (ch === "\0") {
              return "\uFFFD";
            }

            // Control characters and (dependent upon position) numbers get escaped as code points
            return (
              ch.slice(0, -1) +
              "\\" +
              ch.charCodeAt(ch.length - 1).toString(16) +
              " "
            );
          }

          // Other potentially-special ASCII characters get backslash-escaped
          return "\\" + ch;
        },
        // Used for iframes
        // See setDocument()
        // Removing the function wrapper causes a "Permission Denied"
        // error in IE
        unloadHandler = function () {
          setDocument();
        },
        inDisabledFieldset = addCombinator(
          function (elem) {
            return (
              elem.disabled === true &&
              elem.nodeName.toLowerCase() === "fieldset"
            );
          },
          { dir: "parentNode", next: "legend" }
        );

      // Optimize for push.apply( _, NodeList )
      try {
        push.apply(
          (arr = slice.call(preferredDoc.childNodes)),
          preferredDoc.childNodes
        );

        // Support: Android<4.0
        // Detect silently failing push.apply
        // eslint-disable-next-line no-unused-expressions
        arr[preferredDoc.childNodes.length].nodeType;
      } catch (e) {
        push = {
          apply: arr.length
            ? // Leverage slice if possible
              function (target, els) {
                pushNative.apply(target, slice.call(els));
              }
            : // Support: IE<9
              // Otherwise append directly
              function (target, els) {
                var j = target.length,
                  i = 0;

                // Can't trust NodeList.length
                while ((target[j++] = els[i++])) {}
                target.length = j - 1;
              },
        };
      }

      function Sizzle(selector, context, results, seed) {
        var m,
          i,
          elem,
          nid,
          match,
          groups,
          newSelector,
          newContext = context && context.ownerDocument,
          // nodeType defaults to 9, since context defaults to document
          nodeType = context ? context.nodeType : 9;

        results = results || [];

        // Return early from calls with invalid selector or context
        if (
          typeof selector !== "string" ||
          !selector ||
          (nodeType !== 1 && nodeType !== 9 && nodeType !== 11)
        ) {
          return results;
        }

        // Try to shortcut find operations (as opposed to filters) in HTML documents
        if (!seed) {
          setDocument(context);
          context = context || document;

          if (documentIsHTML) {
            // If the selector is sufficiently simple, try using a "get*By*" DOM method
            // (excepting DocumentFragment context, where the methods don't exist)
            if (nodeType !== 11 && (match = rquickExpr.exec(selector))) {
              // ID selector
              if ((m = match[1])) {
                // Document context
                if (nodeType === 9) {
                  if ((elem = context.getElementById(m))) {
                    // Support: IE, Opera, Webkit
                    // TODO: identify versions
                    // getElementById can match elements by name instead of ID
                    if (elem.id === m) {
                      results.push(elem);
                      return results;
                    }
                  } else {
                    return results;
                  }

                  // Element context
                } else {
                  // Support: IE, Opera, Webkit
                  // TODO: identify versions
                  // getElementById can match elements by name instead of ID
                  if (
                    newContext &&
                    (elem = newContext.getElementById(m)) &&
                    contains(context, elem) &&
                    elem.id === m
                  ) {
                    results.push(elem);
                    return results;
                  }
                }

                // Type selector
              } else if (match[2]) {
                push.apply(results, context.getElementsByTagName(selector));
                return results;

                // Class selector
              } else if (
                (m = match[3]) &&
                support.getElementsByClassName &&
                context.getElementsByClassName
              ) {
                push.apply(results, context.getElementsByClassName(m));
                return results;
              }
            }

            // Take advantage of querySelectorAll
            if (
              support.qsa &&
              !nonnativeSelectorCache[selector + " "] &&
              (!rbuggyQSA || !rbuggyQSA.test(selector)) &&
              // Support: IE 8 only
              // Exclude object elements
              (nodeType !== 1 || context.nodeName.toLowerCase() !== "object")
            ) {
              newSelector = selector;
              newContext = context;

              // qSA considers elements outside a scoping root when evaluating child or
              // descendant combinators, which is not what we want.
              // In such cases, we work around the behavior by prefixing every selector in the
              // list with an ID selector referencing the scope context.
              // The technique has to be used as well when a leading combinator is used
              // as such selectors are not recognized by querySelectorAll.
              // Thanks to Andrew Dupont for this technique.
              if (
                nodeType === 1 &&
                (rdescend.test(selector) || rcombinators.test(selector))
              ) {
                // Expand context for sibling selectors
                newContext =
                  (rsibling.test(selector) &&
                    testContext(context.parentNode)) ||
                  context;

                // We can use :scope instead of the ID hack if the browser
                // supports it & if we're not changing the context.
                if (newContext !== context || !support.scope) {
                  // Capture the context ID, setting it first if necessary
                  if ((nid = context.getAttribute("id"))) {
                    nid = nid.replace(rcssescape, fcssescape);
                  } else {
                    context.setAttribute("id", (nid = expando));
                  }
                }

                // Prefix every selector in the list
                groups = tokenize(selector);
                i = groups.length;
                while (i--) {
                  groups[i] =
                    (nid ? "#" + nid : ":scope") + " " + toSelector(groups[i]);
                }
                newSelector = groups.join(",");
              }

              try {
                push.apply(results, newContext.querySelectorAll(newSelector));
                return results;
              } catch (qsaError) {
                nonnativeSelectorCache(selector, true);
              } finally {
                if (nid === expando) {
                  context.removeAttribute("id");
                }
              }
            }
          }
        }

        // All others
        return select(selector.replace(rtrim, "$1"), context, results, seed);
      }

      /**
       * Create key-value caches of limited size
       * @returns {function(string, object)} Returns the Object data after storing it on itself with
       *	property name the (space-suffixed) string and (if the cache is larger than Expr.cacheLength)
       *	deleting the oldest entry
       */
      function createCache() {
        var keys = [];

        function cache(key, value) {
          // Use (key + " ") to avoid collision with native prototype properties (see Issue #157)
          if (keys.push(key + " ") > Expr.cacheLength) {
            // Only keep the most recent entries
            delete cache[keys.shift()];
          }
          return (cache[key + " "] = value);
        }
        return cache;
      }

      /**
       * Mark a function for special use by Sizzle
       * @param {Function} fn The function to mark
       */
      function markFunction(fn) {
        fn[expando] = true;
        return fn;
      }

      /**
       * Support testing using an element
       * @param {Function} fn Passed the created element and returns a boolean result
       */
      function assert(fn) {
        var el = document.createElement("fieldset");

        try {
          return !!fn(el);
        } catch (e) {
          return false;
        } finally {
          // Remove from its parent by default
          if (el.parentNode) {
            el.parentNode.removeChild(el);
          }

          // release memory in IE
          el = null;
        }
      }

      /**
       * Adds the same handler for all of the specified attrs
       * @param {String} attrs Pipe-separated list of attributes
       * @param {Function} handler The method that will be applied
       */
      function addHandle(attrs, handler) {
        var arr = attrs.split("|"),
          i = arr.length;

        while (i--) {
          Expr.attrHandle[arr[i]] = handler;
        }
      }

      /**
       * Checks document order of two siblings
       * @param {Element} a
       * @param {Element} b
       * @returns {Number} Returns less than 0 if a precedes b, greater than 0 if a follows b
       */
      function siblingCheck(a, b) {
        var cur = b && a,
          diff =
            cur &&
            a.nodeType === 1 &&
            b.nodeType === 1 &&
            a.sourceIndex - b.sourceIndex;

        // Use IE sourceIndex if available on both nodes
        if (diff) {
          return diff;
        }

        // Check if b follows a
        if (cur) {
          while ((cur = cur.nextSibling)) {
            if (cur === b) {
              return -1;
            }
          }
        }

        return a ? 1 : -1;
      }

      /**
       * Returns a function to use in pseudos for input types
       * @param {String} type
       */
      function createInputPseudo(type) {
        return function (elem) {
          var name = elem.nodeName.toLowerCase();
          return name === "input" && elem.type === type;
        };
      }

      /**
       * Returns a function to use in pseudos for buttons
       * @param {String} type
       */
      function createButtonPseudo(type) {
        return function (elem) {
          var name = elem.nodeName.toLowerCase();
          return (name === "input" || name === "button") && elem.type === type;
        };
      }

      /**
       * Returns a function to use in pseudos for :enabled/:disabled
       * @param {Boolean} disabled true for :disabled; false for :enabled
       */
      function createDisabledPseudo(disabled) {
        // Known :disabled false positives: fieldset[disabled] > legend:nth-of-type(n+2) :can-disable
        return function (elem) {
          // Only certain elements can match :enabled or :disabled
          // https://html.spec.whatwg.org/multipage/scripting.html#selector-enabled
          // https://html.spec.whatwg.org/multipage/scripting.html#selector-disabled
          if ("form" in elem) {
            // Check for inherited disabledness on relevant non-disabled elements:
            // * listed form-associated elements in a disabled fieldset
            //   https://html.spec.whatwg.org/multipage/forms.html#category-listed
            //   https://html.spec.whatwg.org/multipage/forms.html#concept-fe-disabled
            // * option elements in a disabled optgroup
            //   https://html.spec.whatwg.org/multipage/forms.html#concept-option-disabled
            // All such elements have a "form" property.
            if (elem.parentNode && elem.disabled === false) {
              // Option elements defer to a parent optgroup if present
              if ("label" in elem) {
                if ("label" in elem.parentNode) {
                  return elem.parentNode.disabled === disabled;
                } else {
                  return elem.disabled === disabled;
                }
              }

              // Support: IE 6 - 11
              // Use the isDisabled shortcut property to check for disabled fieldset ancestors
              return (
                elem.isDisabled === disabled ||
                // Where there is no isDisabled, check manually
                /* jshint -W018 */
                (elem.isDisabled !== !disabled &&
                  inDisabledFieldset(elem) === disabled)
              );
            }

            return elem.disabled === disabled;

            // Try to winnow out elements that can't be disabled before trusting the disabled property.
            // Some victims get caught in our net (label, legend, menu, track), but it shouldn't
            // even exist on them, let alone have a boolean value.
          } else if ("label" in elem) {
            return elem.disabled === disabled;
          }

          // Remaining elements are neither :enabled nor :disabled
          return false;
        };
      }

      /**
       * Returns a function to use in pseudos for positionals
       * @param {Function} fn
       */
      function createPositionalPseudo(fn) {
        return markFunction(function (argument) {
          argument = +argument;
          return markFunction(function (seed, matches) {
            var j,
              matchIndexes = fn([], seed.length, argument),
              i = matchIndexes.length;

            // Match elements found at the specified indexes
            while (i--) {
              if (seed[(j = matchIndexes[i])]) {
                seed[j] = !(matches[j] = seed[j]);
              }
            }
          });
        });
      }

      /**
       * Checks a node for validity as a Sizzle context
       * @param {Element|Object=} context
       * @returns {Element|Object|Boolean} The input node if acceptable, otherwise a falsy value
       */
      function testContext(context) {
        return (
          context &&
          typeof context.getElementsByTagName !== "undefined" &&
          context
        );
      }

      // Expose support vars for convenience
      support = Sizzle.support = {};

      /**
       * Detects XML nodes
       * @param {Element|Object} elem An element or a document
       * @returns {Boolean} True iff elem is a non-HTML XML node
       */
      isXML = Sizzle.isXML = function (elem) {
        var namespace = elem && elem.namespaceURI,
          docElem = elem && (elem.ownerDocument || elem).documentElement;

        // Support: IE <=8
        // Assume HTML when documentElement doesn't yet exist, such as inside loading iframes
        // https://bugs.jquery.com/ticket/4833
        return !rhtml.test(
          namespace || (docElem && docElem.nodeName) || "HTML"
        );
      };

      /**
       * Sets document-related variables once based on the current document
       * @param {Element|Object} [doc] An element or document object to use to set the document
       * @returns {Object} Returns the current document
       */
      setDocument = Sizzle.setDocument = function (node) {
        var hasCompare,
          subWindow,
          doc = node ? node.ownerDocument || node : preferredDoc;

        // Return early if doc is invalid or already selected
        // Support: IE 11+, Edge 17 - 18+
        // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
        // two documents; shallow comparisons work.
        // eslint-disable-next-line eqeqeq
        if (doc == document || doc.nodeType !== 9 || !doc.documentElement) {
          return document;
        }

        // Update global variables
        document = doc;
        docElem = document.documentElement;
        documentIsHTML = !isXML(document);

        // Support: IE 9 - 11+, Edge 12 - 18+
        // Accessing iframe documents after unload throws "permission denied" errors (jQuery #13936)
        // Support: IE 11+, Edge 17 - 18+
        // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
        // two documents; shallow comparisons work.
        // eslint-disable-next-line eqeqeq
        if (
          preferredDoc != document &&
          (subWindow = document.defaultView) &&
          subWindow.top !== subWindow
        ) {
          // Support: IE 11, Edge
          if (subWindow.addEventListener) {
            subWindow.addEventListener("unload", unloadHandler, false);

            // Support: IE 9 - 10 only
          } else if (subWindow.attachEvent) {
            subWindow.attachEvent("onunload", unloadHandler);
          }
        }

        // Support: IE 8 - 11+, Edge 12 - 18+, Chrome <=16 - 25 only, Firefox <=3.6 - 31 only,
        // Safari 4 - 5 only, Opera <=11.6 - 12.x only
        // IE/Edge & older browsers don't support the :scope pseudo-class.
        // Support: Safari 6.0 only
        // Safari 6.0 supports :scope but it's an alias of :root there.
        support.scope = assert(function (el) {
          docElem.appendChild(el).appendChild(document.createElement("div"));
          return (
            typeof el.querySelectorAll !== "undefined" &&
            !el.querySelectorAll(":scope fieldset div").length
          );
        });

        /* Attributes
	---------------------------------------------------------------------- */

        // Support: IE<8
        // Verify that getAttribute really returns attributes and not properties
        // (excepting IE8 booleans)
        support.attributes = assert(function (el) {
          el.className = "i";
          return !el.getAttribute("className");
        });

        /* getElement(s)By*
	---------------------------------------------------------------------- */

        // Check if getElementsByTagName("*") returns only elements
        support.getElementsByTagName = assert(function (el) {
          el.appendChild(document.createComment(""));
          return !el.getElementsByTagName("*").length;
        });

        // Support: IE<9
        support.getElementsByClassName = rnative.test(
          document.getElementsByClassName
        );

        // Support: IE<10
        // Check if getElementById returns elements by name
        // The broken getElementById methods don't pick up programmatically-set names,
        // so use a roundabout getElementsByName test
        support.getById = assert(function (el) {
          docElem.appendChild(el).id = expando;
          return (
            !document.getElementsByName ||
            !document.getElementsByName(expando).length
          );
        });

        // ID filter and find
        if (support.getById) {
          Expr.filter["ID"] = function (id) {
            var attrId = id.replace(runescape, funescape);
            return function (elem) {
              return elem.getAttribute("id") === attrId;
            };
          };
          Expr.find["ID"] = function (id, context) {
            if (
              typeof context.getElementById !== "undefined" &&
              documentIsHTML
            ) {
              var elem = context.getElementById(id);
              return elem ? [elem] : [];
            }
          };
        } else {
          Expr.filter["ID"] = function (id) {
            var attrId = id.replace(runescape, funescape);
            return function (elem) {
              var node =
                typeof elem.getAttributeNode !== "undefined" &&
                elem.getAttributeNode("id");
              return node && node.value === attrId;
            };
          };

          // Support: IE 6 - 7 only
          // getElementById is not reliable as a find shortcut
          Expr.find["ID"] = function (id, context) {
            if (
              typeof context.getElementById !== "undefined" &&
              documentIsHTML
            ) {
              var node,
                i,
                elems,
                elem = context.getElementById(id);

              if (elem) {
                // Verify the id attribute
                node = elem.getAttributeNode("id");
                if (node && node.value === id) {
                  return [elem];
                }

                // Fall back on getElementsByName
                elems = context.getElementsByName(id);
                i = 0;
                while ((elem = elems[i++])) {
                  node = elem.getAttributeNode("id");
                  if (node && node.value === id) {
                    return [elem];
                  }
                }
              }

              return [];
            }
          };
        }

        // Tag
        Expr.find["TAG"] = support.getElementsByTagName
          ? function (tag, context) {
              if (typeof context.getElementsByTagName !== "undefined") {
                return context.getElementsByTagName(tag);

                // DocumentFragment nodes don't have gEBTN
              } else if (support.qsa) {
                return context.querySelectorAll(tag);
              }
            }
          : function (tag, context) {
              var elem,
                tmp = [],
                i = 0,
                // By happy coincidence, a (broken) gEBTN appears on DocumentFragment nodes too
                results = context.getElementsByTagName(tag);

              // Filter out possible comments
              if (tag === "*") {
                while ((elem = results[i++])) {
                  if (elem.nodeType === 1) {
                    tmp.push(elem);
                  }
                }

                return tmp;
              }
              return results;
            };

        // Class
        Expr.find["CLASS"] =
          support.getElementsByClassName &&
          function (className, context) {
            if (
              typeof context.getElementsByClassName !== "undefined" &&
              documentIsHTML
            ) {
              return context.getElementsByClassName(className);
            }
          };

        /* QSA/matchesSelector
	---------------------------------------------------------------------- */

        // QSA and matchesSelector support

        // matchesSelector(:active) reports false when true (IE9/Opera 11.5)
        rbuggyMatches = [];

        // qSa(:focus) reports false when true (Chrome 21)
        // We allow this because of a bug in IE8/9 that throws an error
        // whenever `document.activeElement` is accessed on an iframe
        // So, we allow :focus to pass through QSA all the time to avoid the IE error
        // See https://bugs.jquery.com/ticket/13378
        rbuggyQSA = [];

        if ((support.qsa = rnative.test(document.querySelectorAll))) {
          // Build QSA regex
          // Regex strategy adopted from Diego Perini
          assert(function (el) {
            var input;

            // Select is set to empty string on purpose
            // This is to test IE's treatment of not explicitly
            // setting a boolean content attribute,
            // since its presence should be enough
            // https://bugs.jquery.com/ticket/12359
            docElem.appendChild(el).innerHTML =
              "<a id='" +
              expando +
              "'></a>" +
              "<select id='" +
              expando +
              "-\r\\' msallowcapture=''>" +
              "<option selected=''></option></select>";

            // Support: IE8, Opera 11-12.16
            // Nothing should be selected when empty strings follow ^= or $= or *=
            // The test attribute must be unknown in Opera but "safe" for WinRT
            // https://msdn.microsoft.com/en-us/library/ie/hh465388.aspx#attribute_section
            if (el.querySelectorAll("[msallowcapture^='']").length) {
              rbuggyQSA.push("[*^$]=" + whitespace + "*(?:''|\"\")");
            }

            // Support: IE8
            // Boolean attributes and "value" are not treated correctly
            if (!el.querySelectorAll("[selected]").length) {
              rbuggyQSA.push(
                "\\[" + whitespace + "*(?:value|" + booleans + ")"
              );
            }

            // Support: Chrome<29, Android<4.4, Safari<7.0+, iOS<7.0+, PhantomJS<1.9.8+
            if (!el.querySelectorAll("[id~=" + expando + "-]").length) {
              rbuggyQSA.push("~=");
            }

            // Support: IE 11+, Edge 15 - 18+
            // IE 11/Edge don't find elements on a `[name='']` query in some cases.
            // Adding a temporary attribute to the document before the selection works
            // around the issue.
            // Interestingly, IE 10 & older don't seem to have the issue.
            input = document.createElement("input");
            input.setAttribute("name", "");
            el.appendChild(input);
            if (!el.querySelectorAll("[name='']").length) {
              rbuggyQSA.push(
                "\\[" +
                  whitespace +
                  "*name" +
                  whitespace +
                  "*=" +
                  whitespace +
                  "*(?:''|\"\")"
              );
            }

            // Webkit/Opera - :checked should return selected option elements
            // http://www.w3.org/TR/2011/REC-css3-selectors-20110929/#checked
            // IE8 throws error here and will not see later tests
            if (!el.querySelectorAll(":checked").length) {
              rbuggyQSA.push(":checked");
            }

            // Support: Safari 8+, iOS 8+
            // https://bugs.webkit.org/show_bug.cgi?id=136851
            // In-page `selector#id sibling-combinator selector` fails
            if (!el.querySelectorAll("a#" + expando + "+*").length) {
              rbuggyQSA.push(".#.+[+~]");
            }

            // Support: Firefox <=3.6 - 5 only
            // Old Firefox doesn't throw on a badly-escaped identifier.
            el.querySelectorAll("\\\f");
            rbuggyQSA.push("[\\r\\n\\f]");
          });

          assert(function (el) {
            el.innerHTML =
              "<a href='' disabled='disabled'></a>" +
              "<select disabled='disabled'><option/></select>";

            // Support: Windows 8 Native Apps
            // The type and name attributes are restricted during .innerHTML assignment
            var input = document.createElement("input");
            input.setAttribute("type", "hidden");
            el.appendChild(input).setAttribute("name", "D");

            // Support: IE8
            // Enforce case-sensitivity of name attribute
            if (el.querySelectorAll("[name=d]").length) {
              rbuggyQSA.push("name" + whitespace + "*[*^$|!~]?=");
            }

            // FF 3.5 - :enabled/:disabled and hidden elements (hidden elements are still enabled)
            // IE8 throws error here and will not see later tests
            if (el.querySelectorAll(":enabled").length !== 2) {
              rbuggyQSA.push(":enabled", ":disabled");
            }

            // Support: IE9-11+
            // IE's :disabled selector does not pick up the children of disabled fieldsets
            docElem.appendChild(el).disabled = true;
            if (el.querySelectorAll(":disabled").length !== 2) {
              rbuggyQSA.push(":enabled", ":disabled");
            }

            // Support: Opera 10 - 11 only
            // Opera 10-11 does not throw on post-comma invalid pseudos
            el.querySelectorAll("*,:x");
            rbuggyQSA.push(",.*:");
          });
        }

        if (
          (support.matchesSelector = rnative.test(
            (matches =
              docElem.matches ||
              docElem.webkitMatchesSelector ||
              docElem.mozMatchesSelector ||
              docElem.oMatchesSelector ||
              docElem.msMatchesSelector)
          ))
        ) {
          assert(function (el) {
            // Check to see if it's possible to do matchesSelector
            // on a disconnected node (IE 9)
            support.disconnectedMatch = matches.call(el, "*");

            // This should fail with an exception
            // Gecko does not error, returns false instead
            matches.call(el, "[s!='']:x");
            rbuggyMatches.push("!=", pseudos);
          });
        }

        rbuggyQSA = rbuggyQSA.length && new RegExp(rbuggyQSA.join("|"));
        rbuggyMatches =
          rbuggyMatches.length && new RegExp(rbuggyMatches.join("|"));

        /* Contains
	---------------------------------------------------------------------- */
        hasCompare = rnative.test(docElem.compareDocumentPosition);

        // Element contains another
        // Purposefully self-exclusive
        // As in, an element does not contain itself
        contains =
          hasCompare || rnative.test(docElem.contains)
            ? function (a, b) {
                var adown = a.nodeType === 9 ? a.documentElement : a,
                  bup = b && b.parentNode;
                return (
                  a === bup ||
                  !!(
                    bup &&
                    bup.nodeType === 1 &&
                    (adown.contains
                      ? adown.contains(bup)
                      : a.compareDocumentPosition &&
                        a.compareDocumentPosition(bup) & 16)
                  )
                );
              }
            : function (a, b) {
                if (b) {
                  while ((b = b.parentNode)) {
                    if (b === a) {
                      return true;
                    }
                  }
                }
                return false;
              };

        /* Sorting
	---------------------------------------------------------------------- */

        // Document order sorting
        sortOrder = hasCompare
          ? function (a, b) {
              // Flag for duplicate removal
              if (a === b) {
                hasDuplicate = true;
                return 0;
              }

              // Sort on method existence if only one input has compareDocumentPosition
              var compare =
                !a.compareDocumentPosition - !b.compareDocumentPosition;
              if (compare) {
                return compare;
              }

              // Calculate position if both inputs belong to the same document
              // Support: IE 11+, Edge 17 - 18+
              // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
              // two documents; shallow comparisons work.
              // eslint-disable-next-line eqeqeq
              compare =
                (a.ownerDocument || a) == (b.ownerDocument || b)
                  ? a.compareDocumentPosition(b)
                  : // Otherwise we know they are disconnected
                    1;

              // Disconnected nodes
              if (
                compare & 1 ||
                (!support.sortDetached &&
                  b.compareDocumentPosition(a) === compare)
              ) {
                // Choose the first element that is related to our preferred document
                // Support: IE 11+, Edge 17 - 18+
                // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
                // two documents; shallow comparisons work.
                // eslint-disable-next-line eqeqeq
                if (
                  a == document ||
                  (a.ownerDocument == preferredDoc && contains(preferredDoc, a))
                ) {
                  return -1;
                }

                // Support: IE 11+, Edge 17 - 18+
                // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
                // two documents; shallow comparisons work.
                // eslint-disable-next-line eqeqeq
                if (
                  b == document ||
                  (b.ownerDocument == preferredDoc && contains(preferredDoc, b))
                ) {
                  return 1;
                }

                // Maintain original order
                return sortInput
                  ? indexOf(sortInput, a) - indexOf(sortInput, b)
                  : 0;
              }

              return compare & 4 ? -1 : 1;
            }
          : function (a, b) {
              // Exit early if the nodes are identical
              if (a === b) {
                hasDuplicate = true;
                return 0;
              }

              var cur,
                i = 0,
                aup = a.parentNode,
                bup = b.parentNode,
                ap = [a],
                bp = [b];

              // Parentless nodes are either documents or disconnected
              if (!aup || !bup) {
                // Support: IE 11+, Edge 17 - 18+
                // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
                // two documents; shallow comparisons work.
                /* eslint-disable eqeqeq */
                return a == document
                  ? -1
                  : b == document
                  ? 1
                  : /* eslint-enable eqeqeq */
                  aup
                  ? -1
                  : bup
                  ? 1
                  : sortInput
                  ? indexOf(sortInput, a) - indexOf(sortInput, b)
                  : 0;

                // If the nodes are siblings, we can do a quick check
              } else if (aup === bup) {
                return siblingCheck(a, b);
              }

              // Otherwise we need full lists of their ancestors for comparison
              cur = a;
              while ((cur = cur.parentNode)) {
                ap.unshift(cur);
              }
              cur = b;
              while ((cur = cur.parentNode)) {
                bp.unshift(cur);
              }

              // Walk down the tree looking for a discrepancy
              while (ap[i] === bp[i]) {
                i++;
              }

              return i
                ? // Do a sibling check if the nodes have a common ancestor
                  siblingCheck(ap[i], bp[i])
                : // Otherwise nodes in our document sort first
                // Support: IE 11+, Edge 17 - 18+
                // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
                // two documents; shallow comparisons work.
                /* eslint-disable eqeqeq */
                ap[i] == preferredDoc
                ? -1
                : bp[i] == preferredDoc
                ? 1
                : /* eslint-enable eqeqeq */
                  0;
            };

        return document;
      };

      Sizzle.matches = function (expr, elements) {
        return Sizzle(expr, null, null, elements);
      };

      Sizzle.matchesSelector = function (elem, expr) {
        setDocument(elem);

        if (
          support.matchesSelector &&
          documentIsHTML &&
          !nonnativeSelectorCache[expr + " "] &&
          (!rbuggyMatches || !rbuggyMatches.test(expr)) &&
          (!rbuggyQSA || !rbuggyQSA.test(expr))
        ) {
          try {
            var ret = matches.call(elem, expr);

            // IE 9's matchesSelector returns false on disconnected nodes
            if (
              ret ||
              support.disconnectedMatch ||
              // As well, disconnected nodes are said to be in a document
              // fragment in IE 9
              (elem.document && elem.document.nodeType !== 11)
            ) {
              return ret;
            }
          } catch (e) {
            nonnativeSelectorCache(expr, true);
          }
        }

        return Sizzle(expr, document, null, [elem]).length > 0;
      };

      Sizzle.contains = function (context, elem) {
        // Set document vars if needed
        // Support: IE 11+, Edge 17 - 18+
        // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
        // two documents; shallow comparisons work.
        // eslint-disable-next-line eqeqeq
        if ((context.ownerDocument || context) != document) {
          setDocument(context);
        }
        return contains(context, elem);
      };

      Sizzle.attr = function (elem, name) {
        // Set document vars if needed
        // Support: IE 11+, Edge 17 - 18+
        // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
        // two documents; shallow comparisons work.
        // eslint-disable-next-line eqeqeq
        if ((elem.ownerDocument || elem) != document) {
          setDocument(elem);
        }

        var fn = Expr.attrHandle[name.toLowerCase()],
          // Don't get fooled by Object.prototype properties (jQuery #13807)
          val =
            fn && hasOwn.call(Expr.attrHandle, name.toLowerCase())
              ? fn(elem, name, !documentIsHTML)
              : undefined;

        return val !== undefined
          ? val
          : support.attributes || !documentIsHTML
          ? elem.getAttribute(name)
          : (val = elem.getAttributeNode(name)) && val.specified
          ? val.value
          : null;
      };

      Sizzle.escape = function (sel) {
        return (sel + "").replace(rcssescape, fcssescape);
      };

      Sizzle.error = function (msg) {
        throw new Error("Syntax error, unrecognized expression: " + msg);
      };

      /**
       * Document sorting and removing duplicates
       * @param {ArrayLike} results
       */
      Sizzle.uniqueSort = function (results) {
        var elem,
          duplicates = [],
          j = 0,
          i = 0;

        // Unless we *know* we can detect duplicates, assume their presence
        hasDuplicate = !support.detectDuplicates;
        sortInput = !support.sortStable && results.slice(0);
        results.sort(sortOrder);

        if (hasDuplicate) {
          while ((elem = results[i++])) {
            if (elem === results[i]) {
              j = duplicates.push(i);
            }
          }
          while (j--) {
            results.splice(duplicates[j], 1);
          }
        }

        // Clear input after sorting to release objects
        // See https://github.com/jquery/sizzle/pull/225
        sortInput = null;

        return results;
      };

      /**
       * Utility function for retrieving the text value of an array of DOM nodes
       * @param {Array|Element} elem
       */
      getText = Sizzle.getText = function (elem) {
        var node,
          ret = "",
          i = 0,
          nodeType = elem.nodeType;

        if (!nodeType) {
          // If no nodeType, this is expected to be an array
          while ((node = elem[i++])) {
            // Do not traverse comment nodes
            ret += getText(node);
          }
        } else if (nodeType === 1 || nodeType === 9 || nodeType === 11) {
          // Use textContent for elements
          // innerText usage removed for consistency of new lines (jQuery #11153)
          if (typeof elem.textContent === "string") {
            return elem.textContent;
          } else {
            // Traverse its children
            for (elem = elem.firstChild; elem; elem = elem.nextSibling) {
              ret += getText(elem);
            }
          }
        } else if (nodeType === 3 || nodeType === 4) {
          return elem.nodeValue;
        }

        // Do not include comment or processing instruction nodes

        return ret;
      };

      Expr = Sizzle.selectors = {
        // Can be adjusted by the user
        cacheLength: 50,

        createPseudo: markFunction,

        match: matchExpr,

        attrHandle: {},

        find: {},

        relative: {
          ">": { dir: "parentNode", first: true },
          " ": { dir: "parentNode" },
          "+": { dir: "previousSibling", first: true },
          "~": { dir: "previousSibling" },
        },

        preFilter: {
          ATTR: function (match) {
            match[1] = match[1].replace(runescape, funescape);

            // Move the given value to match[3] whether quoted or unquoted
            match[3] = (match[3] || match[4] || match[5] || "").replace(
              runescape,
              funescape
            );

            if (match[2] === "~=") {
              match[3] = " " + match[3] + " ";
            }

            return match.slice(0, 4);
          },

          CHILD: function (match) {
            /* matches from matchExpr["CHILD"]
				1 type (only|nth|...)
				2 what (child|of-type)
				3 argument (even|odd|\d*|\d*n([+-]\d+)?|...)
				4 xn-component of xn+y argument ([+-]?\d*n|)
				5 sign of xn-component
				6 x of xn-component
				7 sign of y-component
				8 y of y-component
			*/
            match[1] = match[1].toLowerCase();

            if (match[1].slice(0, 3) === "nth") {
              // nth-* requires argument
              if (!match[3]) {
                Sizzle.error(match[0]);
              }

              // numeric x and y parameters for Expr.filter.CHILD
              // remember that false/true cast respectively to 0/1
              match[4] = +(match[4]
                ? match[5] + (match[6] || 1)
                : 2 * (match[3] === "even" || match[3] === "odd"));
              match[5] = +(match[7] + match[8] || match[3] === "odd");

              // other types prohibit arguments
            } else if (match[3]) {
              Sizzle.error(match[0]);
            }

            return match;
          },

          PSEUDO: function (match) {
            var excess,
              unquoted = !match[6] && match[2];

            if (matchExpr["CHILD"].test(match[0])) {
              return null;
            }

            // Accept quoted arguments as-is
            if (match[3]) {
              match[2] = match[4] || match[5] || "";

              // Strip excess characters from unquoted arguments
            } else if (
              unquoted &&
              rpseudo.test(unquoted) &&
              // Get excess from tokenize (recursively)
              (excess = tokenize(unquoted, true)) &&
              // advance to the next closing parenthesis
              (excess =
                unquoted.indexOf(")", unquoted.length - excess) -
                unquoted.length)
            ) {
              // excess is a negative index
              match[0] = match[0].slice(0, excess);
              match[2] = unquoted.slice(0, excess);
            }

            // Return only captures needed by the pseudo filter method (type and argument)
            return match.slice(0, 3);
          },
        },

        filter: {
          TAG: function (nodeNameSelector) {
            var nodeName = nodeNameSelector
              .replace(runescape, funescape)
              .toLowerCase();
            return nodeNameSelector === "*"
              ? function () {
                  return true;
                }
              : function (elem) {
                  return (
                    elem.nodeName && elem.nodeName.toLowerCase() === nodeName
                  );
                };
          },

          CLASS: function (className) {
            var pattern = classCache[className + " "];

            return (
              pattern ||
              ((pattern = new RegExp(
                "(^|" + whitespace + ")" + className + "(" + whitespace + "|$)"
              )) &&
                classCache(className, function (elem) {
                  return pattern.test(
                    (typeof elem.className === "string" && elem.className) ||
                      (typeof elem.getAttribute !== "undefined" &&
                        elem.getAttribute("class")) ||
                      ""
                  );
                }))
            );
          },

          ATTR: function (name, operator, check) {
            return function (elem) {
              var result = Sizzle.attr(elem, name);

              if (result == null) {
                return operator === "!=";
              }
              if (!operator) {
                return true;
              }

              result += "";

              /* eslint-disable max-len */

              return operator === "="
                ? result === check
                : operator === "!="
                ? result !== check
                : operator === "^="
                ? check && result.indexOf(check) === 0
                : operator === "*="
                ? check && result.indexOf(check) > -1
                : operator === "$="
                ? check && result.slice(-check.length) === check
                : operator === "~="
                ? (" " + result.replace(rwhitespace, " ") + " ").indexOf(
                    check
                  ) > -1
                : operator === "|="
                ? result === check ||
                  result.slice(0, check.length + 1) === check + "-"
                : false;
              /* eslint-enable max-len */
            };
          },

          CHILD: function (type, what, _argument, first, last) {
            var simple = type.slice(0, 3) !== "nth",
              forward = type.slice(-4) !== "last",
              ofType = what === "of-type";

            return first === 1 && last === 0
              ? // Shortcut for :nth-*(n)
                function (elem) {
                  return !!elem.parentNode;
                }
              : function (elem, _context, xml) {
                  var cache,
                    uniqueCache,
                    outerCache,
                    node,
                    nodeIndex,
                    start,
                    dir =
                      simple !== forward ? "nextSibling" : "previousSibling",
                    parent = elem.parentNode,
                    name = ofType && elem.nodeName.toLowerCase(),
                    useCache = !xml && !ofType,
                    diff = false;

                  if (parent) {
                    // :(first|last|only)-(child|of-type)
                    if (simple) {
                      while (dir) {
                        node = elem;
                        while ((node = node[dir])) {
                          if (
                            ofType
                              ? node.nodeName.toLowerCase() === name
                              : node.nodeType === 1
                          ) {
                            return false;
                          }
                        }

                        // Reverse direction for :only-* (if we haven't yet done so)
                        start = dir =
                          type === "only" && !start && "nextSibling";
                      }
                      return true;
                    }

                    start = [forward ? parent.firstChild : parent.lastChild];

                    // non-xml :nth-child(...) stores cache data on `parent`
                    if (forward && useCache) {
                      // Seek `elem` from a previously-cached index

                      // ...in a gzip-friendly way
                      node = parent;
                      outerCache = node[expando] || (node[expando] = {});

                      // Support: IE <9 only
                      // Defend against cloned attroperties (jQuery gh-1709)
                      uniqueCache =
                        outerCache[node.uniqueID] ||
                        (outerCache[node.uniqueID] = {});

                      cache = uniqueCache[type] || [];
                      nodeIndex = cache[0] === dirruns && cache[1];
                      diff = nodeIndex && cache[2];
                      node = nodeIndex && parent.childNodes[nodeIndex];

                      while (
                        (node =
                          (++nodeIndex && node && node[dir]) ||
                          // Fallback to seeking `elem` from the start
                          (diff = nodeIndex = 0) ||
                          start.pop())
                      ) {
                        // When found, cache indexes on `parent` and break
                        if (node.nodeType === 1 && ++diff && node === elem) {
                          uniqueCache[type] = [dirruns, nodeIndex, diff];
                          break;
                        }
                      }
                    } else {
                      // Use previously-cached element index if available
                      if (useCache) {
                        // ...in a gzip-friendly way
                        node = elem;
                        outerCache = node[expando] || (node[expando] = {});

                        // Support: IE <9 only
                        // Defend against cloned attroperties (jQuery gh-1709)
                        uniqueCache =
                          outerCache[node.uniqueID] ||
                          (outerCache[node.uniqueID] = {});

                        cache = uniqueCache[type] || [];
                        nodeIndex = cache[0] === dirruns && cache[1];
                        diff = nodeIndex;
                      }

                      // xml :nth-child(...)
                      // or :nth-last-child(...) or :nth(-last)?-of-type(...)
                      if (diff === false) {
                        // Use the same loop as above to seek `elem` from the start
                        while (
                          (node =
                            (++nodeIndex && node && node[dir]) ||
                            (diff = nodeIndex = 0) ||
                            start.pop())
                        ) {
                          if (
                            (ofType
                              ? node.nodeName.toLowerCase() === name
                              : node.nodeType === 1) &&
                            ++diff
                          ) {
                            // Cache the index of each encountered element
                            if (useCache) {
                              outerCache =
                                node[expando] || (node[expando] = {});

                              // Support: IE <9 only
                              // Defend against cloned attroperties (jQuery gh-1709)
                              uniqueCache =
                                outerCache[node.uniqueID] ||
                                (outerCache[node.uniqueID] = {});

                              uniqueCache[type] = [dirruns, diff];
                            }

                            if (node === elem) {
                              break;
                            }
                          }
                        }
                      }
                    }

                    // Incorporate the offset, then check against cycle size
                    diff -= last;
                    return (
                      diff === first ||
                      (diff % first === 0 && diff / first >= 0)
                    );
                  }
                };
          },

          PSEUDO: function (pseudo, argument) {
            // pseudo-class names are case-insensitive
            // http://www.w3.org/TR/selectors/#pseudo-classes
            // Prioritize by case sensitivity in case custom pseudos are added with uppercase letters
            // Remember that setFilters inherits from pseudos
            var args,
              fn =
                Expr.pseudos[pseudo] ||
                Expr.setFilters[pseudo.toLowerCase()] ||
                Sizzle.error("unsupported pseudo: " + pseudo);

            // The user may use createPseudo to indicate that
            // arguments are needed to create the filter function
            // just as Sizzle does
            if (fn[expando]) {
              return fn(argument);
            }

            // But maintain support for old signatures
            if (fn.length > 1) {
              args = [pseudo, pseudo, "", argument];
              return Expr.setFilters.hasOwnProperty(pseudo.toLowerCase())
                ? markFunction(function (seed, matches) {
                    var idx,
                      matched = fn(seed, argument),
                      i = matched.length;
                    while (i--) {
                      idx = indexOf(seed, matched[i]);
                      seed[idx] = !(matches[idx] = matched[i]);
                    }
                  })
                : function (elem) {
                    return fn(elem, 0, args);
                  };
            }

            return fn;
          },
        },

        pseudos: {
          // Potentially complex pseudos
          not: markFunction(function (selector) {
            // Trim the selector passed to compile
            // to avoid treating leading and trailing
            // spaces as combinators
            var input = [],
              results = [],
              matcher = compile(selector.replace(rtrim, "$1"));

            return matcher[expando]
              ? markFunction(function (seed, matches, _context, xml) {
                  var elem,
                    unmatched = matcher(seed, null, xml, []),
                    i = seed.length;

                  // Match elements unmatched by `matcher`
                  while (i--) {
                    if ((elem = unmatched[i])) {
                      seed[i] = !(matches[i] = elem);
                    }
                  }
                })
              : function (elem, _context, xml) {
                  input[0] = elem;
                  matcher(input, null, xml, results);

                  // Don't keep the element (issue #299)
                  input[0] = null;
                  return !results.pop();
                };
          }),

          has: markFunction(function (selector) {
            return function (elem) {
              return Sizzle(selector, elem).length > 0;
            };
          }),

          contains: markFunction(function (text) {
            text = text.replace(runescape, funescape);
            return function (elem) {
              return (elem.textContent || getText(elem)).indexOf(text) > -1;
            };
          }),

          // "Whether an element is represented by a :lang() selector
          // is based solely on the element's language value
          // being equal to the identifier C,
          // or beginning with the identifier C immediately followed by "-".
          // The matching of C against the element's language value is performed case-insensitively.
          // The identifier C does not have to be a valid language name."
          // http://www.w3.org/TR/selectors/#lang-pseudo
          lang: markFunction(function (lang) {
            // lang value must be a valid identifier
            if (!ridentifier.test(lang || "")) {
              Sizzle.error("unsupported lang: " + lang);
            }
            lang = lang.replace(runescape, funescape).toLowerCase();
            return function (elem) {
              var elemLang;
              do {
                if (
                  (elemLang = documentIsHTML
                    ? elem.lang
                    : elem.getAttribute("xml:lang") ||
                      elem.getAttribute("lang"))
                ) {
                  elemLang = elemLang.toLowerCase();
                  return (
                    elemLang === lang || elemLang.indexOf(lang + "-") === 0
                  );
                }
              } while ((elem = elem.parentNode) && elem.nodeType === 1);
              return false;
            };
          }),

          // Miscellaneous
          target: function (elem) {
            var hash = window.location && window.location.hash;
            return hash && hash.slice(1) === elem.id;
          },

          root: function (elem) {
            return elem === docElem;
          },

          focus: function (elem) {
            return (
              elem === document.activeElement &&
              (!document.hasFocus || document.hasFocus()) &&
              !!(elem.type || elem.href || ~elem.tabIndex)
            );
          },

          // Boolean properties
          enabled: createDisabledPseudo(false),
          disabled: createDisabledPseudo(true),

          checked: function (elem) {
            // In CSS3, :checked should return both checked and selected elements
            // http://www.w3.org/TR/2011/REC-css3-selectors-20110929/#checked
            var nodeName = elem.nodeName.toLowerCase();
            return (
              (nodeName === "input" && !!elem.checked) ||
              (nodeName === "option" && !!elem.selected)
            );
          },

          selected: function (elem) {
            // Accessing this property makes selected-by-default
            // options in Safari work properly
            if (elem.parentNode) {
              // eslint-disable-next-line no-unused-expressions
              elem.parentNode.selectedIndex;
            }

            return elem.selected === true;
          },

          // Contents
          empty: function (elem) {
            // http://www.w3.org/TR/selectors/#empty-pseudo
            // :empty is negated by element (1) or content nodes (text: 3; cdata: 4; entity ref: 5),
            //   but not by others (comment: 8; processing instruction: 7; etc.)
            // nodeType < 6 works because attributes (2) do not appear as children
            for (elem = elem.firstChild; elem; elem = elem.nextSibling) {
              if (elem.nodeType < 6) {
                return false;
              }
            }
            return true;
          },

          parent: function (elem) {
            return !Expr.pseudos["empty"](elem);
          },

          // Element/input types
          header: function (elem) {
            return rheader.test(elem.nodeName);
          },

          input: function (elem) {
            return rinputs.test(elem.nodeName);
          },

          button: function (elem) {
            var name = elem.nodeName.toLowerCase();
            return (
              (name === "input" && elem.type === "button") || name === "button"
            );
          },

          text: function (elem) {
            var attr;
            return (
              elem.nodeName.toLowerCase() === "input" &&
              elem.type === "text" &&
              // Support: IE<8
              // New HTML5 attribute values (e.g., "search") appear with elem.type === "text"
              ((attr = elem.getAttribute("type")) == null ||
                attr.toLowerCase() === "text")
            );
          },

          // Position-in-collection
          first: createPositionalPseudo(function () {
            return [0];
          }),

          last: createPositionalPseudo(function (_matchIndexes, length) {
            return [length - 1];
          }),

          eq: createPositionalPseudo(function (
            _matchIndexes,
            length,
            argument
          ) {
            return [argument < 0 ? argument + length : argument];
          }),

          even: createPositionalPseudo(function (matchIndexes, length) {
            var i = 0;
            for (; i < length; i += 2) {
              matchIndexes.push(i);
            }
            return matchIndexes;
          }),

          odd: createPositionalPseudo(function (matchIndexes, length) {
            var i = 1;
            for (; i < length; i += 2) {
              matchIndexes.push(i);
            }
            return matchIndexes;
          }),

          lt: createPositionalPseudo(function (matchIndexes, length, argument) {
            var i =
              argument < 0
                ? argument + length
                : argument > length
                ? length
                : argument;
            for (; --i >= 0; ) {
              matchIndexes.push(i);
            }
            return matchIndexes;
          }),

          gt: createPositionalPseudo(function (matchIndexes, length, argument) {
            var i = argument < 0 ? argument + length : argument;
            for (; ++i < length; ) {
              matchIndexes.push(i);
            }
            return matchIndexes;
          }),
        },
      };

      Expr.pseudos["nth"] = Expr.pseudos["eq"];

      // Add button/input type pseudos
      for (i in {
        radio: true,
        checkbox: true,
        file: true,
        password: true,
        image: true,
      }) {
        Expr.pseudos[i] = createInputPseudo(i);
      }
      for (i in { submit: true, reset: true }) {
        Expr.pseudos[i] = createButtonPseudo(i);
      }

      // Easy API for creating new setFilters
      function setFilters() {}
      setFilters.prototype = Expr.filters = Expr.pseudos;
      Expr.setFilters = new setFilters();

      tokenize = Sizzle.tokenize = function (selector, parseOnly) {
        var matched,
          match,
          tokens,
          type,
          soFar,
          groups,
          preFilters,
          cached = tokenCache[selector + " "];

        if (cached) {
          return parseOnly ? 0 : cached.slice(0);
        }

        soFar = selector;
        groups = [];
        preFilters = Expr.preFilter;

        while (soFar) {
          // Comma and first run
          if (!matched || (match = rcomma.exec(soFar))) {
            if (match) {
              // Don't consume trailing commas as valid
              soFar = soFar.slice(match[0].length) || soFar;
            }
            groups.push((tokens = []));
          }

          matched = false;

          // Combinators
          if ((match = rcombinators.exec(soFar))) {
            matched = match.shift();
            tokens.push({
              value: matched,

              // Cast descendant combinators to space
              type: match[0].replace(rtrim, " "),
            });
            soFar = soFar.slice(matched.length);
          }

          // Filters
          for (type in Expr.filter) {
            if (
              (match = matchExpr[type].exec(soFar)) &&
              (!preFilters[type] || (match = preFilters[type](match)))
            ) {
              matched = match.shift();
              tokens.push({
                value: matched,
                type: type,
                matches: match,
              });
              soFar = soFar.slice(matched.length);
            }
          }

          if (!matched) {
            break;
          }
        }

        // Return the length of the invalid excess
        // if we're just parsing
        // Otherwise, throw an error or return tokens
        return parseOnly
          ? soFar.length
          : soFar
          ? Sizzle.error(selector)
          : // Cache the tokens
            tokenCache(selector, groups).slice(0);
      };

      function toSelector(tokens) {
        var i = 0,
          len = tokens.length,
          selector = "";
        for (; i < len; i++) {
          selector += tokens[i].value;
        }
        return selector;
      }

      function addCombinator(matcher, combinator, base) {
        var dir = combinator.dir,
          skip = combinator.next,
          key = skip || dir,
          checkNonElements = base && key === "parentNode",
          doneName = done++;

        return combinator.first
          ? // Check against closest ancestor/preceding element
            function (elem, context, xml) {
              while ((elem = elem[dir])) {
                if (elem.nodeType === 1 || checkNonElements) {
                  return matcher(elem, context, xml);
                }
              }
              return false;
            }
          : // Check against all ancestor/preceding elements
            function (elem, context, xml) {
              var oldCache,
                uniqueCache,
                outerCache,
                newCache = [dirruns, doneName];

              // We can't set arbitrary data on XML nodes, so they don't benefit from combinator caching
              if (xml) {
                while ((elem = elem[dir])) {
                  if (elem.nodeType === 1 || checkNonElements) {
                    if (matcher(elem, context, xml)) {
                      return true;
                    }
                  }
                }
              } else {
                while ((elem = elem[dir])) {
                  if (elem.nodeType === 1 || checkNonElements) {
                    outerCache = elem[expando] || (elem[expando] = {});

                    // Support: IE <9 only
                    // Defend against cloned attroperties (jQuery gh-1709)
                    uniqueCache =
                      outerCache[elem.uniqueID] ||
                      (outerCache[elem.uniqueID] = {});

                    if (skip && skip === elem.nodeName.toLowerCase()) {
                      elem = elem[dir] || elem;
                    } else if (
                      (oldCache = uniqueCache[key]) &&
                      oldCache[0] === dirruns &&
                      oldCache[1] === doneName
                    ) {
                      // Assign to newCache so results back-propagate to previous elements
                      return (newCache[2] = oldCache[2]);
                    } else {
                      // Reuse newcache so results back-propagate to previous elements
                      uniqueCache[key] = newCache;

                      // A match means we're done; a fail means we have to keep checking
                      if ((newCache[2] = matcher(elem, context, xml))) {
                        return true;
                      }
                    }
                  }
                }
              }
              return false;
            };
      }

      function elementMatcher(matchers) {
        return matchers.length > 1
          ? function (elem, context, xml) {
              var i = matchers.length;
              while (i--) {
                if (!matchers[i](elem, context, xml)) {
                  return false;
                }
              }
              return true;
            }
          : matchers[0];
      }

      function multipleContexts(selector, contexts, results) {
        var i = 0,
          len = contexts.length;
        for (; i < len; i++) {
          Sizzle(selector, contexts[i], results);
        }
        return results;
      }

      function condense(unmatched, map, filter, context, xml) {
        var elem,
          newUnmatched = [],
          i = 0,
          len = unmatched.length,
          mapped = map != null;

        for (; i < len; i++) {
          if ((elem = unmatched[i])) {
            if (!filter || filter(elem, context, xml)) {
              newUnmatched.push(elem);
              if (mapped) {
                map.push(i);
              }
            }
          }
        }

        return newUnmatched;
      }

      function setMatcher(
        preFilter,
        selector,
        matcher,
        postFilter,
        postFinder,
        postSelector
      ) {
        if (postFilter && !postFilter[expando]) {
          postFilter = setMatcher(postFilter);
        }
        if (postFinder && !postFinder[expando]) {
          postFinder = setMatcher(postFinder, postSelector);
        }
        return markFunction(function (seed, results, context, xml) {
          var temp,
            i,
            elem,
            preMap = [],
            postMap = [],
            preexisting = results.length,
            // Get initial elements from seed or context
            elems =
              seed ||
              multipleContexts(
                selector || "*",
                context.nodeType ? [context] : context,
                []
              ),
            // Prefilter to get matcher input, preserving a map for seed-results synchronization
            matcherIn =
              preFilter && (seed || !selector)
                ? condense(elems, preMap, preFilter, context, xml)
                : elems,
            matcherOut = matcher
              ? // If we have a postFinder, or filtered seed, or non-seed postFilter or preexisting results,
                postFinder || (seed ? preFilter : preexisting || postFilter)
                ? // ...intermediate processing is necessary
                  []
                : // ...otherwise use results directly
                  results
              : matcherIn;

          // Find primary matches
          if (matcher) {
            matcher(matcherIn, matcherOut, context, xml);
          }

          // Apply postFilter
          if (postFilter) {
            temp = condense(matcherOut, postMap);
            postFilter(temp, [], context, xml);

            // Un-match failing elements by moving them back to matcherIn
            i = temp.length;
            while (i--) {
              if ((elem = temp[i])) {
                matcherOut[postMap[i]] = !(matcherIn[postMap[i]] = elem);
              }
            }
          }

          if (seed) {
            if (postFinder || preFilter) {
              if (postFinder) {
                // Get the final matcherOut by condensing this intermediate into postFinder contexts
                temp = [];
                i = matcherOut.length;
                while (i--) {
                  if ((elem = matcherOut[i])) {
                    // Restore matcherIn since elem is not yet a final match
                    temp.push((matcherIn[i] = elem));
                  }
                }
                postFinder(null, (matcherOut = []), temp, xml);
              }

              // Move matched elements from seed to results to keep them synchronized
              i = matcherOut.length;
              while (i--) {
                if (
                  (elem = matcherOut[i]) &&
                  (temp = postFinder ? indexOf(seed, elem) : preMap[i]) > -1
                ) {
                  seed[temp] = !(results[temp] = elem);
                }
              }
            }

            // Add elements to results, through postFinder if defined
          } else {
            matcherOut = condense(
              matcherOut === results
                ? matcherOut.splice(preexisting, matcherOut.length)
                : matcherOut
            );
            if (postFinder) {
              postFinder(null, results, matcherOut, xml);
            } else {
              push.apply(results, matcherOut);
            }
          }
        });
      }

      function matcherFromTokens(tokens) {
        var checkContext,
          matcher,
          j,
          len = tokens.length,
          leadingRelative = Expr.relative[tokens[0].type],
          implicitRelative = leadingRelative || Expr.relative[" "],
          i = leadingRelative ? 1 : 0,
          // The foundational matcher ensures that elements are reachable from top-level context(s)
          matchContext = addCombinator(
            function (elem) {
              return elem === checkContext;
            },
            implicitRelative,
            true
          ),
          matchAnyContext = addCombinator(
            function (elem) {
              return indexOf(checkContext, elem) > -1;
            },
            implicitRelative,
            true
          ),
          matchers = [
            function (elem, context, xml) {
              var ret =
                (!leadingRelative && (xml || context !== outermostContext)) ||
                ((checkContext = context).nodeType
                  ? matchContext(elem, context, xml)
                  : matchAnyContext(elem, context, xml));

              // Avoid hanging onto element (issue #299)
              checkContext = null;
              return ret;
            },
          ];

        for (; i < len; i++) {
          if ((matcher = Expr.relative[tokens[i].type])) {
            matchers = [addCombinator(elementMatcher(matchers), matcher)];
          } else {
            matcher = Expr.filter[tokens[i].type].apply(
              null,
              tokens[i].matches
            );

            // Return special upon seeing a positional matcher
            if (matcher[expando]) {
              // Find the next relative operator (if any) for proper handling
              j = ++i;
              for (; j < len; j++) {
                if (Expr.relative[tokens[j].type]) {
                  break;
                }
              }
              return setMatcher(
                i > 1 && elementMatcher(matchers),
                i > 1 &&
                  toSelector(
                    // If the preceding token was a descendant combinator, insert an implicit any-element `*`
                    tokens
                      .slice(0, i - 1)
                      .concat({ value: tokens[i - 2].type === " " ? "*" : "" })
                  ).replace(rtrim, "$1"),
                matcher,
                i < j && matcherFromTokens(tokens.slice(i, j)),
                j < len && matcherFromTokens((tokens = tokens.slice(j))),
                j < len && toSelector(tokens)
              );
            }
            matchers.push(matcher);
          }
        }

        return elementMatcher(matchers);
      }

      function matcherFromGroupMatchers(elementMatchers, setMatchers) {
        var bySet = setMatchers.length > 0,
          byElement = elementMatchers.length > 0,
          superMatcher = function (seed, context, xml, results, outermost) {
            var elem,
              j,
              matcher,
              matchedCount = 0,
              i = "0",
              unmatched = seed && [],
              setMatched = [],
              contextBackup = outermostContext,
              // We must always have either seed elements or outermost context
              elems = seed || (byElement && Expr.find["TAG"]("*", outermost)),
              // Use integer dirruns iff this is the outermost matcher
              dirrunsUnique = (dirruns +=
                contextBackup == null ? 1 : Math.random() || 0.1),
              len = elems.length;

            if (outermost) {
              // Support: IE 11+, Edge 17 - 18+
              // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
              // two documents; shallow comparisons work.
              // eslint-disable-next-line eqeqeq
              outermostContext = context == document || context || outermost;
            }

            // Add elements passing elementMatchers directly to results
            // Support: IE<9, Safari
            // Tolerate NodeList properties (IE: "length"; Safari: <number>) matching elements by id
            for (; i !== len && (elem = elems[i]) != null; i++) {
              if (byElement && elem) {
                j = 0;

                // Support: IE 11+, Edge 17 - 18+
                // IE/Edge sometimes throw a "Permission denied" error when strict-comparing
                // two documents; shallow comparisons work.
                // eslint-disable-next-line eqeqeq
                if (!context && elem.ownerDocument != document) {
                  setDocument(elem);
                  xml = !documentIsHTML;
                }
                while ((matcher = elementMatchers[j++])) {
                  if (matcher(elem, context || document, xml)) {
                    results.push(elem);
                    break;
                  }
                }
                if (outermost) {
                  dirruns = dirrunsUnique;
                }
              }

              // Track unmatched elements for set filters
              if (bySet) {
                // They will have gone through all possible matchers
                if ((elem = !matcher && elem)) {
                  matchedCount--;
                }

                // Lengthen the array for every element, matched or not
                if (seed) {
                  unmatched.push(elem);
                }
              }
            }

            // `i` is now the count of elements visited above, and adding it to `matchedCount`
            // makes the latter nonnegative.
            matchedCount += i;

            // Apply set filters to unmatched elements
            // NOTE: This can be skipped if there are no unmatched elements (i.e., `matchedCount`
            // equals `i`), unless we didn't visit _any_ elements in the above loop because we have
            // no element matchers and no seed.
            // Incrementing an initially-string "0" `i` allows `i` to remain a string only in that
            // case, which will result in a "00" `matchedCount` that differs from `i` but is also
            // numerically zero.
            if (bySet && i !== matchedCount) {
              j = 0;
              while ((matcher = setMatchers[j++])) {
                matcher(unmatched, setMatched, context, xml);
              }

              if (seed) {
                // Reintegrate element matches to eliminate the need for sorting
                if (matchedCount > 0) {
                  while (i--) {
                    if (!(unmatched[i] || setMatched[i])) {
                      setMatched[i] = pop.call(results);
                    }
                  }
                }

                // Discard index placeholder values to get only actual matches
                setMatched = condense(setMatched);
              }

              // Add matches to results
              push.apply(results, setMatched);

              // Seedless set matches succeeding multiple successful matchers stipulate sorting
              if (
                outermost &&
                !seed &&
                setMatched.length > 0 &&
                matchedCount + setMatchers.length > 1
              ) {
                Sizzle.uniqueSort(results);
              }
            }

            // Override manipulation of globals by nested matchers
            if (outermost) {
              dirruns = dirrunsUnique;
              outermostContext = contextBackup;
            }

            return unmatched;
          };

        return bySet ? markFunction(superMatcher) : superMatcher;
      }

      compile = Sizzle.compile = function (
        selector,
        match /* Internal Use Only */
      ) {
        var i,
          setMatchers = [],
          elementMatchers = [],
          cached = compilerCache[selector + " "];

        if (!cached) {
          // Generate a function of recursive functions that can be used to check each element
          if (!match) {
            match = tokenize(selector);
          }
          i = match.length;
          while (i--) {
            cached = matcherFromTokens(match[i]);
            if (cached[expando]) {
              setMatchers.push(cached);
            } else {
              elementMatchers.push(cached);
            }
          }

          // Cache the compiled function
          cached = compilerCache(
            selector,
            matcherFromGroupMatchers(elementMatchers, setMatchers)
          );

          // Save selector and tokenization
          cached.selector = selector;
        }
        return cached;
      };

      /**
       * A low-level selection function that works with Sizzle's compiled
       *  selector functions
       * @param {String|Function} selector A selector or a pre-compiled
       *  selector function built with Sizzle.compile
       * @param {Element} context
       * @param {Array} [results]
       * @param {Array} [seed] A set of elements to match against
       */
      select = Sizzle.select = function (selector, context, results, seed) {
        var i,
          tokens,
          token,
          type,
          find,
          compiled = typeof selector === "function" && selector,
          match = !seed && tokenize((selector = compiled.selector || selector));

        results = results || [];

        // Try to minimize operations if there is only one selector in the list and no seed
        // (the latter of which guarantees us context)
        if (match.length === 1) {
          // Reduce context if the leading compound selector is an ID
          tokens = match[0] = match[0].slice(0);
          if (
            tokens.length > 2 &&
            (token = tokens[0]).type === "ID" &&
            context.nodeType === 9 &&
            documentIsHTML &&
            Expr.relative[tokens[1].type]
          ) {
            context = (Expr.find["ID"](
              token.matches[0].replace(runescape, funescape),
              context
            ) || [])[0];
            if (!context) {
              return results;

              // Precompiled matchers will still verify ancestry, so step up a level
            } else if (compiled) {
              context = context.parentNode;
            }

            selector = selector.slice(tokens.shift().value.length);
          }

          // Fetch a seed set for right-to-left matching
          i = matchExpr["needsContext"].test(selector) ? 0 : tokens.length;
          while (i--) {
            token = tokens[i];

            // Abort if we hit a combinator
            if (Expr.relative[(type = token.type)]) {
              break;
            }
            if ((find = Expr.find[type])) {
              // Search, expanding context for leading sibling combinators
              if (
                (seed = find(
                  token.matches[0].replace(runescape, funescape),
                  (rsibling.test(tokens[0].type) &&
                    testContext(context.parentNode)) ||
                    context
                ))
              ) {
                // If seed is empty or no tokens remain, we can return early
                tokens.splice(i, 1);
                selector = seed.length && toSelector(tokens);
                if (!selector) {
                  push.apply(results, seed);
                  return results;
                }

                break;
              }
            }
          }
        }

        // Compile and execute a filtering function if one is not provided
        // Provide `match` to avoid retokenization if we modified the selector above
        (compiled || compile(selector, match))(
          seed,
          context,
          !documentIsHTML,
          results,
          !context ||
            (rsibling.test(selector) && testContext(context.parentNode)) ||
            context
        );
        return results;
      };

      // One-time assignments

      // Sort stability
      support.sortStable =
        expando.split("").sort(sortOrder).join("") === expando;

      // Support: Chrome 14-35+
      // Always assume duplicates if they aren't passed to the comparison function
      support.detectDuplicates = !!hasDuplicate;

      // Initialize against the default document
      setDocument();

      // Support: Webkit<537.32 - Safari 6.0.3/Chrome 25 (fixed in Chrome 27)
      // Detached nodes confoundingly follow *each other*
      support.sortDetached = assert(function (el) {
        // Should return 1, but returns 4 (following)
        return (
          el.compareDocumentPosition(document.createElement("fieldset")) & 1
        );
      });

      // Support: IE<8
      // Prevent attribute/property "interpolation"
      // https://msdn.microsoft.com/en-us/library/ms536429%28VS.85%29.aspx
      if (
        !assert(function (el) {
          el.innerHTML = "<a href='#'></a>";
          return el.firstChild.getAttribute("href") === "#";
        })
      ) {
        addHandle("type|href|height|width", function (elem, name, isXML) {
          if (!isXML) {
            return elem.getAttribute(
              name,
              name.toLowerCase() === "type" ? 1 : 2
            );
          }
        });
      }

      // Support: IE<9
      // Use defaultValue in place of getAttribute("value")
      if (
        !support.attributes ||
        !assert(function (el) {
          el.innerHTML = "<input/>";
          el.firstChild.setAttribute("value", "");
          return el.firstChild.getAttribute("value") === "";
        })
      ) {
        addHandle("value", function (elem, _name, isXML) {
          if (!isXML && elem.nodeName.toLowerCase() === "input") {
            return elem.defaultValue;
          }
        });
      }

      // Support: IE<9
      // Use getAttributeNode to fetch booleans when getAttribute lies
      if (
        !assert(function (el) {
          return el.getAttribute("disabled") == null;
        })
      ) {
        addHandle(booleans, function (elem, name, isXML) {
          var val;
          if (!isXML) {
            return elem[name] === true
              ? name.toLowerCase()
              : (val = elem.getAttributeNode(name)) && val.specified
              ? val.value
              : null;
          }
        });
      }

      return Sizzle;
    })(window);

  jQuery.find = Sizzle;
  jQuery.expr = Sizzle.selectors;

  // Deprecated
  jQuery.expr[":"] = jQuery.expr.pseudos;
  jQuery.uniqueSort = jQuery.unique = Sizzle.uniqueSort;
  jQuery.text = Sizzle.getText;
  jQuery.isXMLDoc = Sizzle.isXML;
  jQuery.contains = Sizzle.contains;
  jQuery.escapeSelector = Sizzle.escape;

  var dir = function (elem, dir, until) {
    var matched = [],
      truncate = until !== undefined;

    while ((elem = elem[dir]) && elem.nodeType !== 9) {
      if (elem.nodeType === 1) {
        if (truncate && jQuery(elem).is(until)) {
          break;
        }
        matched.push(elem);
      }
    }
    return matched;
  };

  var siblings = function (n, elem) {
    var matched = [];

    for (; n; n = n.nextSibling) {
      if (n.nodeType === 1 && n !== elem) {
        matched.push(n);
      }
    }

    return matched;
  };

  var rneedsContext = jQuery.expr.match.needsContext;

  function nodeName(elem, name) {
    return elem.nodeName && elem.nodeName.toLowerCase() === name.toLowerCase();
  }
  var rsingleTag = /^<([a-z][^\/\0>:\x20\t\r\n\f]*)[\x20\t\r\n\f]*\/?>(?:<\/\1>|)$/i;

  // Implement the identical functionality for filter and not
  function winnow(elements, qualifier, not) {
    if (isFunction(qualifier)) {
      return jQuery.grep(elements, function (elem, i) {
        return !!qualifier.call(elem, i, elem) !== not;
      });
    }

    // Single element
    if (qualifier.nodeType) {
      return jQuery.grep(elements, function (elem) {
        return (elem === qualifier) !== not;
      });
    }

    // Arraylike of elements (jQuery, arguments, Array)
    if (typeof qualifier !== "string") {
      return jQuery.grep(elements, function (elem) {
        return indexOf.call(qualifier, elem) > -1 !== not;
      });
    }

    // Filtered directly for both simple and complex selectors
    return jQuery.filter(qualifier, elements, not);
  }

  jQuery.filter = function (expr, elems, not) {
    var elem = elems[0];

    if (not) {
      expr = ":not(" + expr + ")";
    }

    if (elems.length === 1 && elem.nodeType === 1) {
      return jQuery.find.matchesSelector(elem, expr) ? [elem] : [];
    }

    return jQuery.find.matches(
      expr,
      jQuery.grep(elems, function (elem) {
        return elem.nodeType === 1;
      })
    );
  };

  jQuery.fn.extend({
    find: function (selector) {
      var i,
        ret,
        len = this.length,
        self = this;

      if (typeof selector !== "string") {
        return this.pushStack(
          jQuery(selector).filter(function () {
            for (i = 0; i < len; i++) {
              if (jQuery.contains(self[i], this)) {
                return true;
              }
            }
          })
        );
      }

      ret = this.pushStack([]);

      for (i = 0; i < len; i++) {
        jQuery.find(selector, self[i], ret);
      }

      return len > 1 ? jQuery.uniqueSort(ret) : ret;
    },
    filter: function (selector) {
      return this.pushStack(winnow(this, selector || [], false));
    },
    not: function (selector) {
      return this.pushStack(winnow(this, selector || [], true));
    },
    is: function (selector) {
      return !!winnow(
        this,

        // If this is a positional/relative selector, check membership in the returned set
        // so $("p:first").is("p:last") won't return true for a doc with two "p".
        typeof selector === "string" && rneedsContext.test(selector)
          ? jQuery(selector)
          : selector || [],
        false
      ).length;
    },
  });

  // Initialize a jQuery object

  // A central reference to the root jQuery(document)
  var rootjQuery,
    // A simple way to check for HTML strings
    // Prioritize #id over <tag> to avoid XSS via location.hash (#9521)
    // Strict HTML recognition (#11290: must start with <)
    // Shortcut simple #id case for speed
    rquickExpr = /^(?:\s*(<[\w\W]+>)[^>]*|#([\w-]+))$/,
    init = (jQuery.fn.init = function (selector, context, root) {
      var match, elem;

      // HANDLE: $(""), $(null), $(undefined), $(false)
      if (!selector) {
        return this;
      }

      // Method init() accepts an alternate rootjQuery
      // so migrate can support jQuery.sub (gh-2101)
      root = root || rootjQuery;

      // Handle HTML strings
      if (typeof selector === "string") {
        if (
          selector[0] === "<" &&
          selector[selector.length - 1] === ">" &&
          selector.length >= 3
        ) {
          // Assume that strings that start and end with <> are HTML and skip the regex check
          match = [null, selector, null];
        } else {
          match = rquickExpr.exec(selector);
        }

        // Match html or make sure no context is specified for #id
        if (match && (match[1] || !context)) {
          // HANDLE: $(html) -> $(array)
          if (match[1]) {
            context = context instanceof jQuery ? context[0] : context;

            // Option to run scripts is true for back-compat
            // Intentionally let the error be thrown if parseHTML is not present
            jQuery.merge(
              this,
              jQuery.parseHTML(
                match[1],
                context && context.nodeType
                  ? context.ownerDocument || context
                  : document,
                true
              )
            );

            // HANDLE: $(html, props)
            if (rsingleTag.test(match[1]) && jQuery.isPlainObject(context)) {
              for (match in context) {
                // Properties of context are called as methods if possible
                if (isFunction(this[match])) {
                  this[match](context[match]);

                  // ...and otherwise set as attributes
                } else {
                  this.attr(match, context[match]);
                }
              }
            }

            return this;

            // HANDLE: $(#id)
          } else {
            elem = document.getElementById(match[2]);

            if (elem) {
              // Inject the element directly into the jQuery object
              this[0] = elem;
              this.length = 1;
            }
            return this;
          }

          // HANDLE: $(expr, $(...))
        } else if (!context || context.jquery) {
          return (context || root).find(selector);

          // HANDLE: $(expr, context)
          // (which is just equivalent to: $(context).find(expr)
        } else {
          return this.constructor(context).find(selector);
        }

        // HANDLE: $(DOMElement)
      } else if (selector.nodeType) {
        this[0] = selector;
        this.length = 1;
        return this;

        // HANDLE: $(function)
        // Shortcut for document ready
      } else if (isFunction(selector)) {
        return root.ready !== undefined
          ? root.ready(selector)
          : // Execute immediately if ready is not present
            selector(jQuery);
      }

      return jQuery.makeArray(selector, this);
    });

  // Give the init function the jQuery prototype for later instantiation
  init.prototype = jQuery.fn;

  // Initialize central reference
  rootjQuery = jQuery(document);

  var rparentsprev = /^(?:parents|prev(?:Until|All))/,
    // Methods guaranteed to produce a unique set when starting from a unique set
    guaranteedUnique = {
      children: true,
      contents: true,
      next: true,
      prev: true,
    };

  jQuery.fn.extend({
    has: function (target) {
      var targets = jQuery(target, this),
        l = targets.length;

      return this.filter(function () {
        var i = 0;
        for (; i < l; i++) {
          if (jQuery.contains(this, targets[i])) {
            return true;
          }
        }
      });
    },

    closest: function (selectors, context) {
      var cur,
        i = 0,
        l = this.length,
        matched = [],
        targets = typeof selectors !== "string" && jQuery(selectors);

      // Positional selectors never match, since there's no _selection_ context
      if (!rneedsContext.test(selectors)) {
        for (; i < l; i++) {
          for (cur = this[i]; cur && cur !== context; cur = cur.parentNode) {
            // Always skip document fragments
            if (
              cur.nodeType < 11 &&
              (targets
                ? targets.index(cur) > -1
                : // Don't pass non-elements to Sizzle
                  cur.nodeType === 1 &&
                  jQuery.find.matchesSelector(cur, selectors))
            ) {
              matched.push(cur);
              break;
            }
          }
        }
      }

      return this.pushStack(
        matched.length > 1 ? jQuery.uniqueSort(matched) : matched
      );
    },

    // Determine the position of an element within the set
    index: function (elem) {
      // No argument, return index in parent
      if (!elem) {
        return this[0] && this[0].parentNode
          ? this.first().prevAll().length
          : -1;
      }

      // Index in selector
      if (typeof elem === "string") {
        return indexOf.call(jQuery(elem), this[0]);
      }

      // Locate the position of the desired element
      return indexOf.call(
        this,

        // If it receives a jQuery object, the first element is used
        elem.jquery ? elem[0] : elem
      );
    },

    add: function (selector, context) {
      return this.pushStack(
        jQuery.uniqueSort(jQuery.merge(this.get(), jQuery(selector, context)))
      );
    },

    addBack: function (selector) {
      return this.add(
        selector == null ? this.prevObject : this.prevObject.filter(selector)
      );
    },
  });

  function sibling(cur, dir) {
    while ((cur = cur[dir]) && cur.nodeType !== 1) {}
    return cur;
  }

  jQuery.each(
    {
      parent: function (elem) {
        var parent = elem.parentNode;
        return parent && parent.nodeType !== 11 ? parent : null;
      },
      parents: function (elem) {
        return dir(elem, "parentNode");
      },
      parentsUntil: function (elem, _i, until) {
        return dir(elem, "parentNode", until);
      },
      next: function (elem) {
        return sibling(elem, "nextSibling");
      },
      prev: function (elem) {
        return sibling(elem, "previousSibling");
      },
      nextAll: function (elem) {
        return dir(elem, "nextSibling");
      },
      prevAll: function (elem) {
        return dir(elem, "previousSibling");
      },
      nextUntil: function (elem, _i, until) {
        return dir(elem, "nextSibling", until);
      },
      prevUntil: function (elem, _i, until) {
        return dir(elem, "previousSibling", until);
      },
      siblings: function (elem) {
        return siblings((elem.parentNode || {}).firstChild, elem);
      },
      children: function (elem) {
        return siblings(elem.firstChild);
      },
      contents: function (elem) {
        if (
          elem.contentDocument != null &&
          // Support: IE 11+
          // <object> elements with no `data` attribute has an object
          // `contentDocument` with a `null` prototype.
          getProto(elem.contentDocument)
        ) {
          return elem.contentDocument;
        }

        // Support: IE 9 - 11 only, iOS 7 only, Android Browser <=4.3 only
        // Treat the template element as a regular one in browsers that
        // don't support it.
        if (nodeName(elem, "template")) {
          elem = elem.content || elem;
        }

        return jQuery.merge([], elem.childNodes);
      },
    },
    function (name, fn) {
      jQuery.fn[name] = function (until, selector) {
        var matched = jQuery.map(this, fn, until);

        if (name.slice(-5) !== "Until") {
          selector = until;
        }

        if (selector && typeof selector === "string") {
          matched = jQuery.filter(selector, matched);
        }

        if (this.length > 1) {
          // Remove duplicates
          if (!guaranteedUnique[name]) {
            jQuery.uniqueSort(matched);
          }

          // Reverse order for parents* and prev-derivatives
          if (rparentsprev.test(name)) {
            matched.reverse();
          }
        }

        return this.pushStack(matched);
      };
    }
  );
  var rnothtmlwhite = /[^\x20\t\r\n\f]+/g;

  // Convert String-formatted options into Object-formatted ones
  function createOptions(options) {
    var object = {};
    jQuery.each(options.match(rnothtmlwhite) || [], function (_, flag) {
      object[flag] = true;
    });
    return object;
  }

  /*
   * Create a callback list using the following parameters:
   *
   *	options: an optional list of space-separated options that will change how
   *			the callback list behaves or a more traditional option object
   *
   * By default a callback list will act like an event callback list and can be
   * "fired" multiple times.
   *
   * Possible options:
   *
   *	once:			will ensure the callback list can only be fired once (like a Deferred)
   *
   *	memory:			will keep track of previous values and will call any callback added
   *					after the list has been fired right away with the latest "memorized"
   *					values (like a Deferred)
   *
   *	unique:			will ensure a callback can only be added once (no duplicate in the list)
   *
   *	stopOnFalse:	interrupt callings when a callback returns false
   *
   */
  jQuery.Callbacks = function (options) {
    // Convert options from String-formatted to Object-formatted if needed
    // (we check in cache first)
    options =
      typeof options === "string"
        ? createOptions(options)
        : jQuery.extend({}, options);

    var // Flag to know if list is currently firing
      firing,
      // Last fire value for non-forgettable lists
      memory,
      // Flag to know if list was already fired
      fired,
      // Flag to prevent firing
      locked,
      // Actual callback list
      list = [],
      // Queue of execution data for repeatable lists
      queue = [],
      // Index of currently firing callback (modified by add/remove as needed)
      firingIndex = -1,
      // Fire callbacks
      fire = function () {
        // Enforce single-firing
        locked = locked || options.once;

        // Execute callbacks for all pending executions,
        // respecting firingIndex overrides and runtime changes
        fired = firing = true;
        for (; queue.length; firingIndex = -1) {
          memory = queue.shift();
          while (++firingIndex < list.length) {
            // Run callback and check for early termination
            if (
              list[firingIndex].apply(memory[0], memory[1]) === false &&
              options.stopOnFalse
            ) {
              // Jump to end and forget the data so .add doesn't re-fire
              firingIndex = list.length;
              memory = false;
            }
          }
        }

        // Forget the data if we're done with it
        if (!options.memory) {
          memory = false;
        }

        firing = false;

        // Clean up if we're done firing for good
        if (locked) {
          // Keep an empty list if we have data for future add calls
          if (memory) {
            list = [];

            // Otherwise, this object is spent
          } else {
            list = "";
          }
        }
      },
      // Actual Callbacks object
      self = {
        // Add a callback or a collection of callbacks to the list
        add: function () {
          if (list) {
            // If we have memory from a past run, we should fire after adding
            if (memory && !firing) {
              firingIndex = list.length - 1;
              queue.push(memory);
            }

            (function add(args) {
              jQuery.each(args, function (_, arg) {
                if (isFunction(arg)) {
                  if (!options.unique || !self.has(arg)) {
                    list.push(arg);
                  }
                } else if (arg && arg.length && toType(arg) !== "string") {
                  // Inspect recursively
                  add(arg);
                }
              });
            })(arguments);

            if (memory && !firing) {
              fire();
            }
          }
          return this;
        },

        // Remove a callback from the list
        remove: function () {
          jQuery.each(arguments, function (_, arg) {
            var index;
            while ((index = jQuery.inArray(arg, list, index)) > -1) {
              list.splice(index, 1);

              // Handle firing indexes
              if (index <= firingIndex) {
                firingIndex--;
              }
            }
          });
          return this;
        },

        // Check if a given callback is in the list.
        // If no argument is given, return whether or not list has callbacks attached.
        has: function (fn) {
          return fn ? jQuery.inArray(fn, list) > -1 : list.length > 0;
        },

        // Remove all callbacks from the list
        empty: function () {
          if (list) {
            list = [];
          }
          return this;
        },

        // Disable .fire and .add
        // Abort any current/pending executions
        // Clear all callbacks and values
        disable: function () {
          locked = queue = [];
          list = memory = "";
          return this;
        },
        disabled: function () {
          return !list;
        },

        // Disable .fire
        // Also disable .add unless we have memory (since it would have no effect)
        // Abort any pending executions
        lock: function () {
          locked = queue = [];
          if (!memory && !firing) {
            list = memory = "";
          }
          return this;
        },
        locked: function () {
          return !!locked;
        },

        // Call all callbacks with the given context and arguments
        fireWith: function (context, args) {
          if (!locked) {
            args = args || [];
            args = [context, args.slice ? args.slice() : args];
            queue.push(args);
            if (!firing) {
              fire();
            }
          }
          return this;
        },

        // Call all the callbacks with the given arguments
        fire: function () {
          self.fireWith(this, arguments);
          return this;
        },

        // To know if the callbacks have already been called at least once
        fired: function () {
          return !!fired;
        },
      };

    return self;
  };

  function Identity(v) {
    return v;
  }
  function Thrower(ex) {
    throw ex;
  }

  function adoptValue(value, resolve, reject, noValue) {
    var method;

    try {
      // Check for promise aspect first to privilege synchronous behavior
      if (value && isFunction((method = value.promise))) {
        method.call(value).done(resolve).fail(reject);

        // Other thenables
      } else if (value && isFunction((method = value.then))) {
        method.call(value, resolve, reject);

        // Other non-thenables
      } else {
        // Control `resolve` arguments by letting Array#slice cast boolean `noValue` to integer:
        // * false: [ value ].slice( 0 ) => resolve( value )
        // * true: [ value ].slice( 1 ) => resolve()
        resolve.apply(undefined, [value].slice(noValue));
      }

      // For Promises/A+, convert exceptions into rejections
      // Since jQuery.when doesn't unwrap thenables, we can skip the extra checks appearing in
      // Deferred#then to conditionally suppress rejection.
    } catch (value) {
      // Support: Android 4.0 only
      // Strict mode functions invoked without .call/.apply get global-object context
      reject.apply(undefined, [value]);
    }
  }

  jQuery.extend({
    Deferred: function (func) {
      var tuples = [
          // action, add listener, callbacks,
          // ... .then handlers, argument index, [final state]
          [
            "notify",
            "progress",
            jQuery.Callbacks("memory"),
            jQuery.Callbacks("memory"),
            2,
          ],
          [
            "resolve",
            "done",
            jQuery.Callbacks("once memory"),
            jQuery.Callbacks("once memory"),
            0,
            "resolved",
          ],
          [
            "reject",
            "fail",
            jQuery.Callbacks("once memory"),
            jQuery.Callbacks("once memory"),
            1,
            "rejected",
          ],
        ],
        state = "pending",
        promise = {
          state: function () {
            return state;
          },
          always: function () {
            deferred.done(arguments).fail(arguments);
            return this;
          },
          catch: function (fn) {
            return promise.then(null, fn);
          },

          // Keep pipe for back-compat
          pipe: function (/* fnDone, fnFail, fnProgress */) {
            var fns = arguments;

            return jQuery
              .Deferred(function (newDefer) {
                jQuery.each(tuples, function (_i, tuple) {
                  // Map tuples (progress, done, fail) to arguments (done, fail, progress)
                  var fn = isFunction(fns[tuple[4]]) && fns[tuple[4]];

                  // deferred.progress(function() { bind to newDefer or newDefer.notify })
                  // deferred.done(function() { bind to newDefer or newDefer.resolve })
                  // deferred.fail(function() { bind to newDefer or newDefer.reject })
                  deferred[tuple[1]](function () {
                    var returned = fn && fn.apply(this, arguments);
                    if (returned && isFunction(returned.promise)) {
                      returned
                        .promise()
                        .progress(newDefer.notify)
                        .done(newDefer.resolve)
                        .fail(newDefer.reject);
                    } else {
                      newDefer[tuple[0] + "With"](
                        this,
                        fn ? [returned] : arguments
                      );
                    }
                  });
                });
                fns = null;
              })
              .promise();
          },
          then: function (onFulfilled, onRejected, onProgress) {
            var maxDepth = 0;
            function resolve(depth, deferred, handler, special) {
              return function () {
                var that = this,
                  args = arguments,
                  mightThrow = function () {
                    var returned, then;

                    // Support: Promises/A+ section 2.3.3.3.3
                    // https://promisesaplus.com/#point-59
                    // Ignore double-resolution attempts
                    if (depth < maxDepth) {
                      return;
                    }

                    returned = handler.apply(that, args);

                    // Support: Promises/A+ section 2.3.1
                    // https://promisesaplus.com/#point-48
                    if (returned === deferred.promise()) {
                      throw new TypeError("Thenable self-resolution");
                    }

                    // Support: Promises/A+ sections 2.3.3.1, 3.5
                    // https://promisesaplus.com/#point-54
                    // https://promisesaplus.com/#point-75
                    // Retrieve `then` only once
                    then =
                      returned &&
                      // Support: Promises/A+ section 2.3.4
                      // https://promisesaplus.com/#point-64
                      // Only check objects and functions for thenability
                      (typeof returned === "object" ||
                        typeof returned === "function") &&
                      returned.then;

                    // Handle a returned thenable
                    if (isFunction(then)) {
                      // Special processors (notify) just wait for resolution
                      if (special) {
                        then.call(
                          returned,
                          resolve(maxDepth, deferred, Identity, special),
                          resolve(maxDepth, deferred, Thrower, special)
                        );

                        // Normal processors (resolve) also hook into progress
                      } else {
                        // ...and disregard older resolution values
                        maxDepth++;

                        then.call(
                          returned,
                          resolve(maxDepth, deferred, Identity, special),
                          resolve(maxDepth, deferred, Thrower, special),
                          resolve(
                            maxDepth,
                            deferred,
                            Identity,
                            deferred.notifyWith
                          )
                        );
                      }

                      // Handle all other returned values
                    } else {
                      // Only substitute handlers pass on context
                      // and multiple values (non-spec behavior)
                      if (handler !== Identity) {
                        that = undefined;
                        args = [returned];
                      }

                      // Process the value(s)
                      // Default process is resolve
                      (special || deferred.resolveWith)(that, args);
                    }
                  },
                  // Only normal processors (resolve) catch and reject exceptions
                  process = special
                    ? mightThrow
                    : function () {
                        try {
                          mightThrow();
                        } catch (e) {
                          if (jQuery.Deferred.exceptionHook) {
                            jQuery.Deferred.exceptionHook(
                              e,
                              process.stackTrace
                            );
                          }

                          // Support: Promises/A+ section 2.3.3.3.4.1
                          // https://promisesaplus.com/#point-61
                          // Ignore post-resolution exceptions
                          if (depth + 1 >= maxDepth) {
                            // Only substitute handlers pass on context
                            // and multiple values (non-spec behavior)
                            if (handler !== Thrower) {
                              that = undefined;
                              args = [e];
                            }

                            deferred.rejectWith(that, args);
                          }
                        }
                      };

                // Support: Promises/A+ section 2.3.3.3.1
                // https://promisesaplus.com/#point-57
                // Re-resolve promises immediately to dodge false rejection from
                // subsequent errors
                if (depth) {
                  process();
                } else {
                  // Call an optional hook to record the stack, in case of exception
                  // since it's otherwise lost when execution goes async
                  if (jQuery.Deferred.getStackHook) {
                    process.stackTrace = jQuery.Deferred.getStackHook();
                  }
                  window.setTimeout(process);
                }
              };
            }

            return jQuery
              .Deferred(function (newDefer) {
                // progress_handlers.add( ... )
                tuples[0][3].add(
                  resolve(
                    0,
                    newDefer,
                    isFunction(onProgress) ? onProgress : Identity,
                    newDefer.notifyWith
                  )
                );

                // fulfilled_handlers.add( ... )
                tuples[1][3].add(
                  resolve(
                    0,
                    newDefer,
                    isFunction(onFulfilled) ? onFulfilled : Identity
                  )
                );

                // rejected_handlers.add( ... )
                tuples[2][3].add(
                  resolve(
                    0,
                    newDefer,
                    isFunction(onRejected) ? onRejected : Thrower
                  )
                );
              })
              .promise();
          },

          // Get a promise for this deferred
          // If obj is provided, the promise aspect is added to the object
          promise: function (obj) {
            return obj != null ? jQuery.extend(obj, promise) : promise;
          },
        },
        deferred = {};

      // Add list-specific methods
      jQuery.each(tuples, function (i, tuple) {
        var list = tuple[2],
          stateString = tuple[5];

        // promise.progress = list.add
        // promise.done = list.add
        // promise.fail = list.add
        promise[tuple[1]] = list.add;

        // Handle state
        if (stateString) {
          list.add(
            function () {
              // state = "resolved" (i.e., fulfilled)
              // state = "rejected"
              state = stateString;
            },

            // rejected_callbacks.disable
            // fulfilled_callbacks.disable
            tuples[3 - i][2].disable,

            // rejected_handlers.disable
            // fulfilled_handlers.disable
            tuples[3 - i][3].disable,

            // progress_callbacks.lock
            tuples[0][2].lock,

            // progress_handlers.lock
            tuples[0][3].lock
          );
        }

        // progress_handlers.fire
        // fulfilled_handlers.fire
        // rejected_handlers.fire
        list.add(tuple[3].fire);

        // deferred.notify = function() { deferred.notifyWith(...) }
        // deferred.resolve = function() { deferred.resolveWith(...) }
        // deferred.reject = function() { deferred.rejectWith(...) }
        deferred[tuple[0]] = function () {
          deferred[tuple[0] + "With"](
            this === deferred ? undefined : this,
            arguments
          );
          return this;
        };

        // deferred.notifyWith = list.fireWith
        // deferred.resolveWith = list.fireWith
        // deferred.rejectWith = list.fireWith
        deferred[tuple[0] + "With"] = list.fireWith;
      });

      // Make the deferred a promise
      promise.promise(deferred);

      // Call given func if any
      if (func) {
        func.call(deferred, deferred);
      }

      // All done!
      return deferred;
    },

    // Deferred helper
    when: function (singleValue) {
      var // count of uncompleted subordinates
        remaining = arguments.length,
        // count of unprocessed arguments
        i = remaining,
        // subordinate fulfillment data
        resolveContexts = Array(i),
        resolveValues = slice.call(arguments),
        // the primary Deferred
        primary = jQuery.Deferred(),
        // subordinate callback factory
        updateFunc = function (i) {
          return function (value) {
            resolveContexts[i] = this;
            resolveValues[i] =
              arguments.length > 1 ? slice.call(arguments) : value;
            if (!--remaining) {
              primary.resolveWith(resolveContexts, resolveValues);
            }
          };
        };

      // Single- and empty arguments are adopted like Promise.resolve
      if (remaining <= 1) {
        adoptValue(
          singleValue,
          primary.done(updateFunc(i)).resolve,
          primary.reject,
          !remaining
        );

        // Use .then() to unwrap secondary thenables (cf. gh-3000)
        if (
          primary.state() === "pending" ||
          isFunction(resolveValues[i] && resolveValues[i].then)
        ) {
          return primary.then();
        }
      }

      // Multiple arguments are aggregated like Promise.all array elements
      while (i--) {
        adoptValue(resolveValues[i], updateFunc(i), primary.reject);
      }

      return primary.promise();
    },
  });

  // These usually indicate a programmer mistake during development,
  // warn about them ASAP rather than swallowing them by default.
  var rerrorNames = /^(Eval|Internal|Range|Reference|Syntax|Type|URI)Error$/;

  jQuery.Deferred.exceptionHook = function (error, stack) {
    // Support: IE 8 - 9 only
    // Console exists when dev tools are open, which can happen at any time
    if (
      window.console &&
      window.console.warn &&
      error &&
      rerrorNames.test(error.name)
    ) {
      window.console.warn(
        "jQuery.Deferred exception: " + error.message,
        error.stack,
        stack
      );
    }
  };

  jQuery.readyException = function (error) {
    window.setTimeout(function () {
      throw error;
    });
  };

  // The deferred used on DOM ready
  var readyList = jQuery.Deferred();

  jQuery.fn.ready = function (fn) {
    readyList
      .then(fn)

      // Wrap jQuery.readyException in a function so that the lookup
      // happens at the time of error handling instead of callback
      // registration.
      .catch(function (error) {
        jQuery.readyException(error);
      });

    return this;
  };

  jQuery.extend({
    // Is the DOM ready to be used? Set to true once it occurs.
    isReady: false,

    // A counter to track how many items to wait for before
    // the ready event fires. See #6781
    readyWait: 1,

    // Handle when the DOM is ready
    ready: function (wait) {
      // Abort if there are pending holds or we're already ready
      if (wait === true ? --jQuery.readyWait : jQuery.isReady) {
        return;
      }

      // Remember that the DOM is ready
      jQuery.isReady = true;

      // If a normal DOM Ready event fired, decrement, and wait if need be
      if (wait !== true && --jQuery.readyWait > 0) {
        return;
      }

      // If there are functions bound, to execute
      readyList.resolveWith(document, [jQuery]);
    },
  });

  jQuery.ready.then = readyList.then;

  // The ready event handler and self cleanup method
  function completed() {
    document.removeEventListener("DOMContentLoaded", completed);
    window.removeEventListener("load", completed);
    jQuery.ready();
  }

  // Catch cases where $(document).ready() is called
  // after the browser event has already occurred.
  // Support: IE <=9 - 10 only
  // Older IE sometimes signals "interactive" too soon
  if (
    document.readyState === "complete" ||
    (document.readyState !== "loading" && !document.documentElement.doScroll)
  ) {
    // Handle it asynchronously to allow scripts the opportunity to delay ready
    window.setTimeout(jQuery.ready);
  } else {
    // Use the handy event callback
    document.addEventListener("DOMContentLoaded", completed);

    // A fallback to window.onload, that will always work
    window.addEventListener("load", completed);
  }

  // Multifunctional method to get and set values of a collection
  // The value/s can optionally be executed if it's a function
  var access = function (elems, fn, key, value, chainable, emptyGet, raw) {
    var i = 0,
      len = elems.length,
      bulk = key == null;

    // Sets many values
    if (toType(key) === "object") {
      chainable = true;
      for (i in key) {
        access(elems, fn, i, key[i], true, emptyGet, raw);
      }

      // Sets one value
    } else if (value !== undefined) {
      chainable = true;

      if (!isFunction(value)) {
        raw = true;
      }

      if (bulk) {
        // Bulk operations run against the entire set
        if (raw) {
          fn.call(elems, value);
          fn = null;

          // ...except when executing function values
        } else {
          bulk = fn;
          fn = function (elem, _key, value) {
            return bulk.call(jQuery(elem), value);
          };
        }
      }

      if (fn) {
        for (; i < len; i++) {
          fn(
            elems[i],
            key,
            raw ? value : value.call(elems[i], i, fn(elems[i], key))
          );
        }
      }
    }

    if (chainable) {
      return elems;
    }

    // Gets
    if (bulk) {
      return fn.call(elems);
    }

    return len ? fn(elems[0], key) : emptyGet;
  };

  // Matches dashed string for camelizing
  var rmsPrefix = /^-ms-/,
    rdashAlpha = /-([a-z])/g;

  // Used by camelCase as callback to replace()
  function fcamelCase(_all, letter) {
    return letter.toUpperCase();
  }

  // Convert dashed to camelCase; used by the css and data modules
  // Support: IE <=9 - 11, Edge 12 - 15
  // Microsoft forgot to hump their vendor prefix (#9572)
  function camelCase(string) {
    return string.replace(rmsPrefix, "ms-").replace(rdashAlpha, fcamelCase);
  }
  var acceptData = function (owner) {
    // Accepts only:
    //  - Node
    //    - Node.ELEMENT_NODE
    //    - Node.DOCUMENT_NODE
    //  - Object
    //    - Any
    return owner.nodeType === 1 || owner.nodeType === 9 || !+owner.nodeType;
  };

  function Data() {
    this.expando = jQuery.expando + Data.uid++;
  }

  Data.uid = 1;

  Data.prototype = {
    cache: function (owner) {
      // Check if the owner object already has a cache
      var value = owner[this.expando];

      // If not, create one
      if (!value) {
        value = {};

        // We can accept data for non-element nodes in modern browsers,
        // but we should not, see #8335.
        // Always return an empty object.
        if (acceptData(owner)) {
          // If it is a node unlikely to be stringify-ed or looped over
          // use plain assignment
          if (owner.nodeType) {
            owner[this.expando] = value;

            // Otherwise secure it in a non-enumerable property
            // configurable must be true to allow the property to be
            // deleted when data is removed
          } else {
            Object.defineProperty(owner, this.expando, {
              value: value,
              configurable: true,
            });
          }
        }
      }

      return value;
    },
    set: function (owner, data, value) {
      var prop,
        cache = this.cache(owner);

      // Handle: [ owner, key, value ] args
      // Always use camelCase key (gh-2257)
      if (typeof data === "string") {
        cache[camelCase(data)] = value;

        // Handle: [ owner, { properties } ] args
      } else {
        // Copy the properties one-by-one to the cache object
        for (prop in data) {
          cache[camelCase(prop)] = data[prop];
        }
      }
      return cache;
    },
    get: function (owner, key) {
      return key === undefined
        ? this.cache(owner)
        : // Always use camelCase key (gh-2257)
          owner[this.expando] && owner[this.expando][camelCase(key)];
    },
    access: function (owner, key, value) {
      // In cases where either:
      //
      //   1. No key was specified
      //   2. A string key was specified, but no value provided
      //
      // Take the "read" path and allow the get method to determine
      // which value to return, respectively either:
      //
      //   1. The entire cache object
      //   2. The data stored at the key
      //
      if (
        key === undefined ||
        (key && typeof key === "string" && value === undefined)
      ) {
        return this.get(owner, key);
      }

      // When the key is not a string, or both a key and value
      // are specified, set or extend (existing objects) with either:
      //
      //   1. An object of properties
      //   2. A key and value
      //
      this.set(owner, key, value);

      // Since the "set" path can have two possible entry points
      // return the expected data based on which path was taken[*]
      return value !== undefined ? value : key;
    },
    remove: function (owner, key) {
      var i,
        cache = owner[this.expando];

      if (cache === undefined) {
        return;
      }

      if (key !== undefined) {
        // Support array or space separated string of keys
        if (Array.isArray(key)) {
          // If key is an array of keys...
          // We always set camelCase keys, so remove that.
          key = key.map(camelCase);
        } else {
          key = camelCase(key);

          // If a key with the spaces exists, use it.
          // Otherwise, create an array by matching non-whitespace
          key = key in cache ? [key] : key.match(rnothtmlwhite) || [];
        }

        i = key.length;

        while (i--) {
          delete cache[key[i]];
        }
      }

      // Remove the expando if there's no more data
      if (key === undefined || jQuery.isEmptyObject(cache)) {
        // Support: Chrome <=35 - 45
        // Webkit & Blink performance suffers when deleting properties
        // from DOM nodes, so set to undefined instead
        // https://bugs.chromium.org/p/chromium/issues/detail?id=378607 (bug restricted)
        if (owner.nodeType) {
          owner[this.expando] = undefined;
        } else {
          delete owner[this.expando];
        }
      }
    },
    hasData: function (owner) {
      var cache = owner[this.expando];
      return cache !== undefined && !jQuery.isEmptyObject(cache);
    },
  };
  var dataPriv = new Data();

  var dataUser = new Data();

  //	Implementation Summary
  //
  //	1. Enforce API surface and semantic compatibility with 1.9.x branch
  //	2. Improve the module's maintainability by reducing the storage
  //		paths to a single mechanism.
  //	3. Use the same single mechanism to support "private" and "user" data.
  //	4. _Never_ expose "private" data to user code (TODO: Drop _data, _removeData)
  //	5. Avoid exposing implementation details on user objects (eg. expando properties)
  //	6. Provide a clear path for implementation upgrade to WeakMap in 2014

  var rbrace = /^(?:\{[\w\W]*\}|\[[\w\W]*\])$/,
    rmultiDash = /[A-Z]/g;

  function getData(data) {
    if (data === "true") {
      return true;
    }

    if (data === "false") {
      return false;
    }

    if (data === "null") {
      return null;
    }

    // Only convert to a number if it doesn't change the string
    if (data === +data + "") {
      return +data;
    }

    if (rbrace.test(data)) {
      return JSON.parse(data);
    }

    return data;
  }

  function dataAttr(elem, key, data) {
    var name;

    // If nothing was found internally, try to fetch any
    // data from the HTML5 data-* attribute
    if (data === undefined && elem.nodeType === 1) {
      name = "data-" + key.replace(rmultiDash, "-$&").toLowerCase();
      data = elem.getAttribute(name);

      if (typeof data === "string") {
        try {
          data = getData(data);
        } catch (e) {}

        // Make sure we set the data so it isn't changed later
        dataUser.set(elem, key, data);
      } else {
        data = undefined;
      }
    }
    return data;
  }

  jQuery.extend({
    hasData: function (elem) {
      return dataUser.hasData(elem) || dataPriv.hasData(elem);
    },

    data: function (elem, name, data) {
      return dataUser.access(elem, name, data);
    },

    removeData: function (elem, name) {
      dataUser.remove(elem, name);
    },

    // TODO: Now that all calls to _data and _removeData have been replaced
    // with direct calls to dataPriv methods, these can be deprecated.
    _data: function (elem, name, data) {
      return dataPriv.access(elem, name, data);
    },

    _removeData: function (elem, name) {
      dataPriv.remove(elem, name);
    },
  });

  jQuery.fn.extend({
    data: function (key, value) {
      var i,
        name,
        data,
        elem = this[0],
        attrs = elem && elem.attributes;

      // Gets all values
      if (key === undefined) {
        if (this.length) {
          data = dataUser.get(elem);

          if (elem.nodeType === 1 && !dataPriv.get(elem, "hasDataAttrs")) {
            i = attrs.length;
            while (i--) {
              // Support: IE 11 only
              // The attrs elements can be null (#14894)
              if (attrs[i]) {
                name = attrs[i].name;
                if (name.indexOf("data-") === 0) {
                  name = camelCase(name.slice(5));
                  dataAttr(elem, name, data[name]);
                }
              }
            }
            dataPriv.set(elem, "hasDataAttrs", true);
          }
        }

        return data;
      }

      // Sets multiple values
      if (typeof key === "object") {
        return this.each(function () {
          dataUser.set(this, key);
        });
      }

      return access(
        this,
        function (value) {
          var data;

          // The calling jQuery object (element matches) is not empty
          // (and therefore has an element appears at this[ 0 ]) and the
          // `value` parameter was not undefined. An empty jQuery object
          // will result in `undefined` for elem = this[ 0 ] which will
          // throw an exception if an attempt to read a data cache is made.
          if (elem && value === undefined) {
            // Attempt to get data from the cache
            // The key will always be camelCased in Data
            data = dataUser.get(elem, key);
            if (data !== undefined) {
              return data;
            }

            // Attempt to "discover" the data in
            // HTML5 custom data-* attrs
            data = dataAttr(elem, key);
            if (data !== undefined) {
              return data;
            }

            // We tried really hard, but the data doesn't exist.
            return;
          }

          // Set the data...
          this.each(function () {
            // We always store the camelCased key
            dataUser.set(this, key, value);
          });
        },
        null,
        value,
        arguments.length > 1,
        null,
        true
      );
    },

    removeData: function (key) {
      return this.each(function () {
        dataUser.remove(this, key);
      });
    },
  });

  jQuery.extend({
    queue: function (elem, type, data) {
      var queue;

      if (elem) {
        type = (type || "fx") + "queue";
        queue = dataPriv.get(elem, type);

        // Speed up dequeue by getting out quickly if this is just a lookup
        if (data) {
          if (!queue || Array.isArray(data)) {
            queue = dataPriv.access(elem, type, jQuery.makeArray(data));
          } else {
            queue.push(data);
          }
        }
        return queue || [];
      }
    },

    dequeue: function (elem, type) {
      type = type || "fx";

      var queue = jQuery.queue(elem, type),
        startLength = queue.length,
        fn = queue.shift(),
        hooks = jQuery._queueHooks(elem, type),
        next = function () {
          jQuery.dequeue(elem, type);
        };

      // If the fx queue is dequeued, always remove the progress sentinel
      if (fn === "inprogress") {
        fn = queue.shift();
        startLength--;
      }

      if (fn) {
        // Add a progress sentinel to prevent the fx queue from being
        // automatically dequeued
        if (type === "fx") {
          queue.unshift("inprogress");
        }

        // Clear up the last queue stop function
        delete hooks.stop;
        fn.call(elem, next, hooks);
      }

      if (!startLength && hooks) {
        hooks.empty.fire();
      }
    },

    // Not public - generate a queueHooks object, or return the current one
    _queueHooks: function (elem, type) {
      var key = type + "queueHooks";
      return (
        dataPriv.get(elem, key) ||
        dataPriv.access(elem, key, {
          empty: jQuery.Callbacks("once memory").add(function () {
            dataPriv.remove(elem, [type + "queue", key]);
          }),
        })
      );
    },
  });

  jQuery.fn.extend({
    queue: function (type, data) {
      var setter = 2;

      if (typeof type !== "string") {
        data = type;
        type = "fx";
        setter--;
      }

      if (arguments.length < setter) {
        return jQuery.queue(this[0], type);
      }

      return data === undefined
        ? this
        : this.each(function () {
            var queue = jQuery.queue(this, type, data);

            // Ensure a hooks for this queue
            jQuery._queueHooks(this, type);

            if (type === "fx" && queue[0] !== "inprogress") {
              jQuery.dequeue(this, type);
            }
          });
    },
    dequeue: function (type) {
      return this.each(function () {
        jQuery.dequeue(this, type);
      });
    },
    clearQueue: function (type) {
      return this.queue(type || "fx", []);
    },

    // Get a promise resolved when queues of a certain type
    // are emptied (fx is the type by default)
    promise: function (type, obj) {
      var tmp,
        count = 1,
        defer = jQuery.Deferred(),
        elements = this,
        i = this.length,
        resolve = function () {
          if (!--count) {
            defer.resolveWith(elements, [elements]);
          }
        };

      if (typeof type !== "string") {
        obj = type;
        type = undefined;
      }
      type = type || "fx";

      while (i--) {
        tmp = dataPriv.get(elements[i], type + "queueHooks");
        if (tmp && tmp.empty) {
          count++;
          tmp.empty.add(resolve);
        }
      }
      resolve();
      return defer.promise(obj);
    },
  });
  var pnum = /[+-]?(?:\d*\.|)\d+(?:[eE][+-]?\d+|)/.source;

  var rcssNum = new RegExp("^(?:([+-])=|)(" + pnum + ")([a-z%]*)$", "i");

  var cssExpand = ["Top", "Right", "Bottom", "Left"];

  var documentElement = document.documentElement;

  var isAttached = function (elem) {
      return jQuery.contains(elem.ownerDocument, elem);
    },
    composed = { composed: true };

  // Support: IE 9 - 11+, Edge 12 - 18+, iOS 10.0 - 10.2 only
  // Check attachment across shadow DOM boundaries when possible (gh-3504)
  // Support: iOS 10.0-10.2 only
  // Early iOS 10 versions support `attachShadow` but not `getRootNode`,
  // leading to errors. We need to check for `getRootNode`.
  if (documentElement.getRootNode) {
    isAttached = function (elem) {
      return (
        jQuery.contains(elem.ownerDocument, elem) ||
        elem.getRootNode(composed) === elem.ownerDocument
      );
    };
  }
  var isHiddenWithinTree = function (elem, el) {
    // isHiddenWithinTree might be called from jQuery#filter function;
    // in that case, element will be second argument
    elem = el || elem;

    // Inline style trumps all
    return (
      elem.style.display === "none" ||
      (elem.style.display === "" &&
        // Otherwise, check computed style
        // Support: Firefox <=43 - 45
        // Disconnected elements can have computed display: none, so first confirm that elem is
        // in the document.
        isAttached(elem) &&
        jQuery.css(elem, "display") === "none")
    );
  };

  function adjustCSS(elem, prop, valueParts, tween) {
    var adjusted,
      scale,
      maxIterations = 20,
      currentValue = tween
        ? function () {
            return tween.cur();
          }
        : function () {
            return jQuery.css(elem, prop, "");
          },
      initial = currentValue(),
      unit =
        (valueParts && valueParts[3]) || (jQuery.cssNumber[prop] ? "" : "px"),
      // Starting value computation is required for potential unit mismatches
      initialInUnit =
        elem.nodeType &&
        (jQuery.cssNumber[prop] || (unit !== "px" && +initial)) &&
        rcssNum.exec(jQuery.css(elem, prop));

    if (initialInUnit && initialInUnit[3] !== unit) {
      // Support: Firefox <=54
      // Halve the iteration target value to prevent interference from CSS upper bounds (gh-2144)
      initial = initial / 2;

      // Trust units reported by jQuery.css
      unit = unit || initialInUnit[3];

      // Iteratively approximate from a nonzero starting point
      initialInUnit = +initial || 1;

      while (maxIterations--) {
        // Evaluate and update our best guess (doubling guesses that zero out).
        // Finish if the scale equals or crosses 1 (making the old*new product non-positive).
        jQuery.style(elem, prop, initialInUnit + unit);
        if (
          (1 - scale) * (1 - (scale = currentValue() / initial || 0.5)) <=
          0
        ) {
          maxIterations = 0;
        }
        initialInUnit = initialInUnit / scale;
      }

      initialInUnit = initialInUnit * 2;
      jQuery.style(elem, prop, initialInUnit + unit);

      // Make sure we update the tween properties later on
      valueParts = valueParts || [];
    }

    if (valueParts) {
      initialInUnit = +initialInUnit || +initial || 0;

      // Apply relative offset (+=/-=) if specified
      adjusted = valueParts[1]
        ? initialInUnit + (valueParts[1] + 1) * valueParts[2]
        : +valueParts[2];
      if (tween) {
        tween.unit = unit;
        tween.start = initialInUnit;
        tween.end = adjusted;
      }
    }
    return adjusted;
  }

  var defaultDisplayMap = {};

  function getDefaultDisplay(elem) {
    var temp,
      doc = elem.ownerDocument,
      nodeName = elem.nodeName,
      display = defaultDisplayMap[nodeName];

    if (display) {
      return display;
    }

    temp = doc.body.appendChild(doc.createElement(nodeName));
    display = jQuery.css(temp, "display");

    temp.parentNode.removeChild(temp);

    if (display === "none") {
      display = "block";
    }
    defaultDisplayMap[nodeName] = display;

    return display;
  }

  function showHide(elements, show) {
    var display,
      elem,
      values = [],
      index = 0,
      length = elements.length;

    // Determine new display value for elements that need to change
    for (; index < length; index++) {
      elem = elements[index];
      if (!elem.style) {
        continue;
      }

      display = elem.style.display;
      if (show) {
        // Since we force visibility upon cascade-hidden elements, an immediate (and slow)
        // check is required in this first loop unless we have a nonempty display value (either
        // inline or about-to-be-restored)
        if (display === "none") {
          values[index] = dataPriv.get(elem, "display") || null;
          if (!values[index]) {
            elem.style.display = "";
          }
        }
        if (elem.style.display === "" && isHiddenWithinTree(elem)) {
          values[index] = getDefaultDisplay(elem);
        }
      } else {
        if (display !== "none") {
          values[index] = "none";

          // Remember what we're overwriting
          dataPriv.set(elem, "display", display);
        }
      }
    }

    // Set the display of the elements in a second loop to avoid constant reflow
    for (index = 0; index < length; index++) {
      if (values[index] != null) {
        elements[index].style.display = values[index];
      }
    }

    return elements;
  }

  jQuery.fn.extend({
    show: function () {
      return showHide(this, true);
    },
    hide: function () {
      return showHide(this);
    },
    toggle: function (state) {
      if (typeof state === "boolean") {
        return state ? this.show() : this.hide();
      }

      return this.each(function () {
        if (isHiddenWithinTree(this)) {
          jQuery(this).show();
        } else {
          jQuery(this).hide();
        }
      });
    },
  });
  var rcheckableType = /^(?:checkbox|radio)$/i;

  var rtagName = /<([a-z][^\/\0>\x20\t\r\n\f]*)/i;

  var rscriptType = /^$|^module$|\/(?:java|ecma)script/i;

  (function () {
    var fragment = document.createDocumentFragment(),
      div = fragment.appendChild(document.createElement("div")),
      input = document.createElement("input");

    // Support: Android 4.0 - 4.3 only
    // Check state lost if the name is set (#11217)
    // Support: Windows Web Apps (WWA)
    // `name` and `type` must use .setAttribute for WWA (#14901)
    input.setAttribute("type", "radio");
    input.setAttribute("checked", "checked");
    input.setAttribute("name", "t");

    div.appendChild(input);

    // Support: Android <=4.1 only
    // Older WebKit doesn't clone checked state correctly in fragments
    support.checkClone = div.cloneNode(true).cloneNode(true).lastChild.checked;

    // Support: IE <=11 only
    // Make sure textarea (and checkbox) defaultValue is properly cloned
    div.innerHTML = "<textarea>x</textarea>";
    support.noCloneChecked = !!div.cloneNode(true).lastChild.defaultValue;

    // Support: IE <=9 only
    // IE <=9 replaces <option> tags with their contents when inserted outside of
    // the select element.
    div.innerHTML = "<option></option>";
    support.option = !!div.lastChild;
  })();

  // We have to close these tags to support XHTML (#13200)
  var wrapMap = {
    // XHTML parsers do not magically insert elements in the
    // same way that tag soup parsers do. So we cannot shorten
    // this by omitting <tbody> or other required elements.
    thead: [1, "<table>", "</table>"],
    col: [2, "<table><colgroup>", "</colgroup></table>"],
    tr: [2, "<table><tbody>", "</tbody></table>"],
    td: [3, "<table><tbody><tr>", "</tr></tbody></table>"],

    _default: [0, "", ""],
  };

  wrapMap.tbody = wrapMap.tfoot = wrapMap.colgroup = wrapMap.caption =
    wrapMap.thead;
  wrapMap.th = wrapMap.td;

  // Support: IE <=9 only
  if (!support.option) {
    wrapMap.optgroup = wrapMap.option = [
      1,
      "<select multiple='multiple'>",
      "</select>",
    ];
  }

  function getAll(context, tag) {
    // Support: IE <=9 - 11 only
    // Use typeof to avoid zero-argument method invocation on host objects (#15151)
    var ret;

    if (typeof context.getElementsByTagName !== "undefined") {
      ret = context.getElementsByTagName(tag || "*");
    } else if (typeof context.querySelectorAll !== "undefined") {
      ret = context.querySelectorAll(tag || "*");
    } else {
      ret = [];
    }

    if (tag === undefined || (tag && nodeName(context, tag))) {
      return jQuery.merge([context], ret);
    }

    return ret;
  }

  // Mark scripts as having already been evaluated
  function setGlobalEval(elems, refElements) {
    var i = 0,
      l = elems.length;

    for (; i < l; i++) {
      dataPriv.set(
        elems[i],
        "globalEval",
        !refElements || dataPriv.get(refElements[i], "globalEval")
      );
    }
  }

  var rhtml = /<|&#?\w+;/;

  function buildFragment(elems, context, scripts, selection, ignored) {
    var elem,
      tmp,
      tag,
      wrap,
      attached,
      j,
      fragment = context.createDocumentFragment(),
      nodes = [],
      i = 0,
      l = elems.length;

    for (; i < l; i++) {
      elem = elems[i];

      if (elem || elem === 0) {
        // Add nodes directly
        if (toType(elem) === "object") {
          // Support: Android <=4.0 only, PhantomJS 1 only
          // push.apply(_, arraylike) throws on ancient WebKit
          jQuery.merge(nodes, elem.nodeType ? [elem] : elem);

          // Convert non-html into a text node
        } else if (!rhtml.test(elem)) {
          nodes.push(context.createTextNode(elem));

          // Convert html into DOM nodes
        } else {
          tmp = tmp || fragment.appendChild(context.createElement("div"));

          // Deserialize a standard representation
          tag = (rtagName.exec(elem) || ["", ""])[1].toLowerCase();
          wrap = wrapMap[tag] || wrapMap._default;
          tmp.innerHTML = wrap[1] + jQuery.htmlPrefilter(elem) + wrap[2];

          // Descend through wrappers to the right content
          j = wrap[0];
          while (j--) {
            tmp = tmp.lastChild;
          }

          // Support: Android <=4.0 only, PhantomJS 1 only
          // push.apply(_, arraylike) throws on ancient WebKit
          jQuery.merge(nodes, tmp.childNodes);

          // Remember the top-level container
          tmp = fragment.firstChild;

          // Ensure the created nodes are orphaned (#12392)
          tmp.textContent = "";
        }
      }
    }

    // Remove wrapper from fragment
    fragment.textContent = "";

    i = 0;
    while ((elem = nodes[i++])) {
      // Skip elements already in the context collection (trac-4087)
      if (selection && jQuery.inArray(elem, selection) > -1) {
        if (ignored) {
          ignored.push(elem);
        }
        continue;
      }

      attached = isAttached(elem);

      // Append to fragment
      tmp = getAll(fragment.appendChild(elem), "script");

      // Preserve script evaluation history
      if (attached) {
        setGlobalEval(tmp);
      }

      // Capture executables
      if (scripts) {
        j = 0;
        while ((elem = tmp[j++])) {
          if (rscriptType.test(elem.type || "")) {
            scripts.push(elem);
          }
        }
      }
    }

    return fragment;
  }

  var rtypenamespace = /^([^.]*)(?:\.(.+)|)/;

  function returnTrue() {
    return true;
  }

  function returnFalse() {
    return false;
  }

  // Support: IE <=9 - 11+
  // focus() and blur() are asynchronous, except when they are no-op.
  // So expect focus to be synchronous when the element is already active,
  // and blur to be synchronous when the element is not already active.
  // (focus and blur are always synchronous in other supported browsers,
  // this just defines when we can count on it).
  function expectSync(elem, type) {
    return (elem === safeActiveElement()) === (type === "focus");
  }

  // Support: IE <=9 only
  // Accessing document.activeElement can throw unexpectedly
  // https://bugs.jquery.com/ticket/13393
  function safeActiveElement() {
    try {
      return document.activeElement;
    } catch (err) {}
  }

  function on(elem, types, selector, data, fn, one) {
    var origFn, type;

    // Types can be a map of types/handlers
    if (typeof types === "object") {
      // ( types-Object, selector, data )
      if (typeof selector !== "string") {
        // ( types-Object, data )
        data = data || selector;
        selector = undefined;
      }
      for (type in types) {
        on(elem, type, selector, data, types[type], one);
      }
      return elem;
    }

    if (data == null && fn == null) {
      // ( types, fn )
      fn = selector;
      data = selector = undefined;
    } else if (fn == null) {
      if (typeof selector === "string") {
        // ( types, selector, fn )
        fn = data;
        data = undefined;
      } else {
        // ( types, data, fn )
        fn = data;
        data = selector;
        selector = undefined;
      }
    }
    if (fn === false) {
      fn = returnFalse;
    } else if (!fn) {
      return elem;
    }

    if (one === 1) {
      origFn = fn;
      fn = function (event) {
        // Can use an empty set, since event contains the info
        jQuery().off(event);
        return origFn.apply(this, arguments);
      };

      // Use same guid so caller can remove using origFn
      fn.guid = origFn.guid || (origFn.guid = jQuery.guid++);
    }
    return elem.each(function () {
      jQuery.event.add(this, types, fn, data, selector);
    });
  }

  /*
   * Helper functions for managing events -- not part of the public interface.
   * Props to Dean Edwards' addEvent library for many of the ideas.
   */
  jQuery.event = {
    global: {},

    add: function (elem, types, handler, data, selector) {
      var handleObjIn,
        eventHandle,
        tmp,
        events,
        t,
        handleObj,
        special,
        handlers,
        type,
        namespaces,
        origType,
        elemData = dataPriv.get(elem);

      // Only attach events to objects that accept data
      if (!acceptData(elem)) {
        return;
      }

      // Caller can pass in an object of custom data in lieu of the handler
      if (handler.handler) {
        handleObjIn = handler;
        handler = handleObjIn.handler;
        selector = handleObjIn.selector;
      }

      // Ensure that invalid selectors throw exceptions at attach time
      // Evaluate against documentElement in case elem is a non-element node (e.g., document)
      if (selector) {
        jQuery.find.matchesSelector(documentElement, selector);
      }

      // Make sure that the handler has a unique ID, used to find/remove it later
      if (!handler.guid) {
        handler.guid = jQuery.guid++;
      }

      // Init the element's event structure and main handler, if this is the first
      if (!(events = elemData.events)) {
        events = elemData.events = Object.create(null);
      }
      if (!(eventHandle = elemData.handle)) {
        eventHandle = elemData.handle = function (e) {
          // Discard the second event of a jQuery.event.trigger() and
          // when an event is called after a page has unloaded
          return typeof jQuery !== "undefined" &&
            jQuery.event.triggered !== e.type
            ? jQuery.event.dispatch.apply(elem, arguments)
            : undefined;
        };
      }

      // Handle multiple events separated by a space
      types = (types || "").match(rnothtmlwhite) || [""];
      t = types.length;
      while (t--) {
        tmp = rtypenamespace.exec(types[t]) || [];
        type = origType = tmp[1];
        namespaces = (tmp[2] || "").split(".").sort();

        // There *must* be a type, no attaching namespace-only handlers
        if (!type) {
          continue;
        }

        // If event changes its type, use the special event handlers for the changed type
        special = jQuery.event.special[type] || {};

        // If selector defined, determine special event api type, otherwise given type
        type = (selector ? special.delegateType : special.bindType) || type;

        // Update special based on newly reset type
        special = jQuery.event.special[type] || {};

        // handleObj is passed to all event handlers
        handleObj = jQuery.extend(
          {
            type: type,
            origType: origType,
            data: data,
            handler: handler,
            guid: handler.guid,
            selector: selector,
            needsContext:
              selector && jQuery.expr.match.needsContext.test(selector),
            namespace: namespaces.join("."),
          },
          handleObjIn
        );

        // Init the event handler queue if we're the first
        if (!(handlers = events[type])) {
          handlers = events[type] = [];
          handlers.delegateCount = 0;

          // Only use addEventListener if the special events handler returns false
          if (
            !special.setup ||
            special.setup.call(elem, data, namespaces, eventHandle) === false
          ) {
            if (elem.addEventListener) {
              elem.addEventListener(type, eventHandle);
            }
          }
        }

        if (special.add) {
          special.add.call(elem, handleObj);

          if (!handleObj.handler.guid) {
            handleObj.handler.guid = handler.guid;
          }
        }

        // Add to the element's handler list, delegates in front
        if (selector) {
          handlers.splice(handlers.delegateCount++, 0, handleObj);
        } else {
          handlers.push(handleObj);
        }

        // Keep track of which events have ever been used, for event optimization
        jQuery.event.global[type] = true;
      }
    },

    // Detach an event or set of events from an element
    remove: function (elem, types, handler, selector, mappedTypes) {
      var j,
        origCount,
        tmp,
        events,
        t,
        handleObj,
        special,
        handlers,
        type,
        namespaces,
        origType,
        elemData = dataPriv.hasData(elem) && dataPriv.get(elem);

      if (!elemData || !(events = elemData.events)) {
        return;
      }

      // Once for each type.namespace in types; type may be omitted
      types = (types || "").match(rnothtmlwhite) || [""];
      t = types.length;
      while (t--) {
        tmp = rtypenamespace.exec(types[t]) || [];
        type = origType = tmp[1];
        namespaces = (tmp[2] || "").split(".").sort();

        // Unbind all events (on this namespace, if provided) for the element
        if (!type) {
          for (type in events) {
            jQuery.event.remove(elem, type + types[t], handler, selector, true);
          }
          continue;
        }

        special = jQuery.event.special[type] || {};
        type = (selector ? special.delegateType : special.bindType) || type;
        handlers = events[type] || [];
        tmp =
          tmp[2] &&
          new RegExp("(^|\\.)" + namespaces.join("\\.(?:.*\\.|)") + "(\\.|$)");

        // Remove matching events
        origCount = j = handlers.length;
        while (j--) {
          handleObj = handlers[j];

          if (
            (mappedTypes || origType === handleObj.origType) &&
            (!handler || handler.guid === handleObj.guid) &&
            (!tmp || tmp.test(handleObj.namespace)) &&
            (!selector ||
              selector === handleObj.selector ||
              (selector === "**" && handleObj.selector))
          ) {
            handlers.splice(j, 1);

            if (handleObj.selector) {
              handlers.delegateCount--;
            }
            if (special.remove) {
              special.remove.call(elem, handleObj);
            }
          }
        }

        // Remove generic event handler if we removed something and no more handlers exist
        // (avoids potential for endless recursion during removal of special event handlers)
        if (origCount && !handlers.length) {
          if (
            !special.teardown ||
            special.teardown.call(elem, namespaces, elemData.handle) === false
          ) {
            jQuery.removeEvent(elem, type, elemData.handle);
          }

          delete events[type];
        }
      }

      // Remove data and the expando if it's no longer used
      if (jQuery.isEmptyObject(events)) {
        dataPriv.remove(elem, "handle events");
      }
    },

    dispatch: function (nativeEvent) {
      var i,
        j,
        ret,
        matched,
        handleObj,
        handlerQueue,
        args = new Array(arguments.length),
        // Make a writable jQuery.Event from the native event object
        event = jQuery.event.fix(nativeEvent),
        handlers =
          (dataPriv.get(this, "events") || Object.create(null))[event.type] ||
          [],
        special = jQuery.event.special[event.type] || {};

      // Use the fix-ed jQuery.Event rather than the (read-only) native event
      args[0] = event;

      for (i = 1; i < arguments.length; i++) {
        args[i] = arguments[i];
      }

      event.delegateTarget = this;

      // Call the preDispatch hook for the mapped type, and let it bail if desired
      if (
        special.preDispatch &&
        special.preDispatch.call(this, event) === false
      ) {
        return;
      }

      // Determine handlers
      handlerQueue = jQuery.event.handlers.call(this, event, handlers);

      // Run delegates first; they may want to stop propagation beneath us
      i = 0;
      while ((matched = handlerQueue[i++]) && !event.isPropagationStopped()) {
        event.currentTarget = matched.elem;

        j = 0;
        while (
          (handleObj = matched.handlers[j++]) &&
          !event.isImmediatePropagationStopped()
        ) {
          // If the event is namespaced, then each handler is only invoked if it is
          // specially universal or its namespaces are a superset of the event's.
          if (
            !event.rnamespace ||
            handleObj.namespace === false ||
            event.rnamespace.test(handleObj.namespace)
          ) {
            event.handleObj = handleObj;
            event.data = handleObj.data;

            ret = (
              (jQuery.event.special[handleObj.origType] || {}).handle ||
              handleObj.handler
            ).apply(matched.elem, args);

            if (ret !== undefined) {
              if ((event.result = ret) === false) {
                event.preventDefault();
                event.stopPropagation();
              }
            }
          }
        }
      }

      // Call the postDispatch hook for the mapped type
      if (special.postDispatch) {
        special.postDispatch.call(this, event);
      }

      return event.result;
    },

    handlers: function (event, handlers) {
      var i,
        handleObj,
        sel,
        matchedHandlers,
        matchedSelectors,
        handlerQueue = [],
        delegateCount = handlers.delegateCount,
        cur = event.target;

      // Find delegate handlers
      if (
        delegateCount &&
        // Support: IE <=9
        // Black-hole SVG <use> instance trees (trac-13180)
        cur.nodeType &&
        // Support: Firefox <=42
        // Suppress spec-violating clicks indicating a non-primary pointer button (trac-3861)
        // https://www.w3.org/TR/DOM-Level-3-Events/#event-type-click
        // Support: IE 11 only
        // ...but not arrow key "clicks" of radio inputs, which can have `button` -1 (gh-2343)
        !(event.type === "click" && event.button >= 1)
      ) {
        for (; cur !== this; cur = cur.parentNode || this) {
          // Don't check non-elements (#13208)
          // Don't process clicks on disabled elements (#6911, #8165, #11382, #11764)
          if (
            cur.nodeType === 1 &&
            !(event.type === "click" && cur.disabled === true)
          ) {
            matchedHandlers = [];
            matchedSelectors = {};
            for (i = 0; i < delegateCount; i++) {
              handleObj = handlers[i];

              // Don't conflict with Object.prototype properties (#13203)
              sel = handleObj.selector + " ";

              if (matchedSelectors[sel] === undefined) {
                matchedSelectors[sel] = handleObj.needsContext
                  ? jQuery(sel, this).index(cur) > -1
                  : jQuery.find(sel, this, null, [cur]).length;
              }
              if (matchedSelectors[sel]) {
                matchedHandlers.push(handleObj);
              }
            }
            if (matchedHandlers.length) {
              handlerQueue.push({ elem: cur, handlers: matchedHandlers });
            }
          }
        }
      }

      // Add the remaining (directly-bound) handlers
      cur = this;
      if (delegateCount < handlers.length) {
        handlerQueue.push({
          elem: cur,
          handlers: handlers.slice(delegateCount),
        });
      }

      return handlerQueue;
    },

    addProp: function (name, hook) {
      Object.defineProperty(jQuery.Event.prototype, name, {
        enumerable: true,
        configurable: true,

        get: isFunction(hook)
          ? function () {
              if (this.originalEvent) {
                return hook(this.originalEvent);
              }
            }
          : function () {
              if (this.originalEvent) {
                return this.originalEvent[name];
              }
            },

        set: function (value) {
          Object.defineProperty(this, name, {
            enumerable: true,
            configurable: true,
            writable: true,
            value: value,
          });
        },
      });
    },

    fix: function (originalEvent) {
      return originalEvent[jQuery.expando]
        ? originalEvent
        : new jQuery.Event(originalEvent);
    },

    special: {
      load: {
        // Prevent triggered image.load events from bubbling to window.load
        noBubble: true,
      },
      click: {
        // Utilize native event to ensure correct state for checkable inputs
        setup: function (data) {
          // For mutual compressibility with _default, replace `this` access with a local var.
          // `|| data` is dead code meant only to preserve the variable through minification.
          var el = this || data;

          // Claim the first handler
          if (
            rcheckableType.test(el.type) &&
            el.click &&
            nodeName(el, "input")
          ) {
            // dataPriv.set( el, "click", ... )
            leverageNative(el, "click", returnTrue);
          }

          // Return false to allow normal processing in the caller
          return false;
        },
        trigger: function (data) {
          // For mutual compressibility with _default, replace `this` access with a local var.
          // `|| data` is dead code meant only to preserve the variable through minification.
          var el = this || data;

          // Force setup before triggering a click
          if (
            rcheckableType.test(el.type) &&
            el.click &&
            nodeName(el, "input")
          ) {
            leverageNative(el, "click");
          }

          // Return non-false to allow normal event-path propagation
          return true;
        },

        // For cross-browser consistency, suppress native .click() on links
        // Also prevent it if we're currently inside a leveraged native-event stack
        _default: function (event) {
          var target = event.target;
          return (
            (rcheckableType.test(target.type) &&
              target.click &&
              nodeName(target, "input") &&
              dataPriv.get(target, "click")) ||
            nodeName(target, "a")
          );
        },
      },

      beforeunload: {
        postDispatch: function (event) {
          // Support: Firefox 20+
          // Firefox doesn't alert if the returnValue field is not set.
          if (event.result !== undefined && event.originalEvent) {
            event.originalEvent.returnValue = event.result;
          }
        },
      },
    },
  };

  // Ensure the presence of an event listener that handles manually-triggered
  // synthetic events by interrupting progress until reinvoked in response to
  // *native* events that it fires directly, ensuring that state changes have
  // already occurred before other listeners are invoked.
  function leverageNative(el, type, expectSync) {
    // Missing expectSync indicates a trigger call, which must force setup through jQuery.event.add
    if (!expectSync) {
      if (dataPriv.get(el, type) === undefined) {
        jQuery.event.add(el, type, returnTrue);
      }
      return;
    }

    // Register the controller as a special universal handler for all event namespaces
    dataPriv.set(el, type, false);
    jQuery.event.add(el, type, {
      namespace: false,
      handler: function (event) {
        var notAsync,
          result,
          saved = dataPriv.get(this, type);

        if (event.isTrigger & 1 && this[type]) {
          // Interrupt processing of the outer synthetic .trigger()ed event
          // Saved data should be false in such cases, but might be a leftover capture object
          // from an async native handler (gh-4350)
          if (!saved.length) {
            // Store arguments for use when handling the inner native event
            // There will always be at least one argument (an event object), so this array
            // will not be confused with a leftover capture object.
            saved = slice.call(arguments);
            dataPriv.set(this, type, saved);

            // Trigger the native event and capture its result
            // Support: IE <=9 - 11+
            // focus() and blur() are asynchronous
            notAsync = expectSync(this, type);
            this[type]();
            result = dataPriv.get(this, type);
            if (saved !== result || notAsync) {
              dataPriv.set(this, type, false);
            } else {
              result = {};
            }
            if (saved !== result) {
              // Cancel the outer synthetic event
              event.stopImmediatePropagation();
              event.preventDefault();

              // Support: Chrome 86+
              // In Chrome, if an element having a focusout handler is blurred by
              // clicking outside of it, it invokes the handler synchronously. If
              // that handler calls `.remove()` on the element, the data is cleared,
              // leaving `result` undefined. We need to guard against this.
              return result && result.value;
            }

            // If this is an inner synthetic event for an event with a bubbling surrogate
            // (focus or blur), assume that the surrogate already propagated from triggering the
            // native event and prevent that from happening again here.
            // This technically gets the ordering wrong w.r.t. to `.trigger()` (in which the
            // bubbling surrogate propagates *after* the non-bubbling base), but that seems
            // less bad than duplication.
          } else if ((jQuery.event.special[type] || {}).delegateType) {
            event.stopPropagation();
          }

          // If this is a native event triggered above, everything is now in order
          // Fire an inner synthetic event with the original arguments
        } else if (saved.length) {
          // ...and capture the result
          dataPriv.set(this, type, {
            value: jQuery.event.trigger(
              // Support: IE <=9 - 11+
              // Extend with the prototype to reset the above stopImmediatePropagation()
              jQuery.extend(saved[0], jQuery.Event.prototype),
              saved.slice(1),
              this
            ),
          });

          // Abort handling of the native event
          event.stopImmediatePropagation();
        }
      },
    });
  }

  jQuery.removeEvent = function (elem, type, handle) {
    // This "if" is needed for plain objects
    if (elem.removeEventListener) {
      elem.removeEventListener(type, handle);
    }
  };

  jQuery.Event = function (src, props) {
    // Allow instantiation without the 'new' keyword
    if (!(this instanceof jQuery.Event)) {
      return new jQuery.Event(src, props);
    }

    // Event object
    if (src && src.type) {
      this.originalEvent = src;
      this.type = src.type;

      // Events bubbling up the document may have been marked as prevented
      // by a handler lower down the tree; reflect the correct value.
      this.isDefaultPrevented =
        src.defaultPrevented ||
        (src.defaultPrevented === undefined &&
          // Support: Android <=2.3 only
          src.returnValue === false)
          ? returnTrue
          : returnFalse;

      // Create target properties
      // Support: Safari <=6 - 7 only
      // Target should not be a text node (#504, #13143)
      this.target =
        src.target && src.target.nodeType === 3
          ? src.target.parentNode
          : src.target;

      this.currentTarget = src.currentTarget;
      this.relatedTarget = src.relatedTarget;

      // Event type
    } else {
      this.type = src;
    }

    // Put explicitly provided properties onto the event object
    if (props) {
      jQuery.extend(this, props);
    }

    // Create a timestamp if incoming event doesn't have one
    this.timeStamp = (src && src.timeStamp) || Date.now();

    // Mark it as fixed
    this[jQuery.expando] = true;
  };

  // jQuery.Event is based on DOM3 Events as specified by the ECMAScript Language Binding
  // https://www.w3.org/TR/2003/WD-DOM-Level-3-Events-20030331/ecma-script-binding.html
  jQuery.Event.prototype = {
    constructor: jQuery.Event,
    isDefaultPrevented: returnFalse,
    isPropagationStopped: returnFalse,
    isImmediatePropagationStopped: returnFalse,
    isSimulated: false,

    preventDefault: function () {
      var e = this.originalEvent;

      this.isDefaultPrevented = returnTrue;

      if (e && !this.isSimulated) {
        e.preventDefault();
      }
    },
    stopPropagation: function () {
      var e = this.originalEvent;

      this.isPropagationStopped = returnTrue;

      if (e && !this.isSimulated) {
        e.stopPropagation();
      }
    },
    stopImmediatePropagation: function () {
      var e = this.originalEvent;

      this.isImmediatePropagationStopped = returnTrue;

      if (e && !this.isSimulated) {
        e.stopImmediatePropagation();
      }

      this.stopPropagation();
    },
  };

  // Includes all common event props including KeyEvent and MouseEvent specific props
  jQuery.each(
    {
      altKey: true,
      bubbles: true,
      cancelable: true,
      changedTouches: true,
      ctrlKey: true,
      detail: true,
      eventPhase: true,
      metaKey: true,
      pageX: true,
      pageY: true,
      shiftKey: true,
      view: true,
      char: true,
      code: true,
      charCode: true,
      key: true,
      keyCode: true,
      button: true,
      buttons: true,
      clientX: true,
      clientY: true,
      offsetX: true,
      offsetY: true,
      pointerId: true,
      pointerType: true,
      screenX: true,
      screenY: true,
      targetTouches: true,
      toElement: true,
      touches: true,
      which: true,
    },
    jQuery.event.addProp
  );

  jQuery.each({ focus: "focusin", blur: "focusout" }, function (
    type,
    delegateType
  ) {
    jQuery.event.special[type] = {
      // Utilize native event if possible so blur/focus sequence is correct
      setup: function () {
        // Claim the first handler
        // dataPriv.set( this, "focus", ... )
        // dataPriv.set( this, "blur", ... )
        leverageNative(this, type, expectSync);

        // Return false to allow normal processing in the caller
        return false;
      },
      trigger: function () {
        // Force setup before trigger
        leverageNative(this, type);

        // Return non-false to allow normal event-path propagation
        return true;
      },

      // Suppress native focus or blur as it's already being fired
      // in leverageNative.
      _default: function () {
        return true;
      },

      delegateType: delegateType,
    };
  });

  // Create mouseenter/leave events using mouseover/out and event-time checks
  // so that event delegation works in jQuery.
  // Do the same for pointerenter/pointerleave and pointerover/pointerout
  //
  // Support: Safari 7 only
  // Safari sends mouseenter too often; see:
  // https://bugs.chromium.org/p/chromium/issues/detail?id=470258
  // for the description of the bug (it existed in older Chrome versions as well).
  jQuery.each(
    {
      mouseenter: "mouseover",
      mouseleave: "mouseout",
      pointerenter: "pointerover",
      pointerleave: "pointerout",
    },
    function (orig, fix) {
      jQuery.event.special[orig] = {
        delegateType: fix,
        bindType: fix,

        handle: function (event) {
          var ret,
            target = this,
            related = event.relatedTarget,
            handleObj = event.handleObj;

          // For mouseenter/leave call the handler if related is outside the target.
          // NB: No relatedTarget if the mouse left/entered the browser window
          if (
            !related ||
            (related !== target && !jQuery.contains(target, related))
          ) {
            event.type = handleObj.origType;
            ret = handleObj.handler.apply(this, arguments);
            event.type = fix;
          }
          return ret;
        },
      };
    }
  );

  jQuery.fn.extend({
    on: function (types, selector, data, fn) {
      return on(this, types, selector, data, fn);
    },
    one: function (types, selector, data, fn) {
      return on(this, types, selector, data, fn, 1);
    },
    off: function (types, selector, fn) {
      var handleObj, type;
      if (types && types.preventDefault && types.handleObj) {
        // ( event )  dispatched jQuery.Event
        handleObj = types.handleObj;
        jQuery(types.delegateTarget).off(
          handleObj.namespace
            ? handleObj.origType + "." + handleObj.namespace
            : handleObj.origType,
          handleObj.selector,
          handleObj.handler
        );
        return this;
      }
      if (typeof types === "object") {
        // ( types-object [, selector] )
        for (type in types) {
          this.off(type, selector, types[type]);
        }
        return this;
      }
      if (selector === false || typeof selector === "function") {
        // ( types [, fn] )
        fn = selector;
        selector = undefined;
      }
      if (fn === false) {
        fn = returnFalse;
      }
      return this.each(function () {
        jQuery.event.remove(this, types, fn, selector);
      });
    },
  });

  var // Support: IE <=10 - 11, Edge 12 - 13 only
    // In IE/Edge using regex groups here causes severe slowdowns.
    // See https://connect.microsoft.com/IE/feedback/details/1736512/
    rnoInnerhtml = /<script|<style|<link/i,
    // checked="checked" or checked
    rchecked = /checked\s*(?:[^=]|=\s*.checked.)/i,
    rcleanScript = /^\s*<!(?:\[CDATA\[|--)|(?:\]\]|--)>\s*$/g;

  // Prefer a tbody over its parent table for containing new rows
  function manipulationTarget(elem, content) {
    if (
      nodeName(elem, "table") &&
      nodeName(content.nodeType !== 11 ? content : content.firstChild, "tr")
    ) {
      return jQuery(elem).children("tbody")[0] || elem;
    }

    return elem;
  }

  // Replace/restore the type attribute of script elements for safe DOM manipulation
  function disableScript(elem) {
    elem.type = (elem.getAttribute("type") !== null) + "/" + elem.type;
    return elem;
  }
  function restoreScript(elem) {
    if ((elem.type || "").slice(0, 5) === "true/") {
      elem.type = elem.type.slice(5);
    } else {
      elem.removeAttribute("type");
    }

    return elem;
  }

  function cloneCopyEvent(src, dest) {
    var i, l, type, pdataOld, udataOld, udataCur, events;

    if (dest.nodeType !== 1) {
      return;
    }

    // 1. Copy private data: events, handlers, etc.
    if (dataPriv.hasData(src)) {
      pdataOld = dataPriv.get(src);
      events = pdataOld.events;

      if (events) {
        dataPriv.remove(dest, "handle events");

        for (type in events) {
          for (i = 0, l = events[type].length; i < l; i++) {
            jQuery.event.add(dest, type, events[type][i]);
          }
        }
      }
    }

    // 2. Copy user data
    if (dataUser.hasData(src)) {
      udataOld = dataUser.access(src);
      udataCur = jQuery.extend({}, udataOld);

      dataUser.set(dest, udataCur);
    }
  }

  // Fix IE bugs, see support tests
  function fixInput(src, dest) {
    var nodeName = dest.nodeName.toLowerCase();

    // Fails to persist the checked state of a cloned checkbox or radio button.
    if (nodeName === "input" && rcheckableType.test(src.type)) {
      dest.checked = src.checked;

      // Fails to return the selected option to the default selected state when cloning options
    } else if (nodeName === "input" || nodeName === "textarea") {
      dest.defaultValue = src.defaultValue;
    }
  }

  function domManip(collection, args, callback, ignored) {
    // Flatten any nested arrays
    args = flat(args);

    var fragment,
      first,
      scripts,
      hasScripts,
      node,
      doc,
      i = 0,
      l = collection.length,
      iNoClone = l - 1,
      value = args[0],
      valueIsFunction = isFunction(value);

    // We can't cloneNode fragments that contain checked, in WebKit
    if (
      valueIsFunction ||
      (l > 1 &&
        typeof value === "string" &&
        !support.checkClone &&
        rchecked.test(value))
    ) {
      return collection.each(function (index) {
        var self = collection.eq(index);
        if (valueIsFunction) {
          args[0] = value.call(this, index, self.html());
        }
        domManip(self, args, callback, ignored);
      });
    }

    if (l) {
      fragment = buildFragment(
        args,
        collection[0].ownerDocument,
        false,
        collection,
        ignored
      );
      first = fragment.firstChild;

      if (fragment.childNodes.length === 1) {
        fragment = first;
      }

      // Require either new content or an interest in ignored elements to invoke the callback
      if (first || ignored) {
        scripts = jQuery.map(getAll(fragment, "script"), disableScript);
        hasScripts = scripts.length;

        // Use the original fragment for the last item
        // instead of the first because it can end up
        // being emptied incorrectly in certain situations (#8070).
        for (; i < l; i++) {
          node = fragment;

          if (i !== iNoClone) {
            node = jQuery.clone(node, true, true);

            // Keep references to cloned scripts for later restoration
            if (hasScripts) {
              // Support: Android <=4.0 only, PhantomJS 1 only
              // push.apply(_, arraylike) throws on ancient WebKit
              jQuery.merge(scripts, getAll(node, "script"));
            }
          }

          callback.call(collection[i], node, i);
        }

        if (hasScripts) {
          doc = scripts[scripts.length - 1].ownerDocument;

          // Reenable scripts
          jQuery.map(scripts, restoreScript);

          // Evaluate executable scripts on first document insertion
          for (i = 0; i < hasScripts; i++) {
            node = scripts[i];
            if (
              rscriptType.test(node.type || "") &&
              !dataPriv.access(node, "globalEval") &&
              jQuery.contains(doc, node)
            ) {
              if (node.src && (node.type || "").toLowerCase() !== "module") {
                // Optional AJAX dependency, but won't run scripts if not present
                if (jQuery._evalUrl && !node.noModule) {
                  jQuery._evalUrl(
                    node.src,
                    {
                      nonce: node.nonce || node.getAttribute("nonce"),
                    },
                    doc
                  );
                }
              } else {
                DOMEval(node.textContent.replace(rcleanScript, ""), node, doc);
              }
            }
          }
        }
      }
    }

    return collection;
  }

  function remove(elem, selector, keepData) {
    var node,
      nodes = selector ? jQuery.filter(selector, elem) : elem,
      i = 0;

    for (; (node = nodes[i]) != null; i++) {
      if (!keepData && node.nodeType === 1) {
        jQuery.cleanData(getAll(node));
      }

      if (node.parentNode) {
        if (keepData && isAttached(node)) {
          setGlobalEval(getAll(node, "script"));
        }
        node.parentNode.removeChild(node);
      }
    }

    return elem;
  }

  jQuery.extend({
    htmlPrefilter: function (html) {
      return html;
    },

    clone: function (elem, dataAndEvents, deepDataAndEvents) {
      var i,
        l,
        srcElements,
        destElements,
        clone = elem.cloneNode(true),
        inPage = isAttached(elem);

      // Fix IE cloning issues
      if (
        !support.noCloneChecked &&
        (elem.nodeType === 1 || elem.nodeType === 11) &&
        !jQuery.isXMLDoc(elem)
      ) {
        // We eschew Sizzle here for performance reasons: https://jsperf.com/getall-vs-sizzle/2
        destElements = getAll(clone);
        srcElements = getAll(elem);

        for (i = 0, l = srcElements.length; i < l; i++) {
          fixInput(srcElements[i], destElements[i]);
        }
      }

      // Copy the events from the original to the clone
      if (dataAndEvents) {
        if (deepDataAndEvents) {
          srcElements = srcElements || getAll(elem);
          destElements = destElements || getAll(clone);

          for (i = 0, l = srcElements.length; i < l; i++) {
            cloneCopyEvent(srcElements[i], destElements[i]);
          }
        } else {
          cloneCopyEvent(elem, clone);
        }
      }

      // Preserve script evaluation history
      destElements = getAll(clone, "script");
      if (destElements.length > 0) {
        setGlobalEval(destElements, !inPage && getAll(elem, "script"));
      }

      // Return the cloned set
      return clone;
    },

    cleanData: function (elems) {
      var data,
        elem,
        type,
        special = jQuery.event.special,
        i = 0;

      for (; (elem = elems[i]) !== undefined; i++) {
        if (acceptData(elem)) {
          if ((data = elem[dataPriv.expando])) {
            if (data.events) {
              for (type in data.events) {
                if (special[type]) {
                  jQuery.event.remove(elem, type);

                  // This is a shortcut to avoid jQuery.event.remove's overhead
                } else {
                  jQuery.removeEvent(elem, type, data.handle);
                }
              }
            }

            // Support: Chrome <=35 - 45+
            // Assign undefined instead of using delete, see Data#remove
            elem[dataPriv.expando] = undefined;
          }
          if (elem[dataUser.expando]) {
            // Support: Chrome <=35 - 45+
            // Assign undefined instead of using delete, see Data#remove
            elem[dataUser.expando] = undefined;
          }
        }
      }
    },
  });

  jQuery.fn.extend({
    detach: function (selector) {
      return remove(this, selector, true);
    },

    remove: function (selector) {
      return remove(this, selector);
    },

    text: function (value) {
      return access(
        this,
        function (value) {
          return value === undefined
            ? jQuery.text(this)
            : this.empty().each(function () {
                if (
                  this.nodeType === 1 ||
                  this.nodeType === 11 ||
                  this.nodeType === 9
                ) {
                  this.textContent = value;
                }
              });
        },
        null,
        value,
        arguments.length
      );
    },

    append: function () {
      return domManip(this, arguments, function (elem) {
        if (
          this.nodeType === 1 ||
          this.nodeType === 11 ||
          this.nodeType === 9
        ) {
          var target = manipulationTarget(this, elem);
          target.appendChild(elem);
        }
      });
    },

    prepend: function () {
      return domManip(this, arguments, function (elem) {
        if (
          this.nodeType === 1 ||
          this.nodeType === 11 ||
          this.nodeType === 9
        ) {
          var target = manipulationTarget(this, elem);
          target.insertBefore(elem, target.firstChild);
        }
      });
    },

    before: function () {
      return domManip(this, arguments, function (elem) {
        if (this.parentNode) {
          this.parentNode.insertBefore(elem, this);
        }
      });
    },

    after: function () {
      return domManip(this, arguments, function (elem) {
        if (this.parentNode) {
          this.parentNode.insertBefore(elem, this.nextSibling);
        }
      });
    },

    empty: function () {
      var elem,
        i = 0;

      for (; (elem = this[i]) != null; i++) {
        if (elem.nodeType === 1) {
          // Prevent memory leaks
          jQuery.cleanData(getAll(elem, false));

          // Remove any remaining nodes
          elem.textContent = "";
        }
      }

      return this;
    },

    clone: function (dataAndEvents, deepDataAndEvents) {
      dataAndEvents = dataAndEvents == null ? false : dataAndEvents;
      deepDataAndEvents =
        deepDataAndEvents == null ? dataAndEvents : deepDataAndEvents;

      return this.map(function () {
        return jQuery.clone(this, dataAndEvents, deepDataAndEvents);
      });
    },

    html: function (value) {
      return access(
        this,
        function (value) {
          var elem = this[0] || {},
            i = 0,
            l = this.length;

          if (value === undefined && elem.nodeType === 1) {
            return elem.innerHTML;
          }

          // See if we can take a shortcut and just use innerHTML
          if (
            typeof value === "string" &&
            !rnoInnerhtml.test(value) &&
            !wrapMap[(rtagName.exec(value) || ["", ""])[1].toLowerCase()]
          ) {
            value = jQuery.htmlPrefilter(value);

            try {
              for (; i < l; i++) {
                elem = this[i] || {};

                // Remove element nodes and prevent memory leaks
                if (elem.nodeType === 1) {
                  jQuery.cleanData(getAll(elem, false));
                  elem.innerHTML = value;
                }
              }

              elem = 0;

              // If using innerHTML throws an exception, use the fallback method
            } catch (e) {}
          }

          if (elem) {
            this.empty().append(value);
          }
        },
        null,
        value,
        arguments.length
      );
    },

    replaceWith: function () {
      var ignored = [];

      // Make the changes, replacing each non-ignored context element with the new content
      return domManip(
        this,
        arguments,
        function (elem) {
          var parent = this.parentNode;

          if (jQuery.inArray(this, ignored) < 0) {
            jQuery.cleanData(getAll(this));
            if (parent) {
              parent.replaceChild(elem, this);
            }
          }

          // Force callback invocation
        },
        ignored
      );
    },
  });

  jQuery.each(
    {
      appendTo: "append",
      prependTo: "prepend",
      insertBefore: "before",
      insertAfter: "after",
      replaceAll: "replaceWith",
    },
    function (name, original) {
      jQuery.fn[name] = function (selector) {
        var elems,
          ret = [],
          insert = jQuery(selector),
          last = insert.length - 1,
          i = 0;

        for (; i <= last; i++) {
          elems = i === last ? this : this.clone(true);
          jQuery(insert[i])[original](elems);

          // Support: Android <=4.0 only, PhantomJS 1 only
          // .get() because push.apply(_, arraylike) throws on ancient WebKit
          push.apply(ret, elems.get());
        }

        return this.pushStack(ret);
      };
    }
  );
  var rnumnonpx = new RegExp("^(" + pnum + ")(?!px)[a-z%]+$", "i");

  var getStyles = function (elem) {
    // Support: IE <=11 only, Firefox <=30 (#15098, #14150)
    // IE throws on elements created in popups
    // FF meanwhile throws on frame elements through "defaultView.getComputedStyle"
    var view = elem.ownerDocument.defaultView;

    if (!view || !view.opener) {
      view = window;
    }

    return view.getComputedStyle(elem);
  };

  var swap = function (elem, options, callback) {
    var ret,
      name,
      old = {};

    // Remember the old values, and insert the new ones
    for (name in options) {
      old[name] = elem.style[name];
      elem.style[name] = options[name];
    }

    ret = callback.call(elem);

    // Revert the old values
    for (name in options) {
      elem.style[name] = old[name];
    }

    return ret;
  };

  var rboxStyle = new RegExp(cssExpand.join("|"), "i");

  (function () {
    // Executing both pixelPosition & boxSizingReliable tests require only one layout
    // so they're executed at the same time to save the second computation.
    function computeStyleTests() {
      // This is a singleton, we need to execute it only once
      if (!div) {
        return;
      }

      container.style.cssText =
        "position:absolute;left:-11111px;width:60px;" +
        "margin-top:1px;padding:0;border:0";
      div.style.cssText =
        "position:relative;display:block;box-sizing:border-box;overflow:scroll;" +
        "margin:auto;border:1px;padding:1px;" +
        "width:60%;top:1%";
      documentElement.appendChild(container).appendChild(div);

      var divStyle = window.getComputedStyle(div);
      pixelPositionVal = divStyle.top !== "1%";

      // Support: Android 4.0 - 4.3 only, Firefox <=3 - 44
      reliableMarginLeftVal = roundPixelMeasures(divStyle.marginLeft) === 12;

      // Support: Android 4.0 - 4.3 only, Safari <=9.1 - 10.1, iOS <=7.0 - 9.3
      // Some styles come back with percentage values, even though they shouldn't
      div.style.right = "60%";
      pixelBoxStylesVal = roundPixelMeasures(divStyle.right) === 36;

      // Support: IE 9 - 11 only
      // Detect misreporting of content dimensions for box-sizing:border-box elements
      boxSizingReliableVal = roundPixelMeasures(divStyle.width) === 36;

      // Support: IE 9 only
      // Detect overflow:scroll screwiness (gh-3699)
      // Support: Chrome <=64
      // Don't get tricked when zoom affects offsetWidth (gh-4029)
      div.style.position = "absolute";
      scrollboxSizeVal = roundPixelMeasures(div.offsetWidth / 3) === 12;

      documentElement.removeChild(container);

      // Nullify the div so it wouldn't be stored in the memory and
      // it will also be a sign that checks already performed
      div = null;
    }

    function roundPixelMeasures(measure) {
      return Math.round(parseFloat(measure));
    }

    var pixelPositionVal,
      boxSizingReliableVal,
      scrollboxSizeVal,
      pixelBoxStylesVal,
      reliableTrDimensionsVal,
      reliableMarginLeftVal,
      container = document.createElement("div"),
      div = document.createElement("div");

    // Finish early in limited (non-browser) environments
    if (!div.style) {
      return;
    }

    // Support: IE <=9 - 11 only
    // Style of cloned element affects source element cloned (#8908)
    div.style.backgroundClip = "content-box";
    div.cloneNode(true).style.backgroundClip = "";
    support.clearCloneStyle = div.style.backgroundClip === "content-box";

    jQuery.extend(support, {
      boxSizingReliable: function () {
        computeStyleTests();
        return boxSizingReliableVal;
      },
      pixelBoxStyles: function () {
        computeStyleTests();
        return pixelBoxStylesVal;
      },
      pixelPosition: function () {
        computeStyleTests();
        return pixelPositionVal;
      },
      reliableMarginLeft: function () {
        computeStyleTests();
        return reliableMarginLeftVal;
      },
      scrollboxSize: function () {
        computeStyleTests();
        return scrollboxSizeVal;
      },

      // Support: IE 9 - 11+, Edge 15 - 18+
      // IE/Edge misreport `getComputedStyle` of table rows with width/height
      // set in CSS while `offset*` properties report correct values.
      // Behavior in IE 9 is more subtle than in newer versions & it passes
      // some versions of this test; make sure not to make it pass there!
      //
      // Support: Firefox 70+
      // Only Firefox includes border widths
      // in computed dimensions. (gh-4529)
      reliableTrDimensions: function () {
        var table, tr, trChild, trStyle;
        if (reliableTrDimensionsVal == null) {
          table = document.createElement("table");
          tr = document.createElement("tr");
          trChild = document.createElement("div");

          table.style.cssText =
            "position:absolute;left:-11111px;border-collapse:separate";
          tr.style.cssText = "border:1px solid";

          // Support: Chrome 86+
          // Height set through cssText does not get applied.
          // Computed height then comes back as 0.
          tr.style.height = "1px";
          trChild.style.height = "9px";

          // Support: Android 8 Chrome 86+
          // In our bodyBackground.html iframe,
          // display for all div elements is set to "inline",
          // which causes a problem only in Android 8 Chrome 86.
          // Ensuring the div is display: block
          // gets around this issue.
          trChild.style.display = "block";

          documentElement
            .appendChild(table)
            .appendChild(tr)
            .appendChild(trChild);

          trStyle = window.getComputedStyle(tr);
          reliableTrDimensionsVal =
            parseInt(trStyle.height, 10) +
              parseInt(trStyle.borderTopWidth, 10) +
              parseInt(trStyle.borderBottomWidth, 10) ===
            tr.offsetHeight;

          documentElement.removeChild(table);
        }
        return reliableTrDimensionsVal;
      },
    });
  })();

  function curCSS(elem, name, computed) {
    var width,
      minWidth,
      maxWidth,
      ret,
      // Support: Firefox 51+
      // Retrieving style before computed somehow
      // fixes an issue with getting wrong values
      // on detached elements
      style = elem.style;

    computed = computed || getStyles(elem);

    // getPropertyValue is needed for:
    //   .css('filter') (IE 9 only, #12537)
    //   .css('--customProperty) (#3144)
    if (computed) {
      ret = computed.getPropertyValue(name) || computed[name];

      if (ret === "" && !isAttached(elem)) {
        ret = jQuery.style(elem, name);
      }

      // A tribute to the "awesome hack by Dean Edwards"
      // Android Browser returns percentage for some values,
      // but width seems to be reliably pixels.
      // This is against the CSSOM draft spec:
      // https://drafts.csswg.org/cssom/#resolved-values
      if (
        !support.pixelBoxStyles() &&
        rnumnonpx.test(ret) &&
        rboxStyle.test(name)
      ) {
        // Remember the original values
        width = style.width;
        minWidth = style.minWidth;
        maxWidth = style.maxWidth;

        // Put in the new values to get a computed value out
        style.minWidth = style.maxWidth = style.width = ret;
        ret = computed.width;

        // Revert the changed values
        style.width = width;
        style.minWidth = minWidth;
        style.maxWidth = maxWidth;
      }
    }

    return ret !== undefined
      ? // Support: IE <=9 - 11 only
        // IE returns zIndex value as an integer.
        ret + ""
      : ret;
  }

  function addGetHookIf(conditionFn, hookFn) {
    // Define the hook, we'll check on the first run if it's really needed.
    return {
      get: function () {
        if (conditionFn()) {
          // Hook not needed (or it's not possible to use it due
          // to missing dependency), remove it.
          delete this.get;
          return;
        }

        // Hook needed; redefine it so that the support test is not executed again.
        return (this.get = hookFn).apply(this, arguments);
      },
    };
  }

  var cssPrefixes = ["Webkit", "Moz", "ms"],
    emptyStyle = document.createElement("div").style,
    vendorProps = {};

  // Return a vendor-prefixed property or undefined
  function vendorPropName(name) {
    // Check for vendor prefixed names
    var capName = name[0].toUpperCase() + name.slice(1),
      i = cssPrefixes.length;

    while (i--) {
      name = cssPrefixes[i] + capName;
      if (name in emptyStyle) {
        return name;
      }
    }
  }

  // Return a potentially-mapped jQuery.cssProps or vendor prefixed property
  function finalPropName(name) {
    var final = jQuery.cssProps[name] || vendorProps[name];

    if (final) {
      return final;
    }
    if (name in emptyStyle) {
      return name;
    }
    return (vendorProps[name] = vendorPropName(name) || name);
  }

  var // Swappable if display is none or starts with table
    // except "table", "table-cell", or "table-caption"
    // See here for display values: https://developer.mozilla.org/en-US/docs/CSS/display
    rdisplayswap = /^(none|table(?!-c[ea]).+)/,
    rcustomProp = /^--/,
    cssShow = { position: "absolute", visibility: "hidden", display: "block" },
    cssNormalTransform = {
      letterSpacing: "0",
      fontWeight: "400",
    };

  function setPositiveNumber(_elem, value, subtract) {
    // Any relative (+/-) values have already been
    // normalized at this point
    var matches = rcssNum.exec(value);
    return matches
      ? // Guard against undefined "subtract", e.g., when used as in cssHooks
        Math.max(0, matches[2] - (subtract || 0)) + (matches[3] || "px")
      : value;
  }

  function boxModelAdjustment(
    elem,
    dimension,
    box,
    isBorderBox,
    styles,
    computedVal
  ) {
    var i = dimension === "width" ? 1 : 0,
      extra = 0,
      delta = 0;

    // Adjustment may not be necessary
    if (box === (isBorderBox ? "border" : "content")) {
      return 0;
    }

    for (; i < 4; i += 2) {
      // Both box models exclude margin
      if (box === "margin") {
        delta += jQuery.css(elem, box + cssExpand[i], true, styles);
      }

      // If we get here with a content-box, we're seeking "padding" or "border" or "margin"
      if (!isBorderBox) {
        // Add padding
        delta += jQuery.css(elem, "padding" + cssExpand[i], true, styles);

        // For "border" or "margin", add border
        if (box !== "padding") {
          delta += jQuery.css(
            elem,
            "border" + cssExpand[i] + "Width",
            true,
            styles
          );

          // But still keep track of it otherwise
        } else {
          extra += jQuery.css(
            elem,
            "border" + cssExpand[i] + "Width",
            true,
            styles
          );
        }

        // If we get here with a border-box (content + padding + border), we're seeking "content" or
        // "padding" or "margin"
      } else {
        // For "content", subtract padding
        if (box === "content") {
          delta -= jQuery.css(elem, "padding" + cssExpand[i], true, styles);
        }

        // For "content" or "padding", subtract border
        if (box !== "margin") {
          delta -= jQuery.css(
            elem,
            "border" + cssExpand[i] + "Width",
            true,
            styles
          );
        }
      }
    }

    // Account for positive content-box scroll gutter when requested by providing computedVal
    if (!isBorderBox && computedVal >= 0) {
      // offsetWidth/offsetHeight is a rounded sum of content, padding, scroll gutter, and border
      // Assuming integer scroll gutter, subtract the rest and round down
      delta +=
        Math.max(
          0,
          Math.ceil(
            elem["offset" + dimension[0].toUpperCase() + dimension.slice(1)] -
              computedVal -
              delta -
              extra -
              0.5

            // If offsetWidth/offsetHeight is unknown, then we can't determine content-box scroll gutter
            // Use an explicit zero to avoid NaN (gh-3964)
          )
        ) || 0;
    }

    return delta;
  }

  function getWidthOrHeight(elem, dimension, extra) {
    // Start with computed style
    var styles = getStyles(elem),
      // To avoid forcing a reflow, only fetch boxSizing if we need it (gh-4322).
      // Fake content-box until we know it's needed to know the true value.
      boxSizingNeeded = !support.boxSizingReliable() || extra,
      isBorderBox =
        boxSizingNeeded &&
        jQuery.css(elem, "boxSizing", false, styles) === "border-box",
      valueIsBorderBox = isBorderBox,
      val = curCSS(elem, dimension, styles),
      offsetProp = "offset" + dimension[0].toUpperCase() + dimension.slice(1);

    // Support: Firefox <=54
    // Return a confounding non-pixel value or feign ignorance, as appropriate.
    if (rnumnonpx.test(val)) {
      if (!extra) {
        return val;
      }
      val = "auto";
    }

    // Support: IE 9 - 11 only
    // Use offsetWidth/offsetHeight for when box sizing is unreliable.
    // In those cases, the computed value can be trusted to be border-box.
    if (
      ((!support.boxSizingReliable() && isBorderBox) ||
        // Support: IE 10 - 11+, Edge 15 - 18+
        // IE/Edge misreport `getComputedStyle` of table rows with width/height
        // set in CSS while `offset*` properties report correct values.
        // Interestingly, in some cases IE 9 doesn't suffer from this issue.
        (!support.reliableTrDimensions() && nodeName(elem, "tr")) ||
        // Fall back to offsetWidth/offsetHeight when value is "auto"
        // This happens for inline elements with no explicit setting (gh-3571)
        val === "auto" ||
        // Support: Android <=4.1 - 4.3 only
        // Also use offsetWidth/offsetHeight for misreported inline dimensions (gh-3602)
        (!parseFloat(val) &&
          jQuery.css(elem, "display", false, styles) === "inline")) &&
      // Make sure the element is visible & connected
      elem.getClientRects().length
    ) {
      isBorderBox =
        jQuery.css(elem, "boxSizing", false, styles) === "border-box";

      // Where available, offsetWidth/offsetHeight approximate border box dimensions.
      // Where not available (e.g., SVG), assume unreliable box-sizing and interpret the
      // retrieved value as a content box dimension.
      valueIsBorderBox = offsetProp in elem;
      if (valueIsBorderBox) {
        val = elem[offsetProp];
      }
    }

    // Normalize "" and auto
    val = parseFloat(val) || 0;

    // Adjust for the element's box model
    return (
      val +
      boxModelAdjustment(
        elem,
        dimension,
        extra || (isBorderBox ? "border" : "content"),
        valueIsBorderBox,
        styles,

        // Provide the current computed size to request scroll gutter calculation (gh-3589)
        val
      ) +
      "px"
    );
  }

  jQuery.extend({
    // Add in style property hooks for overriding the default
    // behavior of getting and setting a style property
    cssHooks: {
      opacity: {
        get: function (elem, computed) {
          if (computed) {
            // We should always get a number back from opacity
            var ret = curCSS(elem, "opacity");
            return ret === "" ? "1" : ret;
          }
        },
      },
    },

    // Don't automatically add "px" to these possibly-unitless properties
    cssNumber: {
      animationIterationCount: true,
      columnCount: true,
      fillOpacity: true,
      flexGrow: true,
      flexShrink: true,
      fontWeight: true,
      gridArea: true,
      gridColumn: true,
      gridColumnEnd: true,
      gridColumnStart: true,
      gridRow: true,
      gridRowEnd: true,
      gridRowStart: true,
      lineHeight: true,
      opacity: true,
      order: true,
      orphans: true,
      widows: true,
      zIndex: true,
      zoom: true,
    },

    // Add in properties whose names you wish to fix before
    // setting or getting the value
    cssProps: {},

    // Get and set the style property on a DOM Node
    style: function (elem, name, value, extra) {
      // Don't set styles on text and comment nodes
      if (!elem || elem.nodeType === 3 || elem.nodeType === 8 || !elem.style) {
        return;
      }

      // Make sure that we're working with the right name
      var ret,
        type,
        hooks,
        origName = camelCase(name),
        isCustomProp = rcustomProp.test(name),
        style = elem.style;

      // Make sure that we're working with the right name. We don't
      // want to query the value if it is a CSS custom property
      // since they are user-defined.
      if (!isCustomProp) {
        name = finalPropName(origName);
      }

      // Gets hook for the prefixed version, then unprefixed version
      hooks = jQuery.cssHooks[name] || jQuery.cssHooks[origName];

      // Check if we're setting a value
      if (value !== undefined) {
        type = typeof value;

        // Convert "+=" or "-=" to relative numbers (#7345)
        if (type === "string" && (ret = rcssNum.exec(value)) && ret[1]) {
          value = adjustCSS(elem, name, ret);

          // Fixes bug #9237
          type = "number";
        }

        // Make sure that null and NaN values aren't set (#7116)
        if (value == null || value !== value) {
          return;
        }

        // If a number was passed in, add the unit (except for certain CSS properties)
        // The isCustomProp check can be removed in jQuery 4.0 when we only auto-append
        // "px" to a few hardcoded values.
        if (type === "number" && !isCustomProp) {
          value += (ret && ret[3]) || (jQuery.cssNumber[origName] ? "" : "px");
        }

        // background-* props affect original clone's values
        if (
          !support.clearCloneStyle &&
          value === "" &&
          name.indexOf("background") === 0
        ) {
          style[name] = "inherit";
        }

        // If a hook was provided, use that value, otherwise just set the specified value
        if (
          !hooks ||
          !("set" in hooks) ||
          (value = hooks.set(elem, value, extra)) !== undefined
        ) {
          if (isCustomProp) {
            style.setProperty(name, value);
          } else {
            style[name] = value;
          }
        }
      } else {
        // If a hook was provided get the non-computed value from there
        if (
          hooks &&
          "get" in hooks &&
          (ret = hooks.get(elem, false, extra)) !== undefined
        ) {
          return ret;
        }

        // Otherwise just get the value from the style object
        return style[name];
      }
    },

    css: function (elem, name, extra, styles) {
      var val,
        num,
        hooks,
        origName = camelCase(name),
        isCustomProp = rcustomProp.test(name);

      // Make sure that we're working with the right name. We don't
      // want to modify the value if it is a CSS custom property
      // since they are user-defined.
      if (!isCustomProp) {
        name = finalPropName(origName);
      }

      // Try prefixed name followed by the unprefixed name
      hooks = jQuery.cssHooks[name] || jQuery.cssHooks[origName];

      // If a hook was provided get the computed value from there
      if (hooks && "get" in hooks) {
        val = hooks.get(elem, true, extra);
      }

      // Otherwise, if a way to get the computed value exists, use that
      if (val === undefined) {
        val = curCSS(elem, name, styles);
      }

      // Convert "normal" to computed value
      if (val === "normal" && name in cssNormalTransform) {
        val = cssNormalTransform[name];
      }

      // Make numeric if forced or a qualifier was provided and val looks numeric
      if (extra === "" || extra) {
        num = parseFloat(val);
        return extra === true || isFinite(num) ? num || 0 : val;
      }

      return val;
    },
  });

  jQuery.each(["height", "width"], function (_i, dimension) {
    jQuery.cssHooks[dimension] = {
      get: function (elem, computed, extra) {
        if (computed) {
          // Certain elements can have dimension info if we invisibly show them
          // but it must have a current display style that would benefit
          return rdisplayswap.test(jQuery.css(elem, "display")) &&
            // Support: Safari 8+
            // Table columns in Safari have non-zero offsetWidth & zero
            // getBoundingClientRect().width unless display is changed.
            // Support: IE <=11 only
            // Running getBoundingClientRect on a disconnected node
            // in IE throws an error.
            (!elem.getClientRects().length ||
              !elem.getBoundingClientRect().width)
            ? swap(elem, cssShow, function () {
                return getWidthOrHeight(elem, dimension, extra);
              })
            : getWidthOrHeight(elem, dimension, extra);
        }
      },

      set: function (elem, value, extra) {
        var matches,
          styles = getStyles(elem),
          // Only read styles.position if the test has a chance to fail
          // to avoid forcing a reflow.
          scrollboxSizeBuggy =
            !support.scrollboxSize() && styles.position === "absolute",
          // To avoid forcing a reflow, only fetch boxSizing if we need it (gh-3991)
          boxSizingNeeded = scrollboxSizeBuggy || extra,
          isBorderBox =
            boxSizingNeeded &&
            jQuery.css(elem, "boxSizing", false, styles) === "border-box",
          subtract = extra
            ? boxModelAdjustment(elem, dimension, extra, isBorderBox, styles)
            : 0;

        // Account for unreliable border-box dimensions by comparing offset* to computed and
        // faking a content-box to get border and padding (gh-3699)
        if (isBorderBox && scrollboxSizeBuggy) {
          subtract -= Math.ceil(
            elem["offset" + dimension[0].toUpperCase() + dimension.slice(1)] -
              parseFloat(styles[dimension]) -
              boxModelAdjustment(elem, dimension, "border", false, styles) -
              0.5
          );
        }

        // Convert to pixels if value adjustment is needed
        if (
          subtract &&
          (matches = rcssNum.exec(value)) &&
          (matches[3] || "px") !== "px"
        ) {
          elem.style[dimension] = value;
          value = jQuery.css(elem, dimension);
        }

        return setPositiveNumber(elem, value, subtract);
      },
    };
  });

  jQuery.cssHooks.marginLeft = addGetHookIf(
    support.reliableMarginLeft,
    function (elem, computed) {
      if (computed) {
        return (
          (parseFloat(curCSS(elem, "marginLeft")) ||
            elem.getBoundingClientRect().left -
              swap(elem, { marginLeft: 0 }, function () {
                return elem.getBoundingClientRect().left;
              })) + "px"
        );
      }
    }
  );

  // These hooks are used by animate to expand properties
  jQuery.each(
    {
      margin: "",
      padding: "",
      border: "Width",
    },
    function (prefix, suffix) {
      jQuery.cssHooks[prefix + suffix] = {
        expand: function (value) {
          var i = 0,
            expanded = {},
            // Assumes a single number if not a string
            parts = typeof value === "string" ? value.split(" ") : [value];

          for (; i < 4; i++) {
            expanded[prefix + cssExpand[i] + suffix] =
              parts[i] || parts[i - 2] || parts[0];
          }

          return expanded;
        },
      };

      if (prefix !== "margin") {
        jQuery.cssHooks[prefix + suffix].set = setPositiveNumber;
      }
    }
  );

  jQuery.fn.extend({
    css: function (name, value) {
      return access(
        this,
        function (elem, name, value) {
          var styles,
            len,
            map = {},
            i = 0;

          if (Array.isArray(name)) {
            styles = getStyles(elem);
            len = name.length;

            for (; i < len; i++) {
              map[name[i]] = jQuery.css(elem, name[i], false, styles);
            }

            return map;
          }

          return value !== undefined
            ? jQuery.style(elem, name, value)
            : jQuery.css(elem, name);
        },
        name,
        value,
        arguments.length > 1
      );
    },
  });

  function Tween(elem, options, prop, end, easing) {
    return new Tween.prototype.init(elem, options, prop, end, easing);
  }
  jQuery.Tween = Tween;

  Tween.prototype = {
    constructor: Tween,
    init: function (elem, options, prop, end, easing, unit) {
      this.elem = elem;
      this.prop = prop;
      this.easing = easing || jQuery.easing._default;
      this.options = options;
      this.start = this.now = this.cur();
      this.end = end;
      this.unit = unit || (jQuery.cssNumber[prop] ? "" : "px");
    },
    cur: function () {
      var hooks = Tween.propHooks[this.prop];

      return hooks && hooks.get
        ? hooks.get(this)
        : Tween.propHooks._default.get(this);
    },
    run: function (percent) {
      var eased,
        hooks = Tween.propHooks[this.prop];

      if (this.options.duration) {
        this.pos = eased = jQuery.easing[this.easing](
          percent,
          this.options.duration * percent,
          0,
          1,
          this.options.duration
        );
      } else {
        this.pos = eased = percent;
      }
      this.now = (this.end - this.start) * eased + this.start;

      if (this.options.step) {
        this.options.step.call(this.elem, this.now, this);
      }

      if (hooks && hooks.set) {
        hooks.set(this);
      } else {
        Tween.propHooks._default.set(this);
      }
      return this;
    },
  };

  Tween.prototype.init.prototype = Tween.prototype;

  Tween.propHooks = {
    _default: {
      get: function (tween) {
        var result;

        // Use a property on the element directly when it is not a DOM element,
        // or when there is no matching style property that exists.
        if (
          tween.elem.nodeType !== 1 ||
          (tween.elem[tween.prop] != null &&
            tween.elem.style[tween.prop] == null)
        ) {
          return tween.elem[tween.prop];
        }

        // Passing an empty string as a 3rd parameter to .css will automatically
        // attempt a parseFloat and fallback to a string if the parse fails.
        // Simple values such as "10px" are parsed to Float;
        // complex values such as "rotate(1rad)" are returned as-is.
        result = jQuery.css(tween.elem, tween.prop, "");

        // Empty strings, null, undefined and "auto" are converted to 0.
        return !result || result === "auto" ? 0 : result;
      },
      set: function (tween) {
        // Use step hook for back compat.
        // Use cssHook if its there.
        // Use .style if available and use plain properties where available.
        if (jQuery.fx.step[tween.prop]) {
          jQuery.fx.step[tween.prop](tween);
        } else if (
          tween.elem.nodeType === 1 &&
          (jQuery.cssHooks[tween.prop] ||
            tween.elem.style[finalPropName(tween.prop)] != null)
        ) {
          jQuery.style(tween.elem, tween.prop, tween.now + tween.unit);
        } else {
          tween.elem[tween.prop] = tween.now;
        }
      },
    },
  };

  // Support: IE <=9 only
  // Panic based approach to setting things on disconnected nodes
  Tween.propHooks.scrollTop = Tween.propHooks.scrollLeft = {
    set: function (tween) {
      if (tween.elem.nodeType && tween.elem.parentNode) {
        tween.elem[tween.prop] = tween.now;
      }
    },
  };

  jQuery.easing = {
    linear: function (p) {
      return p;
    },
    swing: function (p) {
      return 0.5 - Math.cos(p * Math.PI) / 2;
    },
    _default: "swing",
  };

  jQuery.fx = Tween.prototype.init;

  // Back compat <1.8 extension point
  jQuery.fx.step = {};

  var fxNow,
    inProgress,
    rfxtypes = /^(?:toggle|show|hide)$/,
    rrun = /queueHooks$/;

  function schedule() {
    if (inProgress) {
      if (document.hidden === false && window.requestAnimationFrame) {
        window.requestAnimationFrame(schedule);
      } else {
        window.setTimeout(schedule, jQuery.fx.interval);
      }

      jQuery.fx.tick();
    }
  }

  // Animations created synchronously will run synchronously
  function createFxNow() {
    window.setTimeout(function () {
      fxNow = undefined;
    });
    return (fxNow = Date.now());
  }

  // Generate parameters to create a standard animation
  function genFx(type, includeWidth) {
    var which,
      i = 0,
      attrs = { height: type };

    // If we include width, step value is 1 to do all cssExpand values,
    // otherwise step value is 2 to skip over Left and Right
    includeWidth = includeWidth ? 1 : 0;
    for (; i < 4; i += 2 - includeWidth) {
      which = cssExpand[i];
      attrs["margin" + which] = attrs["padding" + which] = type;
    }

    if (includeWidth) {
      attrs.opacity = attrs.width = type;
    }

    return attrs;
  }

  function createTween(value, prop, animation) {
    var tween,
      collection = (Animation.tweeners[prop] || []).concat(
        Animation.tweeners["*"]
      ),
      index = 0,
      length = collection.length;
    for (; index < length; index++) {
      if ((tween = collection[index].call(animation, prop, value))) {
        // We're done with this property
        return tween;
      }
    }
  }

  function defaultPrefilter(elem, props, opts) {
    var prop,
      value,
      toggle,
      hooks,
      oldfire,
      propTween,
      restoreDisplay,
      display,
      isBox = "width" in props || "height" in props,
      anim = this,
      orig = {},
      style = elem.style,
      hidden = elem.nodeType && isHiddenWithinTree(elem),
      dataShow = dataPriv.get(elem, "fxshow");

    // Queue-skipping animations hijack the fx hooks
    if (!opts.queue) {
      hooks = jQuery._queueHooks(elem, "fx");
      if (hooks.unqueued == null) {
        hooks.unqueued = 0;
        oldfire = hooks.empty.fire;
        hooks.empty.fire = function () {
          if (!hooks.unqueued) {
            oldfire();
          }
        };
      }
      hooks.unqueued++;

      anim.always(function () {
        // Ensure the complete handler is called before this completes
        anim.always(function () {
          hooks.unqueued--;
          if (!jQuery.queue(elem, "fx").length) {
            hooks.empty.fire();
          }
        });
      });
    }

    // Detect show/hide animations
    for (prop in props) {
      value = props[prop];
      if (rfxtypes.test(value)) {
        delete props[prop];
        toggle = toggle || value === "toggle";
        if (value === (hidden ? "hide" : "show")) {
          // Pretend to be hidden if this is a "show" and
          // there is still data from a stopped show/hide
          if (value === "show" && dataShow && dataShow[prop] !== undefined) {
            hidden = true;

            // Ignore all other no-op show/hide data
          } else {
            continue;
          }
        }
        orig[prop] = (dataShow && dataShow[prop]) || jQuery.style(elem, prop);
      }
    }

    // Bail out if this is a no-op like .hide().hide()
    propTween = !jQuery.isEmptyObject(props);
    if (!propTween && jQuery.isEmptyObject(orig)) {
      return;
    }

    // Restrict "overflow" and "display" styles during box animations
    if (isBox && elem.nodeType === 1) {
      // Support: IE <=9 - 11, Edge 12 - 15
      // Record all 3 overflow attributes because IE does not infer the shorthand
      // from identically-valued overflowX and overflowY and Edge just mirrors
      // the overflowX value there.
      opts.overflow = [style.overflow, style.overflowX, style.overflowY];

      // Identify a display type, preferring old show/hide data over the CSS cascade
      restoreDisplay = dataShow && dataShow.display;
      if (restoreDisplay == null) {
        restoreDisplay = dataPriv.get(elem, "display");
      }
      display = jQuery.css(elem, "display");
      if (display === "none") {
        if (restoreDisplay) {
          display = restoreDisplay;
        } else {
          // Get nonempty value(s) by temporarily forcing visibility
          showHide([elem], true);
          restoreDisplay = elem.style.display || restoreDisplay;
          display = jQuery.css(elem, "display");
          showHide([elem]);
        }
      }

      // Animate inline elements as inline-block
      if (
        display === "inline" ||
        (display === "inline-block" && restoreDisplay != null)
      ) {
        if (jQuery.css(elem, "float") === "none") {
          // Restore the original display value at the end of pure show/hide animations
          if (!propTween) {
            anim.done(function () {
              style.display = restoreDisplay;
            });
            if (restoreDisplay == null) {
              display = style.display;
              restoreDisplay = display === "none" ? "" : display;
            }
          }
          style.display = "inline-block";
        }
      }
    }

    if (opts.overflow) {
      style.overflow = "hidden";
      anim.always(function () {
        style.overflow = opts.overflow[0];
        style.overflowX = opts.overflow[1];
        style.overflowY = opts.overflow[2];
      });
    }

    // Implement show/hide animations
    propTween = false;
    for (prop in orig) {
      // General show/hide setup for this element animation
      if (!propTween) {
        if (dataShow) {
          if ("hidden" in dataShow) {
            hidden = dataShow.hidden;
          }
        } else {
          dataShow = dataPriv.access(elem, "fxshow", {
            display: restoreDisplay,
          });
        }

        // Store hidden/visible for toggle so `.stop().toggle()` "reverses"
        if (toggle) {
          dataShow.hidden = !hidden;
        }

        // Show elements before animating them
        if (hidden) {
          showHide([elem], true);
        }

        /* eslint-disable no-loop-func */

        anim.done(function () {
          /* eslint-enable no-loop-func */

          // The final step of a "hide" animation is actually hiding the element
          if (!hidden) {
            showHide([elem]);
          }
          dataPriv.remove(elem, "fxshow");
          for (prop in orig) {
            jQuery.style(elem, prop, orig[prop]);
          }
        });
      }

      // Per-property setup
      propTween = createTween(hidden ? dataShow[prop] : 0, prop, anim);
      if (!(prop in dataShow)) {
        dataShow[prop] = propTween.start;
        if (hidden) {
          propTween.end = propTween.start;
          propTween.start = 0;
        }
      }
    }
  }

  function propFilter(props, specialEasing) {
    var index, name, easing, value, hooks;

    // camelCase, specialEasing and expand cssHook pass
    for (index in props) {
      name = camelCase(index);
      easing = specialEasing[name];
      value = props[index];
      if (Array.isArray(value)) {
        easing = value[1];
        value = props[index] = value[0];
      }

      if (index !== name) {
        props[name] = value;
        delete props[index];
      }

      hooks = jQuery.cssHooks[name];
      if (hooks && "expand" in hooks) {
        value = hooks.expand(value);
        delete props[name];

        // Not quite $.extend, this won't overwrite existing keys.
        // Reusing 'index' because we have the correct "name"
        for (index in value) {
          if (!(index in props)) {
            props[index] = value[index];
            specialEasing[index] = easing;
          }
        }
      } else {
        specialEasing[name] = easing;
      }
    }
  }

  function Animation(elem, properties, options) {
    var result,
      stopped,
      index = 0,
      length = Animation.prefilters.length,
      deferred = jQuery.Deferred().always(function () {
        // Don't match elem in the :animated selector
        delete tick.elem;
      }),
      tick = function () {
        if (stopped) {
          return false;
        }
        var currentTime = fxNow || createFxNow(),
          remaining = Math.max(
            0,
            animation.startTime + animation.duration - currentTime
          ),
          // Support: Android 2.3 only
          // Archaic crash bug won't allow us to use `1 - ( 0.5 || 0 )` (#12497)
          temp = remaining / animation.duration || 0,
          percent = 1 - temp,
          index = 0,
          length = animation.tweens.length;

        for (; index < length; index++) {
          animation.tweens[index].run(percent);
        }

        deferred.notifyWith(elem, [animation, percent, remaining]);

        // If there's more to do, yield
        if (percent < 1 && length) {
          return remaining;
        }

        // If this was an empty animation, synthesize a final progress notification
        if (!length) {
          deferred.notifyWith(elem, [animation, 1, 0]);
        }

        // Resolve the animation and report its conclusion
        deferred.resolveWith(elem, [animation]);
        return false;
      },
      animation = deferred.promise({
        elem: elem,
        props: jQuery.extend({}, properties),
        opts: jQuery.extend(
          true,
          {
            specialEasing: {},
            easing: jQuery.easing._default,
          },
          options
        ),
        originalProperties: properties,
        originalOptions: options,
        startTime: fxNow || createFxNow(),
        duration: options.duration,
        tweens: [],
        createTween: function (prop, end) {
          var tween = jQuery.Tween(
            elem,
            animation.opts,
            prop,
            end,
            animation.opts.specialEasing[prop] || animation.opts.easing
          );
          animation.tweens.push(tween);
          return tween;
        },
        stop: function (gotoEnd) {
          var index = 0,
            // If we are going to the end, we want to run all the tweens
            // otherwise we skip this part
            length = gotoEnd ? animation.tweens.length : 0;
          if (stopped) {
            return this;
          }
          stopped = true;
          for (; index < length; index++) {
            animation.tweens[index].run(1);
          }

          // Resolve when we played the last frame; otherwise, reject
          if (gotoEnd) {
            deferred.notifyWith(elem, [animation, 1, 0]);
            deferred.resolveWith(elem, [animation, gotoEnd]);
          } else {
            deferred.rejectWith(elem, [animation, gotoEnd]);
          }
          return this;
        },
      }),
      props = animation.props;

    propFilter(props, animation.opts.specialEasing);

    for (; index < length; index++) {
      result = Animation.prefilters[index].call(
        animation,
        elem,
        props,
        animation.opts
      );
      if (result) {
        if (isFunction(result.stop)) {
          jQuery._queueHooks(
            animation.elem,
            animation.opts.queue
          ).stop = result.stop.bind(result);
        }
        return result;
      }
    }

    jQuery.map(props, createTween, animation);

    if (isFunction(animation.opts.start)) {
      animation.opts.start.call(elem, animation);
    }

    // Attach callbacks from options
    animation
      .progress(animation.opts.progress)
      .done(animation.opts.done, animation.opts.complete)
      .fail(animation.opts.fail)
      .always(animation.opts.always);

    jQuery.fx.timer(
      jQuery.extend(tick, {
        elem: elem,
        anim: animation,
        queue: animation.opts.queue,
      })
    );

    return animation;
  }

  jQuery.Animation = jQuery.extend(Animation, {
    tweeners: {
      "*": [
        function (prop, value) {
          var tween = this.createTween(prop, value);
          adjustCSS(tween.elem, prop, rcssNum.exec(value), tween);
          return tween;
        },
      ],
    },

    tweener: function (props, callback) {
      if (isFunction(props)) {
        callback = props;
        props = ["*"];
      } else {
        props = props.match(rnothtmlwhite);
      }

      var prop,
        index = 0,
        length = props.length;

      for (; index < length; index++) {
        prop = props[index];
        Animation.tweeners[prop] = Animation.tweeners[prop] || [];
        Animation.tweeners[prop].unshift(callback);
      }
    },

    prefilters: [defaultPrefilter],

    prefilter: function (callback, prepend) {
      if (prepend) {
        Animation.prefilters.unshift(callback);
      } else {
        Animation.prefilters.push(callback);
      }
    },
  });

  jQuery.speed = function (speed, easing, fn) {
    var opt =
      speed && typeof speed === "object"
        ? jQuery.extend({}, speed)
        : {
            complete: fn || (!fn && easing) || (isFunction(speed) && speed),
            duration: speed,
            easing: (fn && easing) || (easing && !isFunction(easing) && easing),
          };

    // Go to the end state if fx are off
    if (jQuery.fx.off) {
      opt.duration = 0;
    } else {
      if (typeof opt.duration !== "number") {
        if (opt.duration in jQuery.fx.speeds) {
          opt.duration = jQuery.fx.speeds[opt.duration];
        } else {
          opt.duration = jQuery.fx.speeds._default;
        }
      }
    }

    // Normalize opt.queue - true/undefined/null -> "fx"
    if (opt.queue == null || opt.queue === true) {
      opt.queue = "fx";
    }

    // Queueing
    opt.old = opt.complete;

    opt.complete = function () {
      if (isFunction(opt.old)) {
        opt.old.call(this);
      }

      if (opt.queue) {
        jQuery.dequeue(this, opt.queue);
      }
    };

    return opt;
  };

  jQuery.fn.extend({
    fadeTo: function (speed, to, easing, callback) {
      // Show any hidden elements after setting opacity to 0
      return (
        this.filter(isHiddenWithinTree)
          .css("opacity", 0)
          .show()

          // Animate to the value specified
          .end()
          .animate({ opacity: to }, speed, easing, callback)
      );
    },
    animate: function (prop, speed, easing, callback) {
      var empty = jQuery.isEmptyObject(prop),
        optall = jQuery.speed(speed, easing, callback),
        doAnimation = function () {
          // Operate on a copy of prop so per-property easing won't be lost
          var anim = Animation(this, jQuery.extend({}, prop), optall);

          // Empty animations, or finishing resolves immediately
          if (empty || dataPriv.get(this, "finish")) {
            anim.stop(true);
          }
        };

      doAnimation.finish = doAnimation;

      return empty || optall.queue === false
        ? this.each(doAnimation)
        : this.queue(optall.queue, doAnimation);
    },
    stop: function (type, clearQueue, gotoEnd) {
      var stopQueue = function (hooks) {
        var stop = hooks.stop;
        delete hooks.stop;
        stop(gotoEnd);
      };

      if (typeof type !== "string") {
        gotoEnd = clearQueue;
        clearQueue = type;
        type = undefined;
      }
      if (clearQueue) {
        this.queue(type || "fx", []);
      }

      return this.each(function () {
        var dequeue = true,
          index = type != null && type + "queueHooks",
          timers = jQuery.timers,
          data = dataPriv.get(this);

        if (index) {
          if (data[index] && data[index].stop) {
            stopQueue(data[index]);
          }
        } else {
          for (index in data) {
            if (data[index] && data[index].stop && rrun.test(index)) {
              stopQueue(data[index]);
            }
          }
        }

        for (index = timers.length; index--; ) {
          if (
            timers[index].elem === this &&
            (type == null || timers[index].queue === type)
          ) {
            timers[index].anim.stop(gotoEnd);
            dequeue = false;
            timers.splice(index, 1);
          }
        }

        // Start the next in the queue if the last step wasn't forced.
        // Timers currently will call their complete callbacks, which
        // will dequeue but only if they were gotoEnd.
        if (dequeue || !gotoEnd) {
          jQuery.dequeue(this, type);
        }
      });
    },
    finish: function (type) {
      if (type !== false) {
        type = type || "fx";
      }
      return this.each(function () {
        var index,
          data = dataPriv.get(this),
          queue = data[type + "queue"],
          hooks = data[type + "queueHooks"],
          timers = jQuery.timers,
          length = queue ? queue.length : 0;

        // Enable finishing flag on private data
        data.finish = true;

        // Empty the queue first
        jQuery.queue(this, type, []);

        if (hooks && hooks.stop) {
          hooks.stop.call(this, true);
        }

        // Look for any active animations, and finish them
        for (index = timers.length; index--; ) {
          if (timers[index].elem === this && timers[index].queue === type) {
            timers[index].anim.stop(true);
            timers.splice(index, 1);
          }
        }

        // Look for any animations in the old queue and finish them
        for (index = 0; index < length; index++) {
          if (queue[index] && queue[index].finish) {
            queue[index].finish.call(this);
          }
        }

        // Turn off finishing flag
        delete data.finish;
      });
    },
  });

  jQuery.each(["toggle", "show", "hide"], function (_i, name) {
    var cssFn = jQuery.fn[name];
    jQuery.fn[name] = function (speed, easing, callback) {
      return speed == null || typeof speed === "boolean"
        ? cssFn.apply(this, arguments)
        : this.animate(genFx(name, true), speed, easing, callback);
    };
  });

  // Generate shortcuts for custom animations
  jQuery.each(
    {
      slideDown: genFx("show"),
      slideUp: genFx("hide"),
      slideToggle: genFx("toggle"),
      fadeIn: { opacity: "show" },
      fadeOut: { opacity: "hide" },
      fadeToggle: { opacity: "toggle" },
    },
    function (name, props) {
      jQuery.fn[name] = function (speed, easing, callback) {
        return this.animate(props, speed, easing, callback);
      };
    }
  );

  jQuery.timers = [];
  jQuery.fx.tick = function () {
    var timer,
      i = 0,
      timers = jQuery.timers;

    fxNow = Date.now();

    for (; i < timers.length; i++) {
      timer = timers[i];

      // Run the timer and safely remove it when done (allowing for external removal)
      if (!timer() && timers[i] === timer) {
        timers.splice(i--, 1);
      }
    }

    if (!timers.length) {
      jQuery.fx.stop();
    }
    fxNow = undefined;
  };

  jQuery.fx.timer = function (timer) {
    jQuery.timers.push(timer);
    jQuery.fx.start();
  };

  jQuery.fx.interval = 13;
  jQuery.fx.start = function () {
    if (inProgress) {
      return;
    }

    inProgress = true;
    schedule();
  };

  jQuery.fx.stop = function () {
    inProgress = null;
  };

  jQuery.fx.speeds = {
    slow: 600,
    fast: 200,

    // Default speed
    _default: 400,
  };

  // Based off of the plugin by Clint Helfers, with permission.
  // https://web.archive.org/web/20100324014747/http://blindsignals.com/index.php/2009/07/jquery-delay/
  jQuery.fn.delay = function (time, type) {
    time = jQuery.fx ? jQuery.fx.speeds[time] || time : time;
    type = type || "fx";

    return this.queue(type, function (next, hooks) {
      var timeout = window.setTimeout(next, time);
      hooks.stop = function () {
        window.clearTimeout(timeout);
      };
    });
  };

  (function () {
    var input = document.createElement("input"),
      select = document.createElement("select"),
      opt = select.appendChild(document.createElement("option"));

    input.type = "checkbox";

    // Support: Android <=4.3 only
    // Default value for a checkbox should be "on"
    support.checkOn = input.value !== "";

    // Support: IE <=11 only
    // Must access selectedIndex to make default options select
    support.optSelected = opt.selected;

    // Support: IE <=11 only
    // An input loses its value after becoming a radio
    input = document.createElement("input");
    input.value = "t";
    input.type = "radio";
    support.radioValue = input.value === "t";
  })();

  var boolHook,
    attrHandle = jQuery.expr.attrHandle;

  jQuery.fn.extend({
    attr: function (name, value) {
      return access(this, jQuery.attr, name, value, arguments.length > 1);
    },

    removeAttr: function (name) {
      return this.each(function () {
        jQuery.removeAttr(this, name);
      });
    },
  });

  jQuery.extend({
    attr: function (elem, name, value) {
      var ret,
        hooks,
        nType = elem.nodeType;

      // Don't get/set attributes on text, comment and attribute nodes
      if (nType === 3 || nType === 8 || nType === 2) {
        return;
      }

      // Fallback to prop when attributes are not supported
      if (typeof elem.getAttribute === "undefined") {
        return jQuery.prop(elem, name, value);
      }

      // Attribute hooks are determined by the lowercase version
      // Grab necessary hook if one is defined
      if (nType !== 1 || !jQuery.isXMLDoc(elem)) {
        hooks =
          jQuery.attrHooks[name.toLowerCase()] ||
          (jQuery.expr.match.bool.test(name) ? boolHook : undefined);
      }

      if (value !== undefined) {
        if (value === null) {
          jQuery.removeAttr(elem, name);
          return;
        }

        if (
          hooks &&
          "set" in hooks &&
          (ret = hooks.set(elem, value, name)) !== undefined
        ) {
          return ret;
        }

        elem.setAttribute(name, value + "");
        return value;
      }

      if (hooks && "get" in hooks && (ret = hooks.get(elem, name)) !== null) {
        return ret;
      }

      ret = jQuery.find.attr(elem, name);

      // Non-existent attributes return null, we normalize to undefined
      return ret == null ? undefined : ret;
    },

    attrHooks: {
      type: {
        set: function (elem, value) {
          if (
            !support.radioValue &&
            value === "radio" &&
            nodeName(elem, "input")
          ) {
            var val = elem.value;
            elem.setAttribute("type", value);
            if (val) {
              elem.value = val;
            }
            return value;
          }
        },
      },
    },

    removeAttr: function (elem, value) {
      var name,
        i = 0,
        // Attribute names can contain non-HTML whitespace characters
        // https://html.spec.whatwg.org/multipage/syntax.html#attributes-2
        attrNames = value && value.match(rnothtmlwhite);

      if (attrNames && elem.nodeType === 1) {
        while ((name = attrNames[i++])) {
          elem.removeAttribute(name);
        }
      }
    },
  });

  // Hooks for boolean attributes
  boolHook = {
    set: function (elem, value, name) {
      if (value === false) {
        // Remove boolean attributes when set to false
        jQuery.removeAttr(elem, name);
      } else {
        elem.setAttribute(name, name);
      }
      return name;
    },
  };

  jQuery.each(jQuery.expr.match.bool.source.match(/\w+/g), function (_i, name) {
    var getter = attrHandle[name] || jQuery.find.attr;

    attrHandle[name] = function (elem, name, isXML) {
      var ret,
        handle,
        lowercaseName = name.toLowerCase();

      if (!isXML) {
        // Avoid an infinite loop by temporarily removing this function from the getter
        handle = attrHandle[lowercaseName];
        attrHandle[lowercaseName] = ret;
        ret = getter(elem, name, isXML) != null ? lowercaseName : null;
        attrHandle[lowercaseName] = handle;
      }
      return ret;
    };
  });

  var rfocusable = /^(?:input|select|textarea|button)$/i,
    rclickable = /^(?:a|area)$/i;

  jQuery.fn.extend({
    prop: function (name, value) {
      return access(this, jQuery.prop, name, value, arguments.length > 1);
    },

    removeProp: function (name) {
      return this.each(function () {
        delete this[jQuery.propFix[name] || name];
      });
    },
  });

  jQuery.extend({
    prop: function (elem, name, value) {
      var ret,
        hooks,
        nType = elem.nodeType;

      // Don't get/set properties on text, comment and attribute nodes
      if (nType === 3 || nType === 8 || nType === 2) {
        return;
      }

      if (nType !== 1 || !jQuery.isXMLDoc(elem)) {
        // Fix name and attach hooks
        name = jQuery.propFix[name] || name;
        hooks = jQuery.propHooks[name];
      }

      if (value !== undefined) {
        if (
          hooks &&
          "set" in hooks &&
          (ret = hooks.set(elem, value, name)) !== undefined
        ) {
          return ret;
        }

        return (elem[name] = value);
      }

      if (hooks && "get" in hooks && (ret = hooks.get(elem, name)) !== null) {
        return ret;
      }

      return elem[name];
    },

    propHooks: {
      tabIndex: {
        get: function (elem) {
          // Support: IE <=9 - 11 only
          // elem.tabIndex doesn't always return the
          // correct value when it hasn't been explicitly set
          // https://web.archive.org/web/20141116233347/http://fluidproject.org/blog/2008/01/09/getting-setting-and-removing-tabindex-values-with-javascript/
          // Use proper attribute retrieval(#12072)
          var tabindex = jQuery.find.attr(elem, "tabindex");

          if (tabindex) {
            return parseInt(tabindex, 10);
          }

          if (
            rfocusable.test(elem.nodeName) ||
            (rclickable.test(elem.nodeName) && elem.href)
          ) {
            return 0;
          }

          return -1;
        },
      },
    },

    propFix: {
      for: "htmlFor",
      class: "className",
    },
  });

  // Support: IE <=11 only
  // Accessing the selectedIndex property
  // forces the browser to respect setting selected
  // on the option
  // The getter ensures a default option is selected
  // when in an optgroup
  // eslint rule "no-unused-expressions" is disabled for this code
  // since it considers such accessions noop
  if (!support.optSelected) {
    jQuery.propHooks.selected = {
      get: function (elem) {
        /* eslint no-unused-expressions: "off" */

        var parent = elem.parentNode;
        if (parent && parent.parentNode) {
          parent.parentNode.selectedIndex;
        }
        return null;
      },
      set: function (elem) {
        /* eslint no-unused-expressions: "off" */

        var parent = elem.parentNode;
        if (parent) {
          parent.selectedIndex;

          if (parent.parentNode) {
            parent.parentNode.selectedIndex;
          }
        }
      },
    };
  }

  jQuery.each(
    [
      "tabIndex",
      "readOnly",
      "maxLength",
      "cellSpacing",
      "cellPadding",
      "rowSpan",
      "colSpan",
      "useMap",
      "frameBorder",
      "contentEditable",
    ],
    function () {
      jQuery.propFix[this.toLowerCase()] = this;
    }
  );

  // Strip and collapse whitespace according to HTML spec
  // https://infra.spec.whatwg.org/#strip-and-collapse-ascii-whitespace
  function stripAndCollapse(value) {
    var tokens = value.match(rnothtmlwhite) || [];
    return tokens.join(" ");
  }

  function getClass(elem) {
    return (elem.getAttribute && elem.getAttribute("class")) || "";
  }

  function classesToArray(value) {
    if (Array.isArray(value)) {
      return value;
    }
    if (typeof value === "string") {
      return value.match(rnothtmlwhite) || [];
    }
    return [];
  }

  jQuery.fn.extend({
    addClass: function (value) {
      var classes,
        elem,
        cur,
        curValue,
        clazz,
        j,
        finalValue,
        i = 0;

      if (isFunction(value)) {
        return this.each(function (j) {
          jQuery(this).addClass(value.call(this, j, getClass(this)));
        });
      }

      classes = classesToArray(value);

      if (classes.length) {
        while ((elem = this[i++])) {
          curValue = getClass(elem);
          cur = elem.nodeType === 1 && " " + stripAndCollapse(curValue) + " ";

          if (cur) {
            j = 0;
            while ((clazz = classes[j++])) {
              if (cur.indexOf(" " + clazz + " ") < 0) {
                cur += clazz + " ";
              }
            }

            // Only assign if different to avoid unneeded rendering.
            finalValue = stripAndCollapse(cur);
            if (curValue !== finalValue) {
              elem.setAttribute("class", finalValue);
            }
          }
        }
      }

      return this;
    },

    removeClass: function (value) {
      var classes,
        elem,
        cur,
        curValue,
        clazz,
        j,
        finalValue,
        i = 0;

      if (isFunction(value)) {
        return this.each(function (j) {
          jQuery(this).removeClass(value.call(this, j, getClass(this)));
        });
      }

      if (!arguments.length) {
        return this.attr("class", "");
      }

      classes = classesToArray(value);

      if (classes.length) {
        while ((elem = this[i++])) {
          curValue = getClass(elem);

          // This expression is here for better compressibility (see addClass)
          cur = elem.nodeType === 1 && " " + stripAndCollapse(curValue) + " ";

          if (cur) {
            j = 0;
            while ((clazz = classes[j++])) {
              // Remove *all* instances
              while (cur.indexOf(" " + clazz + " ") > -1) {
                cur = cur.replace(" " + clazz + " ", " ");
              }
            }

            // Only assign if different to avoid unneeded rendering.
            finalValue = stripAndCollapse(cur);
            if (curValue !== finalValue) {
              elem.setAttribute("class", finalValue);
            }
          }
        }
      }

      return this;
    },

    toggleClass: function (value, stateVal) {
      var type = typeof value,
        isValidValue = type === "string" || Array.isArray(value);

      if (typeof stateVal === "boolean" && isValidValue) {
        return stateVal ? this.addClass(value) : this.removeClass(value);
      }

      if (isFunction(value)) {
        return this.each(function (i) {
          jQuery(this).toggleClass(
            value.call(this, i, getClass(this), stateVal),
            stateVal
          );
        });
      }

      return this.each(function () {
        var className, i, self, classNames;

        if (isValidValue) {
          // Toggle individual class names
          i = 0;
          self = jQuery(this);
          classNames = classesToArray(value);

          while ((className = classNames[i++])) {
            // Check each className given, space separated list
            if (self.hasClass(className)) {
              self.removeClass(className);
            } else {
              self.addClass(className);
            }
          }

          // Toggle whole class name
        } else if (value === undefined || type === "boolean") {
          className = getClass(this);
          if (className) {
            // Store className if set
            dataPriv.set(this, "__className__", className);
          }

          // If the element has a class name or if we're passed `false`,
          // then remove the whole classname (if there was one, the above saved it).
          // Otherwise bring back whatever was previously saved (if anything),
          // falling back to the empty string if nothing was stored.
          if (this.setAttribute) {
            this.setAttribute(
              "class",
              className || value === false
                ? ""
                : dataPriv.get(this, "__className__") || ""
            );
          }
        }
      });
    },

    hasClass: function (selector) {
      var className,
        elem,
        i = 0;

      className = " " + selector + " ";
      while ((elem = this[i++])) {
        if (
          elem.nodeType === 1 &&
          (" " + stripAndCollapse(getClass(elem)) + " ").indexOf(className) > -1
        ) {
          return true;
        }
      }

      return false;
    },
  });

  var rreturn = /\r/g;

  jQuery.fn.extend({
    val: function (value) {
      var hooks,
        ret,
        valueIsFunction,
        elem = this[0];

      if (!arguments.length) {
        if (elem) {
          hooks =
            jQuery.valHooks[elem.type] ||
            jQuery.valHooks[elem.nodeName.toLowerCase()];

          if (
            hooks &&
            "get" in hooks &&
            (ret = hooks.get(elem, "value")) !== undefined
          ) {
            return ret;
          }

          ret = elem.value;

          // Handle most common string cases
          if (typeof ret === "string") {
            return ret.replace(rreturn, "");
          }

          // Handle cases where value is null/undef or number
          return ret == null ? "" : ret;
        }

        return;
      }

      valueIsFunction = isFunction(value);

      return this.each(function (i) {
        var val;

        if (this.nodeType !== 1) {
          return;
        }

        if (valueIsFunction) {
          val = value.call(this, i, jQuery(this).val());
        } else {
          val = value;
        }

        // Treat null/undefined as ""; convert numbers to string
        if (val == null) {
          val = "";
        } else if (typeof val === "number") {
          val += "";
        } else if (Array.isArray(val)) {
          val = jQuery.map(val, function (value) {
            return value == null ? "" : value + "";
          });
        }

        hooks =
          jQuery.valHooks[this.type] ||
          jQuery.valHooks[this.nodeName.toLowerCase()];

        // If set returns undefined, fall back to normal setting
        if (
          !hooks ||
          !("set" in hooks) ||
          hooks.set(this, val, "value") === undefined
        ) {
          this.value = val;
        }
      });
    },
  });

  jQuery.extend({
    valHooks: {
      option: {
        get: function (elem) {
          var val = jQuery.find.attr(elem, "value");
          return val != null
            ? val
            : // Support: IE <=10 - 11 only
              // option.text throws exceptions (#14686, #14858)
              // Strip and collapse whitespace
              // https://html.spec.whatwg.org/#strip-and-collapse-whitespace
              stripAndCollapse(jQuery.text(elem));
        },
      },
      select: {
        get: function (elem) {
          var value,
            option,
            i,
            options = elem.options,
            index = elem.selectedIndex,
            one = elem.type === "select-one",
            values = one ? null : [],
            max = one ? index + 1 : options.length;

          if (index < 0) {
            i = max;
          } else {
            i = one ? index : 0;
          }

          // Loop through all the selected options
          for (; i < max; i++) {
            option = options[i];

            // Support: IE <=9 only
            // IE8-9 doesn't update selected after form reset (#2551)
            if (
              (option.selected || i === index) &&
              // Don't return options that are disabled or in a disabled optgroup
              !option.disabled &&
              (!option.parentNode.disabled ||
                !nodeName(option.parentNode, "optgroup"))
            ) {
              // Get the specific value for the option
              value = jQuery(option).val();

              // We don't need an array for one selects
              if (one) {
                return value;
              }

              // Multi-Selects return an array
              values.push(value);
            }
          }

          return values;
        },

        set: function (elem, value) {
          var optionSet,
            option,
            options = elem.options,
            values = jQuery.makeArray(value),
            i = options.length;

          while (i--) {
            option = options[i];

            /* eslint-disable no-cond-assign */

            if (
              (option.selected =
                jQuery.inArray(jQuery.valHooks.option.get(option), values) > -1)
            ) {
              optionSet = true;
            }

            /* eslint-enable no-cond-assign */
          }

          // Force browsers to behave consistently when non-matching value is set
          if (!optionSet) {
            elem.selectedIndex = -1;
          }
          return values;
        },
      },
    },
  });

  // Radios and checkboxes getter/setter
  jQuery.each(["radio", "checkbox"], function () {
    jQuery.valHooks[this] = {
      set: function (elem, value) {
        if (Array.isArray(value)) {
          return (elem.checked =
            jQuery.inArray(jQuery(elem).val(), value) > -1);
        }
      },
    };
    if (!support.checkOn) {
      jQuery.valHooks[this].get = function (elem) {
        return elem.getAttribute("value") === null ? "on" : elem.value;
      };
    }
  });

  // Return jQuery for attributes-only inclusion

  support.focusin = "onfocusin" in window;

  var rfocusMorph = /^(?:focusinfocus|focusoutblur)$/,
    stopPropagationCallback = function (e) {
      e.stopPropagation();
    };

  jQuery.extend(jQuery.event, {
    trigger: function (event, data, elem, onlyHandlers) {
      var i,
        cur,
        tmp,
        bubbleType,
        ontype,
        handle,
        special,
        lastElement,
        eventPath = [elem || document],
        type = hasOwn.call(event, "type") ? event.type : event,
        namespaces = hasOwn.call(event, "namespace")
          ? event.namespace.split(".")
          : [];

      cur = lastElement = tmp = elem = elem || document;

      // Don't do events on text and comment nodes
      if (elem.nodeType === 3 || elem.nodeType === 8) {
        return;
      }

      // focus/blur morphs to focusin/out; ensure we're not firing them right now
      if (rfocusMorph.test(type + jQuery.event.triggered)) {
        return;
      }

      if (type.indexOf(".") > -1) {
        // Namespaced trigger; create a regexp to match event type in handle()
        namespaces = type.split(".");
        type = namespaces.shift();
        namespaces.sort();
      }
      ontype = type.indexOf(":") < 0 && "on" + type;

      // Caller can pass in a jQuery.Event object, Object, or just an event type string
      event = event[jQuery.expando]
        ? event
        : new jQuery.Event(type, typeof event === "object" && event);

      // Trigger bitmask: & 1 for native handlers; & 2 for jQuery (always true)
      event.isTrigger = onlyHandlers ? 2 : 3;
      event.namespace = namespaces.join(".");
      event.rnamespace = event.namespace
        ? new RegExp("(^|\\.)" + namespaces.join("\\.(?:.*\\.|)") + "(\\.|$)")
        : null;

      // Clean up the event in case it is being reused
      event.result = undefined;
      if (!event.target) {
        event.target = elem;
      }

      // Clone any incoming data and prepend the event, creating the handler arg list
      data = data == null ? [event] : jQuery.makeArray(data, [event]);

      // Allow special events to draw outside the lines
      special = jQuery.event.special[type] || {};
      if (
        !onlyHandlers &&
        special.trigger &&
        special.trigger.apply(elem, data) === false
      ) {
        return;
      }

      // Determine event propagation path in advance, per W3C events spec (#9951)
      // Bubble up to document, then to window; watch for a global ownerDocument var (#9724)
      if (!onlyHandlers && !special.noBubble && !isWindow(elem)) {
        bubbleType = special.delegateType || type;
        if (!rfocusMorph.test(bubbleType + type)) {
          cur = cur.parentNode;
        }
        for (; cur; cur = cur.parentNode) {
          eventPath.push(cur);
          tmp = cur;
        }

        // Only add window if we got to document (e.g., not plain obj or detached DOM)
        if (tmp === (elem.ownerDocument || document)) {
          eventPath.push(tmp.defaultView || tmp.parentWindow || window);
        }
      }

      // Fire handlers on the event path
      i = 0;
      while ((cur = eventPath[i++]) && !event.isPropagationStopped()) {
        lastElement = cur;
        event.type = i > 1 ? bubbleType : special.bindType || type;

        // jQuery handler
        handle =
          (dataPriv.get(cur, "events") || Object.create(null))[event.type] &&
          dataPriv.get(cur, "handle");
        if (handle) {
          handle.apply(cur, data);
        }

        // Native handler
        handle = ontype && cur[ontype];
        if (handle && handle.apply && acceptData(cur)) {
          event.result = handle.apply(cur, data);
          if (event.result === false) {
            event.preventDefault();
          }
        }
      }
      event.type = type;

      // If nobody prevented the default action, do it now
      if (!onlyHandlers && !event.isDefaultPrevented()) {
        if (
          (!special._default ||
            special._default.apply(eventPath.pop(), data) === false) &&
          acceptData(elem)
        ) {
          // Call a native DOM method on the target with the same name as the event.
          // Don't do default actions on window, that's where global variables be (#6170)
          if (ontype && isFunction(elem[type]) && !isWindow(elem)) {
            // Don't re-trigger an onFOO event when we call its FOO() method
            tmp = elem[ontype];

            if (tmp) {
              elem[ontype] = null;
            }

            // Prevent re-triggering of the same event, since we already bubbled it above
            jQuery.event.triggered = type;

            if (event.isPropagationStopped()) {
              lastElement.addEventListener(type, stopPropagationCallback);
            }

            elem[type]();

            if (event.isPropagationStopped()) {
              lastElement.removeEventListener(type, stopPropagationCallback);
            }

            jQuery.event.triggered = undefined;

            if (tmp) {
              elem[ontype] = tmp;
            }
          }
        }
      }

      return event.result;
    },

    // Piggyback on a donor event to simulate a different one
    // Used only for `focus(in | out)` events
    simulate: function (type, elem, event) {
      var e = jQuery.extend(new jQuery.Event(), event, {
        type: type,
        isSimulated: true,
      });

      jQuery.event.trigger(e, null, elem);
    },
  });

  jQuery.fn.extend({
    trigger: function (type, data) {
      return this.each(function () {
        jQuery.event.trigger(type, data, this);
      });
    },
    triggerHandler: function (type, data) {
      var elem = this[0];
      if (elem) {
        return jQuery.event.trigger(type, data, elem, true);
      }
    },
  });

  // Support: Firefox <=44
  // Firefox doesn't have focus(in | out) events
  // Related ticket - https://bugzilla.mozilla.org/show_bug.cgi?id=687787
  //
  // Support: Chrome <=48 - 49, Safari <=9.0 - 9.1
  // focus(in | out) events fire after focus & blur events,
  // which is spec violation - http://www.w3.org/TR/DOM-Level-3-Events/#events-focusevent-event-order
  // Related ticket - https://bugs.chromium.org/p/chromium/issues/detail?id=449857
  if (!support.focusin) {
    jQuery.each({ focus: "focusin", blur: "focusout" }, function (orig, fix) {
      // Attach a single capturing handler on the document while someone wants focusin/focusout
      var handler = function (event) {
        jQuery.event.simulate(fix, event.target, jQuery.event.fix(event));
      };

      jQuery.event.special[fix] = {
        setup: function () {
          // Handle: regular nodes (via `this.ownerDocument`), window
          // (via `this.document`) & document (via `this`).
          var doc = this.ownerDocument || this.document || this,
            attaches = dataPriv.access(doc, fix);

          if (!attaches) {
            doc.addEventListener(orig, handler, true);
          }
          dataPriv.access(doc, fix, (attaches || 0) + 1);
        },
        teardown: function () {
          var doc = this.ownerDocument || this.document || this,
            attaches = dataPriv.access(doc, fix) - 1;

          if (!attaches) {
            doc.removeEventListener(orig, handler, true);
            dataPriv.remove(doc, fix);
          } else {
            dataPriv.access(doc, fix, attaches);
          }
        },
      };
    });
  }
  var location = window.location;

  var nonce = { guid: Date.now() };

  var rquery = /\?/;

  // Cross-browser xml parsing
  jQuery.parseXML = function (data) {
    var xml, parserErrorElem;
    if (!data || typeof data !== "string") {
      return null;
    }

    // Support: IE 9 - 11 only
    // IE throws on parseFromString with invalid input.
    try {
      xml = new window.DOMParser().parseFromString(data, "text/xml");
    } catch (e) {}

    parserErrorElem = xml && xml.getElementsByTagName("parsererror")[0];
    if (!xml || parserErrorElem) {
      jQuery.error(
        "Invalid XML: " +
          (parserErrorElem
            ? jQuery
                .map(parserErrorElem.childNodes, function (el) {
                  return el.textContent;
                })
                .join("\n")
            : data)
      );
    }
    return xml;
  };

  var rbracket = /\[\]$/,
    rCRLF = /\r?\n/g,
    rsubmitterTypes = /^(?:submit|button|image|reset|file)$/i,
    rsubmittable = /^(?:input|select|textarea|keygen)/i;

  function buildParams(prefix, obj, traditional, add) {
    var name;

    if (Array.isArray(obj)) {
      // Serialize array item.
      jQuery.each(obj, function (i, v) {
        if (traditional || rbracket.test(prefix)) {
          // Treat each array item as a scalar.
          add(prefix, v);
        } else {
          // Item is non-scalar (array or object), encode its numeric index.
          buildParams(
            prefix + "[" + (typeof v === "object" && v != null ? i : "") + "]",
            v,
            traditional,
            add
          );
        }
      });
    } else if (!traditional && toType(obj) === "object") {
      // Serialize object item.
      for (name in obj) {
        buildParams(prefix + "[" + name + "]", obj[name], traditional, add);
      }
    } else {
      // Serialize scalar item.
      add(prefix, obj);
    }
  }

  // Serialize an array of form elements or a set of
  // key/values into a query string
  jQuery.param = function (a, traditional) {
    var prefix,
      s = [],
      add = function (key, valueOrFunction) {
        // If value is a function, invoke it and use its return value
        var value = isFunction(valueOrFunction)
          ? valueOrFunction()
          : valueOrFunction;

        s[s.length] =
          encodeURIComponent(key) +
          "=" +
          encodeURIComponent(value == null ? "" : value);
      };

    if (a == null) {
      return "";
    }

    // If an array was passed in, assume that it is an array of form elements.
    if (Array.isArray(a) || (a.jquery && !jQuery.isPlainObject(a))) {
      // Serialize the form elements
      jQuery.each(a, function () {
        add(this.name, this.value);
      });
    } else {
      // If traditional, encode the "old" way (the way 1.3.2 or older
      // did it), otherwise encode params recursively.
      for (prefix in a) {
        buildParams(prefix, a[prefix], traditional, add);
      }
    }

    // Return the resulting serialization
    return s.join("&");
  };

  jQuery.fn.extend({
    serialize: function () {
      return jQuery.param(this.serializeArray());
    },
    serializeArray: function () {
      return this.map(function () {
        // Can add propHook for "elements" to filter or add form elements
        var elements = jQuery.prop(this, "elements");
        return elements ? jQuery.makeArray(elements) : this;
      })
        .filter(function () {
          var type = this.type;

          // Use .is( ":disabled" ) so that fieldset[disabled] works
          return (
            this.name &&
            !jQuery(this).is(":disabled") &&
            rsubmittable.test(this.nodeName) &&
            !rsubmitterTypes.test(type) &&
            (this.checked || !rcheckableType.test(type))
          );
        })
        .map(function (_i, elem) {
          var val = jQuery(this).val();

          if (val == null) {
            return null;
          }

          if (Array.isArray(val)) {
            return jQuery.map(val, function (val) {
              return { name: elem.name, value: val.replace(rCRLF, "\r\n") };
            });
          }

          return { name: elem.name, value: val.replace(rCRLF, "\r\n") };
        })
        .get();
    },
  });

  var r20 = /%20/g,
    rhash = /#.*$/,
    rantiCache = /([?&])_=[^&]*/,
    rheaders = /^(.*?):[ \t]*([^\r\n]*)$/gm,
    // #7653, #8125, #8152: local protocol detection
    rlocalProtocol = /^(?:about|app|app-storage|.+-extension|file|res|widget):$/,
    rnoContent = /^(?:GET|HEAD)$/,
    rprotocol = /^\/\//,
    /* Prefilters
     * 1) They are useful to introduce custom dataTypes (see ajax/jsonp.js for an example)
     * 2) These are called:
     *    - BEFORE asking for a transport
     *    - AFTER param serialization (s.data is a string if s.processData is true)
     * 3) key is the dataType
     * 4) the catchall symbol "*" can be used
     * 5) execution will start with transport dataType and THEN continue down to "*" if needed
     */
    prefilters = {},
    /* Transports bindings
     * 1) key is the dataType
     * 2) the catchall symbol "*" can be used
     * 3) selection will start with transport dataType and THEN go to "*" if needed
     */
    transports = {},
    // Avoid comment-prolog char sequence (#10098); must appease lint and evade compression
    allTypes = "*/".concat("*"),
    // Anchor tag for parsing the document origin
    originAnchor = document.createElement("a");

  originAnchor.href = location.href;

  // Base "constructor" for jQuery.ajaxPrefilter and jQuery.ajaxTransport
  function addToPrefiltersOrTransports(structure) {
    // dataTypeExpression is optional and defaults to "*"
    return function (dataTypeExpression, func) {
      if (typeof dataTypeExpression !== "string") {
        func = dataTypeExpression;
        dataTypeExpression = "*";
      }

      var dataType,
        i = 0,
        dataTypes = dataTypeExpression.toLowerCase().match(rnothtmlwhite) || [];

      if (isFunction(func)) {
        // For each dataType in the dataTypeExpression
        while ((dataType = dataTypes[i++])) {
          // Prepend if requested
          if (dataType[0] === "+") {
            dataType = dataType.slice(1) || "*";
            (structure[dataType] = structure[dataType] || []).unshift(func);

            // Otherwise append
          } else {
            (structure[dataType] = structure[dataType] || []).push(func);
          }
        }
      }
    };
  }

  // Base inspection function for prefilters and transports
  function inspectPrefiltersOrTransports(
    structure,
    options,
    originalOptions,
    jqXHR
  ) {
    var inspected = {},
      seekingTransport = structure === transports;

    function inspect(dataType) {
      var selected;
      inspected[dataType] = true;
      jQuery.each(structure[dataType] || [], function (_, prefilterOrFactory) {
        var dataTypeOrTransport = prefilterOrFactory(
          options,
          originalOptions,
          jqXHR
        );
        if (
          typeof dataTypeOrTransport === "string" &&
          !seekingTransport &&
          !inspected[dataTypeOrTransport]
        ) {
          options.dataTypes.unshift(dataTypeOrTransport);
          inspect(dataTypeOrTransport);
          return false;
        } else if (seekingTransport) {
          return !(selected = dataTypeOrTransport);
        }
      });
      return selected;
    }

    return inspect(options.dataTypes[0]) || (!inspected["*"] && inspect("*"));
  }

  // A special extend for ajax options
  // that takes "flat" options (not to be deep extended)
  // Fixes #9887
  function ajaxExtend(target, src) {
    var key,
      deep,
      flatOptions = jQuery.ajaxSettings.flatOptions || {};

    for (key in src) {
      if (src[key] !== undefined) {
        (flatOptions[key] ? target : deep || (deep = {}))[key] = src[key];
      }
    }
    if (deep) {
      jQuery.extend(true, target, deep);
    }

    return target;
  }

  /* Handles responses to an ajax request:
   * - finds the right dataType (mediates between content-type and expected dataType)
   * - returns the corresponding response
   */
  function ajaxHandleResponses(s, jqXHR, responses) {
    var ct,
      type,
      finalDataType,
      firstDataType,
      contents = s.contents,
      dataTypes = s.dataTypes;

    // Remove auto dataType and get content-type in the process
    while (dataTypes[0] === "*") {
      dataTypes.shift();
      if (ct === undefined) {
        ct = s.mimeType || jqXHR.getResponseHeader("Content-Type");
      }
    }

    // Check if we're dealing with a known content-type
    if (ct) {
      for (type in contents) {
        if (contents[type] && contents[type].test(ct)) {
          dataTypes.unshift(type);
          break;
        }
      }
    }

    // Check to see if we have a response for the expected dataType
    if (dataTypes[0] in responses) {
      finalDataType = dataTypes[0];
    } else {
      // Try convertible dataTypes
      for (type in responses) {
        if (!dataTypes[0] || s.converters[type + " " + dataTypes[0]]) {
          finalDataType = type;
          break;
        }
        if (!firstDataType) {
          firstDataType = type;
        }
      }

      // Or just use first one
      finalDataType = finalDataType || firstDataType;
    }

    // If we found a dataType
    // We add the dataType to the list if needed
    // and return the corresponding response
    if (finalDataType) {
      if (finalDataType !== dataTypes[0]) {
        dataTypes.unshift(finalDataType);
      }
      return responses[finalDataType];
    }
  }

  /* Chain conversions given the request and the original response
   * Also sets the responseXXX fields on the jqXHR instance
   */
  function ajaxConvert(s, response, jqXHR, isSuccess) {
    var conv2,
      current,
      conv,
      tmp,
      prev,
      converters = {},
      // Work with a copy of dataTypes in case we need to modify it for conversion
      dataTypes = s.dataTypes.slice();

    // Create converters map with lowercased keys
    if (dataTypes[1]) {
      for (conv in s.converters) {
        converters[conv.toLowerCase()] = s.converters[conv];
      }
    }

    current = dataTypes.shift();

    // Convert to each sequential dataType
    while (current) {
      if (s.responseFields[current]) {
        jqXHR[s.responseFields[current]] = response;
      }

      // Apply the dataFilter if provided
      if (!prev && isSuccess && s.dataFilter) {
        response = s.dataFilter(response, s.dataType);
      }

      prev = current;
      current = dataTypes.shift();

      if (current) {
        // There's only work to do if current dataType is non-auto
        if (current === "*") {
          current = prev;

          // Convert response if prev dataType is non-auto and differs from current
        } else if (prev !== "*" && prev !== current) {
          // Seek a direct converter
          conv = converters[prev + " " + current] || converters["* " + current];

          // If none found, seek a pair
          if (!conv) {
            for (conv2 in converters) {
              // If conv2 outputs current
              tmp = conv2.split(" ");
              if (tmp[1] === current) {
                // If prev can be converted to accepted input
                conv =
                  converters[prev + " " + tmp[0]] || converters["* " + tmp[0]];
                if (conv) {
                  // Condense equivalence converters
                  if (conv === true) {
                    conv = converters[conv2];

                    // Otherwise, insert the intermediate dataType
                  } else if (converters[conv2] !== true) {
                    current = tmp[0];
                    dataTypes.unshift(tmp[1]);
                  }
                  break;
                }
              }
            }
          }

          // Apply converter (if not an equivalence)
          if (conv !== true) {
            // Unless errors are allowed to bubble, catch and return them
            if (conv && s.throws) {
              response = conv(response);
            } else {
              try {
                response = conv(response);
              } catch (e) {
                return {
                  state: "parsererror",
                  error: conv
                    ? e
                    : "No conversion from " + prev + " to " + current,
                };
              }
            }
          }
        }
      }
    }

    return { state: "success", data: response };
  }

  jQuery.extend({
    // Counter for holding the number of active queries
    active: 0,

    // Last-Modified header cache for next request
    lastModified: {},
    etag: {},

    ajaxSettings: {
      url: location.href,
      type: "GET",
      isLocal: rlocalProtocol.test(location.protocol),
      global: true,
      processData: true,
      async: true,
      contentType: "application/x-www-form-urlencoded; charset=UTF-8",

      /*
		timeout: 0,
		data: null,
		dataType: null,
		username: null,
		password: null,
		cache: null,
		throws: false,
		traditional: false,
		headers: {},
		*/

      accepts: {
        "*": allTypes,
        text: "text/plain",
        html: "text/html",
        xml: "application/xml, text/xml",
        json: "application/json, text/javascript",
      },

      contents: {
        xml: /\bxml\b/,
        html: /\bhtml/,
        json: /\bjson\b/,
      },

      responseFields: {
        xml: "responseXML",
        text: "responseText",
        json: "responseJSON",
      },

      // Data converters
      // Keys separate source (or catchall "*") and destination types with a single space
      converters: {
        // Convert anything to text
        "* text": String,

        // Text to html (true = no transformation)
        "text html": true,

        // Evaluate text as a json expression
        "text json": JSON.parse,

        // Parse text as xml
        "text xml": jQuery.parseXML,
      },

      // For options that shouldn't be deep extended:
      // you can add your own custom options here if
      // and when you create one that shouldn't be
      // deep extended (see ajaxExtend)
      flatOptions: {
        url: true,
        context: true,
      },
    },

    // Creates a full fledged settings object into target
    // with both ajaxSettings and settings fields.
    // If target is omitted, writes into ajaxSettings.
    ajaxSetup: function (target, settings) {
      return settings
        ? // Building a settings object
          ajaxExtend(ajaxExtend(target, jQuery.ajaxSettings), settings)
        : // Extending ajaxSettings
          ajaxExtend(jQuery.ajaxSettings, target);
    },

    ajaxPrefilter: addToPrefiltersOrTransports(prefilters),
    ajaxTransport: addToPrefiltersOrTransports(transports),

    // Main method
    ajax: function (url, options) {
      // If url is an object, simulate pre-1.5 signature
      if (typeof url === "object") {
        options = url;
        url = undefined;
      }

      // Force options to be an object
      options = options || {};

      var transport,
        // URL without anti-cache param
        cacheURL,
        // Response headers
        responseHeadersString,
        responseHeaders,
        // timeout handle
        timeoutTimer,
        // Url cleanup var
        urlAnchor,
        // Request state (becomes false upon send and true upon completion)
        completed,
        // To know if global events are to be dispatched
        fireGlobals,
        // Loop variable
        i,
        // uncached part of the url
        uncached,
        // Create the final options object
        s = jQuery.ajaxSetup({}, options),
        // Callbacks context
        callbackContext = s.context || s,
        // Context for global events is callbackContext if it is a DOM node or jQuery collection
        globalEventContext =
          s.context && (callbackContext.nodeType || callbackContext.jquery)
            ? jQuery(callbackContext)
            : jQuery.event,
        // Deferreds
        deferred = jQuery.Deferred(),
        completeDeferred = jQuery.Callbacks("once memory"),
        // Status-dependent callbacks
        statusCode = s.statusCode || {},
        // Headers (they are sent all at once)
        requestHeaders = {},
        requestHeadersNames = {},
        // Default abort message
        strAbort = "canceled",
        // Fake xhr
        jqXHR = {
          readyState: 0,

          // Builds headers hashtable if needed
          getResponseHeader: function (key) {
            var match;
            if (completed) {
              if (!responseHeaders) {
                responseHeaders = {};
                while ((match = rheaders.exec(responseHeadersString))) {
                  responseHeaders[match[1].toLowerCase() + " "] = (
                    responseHeaders[match[1].toLowerCase() + " "] || []
                  ).concat(match[2]);
                }
              }
              match = responseHeaders[key.toLowerCase() + " "];
            }
            return match == null ? null : match.join(", ");
          },

          // Raw string
          getAllResponseHeaders: function () {
            return completed ? responseHeadersString : null;
          },

          // Caches the header
          setRequestHeader: function (name, value) {
            if (completed == null) {
              name = requestHeadersNames[name.toLowerCase()] =
                requestHeadersNames[name.toLowerCase()] || name;
              requestHeaders[name] = value;
            }
            return this;
          },

          // Overrides response content-type header
          overrideMimeType: function (type) {
            if (completed == null) {
              s.mimeType = type;
            }
            return this;
          },

          // Status-dependent callbacks
          statusCode: function (map) {
            var code;
            if (map) {
              if (completed) {
                // Execute the appropriate callbacks
                jqXHR.always(map[jqXHR.status]);
              } else {
                // Lazy-add the new callbacks in a way that preserves old ones
                for (code in map) {
                  statusCode[code] = [statusCode[code], map[code]];
                }
              }
            }
            return this;
          },

          // Cancel the request
          abort: function (statusText) {
            var finalText = statusText || strAbort;
            if (transport) {
              transport.abort(finalText);
            }
            done(0, finalText);
            return this;
          },
        };

      // Attach deferreds
      deferred.promise(jqXHR);

      // Add protocol if not provided (prefilters might expect it)
      // Handle falsy url in the settings object (#10093: consistency with old signature)
      // We also use the url parameter if available
      s.url = ((url || s.url || location.href) + "").replace(
        rprotocol,
        location.protocol + "//"
      );

      // Alias method option to type as per ticket #12004
      s.type = options.method || options.type || s.method || s.type;

      // Extract dataTypes list
      s.dataTypes = (s.dataType || "*").toLowerCase().match(rnothtmlwhite) || [
        "",
      ];

      // A cross-domain request is in order when the origin doesn't match the current origin.
      if (s.crossDomain == null) {
        urlAnchor = document.createElement("a");

        // Support: IE <=8 - 11, Edge 12 - 15
        // IE throws exception on accessing the href property if url is malformed,
        // e.g. http://example.com:80x/
        try {
          urlAnchor.href = s.url;

          // Support: IE <=8 - 11 only
          // Anchor's host property isn't correctly set when s.url is relative
          urlAnchor.href = urlAnchor.href;
          s.crossDomain =
            originAnchor.protocol + "//" + originAnchor.host !==
            urlAnchor.protocol + "//" + urlAnchor.host;
        } catch (e) {
          // If there is an error parsing the URL, assume it is crossDomain,
          // it can be rejected by the transport if it is invalid
          s.crossDomain = true;
        }
      }

      // Convert data if not already a string
      if (s.data && s.processData && typeof s.data !== "string") {
        s.data = jQuery.param(s.data, s.traditional);
      }

      // Apply prefilters
      inspectPrefiltersOrTransports(prefilters, s, options, jqXHR);

      // If request was aborted inside a prefilter, stop there
      if (completed) {
        return jqXHR;
      }

      // We can fire global events as of now if asked to
      // Don't fire events if jQuery.event is undefined in an AMD-usage scenario (#15118)
      fireGlobals = jQuery.event && s.global;

      // Watch for a new set of requests
      if (fireGlobals && jQuery.active++ === 0) {
        jQuery.event.trigger("ajaxStart");
      }

      // Uppercase the type
      s.type = s.type.toUpperCase();

      // Determine if request has content
      s.hasContent = !rnoContent.test(s.type);

      // Save the URL in case we're toying with the If-Modified-Since
      // and/or If-None-Match header later on
      // Remove hash to simplify url manipulation
      cacheURL = s.url.replace(rhash, "");

      // More options handling for requests with no content
      if (!s.hasContent) {
        // Remember the hash so we can put it back
        uncached = s.url.slice(cacheURL.length);

        // If data is available and should be processed, append data to url
        if (s.data && (s.processData || typeof s.data === "string")) {
          cacheURL += (rquery.test(cacheURL) ? "&" : "?") + s.data;

          // #9682: remove data so that it's not used in an eventual retry
          delete s.data;
        }

        // Add or update anti-cache param if needed
        if (s.cache === false) {
          cacheURL = cacheURL.replace(rantiCache, "$1");
          uncached =
            (rquery.test(cacheURL) ? "&" : "?") +
            "_=" +
            nonce.guid++ +
            uncached;
        }

        // Put hash and anti-cache on the URL that will be requested (gh-1732)
        s.url = cacheURL + uncached;

        // Change '%20' to '+' if this is encoded form body content (gh-2658)
      } else if (
        s.data &&
        s.processData &&
        (s.contentType || "").indexOf("application/x-www-form-urlencoded") === 0
      ) {
        s.data = s.data.replace(r20, "+");
      }

      // Set the If-Modified-Since and/or If-None-Match header, if in ifModified mode.
      if (s.ifModified) {
        if (jQuery.lastModified[cacheURL]) {
          jqXHR.setRequestHeader(
            "If-Modified-Since",
            jQuery.lastModified[cacheURL]
          );
        }
        if (jQuery.etag[cacheURL]) {
          jqXHR.setRequestHeader("If-None-Match", jQuery.etag[cacheURL]);
        }
      }

      // Set the correct header, if data is being sent
      if (
        (s.data && s.hasContent && s.contentType !== false) ||
        options.contentType
      ) {
        jqXHR.setRequestHeader("Content-Type", s.contentType);
      }

      // Set the Accepts header for the server, depending on the dataType
      jqXHR.setRequestHeader(
        "Accept",
        s.dataTypes[0] && s.accepts[s.dataTypes[0]]
          ? s.accepts[s.dataTypes[0]] +
              (s.dataTypes[0] !== "*" ? ", " + allTypes + "; q=0.01" : "")
          : s.accepts["*"]
      );

      // Check for headers option
      for (i in s.headers) {
        jqXHR.setRequestHeader(i, s.headers[i]);
      }

      // Allow custom headers/mimetypes and early abort
      if (
        s.beforeSend &&
        (s.beforeSend.call(callbackContext, jqXHR, s) === false || completed)
      ) {
        // Abort if not done already and return
        return jqXHR.abort();
      }

      // Aborting is no longer a cancellation
      strAbort = "abort";

      // Install callbacks on deferreds
      completeDeferred.add(s.complete);
      jqXHR.done(s.success);
      jqXHR.fail(s.error);

      // Get transport
      transport = inspectPrefiltersOrTransports(transports, s, options, jqXHR);

      // If no transport, we auto-abort
      if (!transport) {
        done(-1, "No Transport");
      } else {
        jqXHR.readyState = 1;

        // Send global event
        if (fireGlobals) {
          globalEventContext.trigger("ajaxSend", [jqXHR, s]);
        }

        // If request was aborted inside ajaxSend, stop there
        if (completed) {
          return jqXHR;
        }

        // Timeout
        if (s.async && s.timeout > 0) {
          timeoutTimer = window.setTimeout(function () {
            jqXHR.abort("timeout");
          }, s.timeout);
        }

        try {
          completed = false;
          transport.send(requestHeaders, done);
        } catch (e) {
          // Rethrow post-completion exceptions
          if (completed) {
            throw e;
          }

          // Propagate others as results
          done(-1, e);
        }
      }

      // Callback for when everything is done
      function done(status, nativeStatusText, responses, headers) {
        var isSuccess,
          success,
          error,
          response,
          modified,
          statusText = nativeStatusText;

        // Ignore repeat invocations
        if (completed) {
          return;
        }

        completed = true;

        // Clear timeout if it exists
        if (timeoutTimer) {
          window.clearTimeout(timeoutTimer);
        }

        // Dereference transport for early garbage collection
        // (no matter how long the jqXHR object will be used)
        transport = undefined;

        // Cache response headers
        responseHeadersString = headers || "";

        // Set readyState
        jqXHR.readyState = status > 0 ? 4 : 0;

        // Determine if successful
        isSuccess = (status >= 200 && status < 300) || status === 304;

        // Get response data
        if (responses) {
          response = ajaxHandleResponses(s, jqXHR, responses);
        }

        // Use a noop converter for missing script but not if jsonp
        if (
          !isSuccess &&
          jQuery.inArray("script", s.dataTypes) > -1 &&
          jQuery.inArray("json", s.dataTypes) < 0
        ) {
          s.converters["text script"] = function () {};
        }

        // Convert no matter what (that way responseXXX fields are always set)
        response = ajaxConvert(s, response, jqXHR, isSuccess);

        // If successful, handle type chaining
        if (isSuccess) {
          // Set the If-Modified-Since and/or If-None-Match header, if in ifModified mode.
          if (s.ifModified) {
            modified = jqXHR.getResponseHeader("Last-Modified");
            if (modified) {
              jQuery.lastModified[cacheURL] = modified;
            }
            modified = jqXHR.getResponseHeader("etag");
            if (modified) {
              jQuery.etag[cacheURL] = modified;
            }
          }

          // if no content
          if (status === 204 || s.type === "HEAD") {
            statusText = "nocontent";

            // if not modified
          } else if (status === 304) {
            statusText = "notmodified";

            // If we have data, let's convert it
          } else {
            statusText = response.state;
            success = response.data;
            error = response.error;
            isSuccess = !error;
          }
        } else {
          // Extract error from statusText and normalize for non-aborts
          error = statusText;
          if (status || !statusText) {
            statusText = "error";
            if (status < 0) {
              status = 0;
            }
          }
        }

        // Set data for the fake xhr object
        jqXHR.status = status;
        jqXHR.statusText = (nativeStatusText || statusText) + "";

        // Success/Error
        if (isSuccess) {
          deferred.resolveWith(callbackContext, [success, statusText, jqXHR]);
        } else {
          deferred.rejectWith(callbackContext, [jqXHR, statusText, error]);
        }

        // Status-dependent callbacks
        jqXHR.statusCode(statusCode);
        statusCode = undefined;

        if (fireGlobals) {
          globalEventContext.trigger(isSuccess ? "ajaxSuccess" : "ajaxError", [
            jqXHR,
            s,
            isSuccess ? success : error,
          ]);
        }

        // Complete
        completeDeferred.fireWith(callbackContext, [jqXHR, statusText]);

        if (fireGlobals) {
          globalEventContext.trigger("ajaxComplete", [jqXHR, s]);

          // Handle the global AJAX counter
          if (!--jQuery.active) {
            jQuery.event.trigger("ajaxStop");
          }
        }
      }

      return jqXHR;
    },

    getJSON: function (url, data, callback) {
      return jQuery.get(url, data, callback, "json");
    },

    getScript: function (url, callback) {
      return jQuery.get(url, undefined, callback, "script");
    },
  });

  jQuery.each(["get", "post"], function (_i, method) {
    jQuery[method] = function (url, data, callback, type) {
      // Shift arguments if data argument was omitted
      if (isFunction(data)) {
        type = type || callback;
        callback = data;
        data = undefined;
      }

      // The url can be an options object (which then must have .url)
      return jQuery.ajax(
        jQuery.extend(
          {
            url: url,
            type: method,
            dataType: type,
            data: data,
            success: callback,
          },
          jQuery.isPlainObject(url) && url
        )
      );
    };
  });

  jQuery.ajaxPrefilter(function (s) {
    var i;
    for (i in s.headers) {
      if (i.toLowerCase() === "content-type") {
        s.contentType = s.headers[i] || "";
      }
    }
  });

  jQuery._evalUrl = function (url, options, doc) {
    return jQuery.ajax({
      url: url,

      // Make this explicit, since user can override this through ajaxSetup (#11264)
      type: "GET",
      dataType: "script",
      cache: true,
      async: false,
      global: false,

      // Only evaluate the response if it is successful (gh-4126)
      // dataFilter is not invoked for failure responses, so using it instead
      // of the default converter is kludgy but it works.
      converters: {
        "text script": function () {},
      },
      dataFilter: function (response) {
        jQuery.globalEval(response, options, doc);
      },
    });
  };

  jQuery.fn.extend({
    wrapAll: function (html) {
      var wrap;

      if (this[0]) {
        if (isFunction(html)) {
          html = html.call(this[0]);
        }

        // The elements to wrap the target around
        wrap = jQuery(html, this[0].ownerDocument).eq(0).clone(true);

        if (this[0].parentNode) {
          wrap.insertBefore(this[0]);
        }

        wrap
          .map(function () {
            var elem = this;

            while (elem.firstElementChild) {
              elem = elem.firstElementChild;
            }

            return elem;
          })
          .append(this);
      }

      return this;
    },

    wrapInner: function (html) {
      if (isFunction(html)) {
        return this.each(function (i) {
          jQuery(this).wrapInner(html.call(this, i));
        });
      }

      return this.each(function () {
        var self = jQuery(this),
          contents = self.contents();

        if (contents.length) {
          contents.wrapAll(html);
        } else {
          self.append(html);
        }
      });
    },

    wrap: function (html) {
      var htmlIsFunction = isFunction(html);

      return this.each(function (i) {
        jQuery(this).wrapAll(htmlIsFunction ? html.call(this, i) : html);
      });
    },

    unwrap: function (selector) {
      this.parent(selector)
        .not("body")
        .each(function () {
          jQuery(this).replaceWith(this.childNodes);
        });
      return this;
    },
  });

  jQuery.expr.pseudos.hidden = function (elem) {
    return !jQuery.expr.pseudos.visible(elem);
  };
  jQuery.expr.pseudos.visible = function (elem) {
    return !!(
      elem.offsetWidth ||
      elem.offsetHeight ||
      elem.getClientRects().length
    );
  };

  jQuery.ajaxSettings.xhr = function () {
    try {
      return new window.XMLHttpRequest();
    } catch (e) {}
  };

  var xhrSuccessStatus = {
      // File protocol always yields status code 0, assume 200
      0: 200,

      // Support: IE <=9 only
      // #1450: sometimes IE returns 1223 when it should be 204
      1223: 204,
    },
    xhrSupported = jQuery.ajaxSettings.xhr();

  support.cors = !!xhrSupported && "withCredentials" in xhrSupported;
  support.ajax = xhrSupported = !!xhrSupported;

  jQuery.ajaxTransport(function (options) {
    var callback, errorCallback;

    // Cross domain only allowed if supported through XMLHttpRequest
    if (support.cors || (xhrSupported && !options.crossDomain)) {
      return {
        send: function (headers, complete) {
          var i,
            xhr = options.xhr();

          xhr.open(
            options.type,
            options.url,
            options.async,
            options.username,
            options.password
          );

          // Apply custom fields if provided
          if (options.xhrFields) {
            for (i in options.xhrFields) {
              xhr[i] = options.xhrFields[i];
            }
          }

          // Override mime type if needed
          if (options.mimeType && xhr.overrideMimeType) {
            xhr.overrideMimeType(options.mimeType);
          }

          // X-Requested-With header
          // For cross-domain requests, seeing as conditions for a preflight are
          // akin to a jigsaw puzzle, we simply never set it to be sure.
          // (it can always be set on a per-request basis or even using ajaxSetup)
          // For same-domain requests, won't change header if already provided.
          if (!options.crossDomain && !headers["X-Requested-With"]) {
            headers["X-Requested-With"] = "XMLHttpRequest";
          }

          // Set headers
          for (i in headers) {
            xhr.setRequestHeader(i, headers[i]);
          }

          // Callback
          callback = function (type) {
            return function () {
              if (callback) {
                callback = errorCallback = xhr.onload = xhr.onerror = xhr.onabort = xhr.ontimeout = xhr.onreadystatechange = null;

                if (type === "abort") {
                  xhr.abort();
                } else if (type === "error") {
                  // Support: IE <=9 only
                  // On a manual native abort, IE9 throws
                  // errors on any property access that is not readyState
                  if (typeof xhr.status !== "number") {
                    complete(0, "error");
                  } else {
                    complete(
                      // File: protocol always yields status 0; see #8605, #14207
                      xhr.status,
                      xhr.statusText
                    );
                  }
                } else {
                  complete(
                    xhrSuccessStatus[xhr.status] || xhr.status,
                    xhr.statusText,

                    // Support: IE <=9 only
                    // IE9 has no XHR2 but throws on binary (trac-11426)
                    // For XHR2 non-text, let the caller handle it (gh-2498)
                    (xhr.responseType || "text") !== "text" ||
                      typeof xhr.responseText !== "string"
                      ? { binary: xhr.response }
                      : { text: xhr.responseText },
                    xhr.getAllResponseHeaders()
                  );
                }
              }
            };
          };

          // Listen to events
          xhr.onload = callback();
          errorCallback = xhr.onerror = xhr.ontimeout = callback("error");

          // Support: IE 9 only
          // Use onreadystatechange to replace onabort
          // to handle uncaught aborts
          if (xhr.onabort !== undefined) {
            xhr.onabort = errorCallback;
          } else {
            xhr.onreadystatechange = function () {
              // Check readyState before timeout as it changes
              if (xhr.readyState === 4) {
                // Allow onerror to be called first,
                // but that will not handle a native abort
                // Also, save errorCallback to a variable
                // as xhr.onerror cannot be accessed
                window.setTimeout(function () {
                  if (callback) {
                    errorCallback();
                  }
                });
              }
            };
          }

          // Create the abort callback
          callback = callback("abort");

          try {
            // Do send the request (this may raise an exception)
            xhr.send((options.hasContent && options.data) || null);
          } catch (e) {
            // #14683: Only rethrow if this hasn't been notified as an error yet
            if (callback) {
              throw e;
            }
          }
        },

        abort: function () {
          if (callback) {
            callback();
          }
        },
      };
    }
  });

  // Prevent auto-execution of scripts when no explicit dataType was provided (See gh-2432)
  jQuery.ajaxPrefilter(function (s) {
    if (s.crossDomain) {
      s.contents.script = false;
    }
  });

  // Install script dataType
  jQuery.ajaxSetup({
    accepts: {
      script:
        "text/javascript, application/javascript, " +
        "application/ecmascript, application/x-ecmascript",
    },
    contents: {
      script: /\b(?:java|ecma)script\b/,
    },
    converters: {
      "text script": function (text) {
        jQuery.globalEval(text);
        return text;
      },
    },
  });

  // Handle cache's special case and crossDomain
  jQuery.ajaxPrefilter("script", function (s) {
    if (s.cache === undefined) {
      s.cache = false;
    }
    if (s.crossDomain) {
      s.type = "GET";
    }
  });

  // Bind script tag hack transport
  jQuery.ajaxTransport("script", function (s) {
    // This transport only deals with cross domain or forced-by-attrs requests
    if (s.crossDomain || s.scriptAttrs) {
      var script, callback;
      return {
        send: function (_, complete) {
          script = jQuery("<script>")
            .attr(s.scriptAttrs || {})
            .prop({ charset: s.scriptCharset, src: s.url })
            .on(
              "load error",
              (callback = function (evt) {
                script.remove();
                callback = null;
                if (evt) {
                  complete(evt.type === "error" ? 404 : 200, evt.type);
                }
              })
            );

          // Use native DOM manipulation to avoid our domManip AJAX trickery
          document.head.appendChild(script[0]);
        },
        abort: function () {
          if (callback) {
            callback();
          }
        },
      };
    }
  });

  var oldCallbacks = [],
    rjsonp = /(=)\?(?=&|$)|\?\?/;

  // Default jsonp settings
  jQuery.ajaxSetup({
    jsonp: "callback",
    jsonpCallback: function () {
      var callback = oldCallbacks.pop() || jQuery.expando + "_" + nonce.guid++;
      this[callback] = true;
      return callback;
    },
  });

  // Detect, normalize options and install callbacks for jsonp requests
  jQuery.ajaxPrefilter("json jsonp", function (s, originalSettings, jqXHR) {
    var callbackName,
      overwritten,
      responseContainer,
      jsonProp =
        s.jsonp !== false &&
        (rjsonp.test(s.url)
          ? "url"
          : typeof s.data === "string" &&
            (s.contentType || "").indexOf(
              "application/x-www-form-urlencoded"
            ) === 0 &&
            rjsonp.test(s.data) &&
            "data");

    // Handle iff the expected data type is "jsonp" or we have a parameter to set
    if (jsonProp || s.dataTypes[0] === "jsonp") {
      // Get callback name, remembering preexisting value associated with it
      callbackName = s.jsonpCallback = isFunction(s.jsonpCallback)
        ? s.jsonpCallback()
        : s.jsonpCallback;

      // Insert callback into url or form data
      if (jsonProp) {
        s[jsonProp] = s[jsonProp].replace(rjsonp, "$1" + callbackName);
      } else if (s.jsonp !== false) {
        s.url +=
          (rquery.test(s.url) ? "&" : "?") + s.jsonp + "=" + callbackName;
      }

      // Use data converter to retrieve json after script execution
      s.converters["script json"] = function () {
        if (!responseContainer) {
          jQuery.error(callbackName + " was not called");
        }
        return responseContainer[0];
      };

      // Force json dataType
      s.dataTypes[0] = "json";

      // Install callback
      overwritten = window[callbackName];
      window[callbackName] = function () {
        responseContainer = arguments;
      };

      // Clean-up function (fires after converters)
      jqXHR.always(function () {
        // If previous value didn't exist - remove it
        if (overwritten === undefined) {
          jQuery(window).removeProp(callbackName);

          // Otherwise restore preexisting value
        } else {
          window[callbackName] = overwritten;
        }

        // Save back as free
        if (s[callbackName]) {
          // Make sure that re-using the options doesn't screw things around
          s.jsonpCallback = originalSettings.jsonpCallback;

          // Save the callback name for future use
          oldCallbacks.push(callbackName);
        }

        // Call if it was a function and we have a response
        if (responseContainer && isFunction(overwritten)) {
          overwritten(responseContainer[0]);
        }

        responseContainer = overwritten = undefined;
      });

      // Delegate to script
      return "script";
    }
  });

  // Support: Safari 8 only
  // In Safari 8 documents created via document.implementation.createHTMLDocument
  // collapse sibling forms: the second one becomes a child of the first one.
  // Because of that, this security measure has to be disabled in Safari 8.
  // https://bugs.webkit.org/show_bug.cgi?id=137337
  support.createHTMLDocument = (function () {
    var body = document.implementation.createHTMLDocument("").body;
    body.innerHTML = "<form></form><form></form>";
    return body.childNodes.length === 2;
  })();

  // Argument "data" should be string of html
  // context (optional): If specified, the fragment will be created in this context,
  // defaults to document
  // keepScripts (optional): If true, will include scripts passed in the html string
  jQuery.parseHTML = function (data, context, keepScripts) {
    if (typeof data !== "string") {
      return [];
    }
    if (typeof context === "boolean") {
      keepScripts = context;
      context = false;
    }

    var base, parsed, scripts;

    if (!context) {
      // Stop scripts or inline event handlers from being executed immediately
      // by using document.implementation
      if (support.createHTMLDocument) {
        context = document.implementation.createHTMLDocument("");

        // Set the base href for the created document
        // so any parsed elements with URLs
        // are based on the document's URL (gh-2965)
        base = context.createElement("base");
        base.href = document.location.href;
        context.head.appendChild(base);
      } else {
        context = document;
      }
    }

    parsed = rsingleTag.exec(data);
    scripts = !keepScripts && [];

    // Single tag
    if (parsed) {
      return [context.createElement(parsed[1])];
    }

    parsed = buildFragment([data], context, scripts);

    if (scripts && scripts.length) {
      jQuery(scripts).remove();
    }

    return jQuery.merge([], parsed.childNodes);
  };

  /**
   * Load a url into a page
   */
  jQuery.fn.load = function (url, params, callback) {
    var selector,
      type,
      response,
      self = this,
      off = url.indexOf(" ");

    if (off > -1) {
      selector = stripAndCollapse(url.slice(off));
      url = url.slice(0, off);
    }

    // If it's a function
    if (isFunction(params)) {
      // We assume that it's the callback
      callback = params;
      params = undefined;

      // Otherwise, build a param string
    } else if (params && typeof params === "object") {
      type = "POST";
    }

    // If we have elements to modify, make the request
    if (self.length > 0) {
      jQuery
        .ajax({
          url: url,

          // If "type" variable is undefined, then "GET" method will be used.
          // Make value of this field explicit since
          // user can override it through ajaxSetup method
          type: type || "GET",
          dataType: "html",
          data: params,
        })
        .done(function (responseText) {
          // Save response for use in complete callback
          response = arguments;

          self.html(
            selector
              ? // If a selector was specified, locate the right elements in a dummy div
                // Exclude scripts to avoid IE 'Permission Denied' errors
                jQuery("<div>")
                  .append(jQuery.parseHTML(responseText))
                  .find(selector)
              : // Otherwise use the full result
                responseText
          );

          // If the request succeeds, this function gets "data", "status", "jqXHR"
          // but they are ignored because response was set above.
          // If it fails, this function gets "jqXHR", "status", "error"
        })
        .always(
          callback &&
            function (jqXHR, status) {
              self.each(function () {
                callback.apply(
                  this,
                  response || [jqXHR.responseText, status, jqXHR]
                );
              });
            }
        );
    }

    return this;
  };

  jQuery.expr.pseudos.animated = function (elem) {
    return jQuery.grep(jQuery.timers, function (fn) {
      return elem === fn.elem;
    }).length;
  };

  jQuery.offset = {
    setOffset: function (elem, options, i) {
      var curPosition,
        curLeft,
        curCSSTop,
        curTop,
        curOffset,
        curCSSLeft,
        calculatePosition,
        position = jQuery.css(elem, "position"),
        curElem = jQuery(elem),
        props = {};

      // Set position first, in-case top/left are set even on static elem
      if (position === "static") {
        elem.style.position = "relative";
      }

      curOffset = curElem.offset();
      curCSSTop = jQuery.css(elem, "top");
      curCSSLeft = jQuery.css(elem, "left");
      calculatePosition =
        (position === "absolute" || position === "fixed") &&
        (curCSSTop + curCSSLeft).indexOf("auto") > -1;

      // Need to be able to calculate position if either
      // top or left is auto and position is either absolute or fixed
      if (calculatePosition) {
        curPosition = curElem.position();
        curTop = curPosition.top;
        curLeft = curPosition.left;
      } else {
        curTop = parseFloat(curCSSTop) || 0;
        curLeft = parseFloat(curCSSLeft) || 0;
      }

      if (isFunction(options)) {
        // Use jQuery.extend here to allow modification of coordinates argument (gh-1848)
        options = options.call(elem, i, jQuery.extend({}, curOffset));
      }

      if (options.top != null) {
        props.top = options.top - curOffset.top + curTop;
      }
      if (options.left != null) {
        props.left = options.left - curOffset.left + curLeft;
      }

      if ("using" in options) {
        options.using.call(elem, props);
      } else {
        curElem.css(props);
      }
    },
  };

  jQuery.fn.extend({
    // offset() relates an element's border box to the document origin
    offset: function (options) {
      // Preserve chaining for setter
      if (arguments.length) {
        return options === undefined
          ? this
          : this.each(function (i) {
              jQuery.offset.setOffset(this, options, i);
            });
      }

      var rect,
        win,
        elem = this[0];

      if (!elem) {
        return;
      }

      // Return zeros for disconnected and hidden (display: none) elements (gh-2310)
      // Support: IE <=11 only
      // Running getBoundingClientRect on a
      // disconnected node in IE throws an error
      if (!elem.getClientRects().length) {
        return { top: 0, left: 0 };
      }

      // Get document-relative position by adding viewport scroll to viewport-relative gBCR
      rect = elem.getBoundingClientRect();
      win = elem.ownerDocument.defaultView;
      return {
        top: rect.top + win.pageYOffset,
        left: rect.left + win.pageXOffset,
      };
    },

    // position() relates an element's margin box to its offset parent's padding box
    // This corresponds to the behavior of CSS absolute positioning
    position: function () {
      if (!this[0]) {
        return;
      }

      var offsetParent,
        offset,
        doc,
        elem = this[0],
        parentOffset = { top: 0, left: 0 };

      // position:fixed elements are offset from the viewport, which itself always has zero offset
      if (jQuery.css(elem, "position") === "fixed") {
        // Assume position:fixed implies availability of getBoundingClientRect
        offset = elem.getBoundingClientRect();
      } else {
        offset = this.offset();

        // Account for the *real* offset parent, which can be the document or its root element
        // when a statically positioned element is identified
        doc = elem.ownerDocument;
        offsetParent = elem.offsetParent || doc.documentElement;
        while (
          offsetParent &&
          (offsetParent === doc.body || offsetParent === doc.documentElement) &&
          jQuery.css(offsetParent, "position") === "static"
        ) {
          offsetParent = offsetParent.parentNode;
        }
        if (
          offsetParent &&
          offsetParent !== elem &&
          offsetParent.nodeType === 1
        ) {
          // Incorporate borders into its offset, since they are outside its content origin
          parentOffset = jQuery(offsetParent).offset();
          parentOffset.top += jQuery.css(offsetParent, "borderTopWidth", true);
          parentOffset.left += jQuery.css(
            offsetParent,
            "borderLeftWidth",
            true
          );
        }
      }

      // Subtract parent offsets and element margins
      return {
        top:
          offset.top - parentOffset.top - jQuery.css(elem, "marginTop", true),
        left:
          offset.left -
          parentOffset.left -
          jQuery.css(elem, "marginLeft", true),
      };
    },

    // This method will return documentElement in the following cases:
    // 1) For the element inside the iframe without offsetParent, this method will return
    //    documentElement of the parent window
    // 2) For the hidden or detached element
    // 3) For body or html element, i.e. in case of the html node - it will return itself
    //
    // but those exceptions were never presented as a real life use-cases
    // and might be considered as more preferable results.
    //
    // This logic, however, is not guaranteed and can change at any point in the future
    offsetParent: function () {
      return this.map(function () {
        var offsetParent = this.offsetParent;

        while (
          offsetParent &&
          jQuery.css(offsetParent, "position") === "static"
        ) {
          offsetParent = offsetParent.offsetParent;
        }

        return offsetParent || documentElement;
      });
    },
  });

  // Create scrollLeft and scrollTop methods
  jQuery.each(
    { scrollLeft: "pageXOffset", scrollTop: "pageYOffset" },
    function (method, prop) {
      var top = "pageYOffset" === prop;

      jQuery.fn[method] = function (val) {
        return access(
          this,
          function (elem, method, val) {
            // Coalesce documents and windows
            var win;
            if (isWindow(elem)) {
              win = elem;
            } else if (elem.nodeType === 9) {
              win = elem.defaultView;
            }

            if (val === undefined) {
              return win ? win[prop] : elem[method];
            }

            if (win) {
              win.scrollTo(
                !top ? val : win.pageXOffset,
                top ? val : win.pageYOffset
              );
            } else {
              elem[method] = val;
            }
          },
          method,
          val,
          arguments.length
        );
      };
    }
  );

  // Support: Safari <=7 - 9.1, Chrome <=37 - 49
  // Add the top/left cssHooks using jQuery.fn.position
  // Webkit bug: https://bugs.webkit.org/show_bug.cgi?id=29084
  // Blink bug: https://bugs.chromium.org/p/chromium/issues/detail?id=589347
  // getComputedStyle returns percent when specified for top/left/bottom/right;
  // rather than make the css module depend on the offset module, just check for it here
  jQuery.each(["top", "left"], function (_i, prop) {
    jQuery.cssHooks[prop] = addGetHookIf(support.pixelPosition, function (
      elem,
      computed
    ) {
      if (computed) {
        computed = curCSS(elem, prop);

        // If curCSS returns percentage, fallback to offset
        return rnumnonpx.test(computed)
          ? jQuery(elem).position()[prop] + "px"
          : computed;
      }
    });
  });

  // Create innerHeight, innerWidth, height, width, outerHeight and outerWidth methods
  jQuery.each({ Height: "height", Width: "width" }, function (name, type) {
    jQuery.each(
      {
        padding: "inner" + name,
        content: type,
        "": "outer" + name,
      },
      function (defaultExtra, funcName) {
        // Margin is only for outerHeight, outerWidth
        jQuery.fn[funcName] = function (margin, value) {
          var chainable =
              arguments.length && (defaultExtra || typeof margin !== "boolean"),
            extra =
              defaultExtra ||
              (margin === true || value === true ? "margin" : "border");

          return access(
            this,
            function (elem, type, value) {
              var doc;

              if (isWindow(elem)) {
                // $( window ).outerWidth/Height return w/h including scrollbars (gh-1729)
                return funcName.indexOf("outer") === 0
                  ? elem["inner" + name]
                  : elem.document.documentElement["client" + name];
              }

              // Get document width or height
              if (elem.nodeType === 9) {
                doc = elem.documentElement;

                // Either scroll[Width/Height] or offset[Width/Height] or client[Width/Height],
                // whichever is greatest
                return Math.max(
                  elem.body["scroll" + name],
                  doc["scroll" + name],
                  elem.body["offset" + name],
                  doc["offset" + name],
                  doc["client" + name]
                );
              }

              return value === undefined
                ? // Get width or height on the element, requesting but not forcing parseFloat
                  jQuery.css(elem, type, extra)
                : // Set width or height on the element
                  jQuery.style(elem, type, value, extra);
            },
            type,
            chainable ? margin : undefined,
            chainable
          );
        };
      }
    );
  });

  jQuery.each(
    [
      "ajaxStart",
      "ajaxStop",
      "ajaxComplete",
      "ajaxError",
      "ajaxSuccess",
      "ajaxSend",
    ],
    function (_i, type) {
      jQuery.fn[type] = function (fn) {
        return this.on(type, fn);
      };
    }
  );

  jQuery.fn.extend({
    bind: function (types, data, fn) {
      return this.on(types, null, data, fn);
    },
    unbind: function (types, fn) {
      return this.off(types, null, fn);
    },

    delegate: function (selector, types, data, fn) {
      return this.on(types, selector, data, fn);
    },
    undelegate: function (selector, types, fn) {
      // ( namespace ) or ( selector, types [, fn] )
      return arguments.length === 1
        ? this.off(selector, "**")
        : this.off(types, selector || "**", fn);
    },

    hover: function (fnOver, fnOut) {
      return this.mouseenter(fnOver).mouseleave(fnOut || fnOver);
    },
  });

  jQuery.each(
    (
      "blur focus focusin focusout resize scroll click dblclick " +
      "mousedown mouseup mousemove mouseover mouseout mouseenter mouseleave " +
      "change select submit keydown keypress keyup contextmenu"
    ).split(" "),
    function (_i, name) {
      // Handle event binding
      jQuery.fn[name] = function (data, fn) {
        return arguments.length > 0
          ? this.on(name, null, data, fn)
          : this.trigger(name);
      };
    }
  );

  // Support: Android <=4.0 only
  // Make sure we trim BOM and NBSP
  var rtrim = /^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g;

  // Bind a function to a context, optionally partially applying any
  // arguments.
  // jQuery.proxy is deprecated to promote standards (specifically Function#bind)
  // However, it is not slated for removal any time soon
  jQuery.proxy = function (fn, context) {
    var tmp, args, proxy;

    if (typeof context === "string") {
      tmp = fn[context];
      context = fn;
      fn = tmp;
    }

    // Quick check to determine if target is callable, in the spec
    // this throws a TypeError, but we will just return undefined.
    if (!isFunction(fn)) {
      return undefined;
    }

    // Simulated bind
    args = slice.call(arguments, 2);
    proxy = function () {
      return fn.apply(context || this, args.concat(slice.call(arguments)));
    };

    // Set the guid of unique handler to the same of original handler, so it can be removed
    proxy.guid = fn.guid = fn.guid || jQuery.guid++;

    return proxy;
  };

  jQuery.holdReady = function (hold) {
    if (hold) {
      jQuery.readyWait++;
    } else {
      jQuery.ready(true);
    }
  };
  jQuery.isArray = Array.isArray;
  jQuery.parseJSON = JSON.parse;
  jQuery.nodeName = nodeName;
  jQuery.isFunction = isFunction;
  jQuery.isWindow = isWindow;
  jQuery.camelCase = camelCase;
  jQuery.type = toType;

  jQuery.now = Date.now;

  jQuery.isNumeric = function (obj) {
    // As of jQuery 3.0, isNumeric is limited to
    // strings and numbers (primitives or objects)
    // that can be coerced to finite numbers (gh-2662)
    var type = jQuery.type(obj);
    return (
      (type === "number" || type === "string") &&
      // parseFloat NaNs numeric-cast false positives ("")
      // ...but misinterprets leading-number strings, particularly hex literals ("0x...")
      // subtraction forces infinities to NaN
      !isNaN(obj - parseFloat(obj))
    );
  };

  jQuery.trim = function (text) {
    return text == null ? "" : (text + "").replace(rtrim, "");
  };

  // Register as a named AMD module, since jQuery can be concatenated with other
  // files that may use define, but not via a proper concatenation script that
  // understands anonymous AMD modules. A named AMD is safest and most robust
  // way to register. Lowercase jquery is used because AMD module names are
  // derived from file names, and jQuery is normally delivered in a lowercase
  // file name. Do this after creating the global so that if an AMD module wants
  // to call noConflict to hide this version of jQuery, it will work.

  // Note that for maximum portability, libraries that are not jQuery should
  // declare themselves as anonymous modules, and avoid setting a global if an
  // AMD loader is present. jQuery is a special case. For more information, see
  // https://github.com/jrburke/requirejs/wiki/Updating-existing-libraries#wiki-anon

  if (typeof define === "function" && define.amd) {
    define("jquery", [], function () {
      return jQuery;
    });
  }

  var // Map over jQuery in case of overwrite
    _jQuery = window.jQuery,
    // Map over the $ in case of overwrite
    _$ = window.$;

  jQuery.noConflict = function (deep) {
    if (window.$ === jQuery) {
      window.$ = _$;
    }

    if (deep && window.jQuery === jQuery) {
      window.jQuery = _jQuery;
    }

    return jQuery;
  };

  // Expose jQuery and $ identifiers, even in AMD
  // (#7102#comment:10, https://github.com/jquery/jquery/pull/557)
  // and CommonJS for browser emulators (#13566)
  if (typeof noGlobal === "undefined") {
    window.jQuery = window.$ = jQuery;
  }

  return jQuery;
});

var BiwaScheme = (function () {
  //
  // variables
  //
  const TopEnv = {};
  const CoreEnv = {};

  //
  // Nil
  // javascript representation of empty list( '() )
  //
  const nil$1 = {
    toString: function () {
      return "nil";
    },
    to_write: function () {
      return "()";
    },
    to_array: function () {
      return [];
    },
    length: function () {
      return 0;
    },
    // Moved to main.js to avoid circular dependency
    //to_set: function() { return new BiwaSet(); },
  };

  //
  // #<undef> (The undefined value)
  // also used as #<unspecified> values
  //
  const undef = new Object();
  undef.toString = function () {
    return "#<undef>";
  };

  //
  // Configurations
  //

  // Maximum depth of stack trace
  // (You can also set Interpreter#max_trace_size for each Interpreter)
  const max_trace_size = 40;

  // Stop showing deprecation warning
  const suppress_deprecation_warning = false;

  // Actual values are set by rollup (see rollup.config.js)
  const VERSION = "0.7.2";
  const GitCommit = "58a7f6a464bbd6501dbe54c587db44e0d4a2d5b6";

  //     Underscore.js 1.10.2
  //     https://underscorejs.org
  //     (c) 2009-2020 Jeremy Ashkenas, DocumentCloud and Investigative Reporters & Editors
  //     Underscore may be freely distributed under the MIT license.

  // Baseline setup
  // --------------

  // Establish the root object, `window` (`self`) in the browser, `global`
  // on the server, or `this` in some virtual machines. We use `self`
  // instead of `window` for `WebWorker` support.
  var root =
    (typeof self == "object" && self.self === self && self) ||
    (typeof global == "object" && global.global === global && global) ||
    Function("return this")() ||
    {};

  // Save bytes in the minified (but not gzipped) version:
  var ArrayProto = Array.prototype,
    ObjProto = Object.prototype;

  // Create quick reference variables for speed access to core prototypes.
  var slice = ArrayProto.slice,
    toString = ObjProto.toString,
    hasOwnProperty = ObjProto.hasOwnProperty;

  // All **ECMAScript 5** native function implementations that we hope to use
  // are declared here.
  var nativeIsArray = Array.isArray,
    nativeKeys = Object.keys,
    nativeCreate = Object.create;

  // Create references to these builtin functions because we override them.
  var _isNaN = root.isNaN;
  root.isFinite;

  // Naked function reference for surrogate-prototype-swapping.
  var Ctor = function () {};

  // The Underscore object. All exported functions below are added to it in the
  // modules/index-all.js using the mixin function.
  function _(obj) {
    if (obj instanceof _) return obj;
    if (!(this instanceof _)) return new _(obj);
    this._wrapped = obj;
  }

  // Current version.
  _.VERSION = "1.10.2";

  // Internal function that returns an efficient (for current engines) version
  // of the passed-in callback, to be repeatedly applied in other Underscore
  // functions.
  function optimizeCb(func, context, argCount) {
    if (context === void 0) return func;
    switch (argCount == null ? 3 : argCount) {
      case 1:
        return function (value) {
          return func.call(context, value);
        };
      // The 2-argument case is omitted because we’re not using it.
      case 3:
        return function (value, index, collection) {
          return func.call(context, value, index, collection);
        };
      case 4:
        return function (accumulator, value, index, collection) {
          return func.call(context, accumulator, value, index, collection);
        };
    }
    return function () {
      return func.apply(context, arguments);
    };
  }

  // An internal function to generate callbacks that can be applied to each
  // element in a collection, returning the desired result — either `identity`,
  // an arbitrary callback, a property matcher, or a property accessor.
  function baseIteratee(value, context, argCount) {
    if (value == null) return identity;
    if (isFunction$1(value)) return optimizeCb(value, context, argCount);
    if (isObject(value) && !isArray(value)) return matcher(value);
    return property(value);
  }

  // External wrapper for our callback generator. Users may customize
  // `_.iteratee` if they want additional predicate/iteratee shorthand styles.
  // This abstraction hides the internal-only argCount argument.
  _.iteratee = iteratee;
  function iteratee(value, context) {
    return baseIteratee(value, context, Infinity);
  }

  // The function we actually call internally. It invokes _.iteratee if
  // overridden, otherwise baseIteratee.
  function cb(value, context, argCount) {
    if (_.iteratee !== iteratee) return _.iteratee(value, context);
    return baseIteratee(value, context, argCount);
  }

  // Some functions take a variable number of arguments, or a few expected
  // arguments at the beginning and then a variable number of values to operate
  // on. This helper accumulates all remaining arguments past the function’s
  // argument length (or an explicit `startIndex`), into an array that becomes
  // the last argument. Similar to ES6’s "rest parameter".
  function restArguments(func, startIndex) {
    startIndex = startIndex == null ? func.length - 1 : +startIndex;
    return function () {
      var length = Math.max(arguments.length - startIndex, 0),
        rest = Array(length),
        index = 0;
      for (; index < length; index++) {
        rest[index] = arguments[index + startIndex];
      }
      switch (startIndex) {
        case 0:
          return func.call(this, rest);
        case 1:
          return func.call(this, arguments[0], rest);
        case 2:
          return func.call(this, arguments[0], arguments[1], rest);
      }
      var args = Array(startIndex + 1);
      for (index = 0; index < startIndex; index++) {
        args[index] = arguments[index];
      }
      args[startIndex] = rest;
      return func.apply(this, args);
    };
  }

  // An internal function for creating a new object that inherits from another.
  function baseCreate(prototype) {
    if (!isObject(prototype)) return {};
    if (nativeCreate) return nativeCreate(prototype);
    Ctor.prototype = prototype;
    var result = new Ctor();
    Ctor.prototype = null;
    return result;
  }

  function shallowProperty(key) {
    return function (obj) {
      return obj == null ? void 0 : obj[key];
    };
  }

  function _has(obj, path) {
    return obj != null && hasOwnProperty.call(obj, path);
  }

  function deepGet(obj, path) {
    var length = path.length;
    for (var i = 0; i < length; i++) {
      if (obj == null) return void 0;
      obj = obj[path[i]];
    }
    return length ? obj : void 0;
  }

  // Helper for collection methods to determine whether a collection
  // should be iterated as an array or as an object.
  // Related: https://people.mozilla.org/~jorendorff/es6-draft.html#sec-tolength
  // Avoids a very nasty iOS 8 JIT bug on ARM-64. #2094
  var MAX_ARRAY_INDEX = Math.pow(2, 53) - 1;
  var getLength = shallowProperty("length");
  function isArrayLike(collection) {
    var length = getLength(collection);
    return (
      typeof length == "number" && length >= 0 && length <= MAX_ARRAY_INDEX
    );
  }

  // Collection Functions
  // --------------------

  // The cornerstone, an `each` implementation, aka `forEach`.
  // Handles raw objects in addition to array-likes. Treats all
  // sparse array-likes as if they were dense.
  function each(obj, iteratee, context) {
    iteratee = optimizeCb(iteratee, context);
    var i, length;
    if (isArrayLike(obj)) {
      for (i = 0, length = obj.length; i < length; i++) {
        iteratee(obj[i], i, obj);
      }
    } else {
      var _keys = keys(obj);
      for (i = 0, length = _keys.length; i < length; i++) {
        iteratee(obj[_keys[i]], _keys[i], obj);
      }
    }
    return obj;
  }

  // Return the results of applying the iteratee to each element.
  function map(obj, iteratee, context) {
    iteratee = cb(iteratee, context);
    var _keys = !isArrayLike(obj) && keys(obj),
      length = (_keys || obj).length,
      results = Array(length);
    for (var index = 0; index < length; index++) {
      var currentKey = _keys ? _keys[index] : index;
      results[index] = iteratee(obj[currentKey], currentKey, obj);
    }
    return results;
  }

  // Create a reducing function iterating left or right.
  function createReduce(dir) {
    // Wrap code that reassigns argument variables in a separate function than
    // the one that accesses `arguments.length` to avoid a perf hit. (#1991)
    var reducer = function (obj, iteratee, memo, initial) {
      var _keys = !isArrayLike(obj) && keys(obj),
        length = (_keys || obj).length,
        index = dir > 0 ? 0 : length - 1;
      if (!initial) {
        memo = obj[_keys ? _keys[index] : index];
        index += dir;
      }
      for (; index >= 0 && index < length; index += dir) {
        var currentKey = _keys ? _keys[index] : index;
        memo = iteratee(memo, obj[currentKey], currentKey, obj);
      }
      return memo;
    };

    return function (obj, iteratee, memo, context) {
      var initial = arguments.length >= 3;
      return reducer(obj, optimizeCb(iteratee, context, 4), memo, initial);
    };
  }

  // **Reduce** builds up a single result from a list of values, aka `inject`,
  // or `foldl`.
  var reduce = createReduce(1);

  // Return all the elements that pass a truth test.
  function filter(obj, predicate, context) {
    var results = [];
    predicate = cb(predicate, context);
    each(obj, function (value, index, list) {
      if (predicate(value, index, list)) results.push(value);
    });
    return results;
  }

  // Determine whether all of the elements match a truth test.
  function every(obj, predicate, context) {
    predicate = cb(predicate, context);
    var _keys = !isArrayLike(obj) && keys(obj),
      length = (_keys || obj).length;
    for (var index = 0; index < length; index++) {
      var currentKey = _keys ? _keys[index] : index;
      if (!predicate(obj[currentKey], currentKey, obj)) return false;
    }
    return true;
  }

  // Determine if at least one element in the object matches a truth test.
  function some(obj, predicate, context) {
    predicate = cb(predicate, context);
    var _keys = !isArrayLike(obj) && keys(obj),
      length = (_keys || obj).length;
    for (var index = 0; index < length; index++) {
      var currentKey = _keys ? _keys[index] : index;
      if (predicate(obj[currentKey], currentKey, obj)) return true;
    }
    return false;
  }

  // Determine if the array or object contains a given item (using `===`).
  function contains(obj, item, fromIndex, guard) {
    if (!isArrayLike(obj)) obj = values(obj);
    if (typeof fromIndex != "number" || guard) fromIndex = 0;
    return indexOf(obj, item, fromIndex) >= 0;
  }

  // Invoke a method (with arguments) on every item in a collection.
  restArguments(function (obj, path, args) {
    var contextPath, func;
    if (isFunction$1(path)) {
      func = path;
    } else if (isArray(path)) {
      contextPath = path.slice(0, -1);
      path = path[path.length - 1];
    }
    return map(obj, function (context) {
      var method = func;
      if (!method) {
        if (contextPath && contextPath.length) {
          context = deepGet(context, contextPath);
        }
        if (context == null) return void 0;
        method = context[path];
      }
      return method == null ? method : method.apply(context, args);
    });
  });

  // Convenience version of a common use case of `map`: fetching a property.
  function pluck(obj, key) {
    return map(obj, property(key));
  }

  // Return the maximum element (or element-based computation).
  function max(obj, iteratee, context) {
    var result = -Infinity,
      lastComputed = -Infinity,
      value,
      computed;
    if (
      iteratee == null ||
      (typeof iteratee == "number" && typeof obj[0] != "object" && obj != null)
    ) {
      obj = isArrayLike(obj) ? obj : values(obj);
      for (var i = 0, length = obj.length; i < length; i++) {
        value = obj[i];
        if (value != null && value > result) {
          result = value;
        }
      }
    } else {
      iteratee = cb(iteratee, context);
      each(obj, function (v, index, list) {
        computed = iteratee(v, index, list);
        if (
          computed > lastComputed ||
          (computed === -Infinity && result === -Infinity)
        ) {
          result = v;
          lastComputed = computed;
        }
      });
    }
    return result;
  }

  var reStrSymbol = /[^\ud800-\udfff]|[\ud800-\udbff][\udc00-\udfff]|[\ud800-\udfff]/g;
  // Safely create a real, live array from anything iterable.
  function toArray(obj) {
    if (!obj) return [];
    if (isArray(obj)) return slice.call(obj);
    if (isString$1(obj)) {
      // Keep surrogate pair characters together
      return obj.match(reStrSymbol);
    }
    if (isArrayLike(obj)) return map(obj, identity);
    return values(obj);
  }

  // Get the last element of an array. Passing **n** will return the last N
  // values in the array.
  function last(array, n, guard) {
    if (array == null || array.length < 1) return n == null ? void 0 : [];
    if (n == null || guard) return array[array.length - 1];
    return rest(array, Math.max(0, array.length - n));
  }

  // Returns everything but the first entry of the array. Especially useful on
  // the arguments object. Passing an **n** will return the rest N values in the
  // array.
  function rest(array, n, guard) {
    return slice.call(array, n == null || guard ? 1 : n);
  }

  // Internal implementation of a recursive `flatten` function.
  function _flatten(input, shallow, strict, output) {
    output = output || [];
    var idx = output.length;
    for (var i = 0, length = getLength(input); i < length; i++) {
      var value = input[i];
      if (isArrayLike(value) && (isArray(value) || isArguments(value))) {
        // Flatten current level of array or arguments object.
        if (shallow) {
          var j = 0,
            len = value.length;
          while (j < len) output[idx++] = value[j++];
        } else {
          _flatten(value, shallow, strict, output);
          idx = output.length;
        }
      } else if (!strict) {
        output[idx++] = value;
      }
    }
    return output;
  }

  // Return a version of the array that does not contain the specified value(s).
  restArguments(function (array, otherArrays) {
    return difference(array, otherArrays);
  });

  // Produce a duplicate-free version of the array. If the array has already
  // been sorted, you have the option of using a faster algorithm.
  // The faster algorithm will not work with an iteratee if the iteratee
  // is not a one-to-one function, so providing an iteratee will disable
  // the faster algorithm.
  function uniq(array, isSorted, iteratee, context) {
    if (!isBoolean$1(isSorted)) {
      context = iteratee;
      iteratee = isSorted;
      isSorted = false;
    }
    if (iteratee != null) iteratee = cb(iteratee, context);
    var result = [];
    var seen = [];
    for (var i = 0, length = getLength(array); i < length; i++) {
      var value = array[i],
        computed = iteratee ? iteratee(value, i, array) : value;
      if (isSorted && !iteratee) {
        if (!i || seen !== computed) result.push(value);
        seen = computed;
      } else if (iteratee) {
        if (!contains(seen, computed)) {
          seen.push(computed);
          result.push(value);
        }
      } else if (!contains(result, value)) {
        result.push(value);
      }
    }
    return result;
  }

  // Produce an array that contains the union: each distinct element from all of
  // the passed-in arrays.
  restArguments(function (arrays) {
    return uniq(_flatten(arrays, true, true));
  });

  // Take the difference between one array and a number of other arrays.
  // Only the elements present in just the first array will remain.
  var difference = restArguments(function (array, rest) {
    rest = _flatten(rest, true, true);
    return filter(array, function (value) {
      return !contains(rest, value);
    });
  });

  // Complement of zip. Unzip accepts an array of arrays and groups
  // each array's elements on shared indices.
  function unzip(array) {
    var length = (array && max(array, getLength).length) || 0;
    var result = Array(length);

    for (var index = 0; index < length; index++) {
      result[index] = pluck(array, index);
    }
    return result;
  }

  // Zip together multiple lists into a single array -- elements that share
  // an index go together.
  restArguments(unzip);

  // Generator function to create the findIndex and findLastIndex functions.
  function createPredicateIndexFinder(dir) {
    return function (array, predicate, context) {
      predicate = cb(predicate, context);
      var length = getLength(array);
      var index = dir > 0 ? 0 : length - 1;
      for (; index >= 0 && index < length; index += dir) {
        if (predicate(array[index], index, array)) return index;
      }
      return -1;
    };
  }

  // Returns the first index on an array-like that passes a predicate test.
  var findIndex = createPredicateIndexFinder(1);

  // Use a comparator function to figure out the smallest index at which
  // an object should be inserted so as to maintain order. Uses binary search.
  function sortedIndex(array, obj, iteratee, context) {
    iteratee = cb(iteratee, context, 1);
    var value = iteratee(obj);
    var low = 0,
      high = getLength(array);
    while (low < high) {
      var mid = Math.floor((low + high) / 2);
      if (iteratee(array[mid]) < value) low = mid + 1;
      else high = mid;
    }
    return low;
  }

  // Generator function to create the indexOf and lastIndexOf functions.
  function createIndexFinder(dir, predicateFind, sortedIndex) {
    return function (array, item, idx) {
      var i = 0,
        length = getLength(array);
      if (typeof idx == "number") {
        if (dir > 0) {
          i = idx >= 0 ? idx : Math.max(idx + length, i);
        } else {
          length = idx >= 0 ? Math.min(idx + 1, length) : idx + length + 1;
        }
      } else if (sortedIndex && idx && length) {
        idx = sortedIndex(array, item);
        return array[idx] === item ? idx : -1;
      }
      if (item !== item) {
        idx = predicateFind(slice.call(array, i, length), isNaN$1);
        return idx >= 0 ? idx + i : -1;
      }
      for (
        idx = dir > 0 ? i : length - 1;
        idx >= 0 && idx < length;
        idx += dir
      ) {
        if (array[idx] === item) return idx;
      }
      return -1;
    };
  }

  // Return the position of the first occurrence of an item in an array,
  // or -1 if the item is not included in the array.
  // If the array is large and already in sort order, pass `true`
  // for **isSorted** to use binary search.
  var indexOf = createIndexFinder(1, findIndex, sortedIndex);

  // Function (ahem) Functions
  // ------------------

  // Determines whether to execute a function as a constructor
  // or a normal function with the provided arguments.
  function executeBound(sourceFunc, boundFunc, context, callingContext, args) {
    if (!(callingContext instanceof boundFunc))
      return sourceFunc.apply(context, args);
    var self = baseCreate(sourceFunc.prototype);
    var result = sourceFunc.apply(self, args);
    if (isObject(result)) return result;
    return self;
  }

  // Create a function bound to a given object (assigning `this`, and arguments,
  // optionally). Delegates to **ECMAScript 5**'s native `Function.bind` if
  // available.
  var bind = restArguments(function (func, context, args) {
    if (!isFunction$1(func))
      throw new TypeError("Bind must be called on a function");
    var bound = restArguments(function (callArgs) {
      return executeBound(func, bound, context, this, args.concat(callArgs));
    });
    return bound;
  });

  // Partially apply a function by creating a version that has had some of its
  // arguments pre-filled, without changing its dynamic `this` context. _ acts
  // as a placeholder by default, allowing any combination of arguments to be
  // pre-filled. Set `partial.placeholder` for a custom placeholder argument.
  var partial = restArguments(function (func, boundArgs) {
    var placeholder = partial.placeholder;
    var bound = function () {
      var position = 0,
        length = boundArgs.length;
      var args = Array(length);
      for (var i = 0; i < length; i++) {
        args[i] =
          boundArgs[i] === placeholder ? arguments[position++] : boundArgs[i];
      }
      while (position < arguments.length) args.push(arguments[position++]);
      return executeBound(func, bound, this, this, args);
    };
    return bound;
  });

  partial.placeholder = _;

  // Bind a number of an object's methods to that object. Remaining arguments
  // are the method names to be bound. Useful for ensuring that all callbacks
  // defined on an object belong to it.
  restArguments(function (obj, _keys) {
    _keys = _flatten(_keys, false, false);
    var index = _keys.length;
    if (index < 1) throw new Error("bindAll must be passed function names");
    while (index--) {
      var key = _keys[index];
      obj[key] = bind(obj[key], obj);
    }
  });

  // Delays a function for the given number of milliseconds, and then calls
  // it with the arguments supplied.
  var delay = restArguments(function (func, wait, args) {
    return setTimeout(function () {
      return func.apply(null, args);
    }, wait);
  });

  // Defers a function, scheduling it to run after the current call stack has
  // cleared.
  partial(delay, _, 1);

  // Returns a negated version of the passed-in predicate.
  function negate(predicate) {
    return function () {
      return !predicate.apply(this, arguments);
    };
  }

  // Returns a function that will only be executed up to (but not including) the Nth call.
  function before(times, func) {
    var memo;
    return function () {
      if (--times > 0) {
        memo = func.apply(this, arguments);
      }
      if (times <= 1) func = null;
      return memo;
    };
  }

  // Returns a function that will be executed at most one time, no matter how
  // often you call it. Useful for lazy initialization.
  partial(before, 2);

  // Object Functions
  // ----------------

  // Keys in IE < 9 that won't be iterated by `for key in ...` and thus missed.
  var hasEnumBug = !{ toString: null }.propertyIsEnumerable("toString");
  var nonEnumerableProps = [
    "valueOf",
    "isPrototypeOf",
    "toString",
    "propertyIsEnumerable",
    "hasOwnProperty",
    "toLocaleString",
  ];

  function collectNonEnumProps(obj, _keys) {
    var nonEnumIdx = nonEnumerableProps.length;
    var constructor = obj.constructor;
    var proto =
      (isFunction$1(constructor) && constructor.prototype) || ObjProto;

    // Constructor is a special case.
    var prop = "constructor";
    if (_has(obj, prop) && !contains(_keys, prop)) _keys.push(prop);

    while (nonEnumIdx--) {
      prop = nonEnumerableProps[nonEnumIdx];
      if (prop in obj && obj[prop] !== proto[prop] && !contains(_keys, prop)) {
        _keys.push(prop);
      }
    }
  }

  // Retrieve the names of an object's own properties.
  // Delegates to **ECMAScript 5**'s native `Object.keys`.
  function keys(obj) {
    if (!isObject(obj)) return [];
    if (nativeKeys) return nativeKeys(obj);
    var _keys = [];
    for (var key in obj) if (_has(obj, key)) _keys.push(key);
    // Ahem, IE < 9.
    if (hasEnumBug) collectNonEnumProps(obj, _keys);
    return _keys;
  }

  // Retrieve all the property names of an object.
  function allKeys(obj) {
    if (!isObject(obj)) return [];
    var _keys = [];
    for (var key in obj) _keys.push(key);
    // Ahem, IE < 9.
    if (hasEnumBug) collectNonEnumProps(obj, _keys);
    return _keys;
  }

  // Retrieve the values of an object's properties.
  function values(obj) {
    var _keys = keys(obj);
    var length = _keys.length;
    var values = Array(length);
    for (var i = 0; i < length; i++) {
      values[i] = obj[_keys[i]];
    }
    return values;
  }

  // Invert the keys and values of an object. The values must be serializable.
  function invert(obj) {
    var result = {};
    var _keys = keys(obj);
    for (var i = 0, length = _keys.length; i < length; i++) {
      result[obj[_keys[i]]] = _keys[i];
    }
    return result;
  }

  // An internal function for creating assigner functions.
  function createAssigner(keysFunc, defaults) {
    return function (obj) {
      var length = arguments.length;
      if (defaults) obj = Object(obj);
      if (length < 2 || obj == null) return obj;
      for (var index = 1; index < length; index++) {
        var source = arguments[index],
          _keys = keysFunc(source),
          l = _keys.length;
        for (var i = 0; i < l; i++) {
          var key = _keys[i];
          if (!defaults || obj[key] === void 0) obj[key] = source[key];
        }
      }
      return obj;
    };
  }

  // Extend a given object with all the properties in passed-in object(s).
  var extend = createAssigner(allKeys);

  // Assigns a given object with all the own properties in the passed-in object(s).
  // (https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Object/assign)
  var extendOwn = createAssigner(keys);

  // Internal pick helper function to determine if `obj` has key `key`.
  function keyInObj(value, key, obj) {
    return key in obj;
  }

  // Return a copy of the object only containing the whitelisted properties.
  var pick = restArguments(function (obj, _keys) {
    var result = {},
      iteratee = _keys[0];
    if (obj == null) return result;
    if (isFunction$1(iteratee)) {
      if (_keys.length > 1) iteratee = optimizeCb(iteratee, _keys[1]);
      _keys = allKeys(obj);
    } else {
      iteratee = keyInObj;
      _keys = _flatten(_keys, false, false);
      obj = Object(obj);
    }
    for (var i = 0, length = _keys.length; i < length; i++) {
      var key = _keys[i];
      var value = obj[key];
      if (iteratee(value, key, obj)) result[key] = value;
    }
    return result;
  });

  // Return a copy of the object without the blacklisted properties.
  restArguments(function (obj, _keys) {
    var iteratee = _keys[0],
      context;
    if (isFunction$1(iteratee)) {
      iteratee = negate(iteratee);
      if (_keys.length > 1) context = _keys[1];
    } else {
      _keys = map(_flatten(_keys, false, false), String);
      iteratee = function (value, key) {
        return !contains(_keys, key);
      };
    }
    return pick(obj, iteratee, context);
  });

  // Create a (shallow-cloned) duplicate of an object.
  function clone(obj) {
    if (!isObject(obj)) return obj;
    return isArray(obj) ? obj.slice() : extend({}, obj);
  }

  // Returns whether an object has a given set of `key:value` pairs.
  function isMatch(object, attrs) {
    var _keys = keys(attrs),
      length = _keys.length;
    if (object == null) return !length;
    var obj = Object(object);
    for (var i = 0; i < length; i++) {
      var key = _keys[i];
      if (attrs[key] !== obj[key] || !(key in obj)) return false;
    }
    return true;
  }

  // Internal function for creating a toString-based type tester.
  function tagTester(name) {
    return function (obj) {
      return toString.call(obj) === "[object " + name + "]";
    };
  }

  // Is a given value an array?
  // Delegates to ECMA5's native Array.isArray
  var isArray = nativeIsArray || tagTester("Array");

  // Is a given variable an object?
  function isObject(obj) {
    var type = typeof obj;
    return type === "function" || (type === "object" && !!obj);
  }

  // Add some isType methods: isArguments, isFunction, isString, isNumber, isDate, isRegExp, isError, isMap, isWeakMap, isSet, isWeakSet.
  var isArguments = tagTester("Arguments");
  var isFunction$1 = tagTester("Function");
  var isString$1 = tagTester("String");
  var isNumber$2 = tagTester("Number");

  // Define a fallback version of the method in browsers (ahem, IE < 9), where
  // there isn't any inspectable "Arguments" type.
  (function () {
    if (!isArguments(arguments)) {
      isArguments = function (obj) {
        return _has(obj, "callee");
      };
    }
  })();

  // Optimize `isFunction` if appropriate. Work around some typeof bugs in old v8,
  // IE 11 (#1621), Safari 8 (#1929), and PhantomJS (#2236).
  var nodelist = root.document && root.document.childNodes;
  if (
    typeof /./ != "function" &&
    typeof Int8Array != "object" &&
    typeof nodelist != "function"
  ) {
    isFunction$1 = function (obj) {
      return typeof obj == "function" || false;
    };
  }

  // Is the given value `NaN`?
  function isNaN$1(obj) {
    return isNumber$2(obj) && _isNaN(obj);
  }

  // Is a given value a boolean?
  function isBoolean$1(obj) {
    return (
      obj === true || obj === false || toString.call(obj) === "[object Boolean]"
    );
  }

  // Is a given value equal to null?
  function isNull(obj) {
    return obj === null;
  }

  // Is a given variable undefined?
  function isUndefined(obj) {
    return obj === void 0;
  }

  // Utility Functions
  // -----------------

  // Keep the identity function around for default iteratees.
  function identity(value) {
    return value;
  }

  // Creates a function that, when passed an object, will traverse that object’s
  // properties down the given `path`, specified as an array of keys or indexes.
  function property(path) {
    if (!isArray(path)) {
      return shallowProperty(path);
    }
    return function (obj) {
      return deepGet(obj, path);
    };
  }

  // Returns a predicate for checking whether an object has a given set of
  // `key:value` pairs.
  function matcher(attrs) {
    attrs = extendOwn({}, attrs);
    return function (obj) {
      return isMatch(obj, attrs);
    };
  }

  // Run a function **n** times.
  function times(n, iteratee, context) {
    var accum = Array(Math.max(0, n));
    iteratee = optimizeCb(iteratee, context, 1);
    for (var i = 0; i < n; i++) accum[i] = iteratee(i);
    return accum;
  }

  // List of HTML entities for escaping.
  var escapeMap = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': "&quot;",
    "'": "&#x27;",
    "`": "&#x60;",
  };
  var unescapeMap = invert(escapeMap);

  // Functions for escaping and unescaping strings to/from HTML interpolation.
  function createEscaper(map) {
    var escaper = function (match) {
      return map[match];
    };
    // Regexes for identifying a key that needs to be escaped.
    var source = "(?:" + keys(map).join("|") + ")";
    var testRegexp = RegExp(source);
    var replaceRegexp = RegExp(source, "g");
    return function (string) {
      string = string == null ? "" : "" + string;
      return testRegexp.test(string)
        ? string.replace(replaceRegexp, escaper)
        : string;
    };
  }
  var escape = createEscaper(escapeMap);
  createEscaper(unescapeMap);

  // Generate a unique integer id (unique within the entire client session).
  // Useful for temporary DOM ids.
  var idCounter = 0;
  function uniqueId(prefix) {
    var id = ++idCounter + "";
    return prefix ? prefix + id : id;
  }

  // By default, Underscore uses ERB-style template delimiters, change the
  // following template settings to use alternative delimiters.
  _.templateSettings = {
    evaluate: /<%([\s\S]+?)%>/g,
    interpolate: /<%=([\s\S]+?)%>/g,
    escape: /<%-([\s\S]+?)%>/g,
  };

  // OOP
  // ---------------
  // If Underscore is called as a function, it returns a wrapped object that
  // can be used OO-style. This wrapper holds altered versions of all the
  // underscore functions. Wrapped objects may be chained.

  // Helper function to continue chaining intermediate results.
  function chainResult(instance, obj) {
    return instance._chain ? _(obj).chain() : obj;
  }

  // Add all mutator Array functions to the wrapper.
  each(
    ["pop", "push", "reverse", "shift", "sort", "splice", "unshift"],
    function (name) {
      var method = ArrayProto[name];
      _.prototype[name] = function () {
        var obj = this._wrapped;
        method.apply(obj, arguments);
        if ((name === "shift" || name === "splice") && obj.length === 0)
          delete obj[0];
        return chainResult(this, obj);
      };
    }
  );

  // Add all accessor Array functions to the wrapper.
  each(["concat", "join", "slice"], function (name) {
    var method = ArrayProto[name];
    _.prototype[name] = function () {
      return chainResult(this, method.apply(this._wrapped, arguments));
    };
  });

  // Extracts the result from a wrapped and chained object.
  _.prototype.value = function () {
    return this._wrapped;
  };

  // Provide unwrapping proxy for some methods used in engine operations
  // such as arithmetic and JSON stringification.
  _.prototype.valueOf = _.prototype.toJSON = _.prototype.value;

  _.prototype.toString = function () {
    return String(this._wrapped);
  };

  //
  // Super-simple class implementation
  //
  // Example usage:
  //
  // BiwaScheme.Foo = BiwaScheme.Class.create({
  //   initialize: function(a){
  //     this.a = a;
  //   },
  //
  //   toString: function(){
  //     return "foo[" + this.a + "]";
  //   }
  // });
  //
  // BiwaScheme.Bar = BiwaScheme.Class.extend(new BiwaScheme.Foo("hello1"), {
  //   initialize: function(b){
  //     this.b = b;
  //   },
  //
  //   printEverything: function(){
  //     console.log("a = ", this.a, "b = ", this.b);
  //   },
  //
  //   toString: function(){
  //     return "bar[" + this.b + "]";
  //   }
  // });

  const Class = {
    create: function (methods) {
      var klass = function () {
        this.initialize.apply(this, arguments);
      };
      extend(klass.prototype, methods);
      return klass;
    },

    extend: function (parent, methods) {
      var klass = function () {
        this.initialize.apply(this, arguments);
      };
      klass.prototype = parent;
      extend(klass.prototype, methods);
      return klass;
    },
  };

  // Update the given method to memoized version.
  //
  // - klass : a class defined by BiwaScheme.Class.create
  // - name_or_names : method name (a string or an array of strings)
  //
  // Example
  //
  //   // Given this method
  //   BiwaScheme.Enumeration.EnumType = ...
  //     universe: function(){
  //       return ...
  //     }
  //   ...
  //   // Memoize
  //   BiwaScheme.Class.memoize(BiwaScheme.Enumeration.EnumType,
  //                            "_universe");
  //
  //   // Equal to:
  //   BiwaScheme.Enumeration.EnumType = ...
  //     universe: function(){
  //       if(!this.hasOwnProperty("cached_universe")){
  //         this.cached_universe = this.compute_universe();
  //       }
  //       return this.cached_universe;
  //     },
  //     compute_universe: function(){
  //       // Original function, renamed to compute_*
  //       return ...
  //     }
  //   ...
  Class.memoize = function (klass, name_or_names) {
    var proto = klass.prototype;
    var names = isArray(name_or_names) ? name_or_names : [name_or_names];

    each(names, function (name) {
      // Copy original function foo as 'compute_foo'
      proto["compute_" + name] = proto[name];

      // Define memoizing version
      proto[name] = function (/*arguments*/) {
        if (!this.hasOwnProperty("cached_" + name)) {
          this["cached_" + name] = this["compute_" + name].apply(
            this,
            toArray(arguments)
          );
        }
        return this["cached_" + name];
      };
    });
  };

  //
  // Symbol
  //

  const Symbols = {};

  const BiwaSymbol = Class.create({
    initialize: function (str) {
      this.name = str;
      Symbols[str] = this;
    },

    inspect: function () {
      return "'" + this.name;
      //return "#<Symbol '"+this.name+"'>";
    },

    toString: function () {
      return "'" + this.name;
    },

    to_write: function () {
      return this.name;
    },
  });

  const Sym = function (name, leaveCase) {
    if (Symbols[name] === undefined) {
      return new BiwaSymbol(name);
    } else if (!(Symbols[name] instanceof BiwaSymbol)) {
      //pre-defined member (like 'eval' in Firefox)
      return new BiwaSymbol(name);
    } else {
      return Symbols[name];
    }
  };

  const gensym = function () {
    return Sym(uniqueId("__gensym"));
  };

  //
  // write.js: Functions to convert objects to strings
  //

  //
  // write
  //

  const truncate = function (str, length) {
    const truncateStr = "...";
    return str.length > length ? str.slice(0, length) + truncateStr : str;
  };

  const to_write$1 = function (obj) {
    if (obj === undefined) return "undefined";
    else if (obj === null) return "null";
    else if (isFunction$1(obj))
      return (
        "#<Function " +
        (obj.fname
          ? obj.fname
          : obj.toSource
          ? truncate(obj.toSource(), 40)
          : "") +
        ">"
      );
    else if (isString$1(obj))
      return (
        '"' +
        obj
          .replace(/\\|\"/g, function ($0) {
            return "\\" + $0;
          })
          .replace(/\x07/g, "\\a")
          .replace(/\x08/g, "\\b")
          .replace(/\t/g, "\\t")
          .replace(/\n/g, "\\n")
          .replace(/\v/g, "\\v")
          .replace(/\f/g, "\\f")
          .replace(/\r/g, "\\r") +
        '"'
      );
    else if (isClosure(obj)) return "#<Closure>";
    else if (isArray(obj))
      return (
        "#(" +
        map(obj, function (e) {
          return to_write$1(e);
        }).join(" ") +
        ")"
      );
    else if (typeof obj.to_write == "function") return obj.to_write();
    else if (isNaN(obj) && typeof obj == "number") return "+nan.0";
    else {
      switch (obj) {
        case true:
          return "#t";
        case false:
          return "#f";
        case Infinity:
          return "+inf.0";
        case -Infinity:
          return "-inf.0";
      }
    }
    return inspect(obj);
  };

  //
  // display
  //

  const to_display = function (obj) {
    if (obj.to_display) return obj.to_display(to_display);
    if (isUndefined(obj)) return "undefined";
    else if (isNull(obj)) return "null";
    else if (typeof obj.valueOf() == "string") return obj;
    else if (obj instanceof BiwaSymbol) return obj.name;
    else if (obj instanceof Array)
      return "#(" + map(obj, to_display).join(" ") + ")";
    else return to_write$1(obj);
  };

  //
  // inspect
  //
  const inspect = function (object, opts) {
    try {
      if (isUndefined(object)) return "undefined";
      if (object === null) return "null";
      if (object === true) return "#t";
      if (object === false) return "#f";
      if (object.inspect) return object.inspect();
      if (isString$1(object)) {
        return '"' + object.replace(/"/g, '\\"') + '"';
      }
      if (isArray(object)) {
        return "[" + map(object, inspect).join(", ") + "]";
      }

      if (opts && opts["fallback"]) {
        return opts["fallback"];
      } else {
        return object.toString();
      }
    } catch (e) {
      if (e instanceof RangeError) return "...";
      throw e;
    }
  };

  //
  // Errors
  //

  const BiwaError = Class.create({
    initialize: function (msg, form = null) {
      const info = form === null ? "" : `: ${to_write$1(form)}`;
      this.message = `Error: ${msg}${info}`;
      this.form = form;
    },
    toString: function () {
      return this.message;
    },
  });

  const Bug$1 = Class.extend(new BiwaError(), {
    initialize: function (msg) {
      this.message = "[BUG] " + msg;
    },
  });

  // currently used by "raise"
  const UserError = Class.extend(new BiwaError(), {
    initialize: function (msg) {
      this.message = msg;
    },
  });

  //
  // Char
  //

  const Chars = {};

  const Char = Class.create({
    initialize: function (c) {
      Chars[(this.value = c)] = this;
    },
    to_write: function () {
      switch (this.value) {
        case "\n":
          return "#\\newline";
        case " ":
          return "#\\space";
        case "\t":
          return "#\\tab";
        default:
          return "#\\" + this.value;
      }
    },
    to_display: function () {
      return this.value;
    },
    inspect: function () {
      return this.to_write();
    },
  });

  Char.get = function (c) {
    if (typeof c != "string") {
      throw new Bug$1("Char.get: " + inspect(c) + " is not a string");
    }
    if (Chars[c] === undefined) return new Char(c);
    else return Chars[c];
  };

  //
  // pause object (facility to stop/resume interpreting)
  //
  var Pause = Class.create({
    //new (on_pause: javascript function calling setTimeout, Ajax.Request, ..)
    initialize: function (on_pause) {
      this.on_pause = on_pause;
    },

    //save state of interpreter
    set_state: function (intp, x, f, c, s) {
      this.interpreter = intp;
      this.x = x;
      this.f = f;
      this.c = c;
      this.s = s;
    },

    //call this when ready (to fire setTimeout, Ajax.Request..)
    ready: function () {
      this.on_pause(this);
    },

    //restart calculation
    resume: function (value) {
      return this.interpreter.resume(
        true,
        value,
        this.x,
        this.f,
        this.c,
        this.s
      );
    },
  });

  //
  // Port
  //

  // (eof-object)
  const eof = new Object();

  const Port = Class.create({
    initialize: function (is_in, is_out) {
      this.is_open = true;
      this.is_binary = false; //??
      this.is_input = is_in;
      this.is_output = is_out;
    },
    close: function () {
      // close port
      this.is_open = false;
    },
    inspect: function () {
      return "#<Port>";
    },
    to_write: function () {
      return "#<Port>";
    },
  });

  //
  // string ports (srfi-6)
  //
  Port.StringOutput = Class.extend(new Port(false, true), {
    initialize: function () {
      this.buffer = [];
    },
    put_string: function (str) {
      this.buffer.push(str);
    },
    output_string: function (str) {
      return this.buffer.join("");
    },
  });

  Port.StringInput = Class.extend(new Port(true, false), {
    initialize: function (str) {
      this.str = str;
    },
    get_string: function (after) {
      return after(this.str);
    },
  });

  Port.NullInput = Class.extend(new Port(true, false), {
    initialize: function () {},
    get_string: function (after) {
      // Never give them anything!
      return after("");
    },
  });

  Port.NullOutput = Class.extend(new Port(false, true), {
    initialize: function (output_function) {
      this.output_function = output_function;
    },
    put_string: function (str) {},
  });

  Port.CustomOutput = Class.extend(new Port(false, true), {
    initialize: function (output_function) {
      this.output_function = output_function;
    },
    put_string: function (str) {
      this.output_function(str);
    },
  });

  Port.CustomInput = Class.extend(new Port(true, false), {
    initialize: function (input_function) {
      this.input_function = input_function;
    },
    get_string: function (after) {
      var input_function = this.input_function;
      return new Pause(function (pause) {
        input_function(function (input) {
          pause.resume(after(input));
        });
      });
    },
  });

  // User must set the current input/output
  Port.current_input = new Port.NullInput();
  Port.current_output = new Port.NullOutput();
  Port.current_error = new Port.NullOutput();

  //

  const isNil = function (obj) {
    return obj === nil$1;
  };

  const isUndef = function (obj) {
    return obj === undef;
  };

  const isBoolean = isBoolean$1; // Return true if arg is either true or false

  //isNumber is defined in number.js (Return true if arg is scheme number)

  const isString = isString$1;

  const isFunction = isFunction$1;

  const isChar = function (obj) {
    return obj instanceof Char;
  };

  const isSymbol = function (obj) {
    return obj instanceof BiwaSymbol;
  };

  const isPort = function (obj) {
    return obj instanceof Port;
  };

  const isVector = function (obj) {
    return obj instanceof Array && obj.closure_p !== true;
  };

  // Returns true if `obj` is a Scheme closure.
  const isClosure = function (obj) {
    return obj instanceof Array && obj.closure_p === true;
  };

  // Change `ary` into a Scheme closure (destructive).
  const makeClosure = function (ary) {
    ary.closure_p = true;
    return ary;
  };

  // procedure: Scheme closure or JavaScript function
  // valid argument for anywhere function is expected
  const isProcedure = function (obj) {
    return isClosure(obj) || isFunction$1(obj);
  };

  // Return true if obj is a scheme value which evaluates to itself
  const isSelfEvaluating = function (obj) {
    return isBoolean(obj) || isNumber(obj) || isString(obj) || isChar(obj);
  };

  //
  // equality
  //
  const eq = function (a, b) {
    return a === b;
  };
  // TODO: Records (etc.)
  const eqv = function (a, b) {
    return a == b && typeof a == typeof b;
  };
  const equal = function (a, b) {
    //TODO: must terminate for cyclic objects
    return to_write$1(a) == to_write$1(b);
  };

  //
  // comaprator
  //
  // Return true when a < b
  const lt = function (a, b) {
    if (typeof a !== typeof b) {
      return compareFn(typeof a, typeof b);
    }
    return a < b;
  };

  //
  // Set - set of string
  // contents must be string (or at least sortable)
  //
  const BiwaSet = Class.create({
    initialize: function (/*args*/) {
      this.arr = [];
      var i;
      for (i = 0; i < arguments.length; i++) this.arr[i] = arguments[i];
    },

    equals: function (other) {
      if (this.arr.length != other.arr.length) return false;

      var a1 = clone(this.arr);
      var a2 = clone(other.arr);
      a1.sort();
      a2.sort();
      for (var i = 0; i < this.arr.length; i++) {
        if (a1[i] != a2[i]) return false;
      }
      return true;
    },
    set_cons: function (item) {
      var o = new BiwaSet(item);
      o.arr = clone(this.arr);
      o.arr.push(item);
      return o;
    },
    set_union: function (/*args*/) {
      var o = new BiwaSet();
      o.arr = clone(this.arr);

      for (var k = 0; k < arguments.length; k++) {
        var s2 = arguments[k];
        if (!(s2 instanceof BiwaSet))
          throw new BiwaError("set_union: arguments must be a set");

        for (var i = 0; i < s2.arr.length; i++) o.add(s2.arr[i]);
      }
      return o;
    },
    set_intersect: function (s2) {
      if (!(s2 instanceof BiwaSet))
        throw new BiwaError("set_intersect: arguments must be a set");

      var o = new BiwaSet();
      for (var i = 0; i < this.arr.length; i++)
        if (s2.member(this.arr[i])) o.add(this.arr[i]);
      return o;
    },
    set_minus: function (s2) {
      if (!(s2 instanceof BiwaSet))
        throw new BiwaError("set_minus: arguments must be a set");

      var o = new BiwaSet();
      for (var i = 0; i < this.arr.length; i++)
        if (!s2.member(this.arr[i])) o.add(this.arr[i]);
      return o;
    },
    add: function (item) {
      if (!this.member(item)) {
        this.arr.push(item);
      }
    },
    member: function (item) {
      for (var i = 0; i < this.arr.length; i++)
        if (this.arr[i] == item) return true;

      return false;
    },
    rindex: function (item) {
      for (var i = this.arr.length - 1; i >= 0; i--)
        if (this.arr[i] == item) return this.arr.length - 1 - i;

      return null;
    },
    index: function (item) {
      for (var i = 0; i < this.arr.length; i++)
        if (this.arr[i] == item) return i;

      return null;
    },
    inspect: function () {
      return "Set(" + this.arr.join(", ") + ")";
    },
    toString: function () {
      return this.inspect();
    },
    size: function () {
      return this.arr.length;
    },
  });

  //
  // Pair
  // cons cell
  //

  const Pair = Class.create({
    initialize: function (car, cdr) {
      this.car = car;
      this.cdr = cdr;
    },

    caar: function () {
      return this.car.car;
    },
    cadr: function () {
      return this.cdr.car;
    },
    cdar: function () {
      return this.cdr.car;
    },
    cddr: function () {
      return this.cdr.cdr;
    },

    first: function () {
      return this.car;
    },
    second: function () {
      return this.cdr.car;
    },
    third: function () {
      return this.cdr.cdr.car;
    },
    fourth: function () {
      return this.cdr.cdr.cdr.car;
    },
    fifth: function () {
      return this.cdr.cdr.cdr.cdr.car;
    },

    // returns array containing all the car's of list
    // '(1 2 3) => [1,2,3]
    // '(1 2 . 3) => [1,2]
    to_array: function () {
      var ary = [];
      for (var o = this; o instanceof Pair; o = o.cdr) {
        ary.push(o.car);
      }
      return ary;
    },

    to_set: function () {
      var set = new BiwaSet();
      for (var o = this; o instanceof Pair; o = o.cdr) {
        set.add(o.car);
      }
      return set;
    },

    length: function () {
      var n = 0;
      for (var o = this; o instanceof Pair; o = o.cdr) {
        n++;
      }
      return n;
    },

    // Return the last cdr
    last_cdr: function () {
      var o;
      for (o = this; o instanceof Pair; o = o.cdr);
      return o;
    },

    // calls the given func passing each car of list
    // returns cdr of last Pair
    forEach: function (func) {
      for (var o = this; o instanceof Pair; o = o.cdr) {
        func(o.car);
      }
      return o;
    },

    // Alias of `forEach` (for backward compatibility)
    foreach: function (func) {
      for (var o = this; o instanceof Pair; o = o.cdr) {
        func(o.car);
      }
      return o;
    },

    // Returns an array which contains the resuls of calling func
    // with the car's as an argument.
    // If the receiver is not a proper list, the last cdr is ignored.
    // The receiver must not be a cyclic list.
    map: function (func) {
      var ary = [];
      for (var o = this; isPair(o); o = o.cdr) {
        ary.push(func(o.car));
      }
      return ary;
    },

    // Returns a new list made by applying `func` to each element
    mapList: function (func) {
      return array_to_list(this.map(func));
    },

    // Destructively concat the given list to the receiver.
    // The receiver must be a proper list.
    // Returns the receiver.
    concat: function (list) {
      var o = this;
      while (o instanceof Pair && o.cdr != nil$1) {
        o = o.cdr;
      }
      o.cdr = list;
      return this;
    },

    // returns human-redable string of pair
    inspect: function (conv) {
      conv || (conv = inspect);
      var a = [];
      var last = this.foreach(function (o) {
        a.push(conv(o));
      });
      if (last != nil$1) {
        a.push(".");
        a.push(conv(last));
      }
      return "(" + a.join(" ") + ")";
    },
    toString: function () {
      return this.inspect();
    },

    to_display: function (to_display) {
      return this.inspect(to_display);
    },

    to_write: function () {
      return this.inspect(to_write$1);
    },
  });

  // Note: '() is not a pair in scheme
  const isPair = function (obj) {
    return obj instanceof Pair;
  };

  // Returns true if obj is a proper list
  // Note: isList returns true for '()
  const isList = function (obj) {
    if (obj === nil$1) {
      // Empty list
      return true;
    }
    if (!(obj instanceof Pair)) {
      // Argument isn't even a pair
      return false;
    }

    var tortoise = obj;
    var hare = obj.cdr;
    while (true) {
      if (hare === nil$1) {
        // End of list
        return true;
      }
      if (hare === tortoise) {
        // Cycle
        return false;
      }
      if (!(hare instanceof Pair)) {
        // Improper list
        return false;
      }

      if (hare.cdr === nil$1) {
        // End of list
        return true;
      }
      if (!(hare.cdr instanceof Pair)) {
        // Improper list
        return false;
      }

      hare = hare.cdr.cdr;
      tortoise = tortoise.cdr;
    }
  };

  // Creates a list out of the arguments, optionally converting any nested arrays into nested lists if the deep argument is true.
  // Example:
  //   BiwaScheme.List(1, 2, [3, 4]) ;=> (list 1 2 (vector 3 4))
  //   BiwaScheme.deep_array_to_list(1, 2, [3, 4]) ;=> (list 1 2 (list 3 4))
  const array_to_list_ = function (ary, deep) {
    var list = nil$1;
    for (var i = ary.length - 1; i >= 0; i--) {
      var obj = ary[i];
      if (deep && isArray(obj) && !obj.is_vector) {
        obj = array_to_list_(obj, deep);
      }
      list = new Pair(obj, list);
    }
    return list;
  };

  // Shallow: List(1, 2, [3]) == (list 1 2 (vector 3 4))
  const List = function () {
    var ary = toArray(arguments);
    return array_to_list_(ary, false);
  };

  // Shallow: array_to_list(1, 2, [3]) == (list 1 2 (vector 3 4))
  const array_to_list = function (ary) {
    return array_to_list_(ary, false);
  };

  // Deep: deep_array_to_list(1, 2, [3, 4]) == (list 1 2 (list 3 4))
  // deep_array_to_list([1, 2, 3]) - deep
  const deep_array_to_list = function (ary) {
    return array_to_list_(ary, true);
  };

  const Cons = function (car, cdr) {
    return new Pair(car, cdr);
  };

  const js_obj_to_alist = function (obj) {
    if (obj === undefined) {
      return nil$1;
    }
    var arr = [];
    each(obj, function (val, key) {
      arr.push(new Pair(key, val));
    });
    var alist = array_to_list(arr);
    return alist;
  };

  const alist_to_js_obj = function (alist) {
    if (alist === nil$1) {
      return {};
    }
    var obj = {};
    alist.foreach(function (item) {
      obj[item.car] = item.cdr;
    });
    return obj;
  };

  //
  // write/ss (write with substructure)
  //

  // example:  > (let ((x (list 'a))) (list x x))                   //           (#0=(a) #0#)
  // 2-pass algorithm.
  // (1) detect all the objects which appears more than once
  //     (find_cyclic, reduce_cyclic_info)
  // (2) write object using this information
  //   * add prefix '#n=' for first appearance
  //   * just write '#n#' for other appearance

  //TODO: support Values
  const write_ss = function (obj, array_mode) {
    var known = [obj],
      used = [false];
    find_cyclic(obj, known, used);
    var cyclic = reduce_cyclic_info(known, used);
    var appeared = new Array(cyclic.length);
    for (var i = cyclic.length - 1; i >= 0; i--) appeared[i] = false;

    return _write_ss(obj, cyclic, appeared, array_mode);
  };
  const to_write_ss = write_ss; // Alias

  const _write_ss = function (obj, cyclic, appeared, array_mode) {
    var ret = "";
    var i = cyclic.indexOf(obj);
    if (i >= 0) {
      if (appeared[i]) {
        return "#" + i + "#";
      } else {
        appeared[i] = true;
        ret = "#" + i + "=";
      }
    }

    if (obj instanceof Pair) {
      var a = [];
      a.push(_write_ss(obj.car, cyclic, appeared, array_mode));
      for (var o = obj.cdr; o != nil$1; o = o.cdr) {
        if (!(o instanceof Pair) || cyclic.indexOf(o) >= 0) {
          a.push(".");
          a.push(_write_ss(o, cyclic, appeared, array_mode));
          break;
        }
        a.push(_write_ss(o.car, cyclic, appeared, array_mode));
      }
      ret += "(" + a.join(" ") + ")";
    } else if (obj instanceof Array) {
      var a = map(obj, function (item) {
        return _write_ss(item, cyclic, appeared, array_mode);
      });
      if (array_mode) ret += "[" + a.join(", ") + "]";
      else ret += "#(" + a.join(" ") + ")";
    } else {
      ret += to_write$1(obj);
    }
    return ret;
  };

  const reduce_cyclic_info = function (known, used) {
    var n_used = 0;
    for (var i = 0; i < used.length; i++) {
      if (used[i]) {
        known[n_used] = known[i];
        n_used++;
      }
    }
    return known.slice(0, n_used);
  };

  const find_cyclic = function (obj, known, used) {
    var items =
      obj instanceof Pair
        ? [obj.car, obj.cdr]
        : obj instanceof Array
        ? obj
        : null;
    if (!items) return;

    each(items, function (item) {
      if (
        typeof item == "number" ||
        typeof item == "string" ||
        item === undef ||
        item === true ||
        item === false ||
        item === nil$1 ||
        item instanceof BiwaSymbol
      )
        return;

      var i = known.indexOf(item);
      if (i >= 0) used[i] = true;
      else {
        known.push(item);
        used.push(false);
        find_cyclic(item, known, used);
      }
    });
  };

  ///
  /// Call
  ///

  // The class Call is used to invoke scheme closure from
  // library functions.
  //
  // Call#initialize takes three arguments: proc, args and after.
  //   * proc is the scheme closure to invoke.
  //   * args is an Array (not list!) of arguments for the invocation.
  //   * after is a javascript function which is invoked when
  //     returned from the proc.
  //
  //     after takes two arguments: ar and intp.
  //       * ar is an Array which contains the result of the invocation.
  //       * intp is an Interpreter which is running.
  //
  //     If after returns another Call object, another invocation
  //     happens. If after returns a normal value, it is the value
  //     of the library function.
  //
  // example:
  //   return new Call(proc, [x, y], function(ar){ ar[0] });
  //
  const Call = Class.create({
    initialize: function (proc, args, after) {
      this.proc = proc;
      this.args = args;
      this.after =
        after ||
        function (ar) {
          // just return result which closure returned
          return ar[0];
        };
    },

    inspect: function () {
      return "#<Call args=" + this.args.inspect() + ">";
    },

    toString: function () {
      return "#<Call>";
    },

    to_write: function () {
      return "#<Call>";
    },
  });

  //
  // Iterator - external iterator for Call.foreach
  //
  const Iterator = {
    ForArray: Class.create({
      initialize: function (arr) {
        this.arr = arr;
        this.i = 0;
      },
      has_next: function () {
        return this.i < this.arr.length;
      },
      next: function () {
        return this.arr[this.i++];
      },
    }),
    ForString: Class.create({
      initialize: function (str) {
        this.str = str;
        this.i = 0;
      },
      has_next: function () {
        return this.i < this.str.length;
      },
      next: function () {
        return Char.get(this.str.charAt(this.i++));
      },
    }),
    ForList: Class.create({
      initialize: function (ls) {
        this.ls = ls;
      },
      has_next: function () {
        return this.ls instanceof Pair && this.ls != nil$1;
      },
      next: function () {
        var pair = this.ls;
        this.ls = this.ls.cdr;
        return pair;
      },
    }),
    ForMulti: Class.create({
      initialize: function (objs) {
        this.objs = objs;
        this.size = objs.length;
        this.iterators = map(objs, function (x) {
          return Iterator.of(x);
        });
      },
      has_next: function () {
        for (var i = 0; i < this.size; i++)
          if (!this.iterators[i].has_next()) return false;

        return true;
      },
      next: function () {
        return map(this.iterators, function (ite) {
          return ite.next();
        });
      },
    }),
    of: function (obj) {
      switch (true) {
        case obj instanceof Array:
          return new this.ForArray(obj);
        case typeof obj == "string":
          return new this.ForString(obj);
        case obj instanceof Pair:
        case obj === nil$1:
          return new this.ForList(obj);
        default:
          throw new Bug$1("Iterator.of: unknown class: " + inspect(obj));
      }
    },
  };

  //
  // Call.foreach - shortcut for successive Calls
  //
  // Some library functions, such as for-each or map,
  // call a closure for each element. Call.foreach is
  // a utility to help defining such methods.
  //
  // Call.foreach takes a sequence and some callbacks.
  // Sequence is an Array, String, or list.
  //
  // Example:
  //   return Call.foreach(sequence, {
  //     // before each call
  //     call: function(elem){
  //       return new Call(proc, [elem]);
  //     },
  //     // after each call
  //     result: function(value, elem){
  //       ary.push(value);
  //       // you can return a value to terminate the loop
  //     },
  //     // after all the calls
  //     finish: function(){
  //       return ary;
  //     }
  //   });

  Call.default_callbacks = {
    call: function (x) {
      return new Call(this.proc, [x]);
    },
    result: function () {},
    finish: function () {},
  };
  Call.foreach = function (obj, callbacks, is_multi) {
    is_multi || (is_multi = false);
    each(["call", "result", "finish"], function (key) {
      if (!callbacks[key]) callbacks[key] = Call.default_callbacks[key];
    });

    var iterator = null;
    var x = null;

    var loop = function (ar) {
      if (iterator) {
        var ret = callbacks["result"](ar[0], x);
        if (ret !== undefined) return ret;
      } else {
        // first lap
        if (is_multi) iterator = new Iterator.ForMulti(obj);
        else iterator = Iterator.of(obj);
      }

      if (!iterator.has_next()) {
        return callbacks["finish"]();
      } else {
        x = iterator.next();
        var result = callbacks["call"](x);
        result.after = loop;
        return result;
      }
    };
    return loop(null);
  };
  Call.multi_foreach = function (obj, callbacks) {
    return Call.foreach(obj, callbacks, true);
  };

  //
  // Syntax
  //
  const Syntax = Class.create({
    initialize: function (sname, func) {
      this.sname = sname;
      this.func = func;
    },
    transform: function (x) {
      if (!this.func) {
        throw new Bug$1(
          "sorry, syntax " + this.sname + " is a pseudo syntax now"
        );
      }
      return this.func(x);
    },
    inspect: function () {
      return "#<Syntax " + this.sname + ">";
    },
  });

  // A built-in syntax did not have associated Syntax object.
  // Following code installed dummy Syntax objects to built-in syntax.
  CoreEnv["define"] = new Syntax("define");
  CoreEnv["begin"] = new Syntax("begin");
  CoreEnv["quote"] = new Syntax("quote");
  CoreEnv["lambda"] = new Syntax("lambda");
  CoreEnv["if"] = new Syntax("if");
  CoreEnv["set!"] = new Syntax("set!");

  ///
  /// Compiler
  ///
  /// Note: macro expansion is done by Intepreter#expand

  const Compiler = Class.create({
    initialize: function () {},

    is_tail: function (x) {
      return x[0] == "return";
    },

    //free: set
    //e: env(= [locals, frees])
    //next: opc
    //ret: opc["refer_*", n, ["argument",
    //          ["refer_*", n, ... ["argument", next]
    collect_free: function (free, e, next) {
      var vars = free;
      var opc = next;
      var arr = vars.arr;
      for (var i = 0; i < arr.length; i++) {
        opc = this.compile_refer(arr[i], e, ["argument", opc]);
      }
      //Console.puts("collect_free "+free.inspect()+" / "+e.inspect()+" => "+opc.inspect());
      return opc;
    },

    //x: Symbol
    //e: env [set of locals, set of frees]
    //ret: opc
    compile_refer: function (x, e, next) {
      return this.compile_lookup(
        x,
        e,
        function (n) {
          return ["refer-local", n, next];
        },
        function (n) {
          return ["refer-free", n, next];
        },
        function (sym) {
          return ["refer-global", sym, next];
        }
      );
    },

    compile_lookup: function (x, e, return_local, return_free, return_global) {
      var locals = e[0],
        free = e[1];
      var n;
      if ((n = locals.index(x)) != null) {
        //Console.puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-local "+n);
        return return_local(n);
      } else if ((n = free.index(x)) != null) {
        //Console.puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-free "+n);
        return return_free(n);
      } else {
        var sym = x.name;
        return return_global(sym);
      }
      //throw new BiwaError("undefined symbol `" + sym + "'");
    },

    //generate boxing code (intersection of sets & vars)
    //if no need of boxing, just returns next
    //  sets(Set): assigned variables
    //  vars(List): used variables
    //  next(opc):
    //  ret(opc):
    make_boxes: function (sets, vars, next) {
      var vars = vars;
      var n = 0;
      var a = [];
      while (vars instanceof Pair) {
        if (sets.member(vars.car)) a.push(n);
        n++;
        vars = vars.cdr;
      }
      var opc = next;
      for (var i = a.length - 1; i >= 0; i--) opc = ["box", a[i], opc];
      return opc;
    },

    // Enumerate variables which (could be assigned && included in v)
    // x: exp
    // v: set(vars)
    // ret: set
    find_sets: function (x, v) {
      //Console.puts("find_sets: " + to_write(x) + " " + to_write(v))
      var ret = null;
      if (x instanceof BiwaSymbol) {
        ret = new BiwaSet();
      } else if (x instanceof Pair) {
        switch (x.first()) {
          case Sym("define"):
            var exp = x.third();
            ret = this.find_sets(exp, v);
          case Sym("begin"):
            ret = this.find_sets(x.cdr, v); //(ignores improper list)
            break;
          case Sym("quote"):
            ret = new BiwaSet();
            break;
          case Sym("lambda"):
            var vars = x.second(),
              body = x.cdr.cdr;
            if (vars instanceof Pair) {
              // (lambda (...) ...)
              ret = this.find_sets(body, v.set_minus(vars.to_set()));
            } else {
              // (lambda args ...)
              ret = this.find_sets(body, v.set_minus(new BiwaSet(vars)));
            }
            break;
          case Sym("if"):
            var testc = x.second(),
              thenc = x.third(),
              elsec = x.fourth();
            ret = this.find_sets(testc, v).set_union(
              this.find_sets(thenc, v),
              this.find_sets(elsec, v)
            );
            break;
          case Sym("set!"):
            var vari = x.second(),
              xx = x.third();
            if (v.member(vari)) ret = this.find_sets(xx, v).set_cons(vari);
            else ret = this.find_sets(xx, v);
            break;
          case Sym("call/cc"):
            var exp = x.second();
            ret = this.find_sets(exp, v);
            break;
          default:
            var set = new BiwaSet();
            for (var p = x; p instanceof Pair; p = p.cdr) {
              set = set.set_union(this.find_sets(p.car, v));
            }
            ret = set;
            break;
        }
      } else {
        ret = new BiwaSet();
      }

      if (ret == null) throw new Bug$1("find_sets() exited in unusual way");
      else return ret;
    },

    // find_free(): find free variables in x
    //              these variables are collected by collect_free().
    // x: expression
    // b: set of local vars (= variables which are not free)
    // f: set of free var candidates
    //    (local vars of outer lambdas)
    // ret: set of free vars
    find_free: function (x, b, f) {
      var ret = null;
      if (x instanceof BiwaSymbol) {
        if (f.member(x)) ret = new BiwaSet(x);
        else ret = new BiwaSet();
      } else if (x instanceof Pair) {
        switch (x.first()) {
          case Sym("define"):
            var exp = x.third();
            ret = this.find_free(exp, b, f);
            break;
          case Sym("begin"):
            ret = this.find_free(x.cdr, b, f); //(ignores improper list)
            break;
          case Sym("quote"):
            ret = new BiwaSet();
            break;
          case Sym("lambda"):
            var vars = x.second(),
              body = x.cdr.cdr;
            if (vars instanceof Pair) {
              // (lambda (...) ...)
              ret = this.find_free(body, b.set_union(vars.to_set()), f);
            } else {
              // (lambda args ...)
              ret = this.find_free(body, b.set_cons(vars), f);
            }
            break;
          case Sym("if"):
            var testc = x.second(),
              thenc = x.third(),
              elsec = x.fourth();
            ret = this.find_free(testc, b, f).set_union(
              this.find_free(thenc, b, f),
              this.find_free(elsec, b, f)
            );
            break;
          case Sym("set!"):
            var vari = x.second(),
              exp = x.third();
            if (f.member(vari)) ret = this.find_free(exp, b, f).set_cons(vari);
            else ret = this.find_free(exp, b, f);
            break;
          case Sym("call/cc"):
            var exp = x.second();
            ret = this.find_free(exp, b, f);
            break;
          default:
            var set = new BiwaSet();
            for (var p = x; p instanceof Pair; p = p.cdr) {
              set = set.set_union(this.find_free(p.car, b, f));
            }
            ret = set;
            break;
        }
      } else {
        ret = new BiwaSet();
      }
      //Console.p("find_free "+x.inspect()+" / "+b.inspect()+" => "+ret.inspect());

      if (ret == null) throw new Bug$1("find_free() exited in unusual way");
      else return ret;
    },

    // Returns the position of the dot pair.
    // Returns -1 if x is a proper list.
    //
    // eg. (a b . c) -> 2
    find_dot_pos: function (x) {
      var idx = 0;
      for (; x instanceof Pair; x = x.cdr, ++idx);
      if (x != nil$1) {
        return idx;
      } else {
        return -1;
      }
    },

    last_pair: function (x) {
      if (x instanceof Pair) {
        for (; x.cdr instanceof Pair; x = x.cdr);
      }
      return x;
    },

    // Takes an dotted list and returns proper list.
    //
    // eg. (x y . z) -> (x y z)
    dotted2proper: function (ls) {
      if (ls === nil$1) return nil$1;

      var nreverse = function (ls) {
        var res = nil$1;
        for (; ls instanceof Pair; ) {
          var d = ls.cdr;
          ls.cdr = res;
          res = ls;
          ls = d;
        }
        return res;
      };
      var copy_list = function (ls) {
        var res = nil$1;
        for (; ls instanceof Pair; ls = ls.cdr) {
          res = new Pair(ls.car, res);
        }
        return nreverse(res);
      };

      if (ls instanceof Pair) {
        var last = this.last_pair(ls);
        if (last instanceof Pair && last.cdr === nil$1) {
          return ls;
        } else {
          var copied = copy_list(ls);
          this.last_pair(copied).cdr = new Pair(last.cdr, nil$1);
          return copied;
        }
      } else {
        return new Pair(ls, nil$1);
      }
    },

    // x: exp(list of symbol or integer or..)
    // e: env (= [locals, frees])
    // s: vars might be set!
    // next: opc
    // ret: opc
    compile: function (x, e, s, f, next) {
      //Console.p(x);
      var ret = null;

      while (1) {
        if (x instanceof BiwaSymbol) {
          // Variable reference
          // compiled into refer-(local|free|global)
          return this.compile_refer(
            x,
            e,
            s.member(x) ? ["indirect", next] : next
          );
        } else if (x instanceof Pair) {
          switch (x.first()) {
            case Sym("define"):
              ret = this._compile_define(x, next);

              x = ret[0];
              next = ret[1];
              break;

            case Sym("begin"):
              var a = [];
              for (var p = x.cdr; p instanceof Pair; p = p.cdr) a.push(p.car);

              //compile each expression (in reverse order)
              var c = next;
              for (var i = a.length - 1; i >= 0; i--) {
                c = this.compile(a[i], e, s, f, c);
              }
              return c;

            case Sym("quote"):
              if (x.length() < 2)
                throw new BiwaError("Invalid quote: " + x.to_write());

              var obj = x.second();
              return ["constant", obj, next];

            case Sym("lambda"):
              return this._compile_lambda(x, e, s, f, next);

            case Sym("if"):
              if (x.length() < 3 || x.length() > 4)
                throw new BiwaError("Invalid if: " + x.to_write());

              var testc = x.second(),
                thenc = x.third(),
                elsec = x.fourth();
              var thenc = this.compile(thenc, e, s, f, next);
              var elsec = this.compile(elsec, e, s, f, next);
              x = testc;
              next = ["test", thenc, elsec];
              break;

            case Sym("set!"):
              // error-checking: should have only 3 things
              if (x.length() != 3)
                throw new BiwaError("Invalid set!: " + x.to_write());

              var v = x.second(),
                x = x.third();
              var do_assign = this.compile_lookup(
                v,
                e,
                function (n) {
                  return ["assign-local", n, next];
                },
                function (n) {
                  return ["assign-free", n, next];
                },
                function (sym) {
                  return ["assign-global", sym, next];
                }
              );
              next = do_assign;
              break;

            case Sym("call/cc"):
              var x = x.second();
              var arity_of_arg = 1; // Always 1. (lambda (cc) ...)
              var c = [
                "conti",
                this.is_tail(next) ? e[0].size() + 1 : 0, //number of args for outer lambda
                [
                  "argument", // Push the continuaion closure onto the stack
                  [
                    "constant",
                    arity_of_arg,
                    [
                      "argument",
                      this.compile(
                        x,
                        e,
                        s,
                        f,
                        this.is_tail(next)
                          ? ["shift", arity_of_arg, ["tco_hinted_apply"]]
                          : ["apply"]
                      ),
                    ],
                  ],
                ],
              ];

              // Do not push stack frame when call/cc is in a tail context
              return this.is_tail(next) ? c : ["frame", c, next];

            default:
              //apply
              //x = (func 1 2)
              //x.car = func = '(lambda (x) ..) or Symbol
              //x.cdr = args = '(1 2)
              var func = x.car;
              var args = x.cdr;
              var c = this.compile(
                func,
                e,
                s,
                f,
                this.is_tail(next)
                  ? ["shift", args.length(), ["tco_hinted_apply"]]
                  : ["apply"]
              );

              // VM will push the number of arguments to the stack.
              c = this.compile(args.length(), e, s, f, ["argument", c]);
              for (var p = args; p instanceof Pair; p = p.cdr) {
                c = this.compile(p.car, e, s, f, ["argument", c]);
              }

              // Do not push stack frame for tail calls
              return this.is_tail(next) ? c : ["frame", c, next];
          }
        } else {
          return ["constant", x, next];
        }
      }
      //Console.p("result of " + x.inspect() + ":");
      //Console.p(ret);
      //dump({"ret":ret, "x":x, "e":e, "s":s, "next":next, "stack":[]});
      //      if(ret == null)
      //        throw new Bug("compile() exited in unusual way");
      //      else
      //        return ret;
    },

    // Compile define.
    //
    // 0. (define) ; => error
    // 1. (define a)
    // 2. (define a 1)
    // 3. (define a 1 2) ; => error
    // 4. (define (f x) x), (define (f . a) a)
    // 5. (define 1 2)
    //
    // Note: define may appear in lambda, let, let*, let-values,
    // let*-values, letrec, letrec*. These definitions are local to the
    // <body> of these forms.
    _compile_define: function (x, next) {
      if (x.length() == 1) {
        // 0. (define)
        throw new BiwaError("Invalid `define': " + x.to_write());
      }

      var first = x.cdr.car;
      var rest = x.cdr.cdr;

      if (first instanceof BiwaSymbol) {
        if (rest === nil$1) {
          // 1. (define a)
          x = undef;
        } else {
          if (rest.cdr === nil$1)
            // 2. (define a 1)
            x = rest.car;
          // 3. (define a 1 2)
          else throw new BiwaError("Invalid `define': " + x.to_write());
        }

        if (!TopEnv.hasOwnProperty(first.name)) {
          TopEnv[first.name] = undef;
        }
        next = ["assign-global", first.name, next];
      } else if (first instanceof Pair) {
        // 4. (define (f x) ...)
        // Note: define of this form may contain internal define.
        // They are handled in compilation of "lambda".

        var fname = first.car,
          args = first.cdr;
        var lambda = new Pair(Sym("lambda"), new Pair(args, rest));
        x = lambda;
        if (!TopEnv.hasOwnProperty(first.name)) {
          TopEnv[fname.name] = undef;
        }
        next = ["assign-global", fname.name, next];
      } else {
        // 5. (define 1 2)
        throw new BiwaError("define: symbol or pair expected but got " + first);
      }

      return [x, next];
    },

    // Compiles various forms of "lambda".
    //
    // * (lambda (x y) ...)
    // * (lambda (x y . rest) ...)
    // * (lambda args ...)
    _compile_lambda: function (x, e, s, f, next) {
      if (x.length() < 3)
        throw new BiwaError("Invalid lambda: " + x.to_write());

      var vars = x.cdr.car;
      var body = x.cdr.cdr;

      // Handle internal defines
      var tbody = Compiler.transform_internal_define(body);
      if (isPair(tbody) && isSymbol(tbody.car) && tbody.car.name == "letrec*") {
        // The body has internal defines.
        // Expand letrec* macro
        var cbody = Compiler.expand(tbody);
      } else {
        // The body has no internal defines.
        // Just wrap the list with begin
        var cbody = new Pair(Sym("begin"), x.cdr.cdr);
      }

      var dotpos = this.find_dot_pos(vars);
      var proper = this.dotted2proper(vars);
      var free = this.find_free(cbody, proper.to_set(), f); //free variables
      var sets = this.find_sets(cbody, proper.to_set()); //local variables

      var do_body = this.compile(
        cbody,
        [proper.to_set(), free],
        sets.set_union(s.set_intersect(free)),
        f.set_union(proper.to_set()),
        ["return"]
      );
      var do_close = [
        "close",
        vars instanceof Pair ? vars.length() : 0,
        free.size(),
        this.make_boxes(sets, proper, do_body),
        next,
        dotpos,
      ];
      return this.collect_free(free, e, do_close);
    },

    run: function (expr) {
      return this.compile(
        expr,
        [new BiwaSet(), new BiwaSet()],
        new BiwaSet(),
        new BiwaSet(),
        ["halt"]
      );
    },
  });

  // Compile an expression with new compiler
  Compiler.compile = function (expr, next) {
    expr = Compiler.expand(expr);
    return new Compiler().run(expr, next);
  };

  // Expand macro calls in a expression recursively.
  //
  // x - expression
  // flag - used internally. do not specify this
  //
  // @throws {BiwaError} when x has syntax error
  Compiler.expand = function (x, flag /*optional*/) {
    var expand = Compiler.expand;
    flag || (flag = {});
    var ret = null;

    if (x instanceof Pair) {
      switch (x.car) {
        case Sym("define"):
          var left = x.cdr.car,
            exp = x.cdr.cdr;
          ret = new Pair(Sym("define"), new Pair(left, expand(exp, flag)));
          break;
        case Sym("begin"):
          ret = new Pair(Sym("begin"), expand(x.cdr, flag));
          break;
        case Sym("quote"):
          ret = x;
          break;
        case Sym("lambda"):
          var vars = x.cdr.car,
            body = x.cdr.cdr;
          ret = new Pair(Sym("lambda"), new Pair(vars, expand(body, flag)));
          break;
        case Sym("if"):
          var testc = x.second(),
            thenc = x.third(),
            elsec = x.fourth();
          ret = List(
            Sym("if"),
            expand(testc, flag),
            expand(thenc, flag),
            expand(elsec, flag)
          );
          break;
        case Sym("set!"):
          var v = x.second(),
            x = x.third();
          ret = List(Sym("set!"), v, expand(x, flag));
          break;
        case Sym("call-with-current-continuation"):
        case Sym("call/cc"):
          var x = x.second();
          ret = List(Sym("call/cc"), expand(x, flag));
          break;
        default:
          //apply
          var transformer = null;
          if (isSymbol(x.car)) {
            if (TopEnv[x.car.name] instanceof Syntax)
              transformer = TopEnv[x.car.name];
            else if (CoreEnv[x.car.name] instanceof Syntax)
              transformer = CoreEnv[x.car.name];
          }

          if (transformer) {
            flag["modified"] = true;
            ret = transformer.transform(x);

            //            // Debug
            //            var before = to_write(x);
            //            var after = to_write(ret);
            //            if(before != after){
            //              console.log("before: " + before)
            //              console.log("expand: " + after)
            //            }

            var fl;
            for (;;) {
              ret = expand(ret, (fl = {}));
              if (!fl["modified"]) break;
            }
          } else {
            var expanded_car = expand(x.car, flag);
            var expanded_cdr;
            if (!(x.cdr instanceof Pair) && x.cdr !== nil$1) {
              throw new BiwaError(
                "proper list required for function application " +
                  "or macro use: " +
                  to_write(x)
              );
            }
            expanded_cdr = array_to_list(
              x.cdr.to_array().map(function (item) {
                return expand(item, flag);
              })
            );
            ret = new Pair(expanded_car, expanded_cdr);
          }
      }
    } else {
      ret = x;
    }
    return ret;
  };

  // Transform internal defines to letrec*.
  //
  // Example
  //   (let ((a 1))
  //     (define (b) a)
  //     (b))
  //
  //   (let ((a 1))
  //     (letrec* ((b (lambda () a)))
  //       (b)))
  //
  // x - expression starts with (define ...)
  //
  // Returns a letrec* expression, or
  // just returns x, when x does not contain definitions.

  // Returns true if x is a definition
  var is_definition = function (x) {
    return isPair(x) && Sym("define") === x.car;
    // TODO: support "begin", nested "begin", "let(rec)-syntax"
  };

  // Convert function definition to lambda binding
  //   (define a ..)         -> (a ..)
  //   (define (f) ..)       -> (f (lambda () ..))
  //   (define (f x . y) ..) -> (f (lambda (x . y) ..))
  //   (define (f . a) ..)   -> (f (lambda a ..))
  var define_to_lambda_bind = function (def) {
    var sig = def.cdr.car;
    var body = def.cdr.cdr;

    if (isSymbol(sig)) {
      var variable = sig;

      return new Pair(variable, body);
    } else {
      var variable = sig.car;
      var value = new Pair(Sym("lambda"), new Pair(sig.cdr, body));

      return List(variable, value);
    }
  };

  Compiler.transform_internal_define = function (x) {
    // 1. Split x into definitions and expressions
    var defs = [],
      item = x;
    while (is_definition(item.car)) {
      defs.push(item.car);
      item = item.cdr;
    }
    var exprs = item;

    // 2. Return x if there is no definitions
    if (defs.length == 0) return x;

    // 3. Return (letrec* <bindings> <expressions>)
    var bindings = List.apply(null, map(defs, define_to_lambda_bind));
    return new Pair(Sym("letrec*"), new Pair(bindings, exprs));
  };

  //
  // assertions - type checks
  //

  const make_assert = function (check) {
    return function (/*args*/) {
      // We cannot use callee/caller in ESM (=JS strict mode)
      //var fname = arguments.callee.caller
      //              ? arguments.callee.caller.fname
      //              : "";
      const fname = "";
      check.apply(this, [fname].concat(toArray(arguments)));
    };
  };

  const make_simple_assert = function (type, test, _fname) {
    return make_assert(function (fname, obj, opt) {
      const option = opt ? "(" + opt + "): " : "";
      if (!test(obj)) {
        throw new BiwaError(
          option + type + " required, but got " + to_write$1(obj)
        );
      }
    });
  };

  //
  // Hashtable
  //
  // TODO: Reimplement with JavaScript Map
  //
  // Based on the base JavaScript Object class, but
  //  * Object takes only strings as keys
  //  * R6RS hashtable needs its own hash function
  // so some hacks are needed.

  const Hashtable = Class.create({
    initialize: function (_hash_proc, _equiv_proc, mutable) {
      this.mutable = mutable === undefined ? true : mutable ? true : false;

      this.hash_proc = _hash_proc;
      this.equiv_proc = _equiv_proc;

      // Hash (hashed) => (array of (key and value))
      this.pairs_of = {};
    },

    clear: function () {
      this.pairs_of = {};
    },

    candidate_pairs: function (hashed) {
      return this.pairs_of[hashed];
    },

    add_pair: function (hashed, key, value) {
      var pairs = this.pairs_of[hashed];

      if (pairs) {
        pairs.push([key, value]);
      } else {
        this.pairs_of[hashed] = [[key, value]];
      }
    },

    remove_pair: function (hashed, pair) {
      var pairs = this.pairs_of[hashed];
      var i = pairs.indexOf(pair);
      if (i == -1) {
        throw new Bug$1("Hashtable#remove_pair: pair not found!");
      } else {
        pairs.splice(i, 1); //remove 1 element from i-th index
      }
    },

    create_copy: function (mutable) {
      var copy = new Hashtable(this.hash_proc, this.equiv_proc, mutable);
      // clone the pairs to copy
      each(
        keys(this.pairs_of),
        bind(function (hashed) {
          var pairs = this.pairs_of[hashed];
          var cloned = map(pairs, function (pair) {
            return clone(pair);
          });
          copy.pairs_of[hashed] = cloned;
        }, this)
      );

      return copy;
    },

    size: function () {
      var n = 0;
      this._apply_pair(function (pair) {
        n++;
      });
      return n;
    },

    keys: function () {
      return this._apply_pair(function (pair) {
        return pair[0];
      });
    },

    values: function () {
      return this._apply_pair(function (pair) {
        return pair[1];
      });
    },

    _apply_pair: function (func) {
      var a = [];
      each(values(this.pairs_of), function (pairs) {
        each(pairs, function (pair) {
          a.push(func(pair));
        });
      });
      return a;
    },

    to_write: function () {
      return "#<Hashtable size=" + this.size() + ">";
    },
  });

  const isHashtable = function (obj) {
    return obj instanceof Hashtable;
  };

  const isMutableHashtable = function (obj) {
    return obj instanceof Hashtable && obj.mutable;
  };

  //
  // Hash functions
  //

  Hashtable.equal_hash = function (ar) {
    return to_write$1(ar[0]);
  };
  Hashtable.eq_hash = Hashtable.equal_hash;
  Hashtable.eqv_hash = Hashtable.equal_hash;

  Hashtable.string_hash = function (ar) {
    return ar[0];
  };

  Hashtable.string_ci_hash = function (ar) {
    return isString$1(ar[0]) ? ar[0].toLowerCase() : ar[0];
  };

  Hashtable.symbol_hash = function (ar) {
    return ar[0] instanceof BiwaSymbol ? ar[0].name : ar[0];
  };

  //
  // Equivalence functions
  //

  Hashtable.eq_equiv = function (ar) {
    return eq(ar[0], ar[1]);
  };

  Hashtable.eqv_equiv = function (ar) {
    return eqv(ar[0], ar[1]);
  };

  //
  // Parser
  // copied from jsScheme - should be rewrriten (support #0=, etc)
  //
  const Parser = Class.create({
    initialize: function (txt) {
      this.tokens = this.tokenize(txt);
      this.i = 0;
    },

    inspect: function () {
      return [
        "#<Parser:",
        this.i,
        "/",
        this.tokens.length,
        " ",
        inspect(this.tokens),
        ">",
      ].join("");
    },

    tokenize: function (txt) {
      var tokens = new Array(),
        oldTxt = null;
      var in_srfi_30_comment = 0;

      while (txt != "" && oldTxt != txt) {
        oldTxt = txt;
        txt = txt.replace(
          /^\s*(;[^\r\n]*(\r|\n|$)|#;|#\||#\\[^\w]|#?(\(|\[|{)|\)|\]|}|\'|`|,@|,|\+inf\.0|-inf\.0|\+nan\.0|\"(\\(.|$)|[^\"\\])*(\"|$)|[^\s()\[\]{}]+)/,
          function ($0, $1) {
            var t = $1;

            if (t == "#|") {
              in_srfi_30_comment++;
              return "";
            } else if (in_srfi_30_comment > 0) {
              if (/(.*\|#)/.test(t)) {
                in_srfi_30_comment--;
                if (in_srfi_30_comment < 0) {
                  throw new BiwaError(
                    "Found an extra comment terminator: `|#'"
                  );
                }
                // Push back the rest substring to input stream.
                return t.substring(RegExp.$1.length, t.length);
              } else {
                return "";
              }
            } else {
              if (t.charAt(0) != ";") tokens[tokens.length] = t;
              return "";
            }
          }
        );
      }
      return tokens;
    },

    sexpCommentMarker: new Object(),
    getObject: function () {
      var r = this.getObject0();

      if (r != this.sexpCommentMarker) return r;

      r = this.getObject();
      if (r == Parser.EOS)
        throw new BiwaError(
          "Readable object not found after S exression comment"
        );

      r = this.getObject();
      return r;
    },

    getList: function (close) {
      var list = nil$1,
        prev = list;
      while (this.i < this.tokens.length) {
        this.eatObjectsInSexpComment(
          "Input stream terminated unexpectedly(in list)"
        );

        if (
          this.tokens[this.i] == ")" ||
          this.tokens[this.i] == "]" ||
          this.tokens[this.i] == "}"
        ) {
          this.i++;
          break;
        }

        if (this.tokens[this.i] == ".") {
          this.i++;
          var o = this.getObject();
          if (o != Parser.EOS && list != nil$1) {
            prev.cdr = o;
          }
        } else {
          var cur = new Pair(this.getObject(), nil$1);
          if (list == nil$1) list = cur;
          else prev.cdr = cur;
          prev = cur;
        }
      }
      return list;
    },

    getVector: function (close) {
      var arr = new Array();
      while (this.i < this.tokens.length) {
        this.eatObjectsInSexpComment(
          "Input stream terminated unexpectedly(in vector)"
        );

        if (
          this.tokens[this.i] == ")" ||
          this.tokens[this.i] == "]" ||
          this.tokens[this.i] == "}"
        ) {
          this.i++;
          break;
        }
        arr[arr.length] = this.getObject();
      }
      return arr;
    },

    eatObjectsInSexpComment: function (err_msg) {
      while (this.tokens[this.i] == "#;") {
        this.i++;
        if (this.getObject() == Parser.EOS || this.i >= this.tokens.length)
          throw new BiwaError(err_msg);
      }
    },

    getObject0: function () {
      if (this.i >= this.tokens.length) return Parser.EOS;

      var t = this.tokens[this.i++];
      // if( t == ')' ) return null;

      if (t == "#;") return this.sexpCommentMarker;

      var s =
        t == "'"
          ? "quote"
          : t == "`"
          ? "quasiquote"
          : t == ","
          ? "unquote"
          : t == ",@"
          ? "unquote-splicing"
          : false;

      if (
        s ||
        t == "(" ||
        t == "#(" ||
        t == "[" ||
        t == "#[" ||
        t == "{" ||
        t == "#{"
      ) {
        return s
          ? new Pair(Sym(s), new Pair(this.getObject(), nil$1))
          : t == "(" || t == "[" || t == "{"
          ? this.getList(t)
          : this.getVector(t);
      } else {
        switch (t) {
          case "+inf.0":
            return Infinity;
          case "-inf.0":
            return -Infinity;
          case "+nan.0":
            return NaN;
        }

        var n;
        if (/^#x[0-9a-z]+$/i.test(t)) {
          // #x... Hex
          n = new Number("0x" + t.substring(2, t.length));
        } else if (/^#d[0-9\.]+$/i.test(t)) {
          // #d... Decimal
          n = new Number(t.substring(2, t.length));
        } else {
          n = new Number(t); // use constrictor as parser
        }

        if (!isNaN(n)) {
          return n.valueOf();
        } else if (t == "#f" || t == "#F") {
          return false;
        } else if (t == "#t" || t == "#T") {
          return true;
        } else if (t.toLowerCase() == "#\\newline") {
          return Char.get("\n");
        } else if (t.toLowerCase() == "#\\space") {
          return Char.get(" ");
        } else if (t.toLowerCase() == "#\\tab") {
          return Char.get("\t");
        } else if (/^#\\.$/.test(t)) {
          return Char.get(t.charAt(2));
        } else if (/^#\\x[a-zA-Z0-9]+$/.test(t)) {
          var scalar = parseInt(t.slice(3), 16);
          // R6RS 11.11 (surrogate codepoints)
          if (scalar >= 0xd800 && scalar <= 0xdfff) {
            throw new BiwaError("Character in Unicode excluded range.");
          }
          // ECMA-262 4.3.16 -- Basically, strings are sequences of 16-bit
          // unsigned integers, so anything greater than 0xFFFF won't fit.
          // NOTE: This violates R6RS which requires the full Unicode range!
          else if (scalar > 0xffff) {
            throw new BiwaError("Character literal out of range.");
          } else {
            return Char.get(String.fromCharCode(scalar));
          }
        } else if (/^\"(\\(.|$)|[^\"\\])*\"?$/.test(t)) {
          return t
            .replace(/(\r?\n|\\n)/g, "\n")
            .replace(/^\"|\\(.|$)|\"$/g, function ($0, $1) {
              return $1 ? $1 : "";
            });
        } else return Sym(t); // 2Do: validate !!
      }
    },
  });
  // indicates end of source file
  Parser.EOS = new Object();

  // Parser the text and return an array of exprs
  Parser.parse = (txt) => {
    const parser = new Parser(txt);
    const ret = [];
    while (true) {
      var expr = parser.getObject();
      if (expr === Parser.EOS) break;
      ret.push(expr);
    }
    return ret;
  };

  ///
  /// Interpreter
  ///

  const Interpreter = Class.create({
    // new Interpreter()
    // new Interpreter(lastInterpreter)
    // new Interpreter(errorHandler)
    // new Interpreter(lastInterpreter, errorHandler)
    initialize: function () {
      var last_interpreter = null;
      var on_error = null;
      if (arguments.length == 2) {
        last_interpreter = arguments[0];
        on_error = arguments[1];
      } else if (arguments.length == 1 && arguments[0] instanceof Interpreter) {
        last_interpreter = arguments[0];
      } else if (arguments.length == 1 && typeof arguments[0] == "function") {
        on_error = arguments[0];
      }

      // Interpreter stack
      this.stack = [];
      // JS function to handle error
      this.on_error =
        on_error ||
        (last_interpreter ? last_interpreter.on_error : function (e) {});
      // JS function to handle result
      this.after_evaluate = function () {};

      // (Variables for stack trace)
      // Name of the last variable read by refer-xx
      this.last_refer = last_interpreter ? last_interpreter.last_refer : null;
      // Call stack (array of last_refer)
      this.call_stack = last_interpreter
        ? clone(last_interpreter.call_stack)
        : [];
      // Counts number of tail calls (= how many times should we pop call_stack
      // in op_return)
      this.tco_counter = [];
      // Maximum length of call_stack
      // (Note: we should cap call_stack for inifinite loop with recursion)
      this.max_trace_size = last_interpreter
        ? last_interpreter.max_trace_size
        : max_trace_size;

      // dynamic-wind
      this.current_dynamic_winder = Interpreter.DynamicWind.ROOT;
    },

    inspect: function () {
      return [
        "#<Interpreter: stack size=>",
        this.stack.length,
        " ",
        "after_evaluate=",
        inspect(this.after_evaluate),
        ">",
      ].join("");
    },

    // private
    push: function (x, s) {
      this.stack[s] = x;
      return s + 1;
    },

    // private
    //s: depth of stack to save
    //ret: saved(copied) stack
    save_stack: function (s) {
      var v = [];
      for (var i = 0; i < s; i++) {
        v[i] = this.stack[i];
      }
      return {
        stack: v,
        last_refer: this.last_refer,
        call_stack: clone(this.call_stack),
        tco_counter: clone(this.tco_counter),
      };
    },

    // private
    //v: stack array to restore
    //ret: lenght of restored stack
    restore_stack: function (stuff) {
      const v = stuff.stack;
      const s = v.length;
      for (var i = 0; i < s; i++) {
        this.stack[i] = v[i];
      }
      this.last_refer = stuff.last_refer;
      this.call_stack = clone(stuff.call_stack);
      this.tco_counter = clone(stuff.tco_counter);
      return s;
    },

    // private
    //s: depth of stack to save
    //n: number of args(for outer lambda) to remove (= 0 unless tail position)
    //ret: closure array
    capture_continuation: function (s, n) {
      // note: implementation of this function for final version doesn't exist in 3imp.pdf..
      var ss = this.push(n, s);
      return this.closure(
        ["nuate1", this.save_stack(ss), this.current_dynamic_winder],
        1, //arity
        0, //n (number of frees)
        null, //s (stack position to get frees)
        -1
      ); // dotpos
    },

    // private
    // shift stack
    // n: number of items to skip (from stack top)
    // m: number of items to shift
    // s: stack pointer (= index of stack top + 1)
    shift_args: function (n, m, s) {
      for (var i = n; i >= 0; i--) {
        this.index_set(s, i + m + 1, this.index(s, i));
      }
      return s - m - 1;
    },

    index: function (s, i) {
      return this.stack[s - 1 - i];
    },

    // private
    index_set: function (s, i, v) {
      this.stack[s - 1 - i] = v;
    },

    // private
    //ret: [body, stack[s-1], stack[s-2], .., stack[s-n], dotpos]
    closure: function (body, args, n, s, dotpos) {
      var v = []; //(make-vector n+1+1)
      v[0] = body;
      for (var i = 0; i < n; i++) v[i + 1] = this.index(s, i);
      v[n + 1] = dotpos;

      if (dotpos == -1) v.expected_args = args;

      makeClosure(v);
      return v;
    },

    // private
    run_dump_hook: function (a, x, f, c, s) {
      var dumper;
      var state;

      if (this.dumper) {
        dumper = this.dumper;
      } else if (Interpreter.dumper) {
        dumper = Interpreter.dumper;
      } else return;

      if (dumper) {
        state = { a: a, f: f, c: c, s: s, x: x, stack: this.stack };
        dumper.dump(state);
      }
    },

    // private
    _execute: function (a, x, f, c, s) {
      var ret = null;
      //Console.puts("executing "+x[0]);

      while (true) {
        //x[0] != "halt"){

        this.run_dump_hook(a, x, f, c, s);

        switch (x[0]) {
          case "halt":
            return a;
          case "refer-local":
            var n = x[1],
              x = x[2];
            a = this.index(f, n + 1);
            this.last_refer = "(anon)";
            break;
          case "refer-free":
            var n = x[1],
              x = x[2];
            a = c[n + 1];
            this.last_refer = "(anon)";
            break;
          case "refer-global":
            var sym = x[1],
              x = x[2];
            if (TopEnv.hasOwnProperty(sym)) var val = TopEnv[sym];
            else if (CoreEnv.hasOwnProperty(sym)) var val = CoreEnv[sym];
            else
              throw new BiwaError("execute: unbound symbol: " + inspect(sym));

            a = val;
            this.last_refer = sym || "(anon)";
            break;
          case "indirect":
            var x = x[1];
            a = a[0]; //unboxing
            break;
          case "constant":
            var obj = x[1],
              x = x[2];
            a = obj;
            this.last_refer = "(anon)";
            break;
          case "close":
            var ox = x;
            var v = ox[1],
              n = ox[2],
              body = ox[3],
              x = ox[4],
              dotpos = ox[5];
            a = this.closure(body, v, n, s, dotpos);
            s -= n;
            break;
          case "box":
            var n = x[1],
              x = x[2];
            this.index_set(s, n + 1, [this.index(s, n + 1)]); //boxing
            break;
          case "test":
            var thenc = x[1],
              elsec = x[2];
            x = a !== false ? thenc : elsec;
            break;
          case "assign-global":
            var name = x[1],
              x = x[2];
            if (!TopEnv.hasOwnProperty(name) && !CoreEnv.hasOwnProperty(name))
              throw new BiwaError(
                "global variable '" + name + "' is not defined"
              );

            TopEnv[name] = a;
            a = undef;
            break;
          case "assign-local":
            var n = x[1],
              x = x[2];
            var box = this.index(f, n + 1);
            box[0] = a;
            a = undef;
            break;
          case "assign-free":
            var n = x[1],
              x = x[2];
            var box = c[n + 1];
            box[0] = a;
            a = undef;
            break;
          case "conti":
            var n = x[1],
              x = x[2];
            a = this.capture_continuation(s, n);
            break;
          case "nuate1":
            var stack = x[1],
              to = x[2];
            var from = this.current_dynamic_winder;
            var winders = Interpreter.DynamicWind.listWinders(from, to);
            x = Interpreter.DynamicWind.joinWinders(winders, [
              "refer-local",
              0,
              ["nuate2", stack],
            ]);
            break;
          case "nuate2":
            var stack = x[1],
              x = ["return"];
            s = this.restore_stack(stack);
            break;
          case "frame":
            var ret = x[2];
            x = x[1];
            s = this.push(ret, this.push(f, this.push(c, s)));
            this.tco_counter[this.tco_counter.length] = 0;
            break;
          case "argument":
            var x = x[1];
            s = this.push(a, s);
            break;
          case "shift":
            var n = x[1],
              x = x[2];

            // the number of arguments in the last call
            var n_args = this.index(s, n + 1);

            s = this.shift_args(n, n_args, s);
            break;
          case "tco_hinted_apply": // just like a regular apply, except we need to trace the # of TCO calls so we can generate a stacktrace
            this.tco_counter[this.tco_counter.length - 1]++;
            x = ["apply"].concat(rest(x));
            break;
          case "apply": //extended: n_args as second argument
            var func = a; //, n_args = x[1];

            // Save stack trace
            this.call_stack.push(this.last_refer);
            if (this.call_stack.length > this.max_trace_size) {
              // Remove old memory if it grows too long
              // Note: this simple way may be inconvenient (e.g. no trace
              // will be shown when an error occurred right after returning
              // from a large function)
              this.call_stack.shift();
            }

            // the number of arguments in the last call is
            // pushed to the stack.
            var n_args = this.index(s, 0);
            if (isClosure(func)) {
              a = func;
              x = func[0];

              // The position of dot in the parameter list.
              var dotpos = func[func.length - 1];

              if (dotpos >= 0) {
                // The dot is found
                // ----------------
                // => Process the &rest args: packing the rest args into a list.
                var ls = nil$1;
                for (var i = n_args; --i >= dotpos; ) {
                  ls = new Pair(this.index(s, i + 1), ls);
                }
                if (dotpos >= n_args) {
                  // No rest argument is passed to this closure.
                  // However, the closure expects the caller passes the rest argument.
                  // In such case this VM prepares an empty list as the rest argument.
                  // --------------------------------------------------------------
                  // => We extend the stack to put the empty list.
                  for (var i = 0; i < n_args + 1; i++) {
                    this.index_set(s, i - 1, this.index(s, i));
                  }
                  s++;
                  // => Update the number of arguments
                  this.index_set(s, 0, this.index(s, 0) + 1);
                }
                this.index_set(s, dotpos + 1, ls);
              } else {
                // the dot is not found
                // --------------------
                // => Verify that number of arguments = expected number of arguments
                // (if the closure knows how many it wants)
                if (func.expected_args && n_args != func.expected_args) {
                  var errMsg =
                    "Function call error: got " +
                    n_args +
                    " but wanted " +
                    func.expected_args;
                  throw new BiwaError(errMsg);
                }
              }
              f = s;
              c = a;
            } else if (func instanceof Function) {
              // Apply JavaScript function
              // load arguments from stack
              var args = [];
              for (var i = 0; i < n_args; i++) args.push(this.index(s, i + 1));

              // invoke the function
              var result = func(args, this);

              if (result instanceof Pause) {
                // it requested the interpreter to suspend
                var pause = result;
                pause.set_state(this, ["return"], f, c, s);
                pause.ready();
                return pause;
              } else if (result instanceof Call) {
                // it requested the interpreter to call a scheme closure

                //   [frame,
                //     [constant... (args)
                //     [constant, proc
                //     [apply]]]]
                //   [frame,
                //     [constant, after
                //     [apply 1]]]]
                //   x
                var call_after = [
                  "frame",
                  [
                    "argument",
                    [
                      "constant",
                      1,
                      ["argument", ["constant", result.after, ["apply"]]],
                    ],
                  ],
                  ["return"],
                ];
                var call_proc = [
                  "constant",
                  result.args.length,
                  [
                    "argument",
                    ["constant", result.proc, ["apply", result.args.length]],
                  ],
                ];
                var push_args = reduce(
                  result.args,
                  function (opc, arg) {
                    // (foo 1 2) => first push 2, then 1
                    //   [constant 2 ... [constant 1 ... ]
                    return ["constant", arg, ["argument", opc]];
                  },
                  call_proc
                );
                x = ["frame", push_args, call_after];
              } else {
                // the JavaScript function returned a normal value
                a = result;
                x = ["return"];
              }
            } else {
              // unknown function type
              throw new BiwaError(inspect(func) + " is not a function");
            }
            break;
          case "return":
            // Pop stack frame
            var n = this.index(s, 0);
            var ss = s - n;
            (x = this.index(ss, 1)),
              (f = this.index(ss, 2)),
              (c = this.index(ss, 3)),
              (s = ss - 3 - 1);

            // Pop stack trace (> 1 times if tail calls are done)
            var n_pops = 1 + this.tco_counter[this.tco_counter.length - 1];
            this.call_stack.splice(-n_pops);
            this.tco_counter.pop();
            break;
          default:
            throw new Bug$1("unknown opecode type: " + x[0]);
        }
      }

      //      if(ret === null)
      //        throw new Bug("interpreter exited in unusual way");
      //      else
      //        return ret;
      return a;
    },

    // Compile and evaluate Scheme program
    evaluate: function (str, after_evaluate) {
      this.call_stack = [];
      this.parser = new Parser(str);
      this.compiler = new Compiler();
      if (after_evaluate) this.after_evaluate = after_evaluate;

      //Console.puts("executing: " + str);

      this.is_top = true;
      this.file_stack = [];

      try {
        return this.resume(false);
      } catch (e) {
        e.message = e.message + " [" + this.call_stack.join(", ") + "]";
        return this.on_error(e);
      }
    },

    // Resume evaluation
    // (internally used from Interpreter#execute and Pause#resume)
    resume: function (is_resume, a, x, f, c, s) {
      var ret = undef;

      for (;;) {
        if (is_resume) {
          ret = this._execute(a, x, f, c, s);
          is_resume = false;
        } else {
          if (!this.parser) break; // adhoc: when Pause is used via invoke_closure
          var expr = this.parser.getObject();
          if (expr === Parser.EOS) break;

          // expand
          expr = Compiler.expand(expr);

          // compile
          var opc = this.compiler.run(expr);
          //Console.p(opc);

          // execute
          ret = this._execute(expr, opc, 0, [], 0);
        }

        if (ret instanceof Pause) {
          //suspend evaluation
          return ret;
        }
      }

      // finished executing all forms
      this.after_evaluate(ret);
      return ret;
    },

    // Invoke a scheme closure
    invoke_closure: function (closure, args) {
      args || (args = []);
      var n_args = args.length;

      var x = [
        "constant",
        n_args,
        ["argument", ["constant", closure, ["apply"]]],
      ];
      for (var i = 0; i < n_args; i++)
        x = ["constant", args[i], ["argument", x]];

      return this._execute(closure, ["frame", x, ["halt"]], 0, closure, 0);
    },

    // only compiling (for debug use only)
    compile: function (str) {
      var obj = Interpreter.read(str);
      var opc = Compiler.compile(obj);
      return opc;
    },

    // before, after: Scheme closure
    push_dynamic_winder: function (before, after) {
      this.current_dynamic_winder = new Interpreter.DynamicWind(
        this.current_dynamic_winder,
        before,
        after
      );
    },

    pop_dynamic_winder: function (before, after) {
      this.current_dynamic_winder = this.current_dynamic_winder.parent;
    },
  });

  // Take a string and returns an expression.
  Interpreter.read = function (str) {
    var parser = new Parser(str);
    var r = parser.getObject();
    return r == Parser.EOS ? eof : r;
  };

  Interpreter.expand = function () {
    throw "Interpreter.expand is moved to Compiler.expand";
  };

  //
  // dynamic-wind
  //

  Interpreter.DynamicWind = Class.create({
    initialize: function (parent, before, after) {
      // Parent `DynamicWind` obj
      this.parent = parent;
      // "before" winder (Scheme closure)
      this.before = before;
      // "after" winder (Scheme closure)
      this.after = after;
    },
  });

  // A special value indicates the root of the winders
  // (the string is just for debugging purpose.)
  Interpreter.DynamicWind.ROOT = { _: "this is ROOT." };

  // Return the list of winders to call
  Interpreter.DynamicWind.listWinders = function (from, to) {
    // List winders from `from` to `ROOT`
    var fromStack = [from];
    while (from !== Interpreter.DynamicWind.ROOT) {
      from = from.parent;
      fromStack.push(from);
    }

    // List winders from `to` to `ROOT` and find the common one
    var toStack = [];
    var common;
    while (true) {
      var matched = fromStack.find(function (item) {
        return item === to;
      });
      if (matched) {
        common = matched;
        break;
      }
      toStack.push(to);
      to = to.parent;
    }

    // List `after`s to call
    var ret = [];
    for (var i = 0; i < fromStack.length; i++) {
      if (fromStack[i] === common) break;
      ret.push(fromStack[i].after);
    }
    // List `before`s to call
    toStack.reverse();
    toStack.forEach(function (item) {
      ret.push(item.before);
    });

    return ret;
  };

  // Return an opecode to run all the winders
  Interpreter.DynamicWind.joinWinders = function (winders, x) {
    return winders.reduceRight(function (acc, winder) {
      return [
        "frame",
        ["constant", 0, ["argument", ["constant", winder, ["apply"]]]],
        acc,
      ];
    }, x);
  };

  //
  // number.js
  //

  //
  // Complex
  //
  const Complex = Class.create({
    initialize: function (real, imag) {
      this.real = real;
      this.imag = imag;
    },
    magnitude: function () {
      return Math.sqrt(this.real * this.real + this.imag * this.imag);
    },
    angle: function () {
      return Math.atan2(this.imag, this.real);
    },
    isReal: function () {
      return this.imag == 0;
    },
    isRational: function () {
      return this.imag == 0 && isRational(this.real);
    },
    isInteger: function () {
      return this.imag == 0 && isInteger(this.real);
    },
    toString: function (radix) {
      if (this.real === 0 && this.imag === 0) return "0";
      var img = "";
      if (this.imag !== 0) {
        if (this.imag > 0 && this.real !== 0) {
          img += "+";
        }
        switch (this.imag) {
          case 1:
            break;
          case -1:
            img += "-";
            break;
          default:
            img += this.imag.toString(radix);
        }
        img += "i";
      }
      var real = "";
      if (this.real !== 0) {
        real += this.real.toString(radix);
      }
      return real + img;
    },
  });
  Complex.from_polar = function (r, theta) {
    var real = r * Math.cos(theta);
    var imag = r * Math.sin(theta);
    return new Complex(real, imag);
  };
  Complex.assure = function (num) {
    if (num instanceof Complex) return num;
    else return new Complex(num, 0);
  };

  //
  // Rational (unfinished)
  //
  const Rational = Class.create({
    initialize: function (numerator, denominator) {
      this.numerator = numerator;
      this.denominator = denominator;
    },

    isInteger: function () {
      // FIXME
    },
  });

  //
  // Predicates
  //
  const isNumber$1 = function (x) {
    return (
      x instanceof Complex || x instanceof Rational || typeof x == "number"
    );
  };
  const isComplex = isNumber$1;
  const isReal = function (x) {
    if (x instanceof Complex || x instanceof Rational) {
      return x.isReal();
    } else {
      return typeof x == "number";
    }
  };
  const isRational = function (x) {
    if (x instanceof Complex) {
      return x.isRational();
    } else if (x instanceof Rational) {
      return true;
    } else {
      return typeof x == "number";
    }
  };
  const isInteger = function (x) {
    if (x instanceof Complex || x instanceof Rational) {
      return x.isInteger();
    } else {
      return typeof x == "number" && x % 1 == 0;
    }
  };

  //
  // R7RS Promise (lazy library)
  //
  const BiwaPromise = Class.create({
    initialize: function (done, thunk_or_value) {
      this.box = [done, thunk_or_value];
    },

    // Return true when this promise is already calculated
    is_done: function () {
      return this.box[0];
    },

    // Return calculated value of this promise
    value: function () {
      if (!this.is_done()) {
        throw new Bug$1("this promise is not calculated yet");
      }
      return this.box[1];
    },

    thunk: function () {
      if (this.is_done()) {
        throw new Bug$1("this promise does not know the thunk");
      }
      return this.box[1];
    },

    update_with: function (new_promise) {
      this.box[0] = new_promise.box[0];
      this.box[1] = new_promise.box[1];
      new_promise.box = this.box;
    },
  });

  const isPromise = function (obj) {
    return obj instanceof BiwaPromise;
  };

  // Create fresh promise
  BiwaPromise.fresh = function (thunk) {
    return new BiwaPromise(false, thunk);
  };
  // Create calculated promise
  BiwaPromise.done = function (value) {
    return new BiwaPromise(true, value);
  };

  ///
  /// infra.js - Basis for library functions
  ///

  //
  // define_*func - define library functions
  //
  const check_arity = function (fname, len, min, max) {
    if (len < min) {
      if (max && max == min)
        throw new BiwaError(
          fname +
            ": wrong number of arguments (expected: " +
            min +
            " got: " +
            len +
            ")"
        );
      else
        throw new BiwaError(
          fname + ": too few arguments (at least: " + min + " got: " + len + ")"
        );
    } else if (max && max < len)
      throw new BiwaError(
        fname + ": too many arguments (at most: " + max + " got: " + len + ")"
      );
  };

  const define_libfunc = function (fname, min, max, func) {
    var f = function (ar, intp) {
      check_arity(fname, ar.length, min, max);
      return func(ar, intp);
    };

    func["fname"] = fname; // for assert_*
    f["inspect"] = function () {
      return this.fname;
    };
    CoreEnv[fname] = f;
  };

  const alias_libfunc = function (fname, aliases) {
    if (CoreEnv[fname]) {
      if (isArray(aliases)) {
        map(aliases, function (a) {
          alias_libfunc(fname, a);
        });
      } else if (isString$1(aliases)) {
        CoreEnv[aliases] = CoreEnv[fname];
      } else {
        console.error(
          "[BUG] bad alias for library function " +
            "`" +
            fname +
            "': " +
            aliases.toString()
        );
      }
    } else {
      console.error(
        "[BUG] library function " +
          "`" +
          fname +
          "'" +
          " does not exist, so can't alias it."
      );
    }
  };

  const define_syntax = function (sname, func) {
    var s = new Syntax(sname, func);
    CoreEnv[sname] = s;
  };

  const define_scmfunc = function (fname, min, max, str) {
    new Interpreter().evaluate("(define " + fname + " " + str + "\n)");
  };

  //  define_scmfunc("map+", 2, null,
  //    "(lambda (proc ls) (if (null? ls) ls (cons (proc (car ls)) (map proc (cdr ls)))))");

  //
  // assertions - type checks
  //

  const assert_number = make_simple_assert("number", function (obj) {
    return typeof obj == "number" || obj instanceof Complex;
  });

  const assert_integer = make_simple_assert("integer", function (obj) {
    return typeof obj == "number" && obj % 1 == 0;
  });

  const assert_real = make_simple_assert("real number", function (obj) {
    return typeof obj == "number";
  });

  const assert_between = make_assert(function (fname, obj, from, to) {
    if (typeof obj != "number" || obj != Math.round(obj)) {
      throw new BiwaError(
        fname + ": " + "number required, but got " + to_write$1(obj)
      );
    }

    if (obj < from || to < obj) {
      throw new BiwaError(
        fname +
          ": " +
          "number must be between " +
          from +
          " and " +
          to +
          ", but got " +
          to_write$1(obj)
      );
    }
  });

  const assert_string = make_simple_assert("string", isString);
  const assert_char = make_simple_assert("character", isChar);
  const assert_symbol = make_simple_assert("symbol", isSymbol);
  const assert_port = make_simple_assert("port", isPort);
  const assert_pair = make_simple_assert("pair", isPair);
  const assert_list = make_simple_assert("list", isList);
  const assert_vector = make_simple_assert("vector", isVector);
  const assert_hashtable = make_simple_assert("hashtable", isHashtable);
  const assert_promise = make_simple_assert("promise", isPromise);

  const assert_function = make_simple_assert("JavaScript function", isFunction);
  const assert_closure = make_simple_assert("scheme function", isClosure);
  const assert_procedure = make_simple_assert("scheme/js function", function (
    obj
  ) {
    return isClosure(obj) || isFunction(obj);
  });

  const assert_date = make_simple_assert("date", function (obj) {
    // FIXME: this is not accurate (about cross-frame issue)
    // https://prototype.lighthouseapp.com/projects/8886/tickets/443
    return obj instanceof Date;
  });

  //var assert_instance_of = BiwaScheme.make_assert(function(fname, type, obj, klass){
  //  if(!(obj instanceof klass)){
  //    throw new BiwaScheme.Error(fname + ": " +
  //                               type + " required, but got " +
  //                               BiwaScheme.to_write(obj));
  //  }
  //});

  const assert = make_assert(function (fname, success, message, _fname) {
    if (!success) {
      throw new BiwaError((_fname || fname) + ": " + message);
    }
  });

  //
  // deprecation
  //

  // Show deprecation warnig
  // @param {string} title - feature to be deprecated
  // @param {string} ver - when it will be removed (eg. "1.0")
  // @param {string} alt - alternatives
  const deprecate = function (title, ver, alt) {
    var msg =
      title +
      " is deprecated and will be removed in BiwaScheme " +
      ver +
      ". " +
      "Please use " +
      alt +
      " instead";
    console.warn(msg);
  };

  //
  // utils
  //

  // Parses a fractional notation in the format: <num>/<denom> (e.g. 3/7, -9/4),
  // where <num> is a valid integer notation, and <denom> is a valid notation
  // for a positive integer.
  //
  // Returns a float if the notation is valid, otherwise false.
  //
  // @param {string} rep - the string representation of the fraction
  // @return {float|false}
  const parse_fraction = function (rep) {
    assert_string(rep);

    var frac_parts = rep.split("/");

    if (frac_parts.length !== 2) return false;

    var num_rep = frac_parts[0];
    var denom_rep = frac_parts[1];

    var num = parse_integer(num_rep, 10);
    var denom = parse_integer(denom_rep, 10);

    if (num === false || denom === false) return false;

    if (denom <= 0) return false;

    return num / denom;
  };

  // Given a string notation of an integer, and the radix, validates the
  // notation: returns true if the notation is valid, otherwise false.
  //
  // @param {string} rep - the string representation of the integer
  // @param {integer} rdx - the radix, where 2 <= rdx <= 36
  // @return {boolean}
  const is_valid_integer_notation = function (rep, rdx) {
    assert_string(rep);
    assert_integer(rdx);

    if (rdx < 2 || rdx > 36) return false;

    var rdx_symbols = "0123456789abcdefghijklmnopqrstuvwxyz";

    var valid_symbols = rdx_symbols.slice(0, rdx);
    var sym_regex = new RegExp("^[+-]?" + "[" + valid_symbols + "]+$", "ig");

    return sym_regex.test(rep);
  };

  // Parse an integer. If the integer does not have a valid representation, or
  // produces NaN, - false is returned. If the radix is not within [2..36]
  // range, false is returned as well.
  //
  // @param {string} rep - the string representation of the integer
  // @param {integer} rdx - the radix, where 2 <= rdx <= 36
  // @return {integer|false}
  const parse_integer = function (rep, rdx) {
    assert_string(rep);
    assert_integer(rdx);

    if (rdx < 2 || rdx > 36) return false;

    if (!is_valid_integer_notation(rep, rdx)) return false;

    var res = parseInt(rep, rdx);

    if (Number.isNaN(res)) return false;

    return res;
  };

  // Given a string notation of a floating-point number in the standard or
  // scientific notation, returns true if the notation valid, otherwise false.
  //
  // For example:
  // "1"      -> true
  // "1."     -> true
  // "1.23"   -> true
  // "1e4"    -> true
  // "1E4"    -> true
  // "1E4.34" -> false
  // "e34"    -> false
  //
  // @param {string} rep - the string representation of the float.
  // @return {boolean}
  const is_valid_float_notation = function (rep) {
    assert_string(rep);

    var sci_regex = /^[+-]?[0-9]+[.]?[0-9]*e[+-]?[0-9]+$/i;
    var fp_regex = /(^[+-]?[0-9]*[.][0-9]+$)|(^[+-]?[0-9]+[.][0-9]*$)/;

    if (sci_regex.test(rep) || fp_regex.test(rep)) return true;

    return is_valid_integer_notation(rep, 10);
  };

  // Parse a floating-point number. If the floating-point number does not have a
  // valid representation, or produces -Infinity, +Infinity or NaN, - false is
  // returned.
  //
  // @param {string} rep - the string representation of the floating-point value
  // @return {float|false}
  const parse_float = function (rep) {
    assert_string(rep);

    if (!is_valid_float_notation(rep)) return false;

    var res = new Number(rep).valueOf();

    if (Number.isNaN(res)) return false;

    if (!Number.isFinite(res)) return false;

    return res;
  };

  //
  // R6RS Enumerations
  // http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-15.html#node_chap_14
  //
  // Example
  //
  //   (define-enumeration color
  //     (black white purple maroon)
  //     color-set)
  //
  //   (color black)                  ;=> 'black
  //   (color purpel)                 ;=> &syntax exception
  //   (enum-set->list
  //     (color-set maroon white))    ;=> #<enum-set (white maroon)>

  const Enumeration = {};

  // Represents an enum_type.
  //
  // Becuase there is no way to access an EnumType directly from Scheme,
  // EnumType#to_write is not defined.
  //
  // Properties
  //
  // members - Array of symbols (no duplicate)
  //
  Enumeration.EnumType = Class.create({
    // Creates a new enum_type.
    //
    // members - Array of symbols.
    //           Symbols may be duplicate (I think you shouldn't, though :-p).
    initialize: function (members) {
      this.members = uniq(members);
    },

    // Returns an EnumSet.
    universe: function () {
      return new Enumeration.EnumSet(this, this.members);
    },

    // Returns a function which map a symbol to an integer (or #f, if
    // the symbol is out of the universe).
    //
    // Implementation note: don't forget this.members may have duplicates.
    indexer: function () {
      // ar[0] - a symbol
      // Returns an integer or #f.
      return bind(function (ar) {
        assert_symbol(ar[0], "(enum-set indexer)");
        var idx = indexOf(this.members, ar[0]);
        return idx === -1 ? false : idx;
      }, this);
    },

    // Retuns a function which creates an enum_set from a list of
    // symbols (Symbols may be duplicate.)
    constructor: function () {
      // ar[0] - a list of symbol
      // Returns a enum_set.
      return bind(function (ar) {
        assert_list(ar[0], "(enum-set constructor)");
        var symbols = ar[0].to_array();
        each(symbols, function (arg) {
          assert_symbol(arg, "(enum-set constructor)");
        });

        return new Enumeration.EnumSet(this, symbols);
      }, this);
    },
  });
  Class.memoize(Enumeration.EnumType, ["universe", "indexer", "constructor"]);

  // Represents an enum_set of an enum_type.
  //
  // Properties
  //
  // enum_type - The enum_type.
  // symbols   - Array of symbols (no duplicate, properly ordered)
  //
  Enumeration.EnumSet = Class.create({
    // Creates a new enum_set.
    //
    // enum_type - An EnumType
    // symbols   - Array of symbols.
    //
    // initialize normalizes symbols.
    //   - remove duplicates
    //   - order by universe
    initialize: function (enum_type, symbols) {
      this.enum_type = enum_type;
      this.symbols = filter(enum_type.members, function (sym) {
        return contains(symbols, sym);
      });
    },

    // Returns a list of symbols.
    symbol_list: function () {
      return array_to_list(this.symbols);
    },

    // Returns true if the enum_set includes the symbol.
    // 'symbol' is allowed to be a symbol which is not included in the universe.
    is_member: function (symbol) {
      return contains(this.symbols, symbol);
    },

    // Returns true if:
    // - the enum_set is a subset of the enum_set 'other', and
    // - the universe of the enum_set is a subset of
    //   the universe of 'other'.
    // The enum_set and 'other' may belong to different enum_type.
    is_subset: function (other) {
      // Check elements
      if (
        some(this.symbols, function (sym) {
          return !contains(other.symbols, sym);
        })
      ) {
        return false;
      }

      // Check universe
      if (this.enum_type === other.enum_type) {
        return true;
      } else {
        return every(this.enum_type.members, function (sym) {
          return contains(other.enum_type.members, sym);
        });
      }
    },

    // Returns true if:
    //   - the enum_set contains the same set of symbols as 'other', and
    //   - universe of the enum_set contains the same set of symbols
    //     as the universe of 'other'.
    //
    // The enum_set and 'other' may belong to different enum_type.
    equal_to: function (other) {
      return this.is_subset(other) && other.is_subset(this);
    },

    // Returns a enum_set which has:
    // - all the symbols included in the enum_set or the enum_set 'other'.
    // The enum_set and 'other' *must* belong to the same enum_type.
    union: function (other) {
      var syms = filter(
        this.enum_type.members,
        bind(function (sym) {
          return contains(this.symbols, sym) || contains(other.symbols, sym);
        }, this)
      );
      return new Enumeration.EnumSet(this.enum_type, syms);
    },

    // Returns a enum_set which has:
    // - the symbols included both in the enum_set or the enum_set 'other'.
    // The enum_set and 'other' *must* belong to the same enum_type.
    intersection: function (other) {
      var syms = filter(this.symbols, function (sym) {
        return contains(other.symbols, sym);
      });
      return new Enumeration.EnumSet(this.enum_type, syms);
    },

    // Returns a enum_set which has:
    // - the symbols included in the enum_set and not in the enum_set 'other'.
    // The enum_set and 'other' *must* belong to the same enum_type.
    difference: function (other) {
      var syms = filter(this.symbols, function (sym) {
        return !contains(other.symbols, sym);
      });
      return new Enumeration.EnumSet(this.enum_type, syms);
    },

    // Returns a enum_set which has:
    // - the symbols included in the universe but not in the enum_set.
    complement: function () {
      var syms = filter(
        this.enum_type.members,
        bind(function (sym) {
          return !contains(this.symbols, sym);
        }, this)
      );
      return new Enumeration.EnumSet(this.enum_type, syms);
    },

    // Returns a enum_set which has:
    // - the symbols included in the enum_set and the universe of the enum_set 'other'.
    // The enum_set and 'other' may belong to different enum_type.
    projection: function (other) {
      var syms = filter(this.symbols, function (sym) {
        return contains(other.enum_type.members, sym);
      });
      return new Enumeration.EnumSet(other.enum_type, syms);
    },

    // Returns a string which represents the enum_set.
    toString: function () {
      return "#<EnumSet " + inspect(this.symbols) + ">";
    },
  });
  Class.memoize(Enumeration.EnumSet, "symbol_list");

  const isEnumSet = function (obj) {
    return obj instanceof Enumeration.EnumSet;
  };

  const assert_enum_set = make_simple_assert("enum_set", isEnumSet);

  //
  // R6RS Records
  // http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-7.html#node_chap_6
  //
  // Record is like struct in C, but supports more feature like inheritance.
  // see also: src/library/r6rs_lib.js

  //
  // Record
  // represents each instance of record type
  //
  const Record = Class.create({
    initialize: function (rtd, values) {
      assert_record_td(rtd, "new Record");

      this.rtd = rtd;
      this.fields = values;
    },

    get: function (k) {
      return this.fields[k];
    },

    set: function (k, v) {
      this.fields[k] = v;
    },

    toString: function () {
      var contents = to_write$1(this.fields);
      return "#<Record " + this.rtd.name + " " + contents + ">";
    },
  });

  const isRecord = function (o) {
    return o instanceof Record;
  };

  // Defined record types
  Record._DefinedTypes = {};

  Record.define_type = function (name_str, rtd, cd) {
    return (Record._DefinedTypes[name_str] = { rtd: rtd, cd: cd });
  };
  Record.get_type = function (name_str) {
    return Record._DefinedTypes[name_str];
  };

  //
  // RTD (Record type descriptor)
  //
  Record.RTD = Class.create({
    //                   Symbol RTD        Symbol Bool  Bool    Array
    initialize: function (name, parent_rtd, uid, sealed, opaque, fields) {
      this.name = name;
      this.parent_rtd = parent_rtd;
      this.is_base_type = !parent_rtd;

      if (uid) {
        this.uid = uid;
        this.generative = false;
      } else {
        this.uid = this._generate_new_uid();
        this.generative = true;
      }

      this.sealed = !!sealed;
      this.opaque = parent_rtd.opaque || !!opaque;

      this.fields = map(fields, function (field) {
        return { name: field[0], mutable: !!field[1] };
      });
    },

    // Returns the name of the k-th field.
    // Only used for error messages.
    field_name: function (k) {
      var names = this._field_names();

      for (var par = this.parent_rtd; par; par = par.parent_rtd) {
        names = par._field_names() + names;
      }

      return names[k];
    },
    _field_names: function () {
      return map(this.fields, function (spec) {
        return spec.name;
      });
    },

    _generate_new_uid: function () {
      return Sym(uniqueId("__record_td_uid"));
    },

    toString: function () {
      return "#<RecordTD " + name + ">";
    },
  });

  Record.RTD.NongenerativeRecords = {};
  const isRecordTD = function (o) {
    return o instanceof Record.RTD;
  };

  //
  // CD (Record constructor descriptor)
  //
  Record.CD = Class.create({
    initialize: function (rtd, parent_cd, protocol) {
      this._check(rtd, parent_cd, protocol);
      this.rtd = rtd;
      this.parent_cd = parent_cd;
      if (protocol) {
        this.has_custom_protocol = true;
        this.protocol = protocol;
      } else {
        this.has_custom_protocol = false;
        if (rtd.parent_rtd)
          this.protocol = this._default_protocol_for_derived_types();
        else this.protocol = this._default_protocol_for_base_types();
      }
    },

    _check: function (rtd, parent_cd, protocol) {
      if (rtd.is_base_type && parent_cd)
        throw new Error(
          "Record.CD.new: cannot specify parent cd of a base type"
        );

      if (parent_cd && rtd.parent_rtd && parent_cd.rtd != rtd.parent_rtd)
        throw new Error(
          "Record.CD.new: mismatched parents between rtd and parent_cd"
        );

      if (rtd.parent_rtd && !parent_cd && protocol)
        throw new Error(
          "Record.CD.new: protocol must be #f when parent_cd is not given"
        );

      if (parent_cd && parent_cd.has_custom_protocol && !protocol)
        throw new Error(
          "Record.CD.new: protocol must be specified when parent_cd has a custom protocol"
        );
    },

    _default_protocol_for_base_types: function () {
      // (lambda (p) p)
      // called with `p' as an argument
      return function (ar) {
        var p = ar[0];
        assert_procedure(p, "_default_protocol/base");
        return p;
      };
    },

    _default_protocol_for_derived_types: function () {
      // (lambda (n)
      //   (lambda (a b x y s t)
      //     (let1 p (n a b x y) (p s t))))
      // called with `n' as an argument
      var rtd = this.rtd;
      return function (ar) {
        var n = ar[0];
        assert_procedure(n, "_default_protocol/n");

        var ctor = function (args) {
          var my_argc = rtd.fields.length;
          var ancestor_argc = args.length - my_argc;

          var ancestor_values = args.slice(0, ancestor_argc);
          var my_values = args.slice(ancestor_argc);

          // (n a b x y) => p
          return new Call(n, ancestor_values, function (ar) {
            var p = ar[0];
            assert_procedure(p, "_default_protocol/p");

            // (p s t) => record
            return new Call(p, my_values, function (ar) {
              var record = ar[0];
              assert_record(record, "_default_protocol/result");

              return record;
            });
          });
        };
        return ctor;
      };
    },

    toString: function () {
      return "#<RecordCD " + this.rtd.name + ">";
    },

    record_constructor: function () {
      var arg_for_protocol = this.parent_cd
        ? this._make_n([], this.rtd)
        : this._make_p();
      arg_for_protocol = bind(arg_for_protocol, this);

      return new Call(this.protocol, [arg_for_protocol], function (ar) {
        var ctor = ar[0];
        assert_procedure(ctor, "record_constructor");
        return ctor;
      });
    },

    // Create the function `p' which is given to the protocol.
    _make_p: function () {
      return function (values) {
        return new Record(this.rtd, values);
        // TODO: check argc
      };
    },

    // Create the function `n' which is given to the protocol.
    // When creating an instance of a derived type,
    // _make_n is called for each ancestor rtd's.
    _make_n: function (children_values, rtd) {
      var parent_cd = this.parent_cd;

      if (parent_cd) {
        // called from protocol (n)
        var n = function (args_for_n) {
          // called from protocol (p)
          var p = function (args_for_p) {
            var values = [].concat(args_for_p[0]).concat(children_values);
            var parent_n = parent_cd._make_n(values, rtd);

            return new Call(parent_cd.protocol, [parent_n], function (ar) {
              var ctor = ar[0];
              assert_procedure(ctor, "_make_n");

              return new Call(ctor, args_for_n, function (ar) {
                var record = ar[0];
                assert_record(record);
                return record;
              });
            });
          };
          return p;
        };
        return n;
      } else {
        var n = function (my_values) {
          var values = my_values.concat(children_values);
          return new Record(rtd, values);
          // TODO: check argc
        };
        return n;
      }
    },
  });

  const isRecordCD = function (o) {
    return o instanceof Record.CD;
  };

  const assert_record = make_simple_assert("record", isRecord);
  const assert_record_td = make_simple_assert(
    "record type descriptor",
    isRecordTD
  );
  const assert_record_cd = make_simple_assert(
    "record constructor descriptor",
    isRecordCD
  );

  //
  // Values
  //
  const Values$1 = Class.create({
    initialize: function (values) {
      this.content = values;
    },
    to_write: function () {
      return "#<Values " + map(this.content, to_write$1).join(" ") + ">";
    },
  });

  const Console = {};

  // Actual implementation is in src/platforms/*/console.js

  define_libfunc("html-escape", 1, 1, function (ar) {
    assert_string(ar[0]);
    return escape(ar[0]);
  });
  const inspect_objs = function (objs) {
    return map(objs, inspect).join(", ");
  };
  define_libfunc("inspect", 1, null, function (ar) {
    return inspect_objs(ar);
  });
  define_libfunc("inspect!", 1, null, function (ar) {
    Console.puts(inspect_objs(ar));
    return undef;
  });

  //
  // json
  //
  // json->sexp
  // Array -> list
  // Object -> alist
  // (number, boolean, string,
  //
  const json2sexp = function (json) {
    switch (true) {
      case isNumber$2(json) ||
        isString$1(json) ||
        json === true ||
        json === false:
        return json;
      case isArray(json):
        return array_to_list(map(json, json2sexp));
      case typeof json == "object":
        var ls = nil$1;
        for (key in json) {
          ls = new Pair(new Pair(key, json2sexp(json[key])), ls);
        }
        return ls;
      default:
        throw new Error(
          "json->sexp: detected invalid value for json: " + inspect(json)
        );
    }
  };
  define_libfunc("json->sexp", 1, 1, function (ar) {
    return json2sexp(ar[0]);
  });

  // (vector-push! v item1 item2 ...)
  define_libfunc("vector-push!", 2, null, function (ar) {
    assert_vector(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      ar[0].push(ar[i]);
    }
    return ar[0];
  });

  //
  //from Gauche
  //

  // (identity obj)
  // Returns obj.
  define_libfunc("identity", 1, 1, function (ar) {
    return ar[0];
  });

  // (inc! i)
  // = (begin (set! i (+ i 1)) i)
  // Increments i (i.e., set i+1 to i).
  define_syntax("inc!", function (x) {
    var target = x.cdr.car;
    return List(
      Sym("begin"),
      List(Sym("set!"), target, List(Sym("+"), target, 1)),
      target
    );
  });

  // (dec! i)
  // = (begin (set! i (- i 1)) i)
  // Decrements i (i.e., set i-1 to i).
  define_syntax("dec!", function (x) {
    var target = x.cdr.car;
    return List(
      Sym("begin"),
      List(Sym("set!"), target, List(Sym("-"), target, 1)),
      target
    );
  });

  // string

  define_libfunc("string-concat", 1, 1, function (ar) {
    assert_list(ar[0]);
    return ar[0].to_array().join("");
  });

  define_libfunc("string-split", 2, 2, function (ar) {
    assert_string(ar[0]);
    assert_string(ar[1]);
    return array_to_list(ar[0].split(ar[1]));
  });

  define_libfunc("string-join", 1, 2, function (ar) {
    assert_list(ar[0]);
    var delim = "";
    if (ar[1]) {
      assert_string(ar[1]);
      delim = ar[1];
    }
    return ar[0].to_array().join(delim);
  });

  // lists

  define_libfunc("intersperse", 2, 2, function (ar) {
    var item = ar[0],
      ls = ar[1];
    assert_list(ls);

    var ret = [];
    each(ls.to_array().reverse(), function (x) {
      ret.push(x);
      ret.push(item);
    });
    ret.pop();
    return array_to_list(ret);
  });

  define_libfunc("map-with-index", 2, null, function (ar) {
    var proc = ar.shift(),
      lists = ar;
    each(lists, assert_list);

    var results = [],
      i = 0;
    return Call.multi_foreach(lists, {
      call: function (xs) {
        var args = map(xs, function (x) {
          return x.car;
        });
        args.unshift(i);
        i++;
        return new Call(proc, args);
      },
      result: function (res) {
        results.push(res);
      },
      finish: function () {
        return array_to_list(results);
      },
    });
  });

  // loop

  // (dotimes (variable limit result) body ...)
  // Iterate with variable 0 to limit-1.
  // ->
  //    (do ((tlimit limit)
  //         (variable 0 (+ variable 1)))
  //        ((>= variable tlimit) result)
  //      body ...)
  define_syntax("dotimes", function (x) {
    var spec = x.cdr.car,
      bodies = x.cdr.cdr;
    var variable = spec.car,
      limit = spec.cdr.car,
      result = spec.cdr.cdr.car;
    var tlimit = gensym();

    var do_vars = deep_array_to_list([
      [tlimit, limit],
      [variable, 0, [Sym("+"), variable, 1]],
    ]);
    var do_check = deep_array_to_list([[Sym(">="), variable, tlimit], result]);

    return new Pair(Sym("do"), new Pair(do_vars, new Pair(do_check, bodies)));
  });

  // sorting (Obsolete: use list-sort, etc. instead of these.)

  // utility function. takes a JS Array and a Scheme procedure,
  // returns sorted array
  var sort_with_comp = function (ary, proc, intp) {
    return ary.sort(function (a, b) {
      var intp2 = new Interpreter(intp);
      return intp2.invoke_closure(proc, [a, b]);
    });
  };

  define_libfunc("list-sort/comp", 1, 2, function (ar, intp) {
    assert_procedure(ar[0]);
    assert_list(ar[1]);

    return array_to_list(sort_with_comp(ar[1].to_array(), ar[0], intp));
  });
  define_libfunc("vector-sort/comp", 1, 2, function (ar, intp) {
    assert_procedure(ar[0]);
    assert_vector(ar[1]);

    return sort_with_comp(clone(ar[1]), ar[0], intp);
  });
  define_libfunc("vector-sort/comp!", 1, 2, function (ar, intp) {
    assert_procedure(ar[0]);
    assert_vector(ar[1]);

    sort_with_comp(ar[1], ar[0], intp);
    return undef;
  });

  // macros

  //(define-macro (foo x y) body ...)
  //(define-macro foo lambda)

  var rearrange_args = function (expected, given) {
    var args = [];
    var dotpos = new Compiler().find_dot_pos(expected);
    if (dotpos == -1) args = given;
    else {
      for (var i = 0; i < dotpos; i++) {
        args[i] = given[i];
      }
      args[i] = array_to_list(given.slice(i));
    }
    return args;
  };
  define_syntax("define-macro", function (x) {
    var head = x.cdr.car;
    var expected_args;
    if (head instanceof Pair) {
      var name = head.car;
      expected_args = head.cdr;
      var body = x.cdr.cdr;
      var lambda = new Pair(Sym("lambda"), new Pair(expected_args, body));
    } else {
      var name = head;
      var lambda = x.cdr.cdr.car;
      expected_args = lambda.cdr.car;
    }

    //["close", <args>, <n>, <body>, <opecodes_next>, <dotpos>]
    var opc = Compiler.compile(lambda);
    if (opc[2] != 0)
      throw new Bug(
        "you cannot use free variables in macro expander (or define-macro must be on toplevel)"
      );
    var cls = makeClosure([opc[3]]);

    TopEnv[name.name] = new Syntax(name.name, function (sexp) {
      var given_args = sexp.to_array();

      given_args.shift();

      var intp = new Interpreter();
      var args = rearrange_args(expected_args, given_args);
      var result = intp.invoke_closure(cls, args);
      return result;
    });

    return undef;
  });

  var macroexpand_1 = function (x) {
    if (x instanceof Pair) {
      // TODO: Should we check CoreEnv too?
      if (x.car instanceof BiwaSymbol && TopEnv[x.car.name] instanceof Syntax) {
        var transformer = TopEnv[x.car.name];
        x = transformer.transform(x);
      } else
        throw new Error(
          "macroexpand-1: `" + to_write_ss(x) + "' is not a macro"
        );
    }
    return x;
  };
  define_syntax("%macroexpand", function (x) {
    var expanded = Compiler.expand(x.cdr.car);
    return List(Sym("quote"), expanded);
  });
  define_syntax("%macroexpand-1", function (x) {
    var expanded = macroexpand_1(x.cdr.car);
    return List(Sym("quote"), expanded);
  });

  define_libfunc("macroexpand", 1, 1, function (ar) {
    return Compiler.expand(ar[0]);
  });
  define_libfunc("macroexpand-1", 1, 1, function (ar) {
    return macroexpand_1(ar[0]);
  });

  define_libfunc("gensym", 0, 0, function (ar) {
    return gensym();
  });

  // i/o

  define_libfunc("print", 1, null, function (ar) {
    map(ar, function (item) {
      Console.puts(to_display(item), true);
    });
    Console.puts(""); //newline
    return undef;
  });
  define_libfunc("write-to-string", 1, 1, function (ar) {
    return to_write$1(ar[0]);
  });
  define_libfunc("read-from-string", 1, 1, function (ar) {
    assert_string(ar[0]);
    return Interpreter.read(ar[0]);
  });
  define_libfunc("port-closed?", 1, 1, function (ar) {
    assert_port(ar[0]);
    return !ar[0].is_open;
  });
  //define_libfunc("with-input-from-port", 2, 2, function(ar){
  //define_libfunc("with-error-to-port", 2, 2, function(ar){
  define_libfunc("with-output-to-port", 2, 2, function (ar) {
    var port = ar[0],
      proc = ar[1];
    assert_port(port);
    assert_procedure(proc);

    var original_port = Port.current_output;
    Port.current_output = port;

    return new Call(proc, [port], function (ar) {
      port.close();
      Port.current_output = original_port;

      return ar[0];
    });
  });

  // syntax

  define_syntax("let1", function (x) {
    //(let1 vari expr body ...)
    //=> ((lambda (var) body ...) expr)
    var vari = x.cdr.car;
    var expr = x.cdr.cdr.car;
    var body = x.cdr.cdr.cdr;

    return new Pair(
      new Pair(Sym("lambda"), new Pair(new Pair(vari, nil$1), body)),
      new Pair(expr, nil$1)
    );
  });

  //
  // Regular Expression
  //
  var assert_regexp = function (obj, fname) {
    if (!(obj instanceof RegExp))
      throw new Error(fname + ": regexp required, but got " + to_write$1(obj));
  };

  //Function: string->regexp string &keyword case-fold
  define_libfunc("string->regexp", 1, 1, function (ar) {
    assert_string(ar[0], "string->regexp");
    return new RegExp(ar[0]); //todo: case-fold
  });
  //Function: regexp? obj
  define_libfunc("regexp?", 1, 1, function (ar) {
    return ar[0] instanceof RegExp;
  });
  //Function: regexp->string regexp
  define_libfunc("regexp->string", 1, 1, function (ar) {
    assert_regexp(ar[0], "regexp->string");
    return ar[0].toString().slice(1, -1); //cut '/'
  });

  define_libfunc("regexp-exec", 2, 2, function (ar) {
    var rexp = ar[0];
    if (isString$1(ar[0])) {
      rexp = new RegExp(ar[0]);
    }
    assert_regexp(rexp, "regexp-exec");
    assert_string(ar[1], "regexp-exec");
    var ret = rexp.exec(ar[1]);
    return ret === null ? false : array_to_list(ret);
  });

  //  //Function: rxmatch regexp string
  //  define_libfunc("rxmatch", 1, 1, function(ar){
  //    assert_regexp(ar[0], "rxmatch");
  //    assert_string(ar[1], "rxmatch");
  //    return ar[0].match(ar[1]);
  //  });
  //Function: rxmatch-start match &optional (i 0)
  //Function: rxmatch-end match &optional (i 0)
  //Function: rxmatch-substring match &optional (i 0)
  //Function: rxmatch-num-matches match
  //Function: rxmatch-after match &optional (i 0)
  //Function: rxmatch-before match &optional (i 0)
  //Generic application: regmatch &optional index
  //Generic application: regmatch 'before &optional index
  //Generic application: regmatch 'after &optional index
  //Function: regexp-replace regexp string substitution

  // regexp-replace-all regexp string substitution
  define_libfunc("regexp-replace-all", 3, 3, function (ar) {
    var pat = ar[0];
    if (isString$1(pat)) {
      var rexp = new RegExp(pat, "g");
    } else {
      assert_regexp(pat);
      var rexp = new RegExp(pat.source, "g");
    }
    assert_string(ar[1]);
    assert_string(ar[2]);
    return ar[1].replace(rexp, ar[2]);
  });
  //Function: regexp-replace* string rx1 sub1 rx2 sub2 ...
  //Function: regexp-replace-all* string rx1 sub1 rx2 sub2 ...
  //Function: regexp-quote string
  //Macro: rxmatch-let match-expr (var ...) form ...
  //Macro: rxmatch-if match-expr (var ...) then-form else-form
  //Macro: rxmatch-cond clause ...
  //Macro: rxmatch-case string-expr clause ...

  //
  // interface to javascript
  //

  // Rebind uses of eval to the global scope, to avoid polluting downstream code.
  // See: https://github.com/rollup/rollup/wiki/Troubleshooting#avoiding-eval
  let eval2 = eval;

  define_libfunc("js-eval", 1, 1, function (ar) {
    return eval2(ar[0]);
  });
  define_libfunc("js-ref", 2, 2, function (ar) {
    if (isString$1(ar[1])) {
      return ar[0][ar[1]];
    } else {
      assert_symbol(ar[1]);
      return ar[0][ar[1].name];
    }
  });
  define_libfunc("js-set!", 3, 3, function (ar) {
    assert_string(ar[1]);
    ar[0][ar[1]] = ar[2];
    return undef;
  });

  // (js-call (js-eval "Math.pow") 2 4)
  define_libfunc("js-call", 1, null, function (ar) {
    var js_func = ar.shift();
    assert_function(js_func);

    var receiver = null;
    return js_func.apply(receiver, ar);
  });
  // (js-invoke (js-new "Date") "getTime")
  define_libfunc("js-invoke", 2, null, function (ar) {
    var js_obj = ar.shift();
    var func_name = ar.shift();
    if (!isString$1(func_name)) {
      assert_symbol(func_name);
      func_name = func_name.name;
    }
    if (js_obj[func_name]) return js_obj[func_name].apply(js_obj, ar);
    else
      throw new Error("js-invoke: function " + func_name + " is not defined");
  });

  // Short hand for JavaScript method call.
  //
  // (js-invocation obj '(foo 1 2 3))  ;=> obj.foo(1,2,3)
  // (js-invocation obj '(foo 1 2 3)   ;=> obj.foo(1,2,3)
  //                    'bar           ;      .bar
  //                    '(baz 4 5))    ;      .baz(4,5)
  // (js-invocation 'Math '(pow 2 3))  ;=> Math.pow(2,3)
  //
  // It also converts
  //   (lambda (e) ...) to
  //   (js-closure (lambda (e) ...))
  //   and
  //   '((a . b) (c . 4)) to
  //   {a: "b", c: 4}
  //
  define_libfunc("js-invocation", 2, null, function (ar, intp) {
    var receiver = ar.shift();
    // TODO: convert lambdas by js-closure
    if (isSymbol(receiver)) {
      receiver = eval2(receiver.name); //XXX: is this ok?
    }

    var v = receiver;

    // Process each method call
    each(ar, function (callspec) {
      if (isSymbol(callspec)) {
        // Property access
        v = v[callspec.name];
      } else if (isList(callspec)) {
        // Method call
        var args = callspec.to_array();

        assert_symbol(args[0]);
        var method = args.shift().name;

        // Convert arguments
        args = map(args, function (arg) {
          if (isClosure(arg)) {
            // closure -> JavaScript funciton
            return js_closure(arg, intp);
          } else if (isList(arg)) {
            // alist -> JavaScript Object
            var o = {};
            arg.foreach(function (pair) {
              assert_symbol(pair.car);
              o[pair.car.name] = pair.cdr;
            });
            return o;
          } else return arg;
        });

        // Call the method
        if (!isFunction$1(v[method])) {
          throw new BiwaError(
            "js-invocation: the method `" + method + "' not found"
          );
        }
        v = v[method].apply(v, args);
      } else {
        // (wrong argument)
        throw new BiwaError(
          "js-invocation: expected list or symbol for callspec but got " +
            inspect(callspec)
        );
      }
    });

    return v;
  });

  // TODO: provide corresponding macro ".."
  define_syntax("..", function (x) {
    if (x.cdr == nil$1) {
      throw new Error("malformed ..");
    }
    return new Pair(Sym("js-invocation"), x.cdr);
  });

  // (js-new (js-eval "Date") 2005 1 1)
  // (js-new (js-eval "Draggable") elem 'onEnd (lambda (drg) ...))
  //   If symbol is given, following arguments are converted to
  //   an js object. If any of them is a scheme closure,
  //   it is converted to js function which invokes that closure.
  //
  // (js-new "Date" 2005 1 1)
  //   You can pass javascript program string for constructor.
  define_libfunc("js-new", 1, null, function (ar, intp) {
    // make js object from key-value pair
    var array_to_obj = function (ary) {
      if (ary.length % 2 != 0)
        throw new Error("js-new: odd number of key-value pair");

      var obj = {};
      for (var i = 0; i < ary.length; i += 2) {
        var key = ary[i],
          value = ary[i + 1];
        assert_symbol(key);
        if (isClosure(value)) value = js_closure(value, intp);

        obj[key.name] = value;
      }
      return obj;
    };

    var ctor = ar.shift();
    if (isString$1(ctor)) ctor = eval2(ctor);

    if (ar.length == 0) {
      return new ctor();
    } else {
      // pack args to js object, if symbol appears
      var args = [];
      for (var i = 0; i < ar.length; i++) {
        if (ar[i] instanceof BiwaSymbol) {
          args.push(array_to_obj(ar.slice(i)));
          break;
        } else {
          args.push(ar[i]);
        }
      }
      // Run `new ctor(...args)`;
      return new (Function.prototype.bind.apply(ctor, [null].concat(args)))();
    }
  });

  // (js-obj "foo" 1 "bar" 2)
  // -> {"foo": 1, "bar": 2}
  define_libfunc("js-obj", 0, null, function (ar) {
    if (ar.length % 2 != 0) {
      throw new Error("js-obj: number of arguments must be even");
    }

    var obj = {};
    for (i = 0; i < ar.length / 2; i++) {
      assert_string(ar[i * 2]);
      obj[ar[i * 2]] = ar[i * 2 + 1];
    }
    return obj;
  });

  const js_closure = function (proc, intp) {
    var intp2 = new Interpreter(intp);
    return function (/*args*/) {
      return intp2.invoke_closure(proc, toArray(arguments));
    };
  };
  // (js-closure (lambda (event) ..))
  // Returns a js function which executes the given procedure.
  //
  // Example
  //   (add-handler! ($ "#btn") "click" (js-closure on-click))
  define_libfunc("js-closure", 1, 1, function (ar, intp) {
    assert_closure(ar[0]);
    return js_closure(ar[0], intp);
  });

  define_libfunc("js-null?", 1, 1, function (ar) {
    return ar[0] === null;
  });

  define_libfunc("js-undefined?", 1, 1, function (ar) {
    return ar[0] === undefined;
  });

  define_libfunc("js-function?", 1, 1, function (ar) {
    return isFunction$1(ar[0]);
  });

  define_libfunc("js-array-to-list", 1, 1, function (ar) {
    deprecate("js-array-to-list", "1.0", "js-array->list");
    return array_to_list(ar[0]);
  });

  define_libfunc("js-array->list", 1, 1, function (ar) {
    return array_to_list(ar[0]);
  });

  define_libfunc("list-to-js-array", 1, 1, function (ar) {
    deprecate("list-to-js-array", "1.0", "list->js-array");
    return ar[0].to_array();
  });

  define_libfunc("list->js-array", 1, 1, function (ar) {
    return ar[0].to_array();
  });

  define_libfunc("alist-to-js-obj", 1, 1, function (ar) {
    deprecate("alist-to-js-obj", "1.0", "alist->js-obj");
    return alist_to_js_obj(ar[0]);
  });

  define_libfunc("alist->js-obj", 1, 1, function (ar) {
    assert_list(ar[0]);
    return alist_to_js_obj(ar[0]);
  });

  define_libfunc("js-obj-to-alist", 1, 1, function (ar) {
    deprecate("js-obj-to-alist", "1.0", "js-obj->alist");
    return js_obj_to_alist(ar[0]);
  });
  define_libfunc("js-obj->alist", 1, 1, function (ar) {
    return js_obj_to_alist(ar[0]);
  });

  //
  // timer, sleep
  //
  define_libfunc("timer", 2, 2, function (ar, intp) {
    var proc = ar[0],
      sec = ar[1];
    assert_closure(proc);
    assert_real(sec);
    var intp2 = new Interpreter(intp);
    setTimeout(function () {
      intp2.invoke_closure(proc);
    }, sec * 1000);
    return undef;
  });
  define_libfunc("set-timer!", 2, 2, function (ar, intp) {
    var proc = ar[0],
      sec = ar[1];
    assert_closure(proc);
    assert_real(sec);
    var intp2 = new Interpreter(intp);
    return setInterval(function () {
      intp2.invoke_closure(proc);
    }, sec * 1000);
  });
  define_libfunc("clear-timer!", 1, 1, function (ar) {
    var timer_id = ar[0];
    clearInterval(timer_id);
    return undef;
  });
  define_libfunc("sleep", 1, 1, function (ar) {
    var sec = ar[0];
    assert_real(sec);
    return new Pause(function (pause) {
      setTimeout(function () {
        pause.resume(nil$1);
      }, sec * 1000);
    });
  });

  //
  // console
  //
  // (console-debug obj1 ...)
  // (console-log obj1 ...)
  // (console-info obj1 ...)
  // (console-warn obj1 ...)
  // (console-error obj1 ...)
  //   Put objects to console, if window.console is defined.
  //   Returns obj1.
  //
  // Example:
  //     (some-func arg1 (console-debug arg2) arg3)
  var define_console_func = function (name) {
    define_libfunc("console-" + name, 1, null, function (ar) {
      var con = window.console;
      if (con) {
        var vals = map(ar, function (item) {
          return inspect(item, { fallback: item });
        });

        con[name].apply(con, vals);
      }
      return ar[0];
    });
  };
  define_console_func("debug");
  define_console_func("log");
  define_console_func("info");
  define_console_func("warn");
  define_console_func("error");

  ///
  /// R6RS Base library
  ///

  //
  //        11.4  Expressions
  //
  //            11.4.1  Quotation
  //(quote)
  //            11.4.2  Procedures
  //(lambda)
  //            11.4.3  Conditionaar
  //(if)
  //            11.4.4  Assignments
  //(set!)
  //            11.4.5  Derived conditionaar

  define_syntax("cond", function (x) {
    var clauses = x.cdr;
    if (!(clauses instanceof Pair) || clauses === nil$1) {
      throw new BiwaError(
        "malformed cond: cond needs list but got " + to_write_ss(clauses)
      );
    }
    // TODO: assert that clauses is a proper list

    var ret = null;
    each(clauses.to_array().reverse(), function (clause) {
      if (!(clause instanceof Pair)) {
        throw new BiwaError("bad clause in cond: " + to_write_ss(clause));
      }

      if (clause.car === Sym("else")) {
        if (ret !== null) {
          throw new BiwaError(
            "'else' clause of cond followed by more clauses: " +
              to_write_ss(clauses)
          );
        } else if (clause.cdr === nil$1) {
          // pattern A: (else)
          //  -> #f            ; not specified in R6RS...?
          ret = false;
        } else if (clause.cdr.cdr === nil$1) {
          // pattern B: (else expr)
          //  -> expr
          ret = clause.cdr.car;
        } else {
          // pattern C: (else expr ...)
          //  -> (begin expr ...)
          ret = new Pair(Sym("begin"), clause.cdr);
        }
      } else {
        var test = clause.car;
        if (clause.cdr === nil$1) {
          // pattern 1: (test)
          //  -> (or test ret)
          ret = List(Sym("or"), test, ret);
        } else if (clause.cdr.cdr === nil$1) {
          // pattern 2: (test expr)
          //  -> (if test expr ret)
          ret = List(Sym("if"), test, clause.cdr.car, ret);
        } else if (clause.cdr.car === Sym("=>")) {
          // pattern 3: (test => expr)
          //  -> (let ((#<gensym1> test))
          //       (if test (expr #<gensym1>) ret))
          var test = clause.car,
            expr = clause.cdr.cdr.car;
          var tmp_sym = gensym();

          ret = List(
            Sym("let"),
            List(List(tmp_sym, test)),
            List(Sym("if"), test, List(expr, tmp_sym), ret)
          );
        } else {
          // pattern 4: (test expr ...)
          //  -> (if test (begin expr ...) ret)
          ret = List(Sym("if"), test, new Pair(Sym("begin"), clause.cdr), ret);
        }
      }
    });
    return ret;
  });

  define_syntax("case", function (x) {
    var tmp_sym = gensym();

    if (x.cdr === nil$1) {
      throw new BiwaError("case: at least one clause is required");
    } else if (!(x.cdr instanceof Pair)) {
      throw new BiwaError("case: proper list is required");
    } else {
      // (case key clauses ....)
      //  -> (let ((#<gensym1> key))
      var key = x.cdr.car;
      var clauses = x.cdr.cdr;

      var ret = undefined;
      each(clauses.to_array().reverse(), function (clause) {
        if (clause.car === Sym("else")) {
          // pattern 0: (else expr ...)
          //  -> (begin expr ...)
          if (ret === undefined) {
            ret = new Pair(Sym("begin"), clause.cdr);
          } else {
            throw new BiwaError(
              "case: 'else' clause followed by more clauses: " +
                to_write_ss(clauses)
            );
          }
        } else {
          // pattern 1: ((datum ...) expr ...)
          //  -> (if (or (eqv? key (quote d1)) ...) (begin expr ...) ret)
          ret = List(
            Sym("if"),
            new Pair(
              Sym("or"),
              array_to_list(
                map(clause.car.to_array(), function (d) {
                  return List(Sym("eqv?"), tmp_sym, List(Sym("quote"), d));
                })
              )
            ),
            new Pair(Sym("begin"), clause.cdr),
            ret
          );
        }
      });
      return new Pair(
        Sym("let1"),
        new Pair(tmp_sym, new Pair(key, new Pair(ret, nil$1)))
      );
    }
  });

  define_syntax("and", function (x) {
    // (and a b c) => (if a (if b c #f) #f)
    //todo: check improper list
    if (x.cdr == nil$1) return true;

    var objs = x.cdr.to_array();
    var i = objs.length - 1;
    var t = objs[i];
    for (i = i - 1; i >= 0; i--) t = List(Sym("if"), objs[i], t, false);

    return t;
  });

  define_syntax("or", function (x) {
    // (or a b c) => (if a a (if b b (if c c #f)))
    //todo: check improper list

    var objs = x.cdr.to_array();
    var f = false;
    for (var i = objs.length - 1; i >= 0; i--)
      f = List(Sym("if"), objs[i], objs[i], f);

    return f;
  });

  //            11.4.6  Binding constructs
  define_syntax("let", function (x) {
    //(let ((a 1) (b 2)) (print a) (+ a b))
    //=> ((lambda (a b) (print a) (+ a b)) 1 2)
    var name = null;
    if (x.cdr.car instanceof BiwaSymbol) {
      name = x.cdr.car;
      x = x.cdr;
    }
    var binds = x.cdr.car,
      body = x.cdr.cdr;

    if (!(binds instanceof Pair) && binds != nil$1) {
      throw new BiwaError(
        "let: need a pair for bindings: got " + to_write$1(binds)
      );
    }

    var vars = nil$1,
      vals = nil$1;
    for (var p = binds; p instanceof Pair; p = p.cdr) {
      if (!(p.car instanceof Pair)) {
        throw new BiwaError(
          "let: need a pair for bindings: got " + to_write$1(p.car)
        );
      }
      vars = new Pair(p.car.car, vars);
      vals = new Pair(p.car.cdr.car, vals);
    }

    var lambda = null;
    if (name) {
      // (let loop ((a 1) (b 2)) body ..)
      //=> (letrec ((loop (lambda (a b) body ..))) (loop 1 2))
      vars = array_to_list(vars.to_array().reverse());
      vals = array_to_list(vals.to_array().reverse());

      var body_lambda = new Pair(Sym("lambda"), new Pair(vars, body));
      var init_call = new Pair(name, vals);

      lambda = List(
        Sym("letrec"),
        new Pair(List(name, body_lambda), nil$1),
        init_call
      );
    } else {
      lambda = new Pair(new Pair(Sym("lambda"), new Pair(vars, body)), vals);
    }
    return lambda;
  });

  define_syntax("let*", function (x) {
    //(let* ((a 1) (b a)) (print a) (+ a b))
    //-> (let ((a 1))
    //     (let ((b a)) (print a) (+ a b)))
    var binds = x.cdr.car,
      body = x.cdr.cdr;

    if (binds === nil$1) return new Pair(Sym("let"), new Pair(nil$1, body));

    if (!(binds instanceof Pair))
      throw new BiwaError(
        "let*: need a pair for bindings: got " + to_write$1(binds)
      );

    var ret = null;
    each(binds.to_array().reverse(), function (bind) {
      ret = new Pair(
        Sym("let"),
        new Pair(
          new Pair(bind, nil$1),
          ret == null ? body : new Pair(ret, nil$1)
        )
      );
    });
    return ret;
  });

  var expand_letrec_star = function (x) {
    var binds = x.cdr.car,
      body = x.cdr.cdr;

    if (!(binds instanceof Pair))
      throw new BiwaError(
        "letrec*: need a pair for bindings: got " + to_write$1(binds)
      );

    var ret = body;
    each(binds.to_array().reverse(), function (bind) {
      ret = new Pair(new Pair(Sym("set!"), bind), ret);
    });
    var letbody = nil$1;
    each(binds.to_array().reverse(), function (bind) {
      letbody = new Pair(new Pair(bind.car, new Pair(undef, nil$1)), letbody);
    });
    return new Pair(Sym("let"), new Pair(letbody, ret));
  };
  define_syntax("letrec", expand_letrec_star);
  define_syntax("letrec*", expand_letrec_star);

  define_syntax("let-values", function (x) {
    // (let-values (((a b) (values 1 2))
    //               ((c d . e) (values 3 4 a)))
    //              (print a b c d e))
    // =>
    // (let ((#<gensym1> (lambda () (values 1 2)))
    //       (#<gensym2> (lambda () (values 3 4 a))))
    //   (let*-values (((a b) #<gensym1>)
    //                 ((c d . e) #<gensym2>))
    //                 (print a b c d e)))
    var mv_bindings = x.cdr.car;
    var body = x.cdr.cdr;
    var ret = null;

    var let_bindings = nil$1;
    var let_star_values_bindings = nil$1;
    each(mv_bindings.to_array().reverse(), function (item) {
      var init = item.cdr.car;
      var tmpsym = gensym();
      var binding = new Pair(
        tmpsym,
        new Pair(
          new Pair(Sym("lambda"), new Pair(nil$1, new Pair(init, nil$1))),
          nil$1
        )
      );
      let_bindings = new Pair(binding, let_bindings);

      var formals = item.car;
      let_star_values_bindings = new Pair(
        new Pair(formals, new Pair(new Pair(tmpsym, nil$1), nil$1)),
        let_star_values_bindings
      );
    });

    var let_star_values = new Pair(
      Sym("let*-values"),
      new Pair(let_star_values_bindings, body)
    );
    ret = new Pair(
      Sym("let"),
      new Pair(let_bindings, new Pair(let_star_values, nil$1))
    );
    return ret;
  });

  //let*-values
  define_syntax("let*-values", function (x) {
    // (let*-values (((a b) (values 1 2))
    //               ((c d . e) (values 3 4 a)))
    //   (print a b c d e))
    // -> (call-with-values
    //      (lambda () (values 1 2))
    //      (lambda (a b)
    //        (call-with-values
    //          (lambda () (values 3 4 a))
    //          (lambda (c d . e)
    //            (print a b c d e)))))
    var mv_bindings = x.cdr.car;
    var body = x.cdr.cdr;

    var ret = null;

    each(mv_bindings.to_array().reverse(), function (item) {
      var formals = item.car,
        init = item.cdr.car;
      ret = new Pair(
        Sym("call-with-values"),
        new Pair(
          new Pair(Sym("lambda"), new Pair(nil$1, new Pair(init, nil$1))),
          new Pair(
            new Pair(
              Sym("lambda"),
              new Pair(formals, ret == null ? body : new Pair(ret, nil$1))
            ),
            nil$1
          )
        )
      );
    });
    return ret;
  });
  //            11.4.7  Sequencing
  //(begin)

  //
  //        11.5  Equivalence predicates
  //
  define_libfunc("eqv?", 2, 2, function (ar) {
    return eqv(ar[0], ar[1]);
  });
  define_libfunc("eq?", 2, 2, function (ar) {
    return eq(ar[0], ar[1]);
  });
  define_libfunc("equal?", 2, 2, function (ar) {
    return equal(ar[0], ar[1]);
  });

  //
  //        11.6  Procedure predicate
  //
  //"procedure?", 1, 1
  define_libfunc("procedure?", 1, 1, function (ar) {
    return isProcedure(ar[0]);
  });

  //
  //        11.7  Arithmetic
  //

  //            11.7.1  Propagation of exactness and inexactness
  //            11.7.2  Representability of infinities and NaNs
  //            11.7.3  Semantics of common operations
  //                11.7.3.1  Integer division
  //                11.7.3.2  Transcendental functions
  //(no functions are introduced by above sections)

  //
  //            11.7.4  Numerical operations
  //

  //                11.7.4.1  Numerical type predicates
  define_libfunc("number?", 1, 1, function (ar) {
    return isNumber$1(ar[0]);
  });
  define_libfunc("complex?", 1, 1, function (ar) {
    return isComplex(ar[0]);
  });
  define_libfunc("real?", 1, 1, function (ar) {
    return isReal(ar[0]);
  });
  define_libfunc("rational?", 1, 1, function (ar) {
    return isRational(ar[0]);
  });
  define_libfunc("integer?", 1, 1, function (ar) {
    return isInteger(ar[0]);
  });

  //(real-valued? obj)    procedure
  //(rational-valued? obj)    procedure
  //(integer-valued? obj)    procedure
  //
  //(exact? z)    procedure
  //(inexact? z)    procedure

  //                11.7.4.2  Generic conversions
  //
  //(inexact z)    procedure
  //(exact z)    procedure
  //
  //                11.7.4.3  Arithmetic operations

  //inf & nan: ok (for this section)
  define_libfunc("=", 2, null, function (ar) {
    var v = ar[0];
    assert_number(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_number(ar[i]);
      if (real_part(ar[i]) != real_part(v)) return false;
      if (imag_part(ar[i]) != imag_part(v)) return false;
    }
    return true;
  });
  define_libfunc("<", 2, null, function (ar) {
    assert_number(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_number(ar[i]);
      if (!(ar[i - 1] < ar[i])) return false;
    }
    return true;
  });
  define_libfunc(">", 2, null, function (ar) {
    assert_number(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_number(ar[i]);
      if (!(ar[i - 1] > ar[i])) return false;
    }
    return true;
  });
  define_libfunc("<=", 2, null, function (ar) {
    assert_number(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_number(ar[i]);
      if (!(ar[i - 1] <= ar[i])) return false;
    }
    return true;
  });
  define_libfunc(">=", 2, null, function (ar) {
    assert_number(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_number(ar[i]);
      if (!(ar[i - 1] >= ar[i])) return false;
    }
    return true;
  });

  define_libfunc("zero?", 1, 1, function (ar) {
    assert_number(ar[0]);
    return ar[0] === 0;
  });
  define_libfunc("positive?", 1, 1, function (ar) {
    assert_number(ar[0]);
    return ar[0] > 0;
  });
  define_libfunc("negative?", 1, 1, function (ar) {
    assert_number(ar[0]);
    return ar[0] < 0;
  });
  define_libfunc("odd?", 1, 1, function (ar) {
    assert_number(ar[0]);
    return ar[0] % 2 == 1 || ar[0] % 2 == -1;
  });
  define_libfunc("even?", 1, 1, function (ar) {
    assert_number(ar[0]);
    return ar[0] % 2 == 0;
  });
  define_libfunc("finite?", 1, 1, function (ar) {
    assert_number(ar[0]);
    return ar[0] != Infinity && ar[0] != -Infinity && !isNaN(ar[0]);
  });
  define_libfunc("infinite?", 1, 1, function (ar) {
    assert_number(ar[0]);
    return ar[0] == Infinity || ar[0] == -Infinity;
  });
  define_libfunc("nan?", 1, 1, function (ar) {
    assert_number(ar[0]);
    return isNaN(ar[0]);
  });
  define_libfunc("max", 2, null, function (ar) {
    for (var i = 0; i < ar.length; i++) assert_number(ar[i]);

    return Math.max.apply(null, ar);
  });
  define_libfunc("min", 2, null, function (ar) {
    for (var i = 0; i < ar.length; i++) assert_number(ar[i]);

    return Math.min.apply(null, ar);
  });

  var complex_or_real = function (real, imag) {
    if (imag === 0) return real;
    return new Complex(real, imag);
  };
  var polar_or_real = function (magnitude, angle) {
    if (angle === 0) return magnitude;
    return Complex.from_polar(magnitude, angle);
  };
  define_libfunc("+", 0, null, function (ar) {
    var real = 0;
    var imag = 0;
    for (var i = 0; i < ar.length; i++) {
      assert_number(ar[i]);
      real += real_part(ar[i]);
      imag += imag_part(ar[i]);
    }
    return complex_or_real(real, imag);
  });
  var the_magnitude = function (n) {
    if (n instanceof Complex) return n.magnitude();
    return n;
  };
  var the_angle = function (n) {
    if (n instanceof Complex) return n.angle();
    return 0;
  };
  define_libfunc("*", 0, null, function (ar) {
    var magnitude = 1;
    var angle = 0;
    for (var i = 0; i < ar.length; i++) {
      assert_number(ar[i]);
      magnitude *= the_magnitude(ar[i]);
      angle += the_angle(ar[i]);
    }
    return polar_or_real(magnitude, angle);
  });
  define_libfunc("-", 1, null, function (ar) {
    var len = ar.length;
    assert_number(ar[0]);

    if (len == 1) {
      if (ar[0] instanceof Complex)
        return new Complex(-real_part(ar[0]), -imag_part(ar[0]));
      return -ar[0];
    } else {
      var real = real_part(ar[0]);
      var imag = imag_part(ar[0]);
      for (var i = 1; i < len; i++) {
        assert_number(ar[i]);
        real -= real_part(ar[i]);
        imag -= imag_part(ar[i]);
      }
      return complex_or_real(real, imag);
    }
  });
  //for r6rs specification, (/ 0 0) or (/ 3 0) raises '&assertion exception'
  define_libfunc("/", 1, null, function (ar) {
    var len = ar.length;
    assert_number(ar[0]);

    if (len == 1) {
      if (ar[0] instanceof Complex)
        return Complex.from_polar(1 / the_magnitude(ar[0]), -the_angle(ar[0]));
      return 1 / ar[0];
    } else {
      var magnitude = the_magnitude(ar[0]);
      var angle = the_angle(ar[0]);
      for (var i = 1; i < len; i++) {
        assert_number(ar[i]);
        magnitude /= the_magnitude(ar[i]);
        angle -= the_angle(ar[i]);
      }
      return polar_or_real(magnitude, angle);
    }
  });

  define_libfunc("abs", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.abs(ar[0]);
  });

  var div = function (n, m) {
    return Math.floor(n / m);
  };
  var mod = function (n, m) {
    return n - Math.floor(n / m) * m;
  };
  var div0 = function (n, m) {
    return n > 0 ? Math.floor(n / m) : Math.ceil(n / m);
  };
  var mod0 = function (n, m) {
    return n > 0 ? n - Math.floor(n / m) * m : n - Math.ceil(n / m) * m;
  };
  define_libfunc("div0-and-mod0", 2, 2, function (ar) {
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Values$1([div(ar[0], ar[1]), mod(ar[0], ar[1])]);
  });
  define_libfunc("div", 2, 2, function (ar) {
    assert_number(ar[0]);
    assert_number(ar[1]);
    return div(ar[0], ar[1]);
  });
  define_libfunc("mod", 2, 2, function (ar) {
    assert_number(ar[0]);
    assert_number(ar[1]);
    return mod(ar[0], ar[1]);
  });
  define_libfunc("div0-and-mod0", 2, 2, function (ar) {
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Values$1([div0(ar[0], ar[1]), mod0(ar[0], ar[1])]);
  });
  define_libfunc("div0", 2, 2, function (ar) {
    assert_number(ar[0]);
    assert_number(ar[1]);
    return div0(ar[0], ar[1]);
  });
  define_libfunc("mod0", 2, 2, function (ar) {
    assert_number(ar[0]);
    assert_number(ar[1]);
    return mod0(ar[0], ar[1]);
  });

  //(gcd n1 ...)    procedure
  //(lcm n1 ...)    procedure

  define_libfunc("numerator", 1, 1, function (ar) {
    assert_number(ar[0]);
    if (ar[0] instanceof Rational) return ar[0].numerator;
    else throw new Bug$1("todo");
  });
  define_libfunc("denominator", 1, 1, function (ar) {
    assert_number(ar[0]);
    if (ar[0] instanceof Rational) return ar[0].denominator;
    else throw new Bug$1("todo");
  });
  define_libfunc("floor", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.floor(ar[0]);
  });
  define_libfunc("ceiling", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.ceil(ar[0]);
  });
  define_libfunc("truncate", 1, 1, function (ar) {
    assert_number(ar[0]);
    return ar[0] < 0 ? Math.ceil(ar[0]) : Math.floor(ar[0]);
  });
  define_libfunc("round", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.round(ar[0]);
  });

  //(rationalize x1 x2)    procedure

  define_libfunc("exp", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.exp(ar[0]);
  });
  define_libfunc("log", 1, 2, function (ar) {
    var num = ar[0],
      base = ar[1];
    assert_number(num);

    if (base) {
      // log b num == log e num / log e b
      assert_number(base);
      return Math.log(num) / Math.log(base);
    } else return Math.log(num);
  });
  define_libfunc("sin", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.sin(ar[0]);
  });
  define_libfunc("cos", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.cos(ar[0]);
  });
  define_libfunc("tan", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.tan(ar[0]);
  });
  define_libfunc("asin", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.asin(ar[0]);
  });
  define_libfunc("acos", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.acos(ar[0]);
  });
  define_libfunc("atan", 1, 2, function (ar) {
    assert_number(ar[0]);
    if (ar.length == 2) {
      assert_number(ar[1]);
      return Math.atan2(ar[0], ar[1]);
    } else return Math.atan(ar[0]);
  });
  define_libfunc("sqrt", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Math.sqrt(ar[0]);
  });
  define_libfunc("exact-integer-sqrt", 1, 1, function (ar) {
    assert_number(ar[0]);
    var sqrt_f = Math.sqrt(ar[0]);
    var sqrt_i = sqrt_f - (sqrt_f % 1);
    var rest = ar[0] - sqrt_i * sqrt_i;

    return new Values$1([sqrt_i, rest]);
  });
  define_libfunc("expt", 2, 2, function (ar) {
    assert_number(ar[0]);
    assert_number(ar[1]);
    return Math.pow(ar[0], ar[1]);
  });
  define_libfunc("make-rectangular", 2, 2, function (ar) {
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Complex(ar[0], ar[1]);
  });
  define_libfunc("make-polar", 2, 2, function (ar) {
    assert_number(ar[0]);
    assert_number(ar[1]);
    return Complex.from_polar(ar[0], ar[1]);
  });
  var real_part = function (n) {
    return Complex.assure(n).real;
  };
  var imag_part = function (n) {
    return Complex.assure(n).imag;
  };
  define_libfunc("real-part", 1, 1, function (ar) {
    assert_number(ar[0]);
    return real_part(ar[0]);
  });
  define_libfunc("imag-part", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Complex.assure(ar[0]).imag;
  });
  define_libfunc("magnitude", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Complex.assure(ar[0]).magnitude();
  });
  define_libfunc("angle", 1, 1, function (ar) {
    assert_number(ar[0]);
    return Complex.assure(ar[0]).angle();
  });

  //
  //                11.7.4.4  Numerical Input and Output
  //
  define_libfunc("number->string", 1, 3, function (ar) {
    var z = ar[0],
      radix = ar[1],
      precision = ar[2];
    if (precision)
      throw new Bug$1("number->string: precision is not yet implemented");

    radix = radix || 10; //TODO: check radix is 2, 8, 10, or 16.
    return z.toString(radix);
  });
  define_libfunc("string->number", 1, 3, function (ar) {
    var s = ar[0];

    if (s === "+inf.0") return Infinity;

    if (s === "-inf.0") return -Infinity;

    if (s === "+nan.0") return NaN;

    var radix = ar[1];

    var int_res = parse_integer(s, radix === 0 ? 0 : radix || 10);

    if (int_res !== false) return int_res;

    if (radix !== undefined && radix !== 10) return false;

    var fp_res = parse_float(s);

    if (fp_res !== false) return fp_res;

    var frac_res = parse_fraction(s);

    if (frac_res !== false) return frac_res;

    return false;
  });

  //
  //        11.8  Booleans
  //

  define_libfunc("not", 1, 1, function (ar) {
    return ar[0] === false ? true : false;
  });
  define_libfunc("boolean?", 1, 1, function (ar) {
    return ar[0] === false || ar[0] === true ? true : false;
  });
  define_libfunc("boolean=?", 2, null, function (ar) {
    var len = ar.length;
    for (var i = 1; i < len; i++) {
      if (ar[i] != ar[0]) return false;
    }
    return true;
  });

  //        11.9  Pairs and lists

  define_libfunc("pair?", 1, 1, function (ar) {
    return ar[0] instanceof Pair ? true : false;
  });
  define_libfunc("cons", 2, 2, function (ar) {
    return new Pair(ar[0], ar[1]);
  });
  define_libfunc("car", 1, 1, function (ar) {
    //should raise &assertion for '()...
    if (!(ar[0] instanceof Pair))
      throw new BiwaError("Attempt to apply car on " + ar[0]);
    return ar[0].car;
  });
  define_libfunc("cdr", 1, 1, function (ar) {
    //should raise &assertion for '()...
    if (!(ar[0] instanceof Pair))
      throw new BiwaError("Attempt to apply cdr on " + ar[0]);
    return ar[0].cdr;
  });
  define_libfunc("set-car!", 2, 2, function (ar) {
    if (!(ar[0] instanceof Pair))
      throw new BiwaError("Attempt to apply set-car! on " + ar[0]);
    ar[0].car = ar[1];
    return undef;
  });
  define_libfunc("set-cdr!", 2, 2, function (ar) {
    if (!(ar[0] instanceof Pair))
      throw new BiwaError("Attempt to apply set-cdr! on " + ar[0]);
    ar[0].cdr = ar[1];
    return undef;
  });

  // cadr, caddr, cadddr, etc.
  (function () {
    // To traverse into pair and raise error
    var get = function (funcname, spec, obj) {
      var ret = obj;
      each(spec, function (is_cdr) {
        if (ret instanceof Pair) {
          ret = is_cdr ? ret.cdr : ret.car;
        } else {
          throw new BiwaError(
            funcname +
              ": attempt to get " +
              (is_cdr ? "cdr" : "car") +
              " of " +
              ret
          );
        }
      });
      return ret;
    };
    define_libfunc("caar", 1, 1, function (ar) {
      return get("caar", [0, 0], ar[0]);
    });
    define_libfunc("cadr", 1, 1, function (ar) {
      return get("cadr", [1, 0], ar[0]);
    });
    define_libfunc("cdar", 1, 1, function (ar) {
      return get("cadr", [0, 1], ar[0]);
    });
    define_libfunc("cddr", 1, 1, function (ar) {
      return get("cadr", [1, 1], ar[0]);
    });

    define_libfunc("caaar", 1, 1, function (ar) {
      return get("caaar", [0, 0, 0], ar[0]);
    });
    define_libfunc("caadr", 1, 1, function (ar) {
      return get("caadr", [1, 0, 0], ar[0]);
    });
    define_libfunc("cadar", 1, 1, function (ar) {
      return get("cadar", [0, 1, 0], ar[0]);
    });
    define_libfunc("caddr", 1, 1, function (ar) {
      return get("caddr", [1, 1, 0], ar[0]);
    });
    define_libfunc("cdaar", 1, 1, function (ar) {
      return get("cdaar", [0, 0, 1], ar[0]);
    });
    define_libfunc("cdadr", 1, 1, function (ar) {
      return get("cdadr", [1, 0, 1], ar[0]);
    });
    define_libfunc("cddar", 1, 1, function (ar) {
      return get("cddar", [0, 1, 1], ar[0]);
    });
    define_libfunc("cdddr", 1, 1, function (ar) {
      return get("cdddr", [1, 1, 1], ar[0]);
    });

    define_libfunc("caaaar", 1, 1, function (ar) {
      return get("caaaar", [0, 0, 0, 0], ar[0]);
    });
    define_libfunc("caaadr", 1, 1, function (ar) {
      return get("caaadr", [1, 0, 0, 0], ar[0]);
    });
    define_libfunc("caadar", 1, 1, function (ar) {
      return get("caadar", [0, 1, 0, 0], ar[0]);
    });
    define_libfunc("caaddr", 1, 1, function (ar) {
      return get("caaddr", [1, 1, 0, 0], ar[0]);
    });
    define_libfunc("cadaar", 1, 1, function (ar) {
      return get("cadaar", [0, 0, 1, 0], ar[0]);
    });
    define_libfunc("cadadr", 1, 1, function (ar) {
      return get("cadadr", [1, 0, 1, 0], ar[0]);
    });
    define_libfunc("caddar", 1, 1, function (ar) {
      return get("caddar", [0, 1, 1, 0], ar[0]);
    });
    define_libfunc("cadddr", 1, 1, function (ar) {
      return get("cadddr", [1, 1, 1, 0], ar[0]);
    });
    define_libfunc("cdaaar", 1, 1, function (ar) {
      return get("cdaaar", [0, 0, 0, 1], ar[0]);
    });
    define_libfunc("cdaadr", 1, 1, function (ar) {
      return get("cdaadr", [1, 0, 0, 1], ar[0]);
    });
    define_libfunc("cdadar", 1, 1, function (ar) {
      return get("cdadar", [0, 1, 0, 1], ar[0]);
    });
    define_libfunc("cdaddr", 1, 1, function (ar) {
      return get("cdaddr", [1, 1, 0, 1], ar[0]);
    });
    define_libfunc("cddaar", 1, 1, function (ar) {
      return get("cddaar", [0, 0, 1, 1], ar[0]);
    });
    define_libfunc("cddadr", 1, 1, function (ar) {
      return get("cddadr", [1, 0, 1, 1], ar[0]);
    });
    define_libfunc("cdddar", 1, 1, function (ar) {
      return get("cdddar", [0, 1, 1, 1], ar[0]);
    });
    define_libfunc("cddddr", 1, 1, function (ar) {
      return get("cddddr", [1, 1, 1, 1], ar[0]);
    });
  })();

  define_libfunc("null?", 1, 1, function (ar) {
    return ar[0] === nil$1;
  });
  define_libfunc("list?", 1, 1, function (ar) {
    return isList(ar[0]);
  });
  define_libfunc("list", 0, null, function (ar) {
    var l = nil$1;
    for (var i = ar.length - 1; i >= 0; i--) l = new Pair(ar[i], l);
    return l;
  });
  define_libfunc("length", 1, 1, function (ar) {
    assert_list(ar[0]);
    var n = 0;
    for (var o = ar[0]; o != nil$1; o = o.cdr) n++;
    return n;
  });
  define_libfunc("append", 1, null, function (ar) {
    var k = ar.length;
    var ret = ar[--k];
    while (k--) {
      each(ar[k].to_array().reverse(), function (item) {
        ret = new Pair(item, ret);
      });
    }
    return ret;
  });
  define_libfunc("reverse", 1, 1, function (ar) {
    // (reverse '()) => '()
    if (ar[0] == nil$1) return nil$1;
    assert_pair(ar[0]);

    var l = nil$1;
    for (var o = ar[0]; o != nil$1; o = o.cdr) l = new Pair(o.car, l);
    return l;
  });
  define_libfunc("list-tail", 2, 2, function (ar) {
    assert_pair(ar[0]);
    assert_integer(ar[1]);
    if (ar[1] < 0)
      throw new BiwaError("list-tail: index out of range (" + ar[1] + ")");

    var o = ar[0];
    for (var i = 0; i < ar[1]; i++) {
      if (!(o instanceof Pair))
        throw new BiwaError("list-tail: the list is shorter than " + ar[1]);
      o = o.cdr;
    }
    return o;
  });
  define_libfunc("list-ref", 2, 2, function (ar) {
    assert_pair(ar[0]);
    assert_integer(ar[1]);
    if (ar[1] < 0)
      throw new BiwaError("list-tail: index out of range (" + ar[1] + ")");

    var o = ar[0];
    for (var i = 0; i < ar[1]; i++) {
      if (!(o instanceof Pair))
        throw new BiwaError("list-ref: the list is shorter than " + ar[1]);
      o = o.cdr;
    }
    return o.car;
  });
  define_libfunc("map", 2, null, function (ar) {
    var proc = ar.shift(),
      lists = ar;
    each(lists, assert_list);

    var a = [];
    return Call.multi_foreach(lists, {
      // Called for each element
      // input: the element (or the elements, if more than one list is given)
      // output: a Call request of proc and args
      call: function (xs) {
        return new Call(
          proc,
          map(xs, function (x) {
            return x.car;
          })
        );
      },

      // Called when each Call request is finished
      // input: the result of Call request,
      //   the element(s) of the Call request (which is not used here)
      // output: `undefined' to continue,
      //   some value to terminate (the value will be the result)
      result: function (res) {
        a.push(res);
      },

      // Called when reached to the end of the list(s)
      // input: none
      // output: the resultant value
      finish: function () {
        return array_to_list(a);
      },
    });
  });
  define_libfunc("for-each", 2, null, function (ar) {
    var proc = ar.shift(),
      lists = ar;
    each(lists, assert_list);

    return Call.multi_foreach(lists, {
      call: function (xs) {
        return new Call(
          proc,
          map(xs, function (x) {
            return x.car;
          })
        );
      },
      finish: function () {
        return undef;
      },
    });
  });

  //        11.10  Symbols

  define_libfunc("symbol?", 1, 1, function (ar) {
    return ar[0] instanceof BiwaSymbol ? true : false;
  });
  define_libfunc("symbol->string", 1, 1, function (ar) {
    assert_symbol(ar[0]);
    return ar[0].name;
  });
  define_libfunc("symbol=?", 2, null, function (ar) {
    assert_symbol(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_symbol(ar[i]);
      if (ar[i] != ar[0]) return false;
    }
    return true;
  });
  define_libfunc("string->symbol", 1, 1, function (ar) {
    assert_string(ar[0]);
    return Sym(ar[0]);
  });

  //
  //        11.11  Characters
  //
  define_libfunc("char?", 1, 1, function (ar) {
    return ar[0] instanceof Char;
  });
  define_libfunc("char->integer", 1, 1, function (ar) {
    assert_char(ar[0]);
    return ar[0].value.charCodeAt(0);
  });
  define_libfunc("integer->char", 1, 1, function (ar) {
    assert_integer(ar[0]);
    return Char.get(String.fromCharCode(ar[0]));
  });

  var make_char_compare_func = function (test) {
    return function (ar) {
      assert_char(ar[0]);
      for (var i = 1; i < ar.length; i++) {
        assert_char(ar[i]);
        if (!test(ar[i - 1].value, ar[i].value)) return false;
      }
      return true;
    };
  };
  define_libfunc(
    "char=?",
    2,
    null,
    make_char_compare_func(function (a, b) {
      return a == b;
    })
  );
  define_libfunc(
    "char<?",
    2,
    null,
    make_char_compare_func(function (a, b) {
      return a < b;
    })
  );
  define_libfunc(
    "char>?",
    2,
    null,
    make_char_compare_func(function (a, b) {
      return a > b;
    })
  );
  define_libfunc(
    "char<=?",
    2,
    null,
    make_char_compare_func(function (a, b) {
      return a <= b;
    })
  );
  define_libfunc(
    "char>=?",
    2,
    null,
    make_char_compare_func(function (a, b) {
      return a >= b;
    })
  );

  //
  //        11.12  Strings
  //
  define_libfunc("string?", 1, 1, function (ar) {
    return typeof ar[0] == "string";
  });
  define_libfunc("make-string", 1, 2, function (ar) {
    assert_integer(ar[0]);
    var c = " ";
    if (ar[1]) {
      assert_char(ar[1]);
      c = ar[1].value;
    }
    var out = "";
    times(ar[0], function () {
      out += c;
    });
    return out;
  });
  define_libfunc("string", 0, null, function (ar) {
    if (ar.length == 0) return "";
    for (var i = 0; i < ar.length; i++) assert_char(ar[i]);
    return map(ar, function (c) {
      return c.value;
    }).join("");
  });
  define_libfunc("string-length", 1, 1, function (ar) {
    assert_string(ar[0]);
    return ar[0].length;
  });
  define_libfunc("string-ref", 2, 2, function (ar) {
    assert_string(ar[0]);
    assert_between(ar[1], 0, ar[0].length - 1);
    return Char.get(ar[0].charAt([ar[1]]));
  });
  define_libfunc("string=?", 2, null, function (ar) {
    assert_string(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_string(ar[i]);
      if (ar[0] != ar[i]) return false;
    }
    return true;
  });
  define_libfunc("string<?", 2, null, function (ar) {
    assert_string(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_string(ar[i]);
      if (!(ar[i - 1] < ar[i])) return false;
    }
    return true;
  });
  define_libfunc("string>?", 2, null, function (ar) {
    assert_string(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_string(ar[i]);
      if (!(ar[i - 1] > ar[i])) return false;
    }
    return true;
  });
  define_libfunc("string<=?", 2, null, function (ar) {
    assert_string(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_string(ar[i]);
      if (!(ar[i - 1] <= ar[i])) return false;
    }
    return true;
  });
  define_libfunc("string>=?", 2, null, function (ar) {
    assert_string(ar[0]);
    for (var i = 1; i < ar.length; i++) {
      assert_string(ar[i]);
      if (!(ar[i - 1] >= ar[i])) return false;
    }
    return true;
  });

  define_libfunc("substring", 3, 3, function (ar) {
    assert_string(ar[0]);
    assert_integer(ar[1]);
    assert_integer(ar[2]);

    if (ar[1] < 0) throw new BiwaError("substring: start too small: " + ar[1]);
    if (ar[2] < 0) throw new BiwaError("substring: end too small: " + ar[2]);
    if (ar[0].length + 1 <= ar[1])
      throw new BiwaError("substring: start too big: " + ar[1]);
    if (ar[0].length + 1 <= ar[2])
      throw new BiwaError("substring: end too big: " + ar[2]);
    if (!(ar[1] <= ar[2]))
      throw new BiwaError(
        "substring: not start <= end: " + ar[1] + ", " + ar[2]
      );

    return ar[0].substring(ar[1], ar[2]);
  });

  define_libfunc("string-append", 0, null, function (ar) {
    for (var i = 0; i < ar.length; i++) assert_string(ar[i]);

    return ar.join("");
  });
  define_libfunc("string->list", 1, 1, function (ar) {
    assert_string(ar[0]);
    return array_to_list(
      map(ar[0].split(""), function (s) {
        return Char.get(s[0]);
      })
    );
  });
  define_libfunc("list->string", 1, 1, function (ar) {
    assert_list(ar[0]);
    return map(ar[0].to_array(), function (c) {
      return c.value;
    }).join("");
  });
  define_libfunc("string-for-each", 2, null, function (ar) {
    var proc = ar.shift(),
      strs = ar;
    each(strs, assert_string);

    return Call.multi_foreach(strs, {
      call: function (chars) {
        return new Call(proc, chars);
      },
      finish: function () {
        return undef;
      },
    });
  });
  define_libfunc("string-copy", 1, 1, function (ar) {
    // note: this is useless, because javascript strings are immutable
    assert_string(ar[0]);
    return ar[0];
  });

  //
  //        11.13  Vectors
  //
  define_libfunc("vector?", 1, 1, function (ar) {
    return isVector(ar[0]);
  });
  define_libfunc("make-vector", 1, 2, function (ar) {
    assert_integer(ar[0]);
    var vec = new Array(ar[0]);

    if (ar.length == 2) {
      for (var i = 0; i < ar[0]; i++) vec[i] = ar[1];
    }
    return vec;
  });
  define_libfunc("vector", 0, null, function (ar) {
    return ar;
  });
  define_libfunc("vector-length", 1, 1, function (ar) {
    assert_vector(ar[0]);
    return ar[0].length;
  });
  define_libfunc("vector-ref", 2, 2, function (ar) {
    assert_vector(ar[0]);
    assert_integer(ar[1]);
    assert_between(ar[1], 0, ar[0].length - 1);

    return ar[0][ar[1]];
  });
  define_libfunc("vector-set!", 3, 3, function (ar) {
    assert_vector(ar[0]);
    assert_integer(ar[1]);

    ar[0][ar[1]] = ar[2];
    return undef;
  });
  define_libfunc("vector->list", 1, 1, function (ar) {
    assert_vector(ar[0]);
    return array_to_list(ar[0]);
  });
  define_libfunc("list->vector", 1, 1, function (ar) {
    assert_list(ar[0]);
    return ar[0].to_array();
  });
  define_libfunc("vector-fill!", 2, 2, function (ar) {
    assert_vector(ar[0]);
    var vec = ar[0],
      obj = ar[1];

    for (var i = 0; i < vec.length; i++) vec[i] = obj;
    return vec;
  });
  define_libfunc("vector-map", 2, null, function (ar) {
    var proc = ar.shift(),
      vecs = ar;
    each(vecs, assert_vector);

    var a = [];
    return Call.multi_foreach(vecs, {
      call: function (objs) {
        return new Call(proc, objs);
      },
      result: function (res) {
        a.push(res);
      },
      finish: function () {
        return a;
      },
    });
  });
  define_libfunc("vector-for-each", 2, null, function (ar) {
    var proc = ar.shift(),
      vecs = ar;
    each(vecs, assert_vector);

    return Call.multi_foreach(vecs, {
      call: function (objs) {
        return new Call(proc, objs);
      },
      finish: function () {
        return undef;
      },
    });
  });

  //
  //        11.14  Errors and violations
  //
  //(error who message irritant1 ...)    procedure
  //(assertion-violation who message irritant1 ...)    procedure
  //(assert <expression>)    syntax

  //
  //        11.15  Control features
  //
  define_libfunc("apply", 2, null, function (ar) {
    var proc = ar.shift(),
      rest_args = ar.pop(),
      args = ar;
    args = args.concat(rest_args.to_array());

    return new Call(proc, args);
  });
  define_syntax("call-with-current-continuation", function (x) {
    return new Pair(Sym("call/cc"), x.cdr);
  });
  define_libfunc("values", 0, null, function (ar) {
    if (ar.length == 1)
      // eg. (values 3)
      return ar[0];
    else return new Values$1(ar);
  });
  define_libfunc("call-with-values", 2, 2, function (ar) {
    var producer = ar[0],
      consumer = ar[1];
    assert_procedure(producer);
    assert_procedure(consumer);
    return new Call(producer, [], function (ar) {
      var result = ar[0];
      if (result instanceof Values$1) {
        return new Call(consumer, result.content);
      } else {
        // eg. (call-with-values (lambda () 3)
        //                       (lambda (x) x))
        return new Call(consumer, [result]);
      }
    });
  });

  define_libfunc("dynamic-wind", 3, 3, function (ar, intp) {
    var before = ar[0],
      thunk = ar[1],
      after = ar[2];
    return new Call(before, [], function () {
      intp.push_dynamic_winder(before, after);
      return new Call(thunk, [], function (ar2) {
        var result = ar2[0];
        intp.pop_dynamic_winder();
        return new Call(after, [], function () {
          return result;
        });
      });
    });
  });

  //        11.16  Iteration
  //named let

  //        11.17  Quasiquotation
  // `() is expanded to `cons` and `append`.
  // `#() is expanded to `vector` and `vector-append`.
  var expand_qq = function (f, lv) {
    if (f instanceof BiwaSymbol || f === nil$1) {
      return List(Sym("quote"), f);
    } else if (f instanceof Pair) {
      var car = f.car;
      if (car instanceof Pair && car.car === Sym("unquote-splicing")) {
        if (lv == 1)
          return List(Sym("append"), f.car.cdr.car, expand_qq(f.cdr, lv));
        else
          return List(
            Sym("cons"),
            List(
              Sym("list"),
              List(Sym("quote"), Sym("unquote-splicing")),
              expand_qq(f.car.cdr.car, lv - 1)
            ),
            expand_qq(f.cdr, lv)
          );
      } else if (car === Sym("unquote")) {
        if (lv == 1) return f.cdr.car;
        else
          return List(
            Sym("list"),
            List(Sym("quote"), Sym("unquote")),
            expand_qq(f.cdr.car, lv - 1)
          );
      } else if (car === Sym("quasiquote"))
        return List(
          Sym("list"),
          List(Sym("quote"), Sym("quasiquote")),
          expand_qq(f.cdr.car, lv + 1)
        );
      else return List(Sym("cons"), expand_qq(f.car, lv), expand_qq(f.cdr, lv));
    } else if (f instanceof Array) {
      var vecs = [[]];
      for (var i = 0; i < f.length; i++) {
        if (f[i] instanceof Pair && f[i].car === Sym("unquote-splicing")) {
          if (lv == 1) {
            var item = List(Sym("list->vector"), f[i].cdr.car);
            item["splicing"] = true;
            vecs.push(item);
            vecs.push([]);
          } else {
            var item = List(
              Sym("cons"),
              List(
                Sym("list"),
                List(Sym("quote"), Sym("unquote-splicing")),
                expand_qq(f[i].car.cdr.car, lv - 1)
              ),
              expand_qq(f[i].cdr, lv)
            );
            last(vecs).push(item);
          }
        } else {
          // Expand other things as the same as if they are in a list quasiquote
          last(vecs).push(expand_qq(f[i], lv));
        }
      }

      var vectors = vecs.map(function (vec) {
        if (vec["splicing"]) {
          return vec;
        } else {
          return Cons(Sym("vector"), array_to_list(vec));
        }
      });
      if (vectors.length == 1) {
        return Cons(Sym("vector"), array_to_list(vecs[0]));
      } else {
        return Cons(Sym("vector-append"), array_to_list(vectors));
      }
    } else return f;
  };
  define_syntax("quasiquote", function (x) {
    return expand_qq(x.cdr.car, 1);
  });
  //unquote
  define_syntax("unquote", function (x) {
    throw new BiwaError("unquote(,) must be inside quasiquote(`)");
  });
  //unquote-splicing
  define_syntax("unquote-splicing", function (x) {
    throw new BiwaError("unquote-splicing(,@) must be inside quasiquote(`)");
  });

  //        11.18  Binding constructs for syntactic keywords
  //let-syntax
  //letrec-syntax

  //        11.19  Macro transformers
  //syntax-rules
  //identifier-syntax
  //

  //        11.20  Tail calls and tail contexts
  //(no library function introduced)

  ///
  /// R6RS Standard Libraries
  ///

  //
  // Chapter 1 Unicode
  //
  //(char-upcase char)    procedure
  //(char-downcase char)    procedure
  //(char-titlecase char)    procedure
  //(char-foldcase char)    procedure
  //
  //(char-ci=? char1 char2 char3 ...)    procedure
  //(char-ci<? char1 char2 char3 ...)    procedure
  //(char-ci>? char1 char2 char3 ...)    procedure
  //(char-ci<=? char1 char2 char3 ...)    procedure
  //(char-ci>=? char1 char2 char3 ...)    procedure
  //
  //(char-alphabetic? char)    procedure
  //(char-numeric? char)    procedure
  //(char-whitespace? char)    procedure
  //(char-upper-case? char)    procedure
  //(char-lower-case? char)    procedure
  //(char-title-case? char)    procedure
  //
  //(char-general-category char)    procedure

  //(string-upcase string)    procedure
  define_libfunc("string-upcase", 1, 1, function (ar) {
    assert_string(ar[0]);
    return ar[0].toUpperCase();
  });
  //(string-downcase string)    procedure
  define_libfunc("string-downcase", 1, 1, function (ar) {
    assert_string(ar[0]);
    return ar[0].toLowerCase();
  });
  //(string-titlecase string)    procedure
  //(string-foldcase string)    procedure

  const make_string_ci_function = function (compare) {
    return function (ar) {
      assert_string(ar[0]);
      var str = ar[0].toUpperCase();

      for (var i = 1; i < ar.length; i++) {
        assert_string(ar[i]);
        if (!compare(str, ar[i].toUpperCase())) return false;
      }
      return true;
    };
  };
  //(string-ci=? string1 string2 string3 ...)    procedure
  define_libfunc(
    "string-ci=?",
    2,
    null,
    make_string_ci_function(function (a, b) {
      return a == b;
    })
  );
  //(string-ci<? string1 string2 string3 ...)    procedure
  define_libfunc(
    "string-ci<?",
    2,
    null,
    make_string_ci_function(function (a, b) {
      return a < b;
    })
  );
  //(string-ci>? string1 string2 string3 ...)    procedure
  define_libfunc(
    "string-ci>?",
    2,
    null,
    make_string_ci_function(function (a, b) {
      return a > b;
    })
  );
  //(string-ci<=? string1 string2 string3 ...)    procedure
  define_libfunc(
    "string-ci<=?",
    2,
    null,
    make_string_ci_function(function (a, b) {
      return a <= b;
    })
  );
  //(string-ci>=? string1 string2 string3 ...)    procedure
  define_libfunc(
    "string-ci>=?",
    2,
    null,
    make_string_ci_function(function (a, b) {
      return a >= b;
    })
  );

  //(string-normalize-nfd string)    procedure
  //(string-normalize-nfkd string)    procedure
  //(string-normalize-nfc string)    procedure
  //(string-normalize-nfkc string)    procedure

  //
  // Chapter 2 Bytevectors
  //

  //
  // Chapter 3 List utilities
  //
  define_libfunc("find", 2, 2, function (ar) {
    var proc = ar[0],
      ls = ar[1];
    assert_list(ls);
    return Call.foreach(ls, {
      call: function (x) {
        return new Call(proc, [x.car]);
      },
      result: function (res, x) {
        if (res) return x.car;
      },
      finish: function () {
        return false;
      },
    });
  });
  define_libfunc("for-all", 2, null, function (ar) {
    var proc = ar.shift();
    var lists = ar;
    each(lists, assert_list);

    var last = true; //holds last result which proc returns
    return Call.multi_foreach(lists, {
      call: function (pairs) {
        return new Call(
          proc,
          map(pairs, function (x) {
            return x.car;
          })
        );
      },
      result: function (res, pairs) {
        if (res === false) return false;
        last = res;
      },
      finish: function () {
        return last;
      },
    });
  });
  define_libfunc("exists", 2, null, function (ar) {
    var proc = ar.shift();
    var lists = ar;
    each(lists, assert_list);

    return Call.multi_foreach(lists, {
      call: function (pairs) {
        return new Call(
          proc,
          map(pairs, function (x) {
            return x.car;
          })
        );
      },
      result: function (res, pairs) {
        if (res !== false) return res;
      },
      finish: function () {
        return false;
      },
    });
  });
  define_libfunc("filter", 2, 2, function (ar) {
    var proc = ar[0],
      ls = ar[1];
    assert_list(ls);

    var a = [];
    return Call.foreach(ls, {
      call: function (x) {
        return new Call(proc, [x.car]);
      },
      result: function (res, x) {
        if (res) a.push(x.car);
      },
      finish: function () {
        return array_to_list(a);
      },
    });
  });
  //  define_scmfunc("partition+", 2, 2,
  //    "(lambda (proc ls)  \
  //       (define (partition2 proc ls t f) \
  //         (if (null? ls) \
  //           (values (reverse t) (reverse f)) \
  //           (if (proc (car ls)) \
  //             (partition2 proc (cdr ls) (cons (car ls) t) f) \
  //             (partition2 proc (cdr ls) t (cons (car ls) f))))) \
  //       (partition2 proc ls '() '()))");

  define_libfunc("partition", 2, 2, function (ar) {
    var proc = ar[0],
      ls = ar[1];
    assert_list(ls);

    var t = [],
      f = [];
    return Call.foreach(ls, {
      call: function (x) {
        return new Call(proc, [x.car]);
      },
      result: function (res, x) {
        if (res) t.push(x.car);
        else f.push(x.car);
      },
      finish: function () {
        return new Values$1([array_to_list(t), array_to_list(f)]);
      },
    });
  });
  define_libfunc("fold-left", 3, null, function (ar) {
    var proc = ar.shift(),
      accum = ar.shift(),
      lists = ar;
    each(lists, assert_list);

    return Call.multi_foreach(lists, {
      call: function (pairs) {
        var args = map(pairs, function (x) {
          return x.car;
        });
        args.unshift(accum);
        return new Call(proc, args);
      },
      result: function (res, pairs) {
        accum = res;
      },
      finish: function () {
        return accum;
      },
    });
  });
  define_libfunc("fold-right", 3, null, function (ar) {
    var proc = ar.shift(),
      accum = ar.shift();
    var lists = map(ar, function (ls) {
      // reverse each list
      assert_list(ls);
      return array_to_list(ls.to_array().reverse());
    });

    return Call.multi_foreach(lists, {
      call: function (pairs) {
        var args = map(pairs, function (x) {
          return x.car;
        });
        args.push(accum);
        return new Call(proc, args);
      },
      result: function (res, pairs) {
        accum = res;
      },
      finish: function () {
        return accum;
      },
    });
  });
  define_libfunc("remp", 2, 2, function (ar) {
    var proc = ar[0],
      ls = ar[1];
    assert_list(ls);

    var ret = [];
    return Call.foreach(ls, {
      call: function (x) {
        return new Call(proc, [x.car]);
      },
      result: function (res, x) {
        if (!res) ret.push(x.car);
      },
      finish: function () {
        return array_to_list(ret);
      },
    });
  });
  var make_remover = function (key) {
    return function (ar) {
      var obj = ar[0],
        ls = ar[1];
      assert_list(ls);

      var ret = [];
      return Call.foreach(ls, {
        call: function (x) {
          return new Call(TopEnv[key] || CoreEnv[key], [obj, x.car]);
        },
        result: function (res, x) {
          if (!res) ret.push(x.car);
        },
        finish: function () {
          return array_to_list(ret);
        },
      });
    };
  };
  define_libfunc("remove", 2, 2, make_remover("equal?"));
  define_libfunc("remv", 2, 2, make_remover("eqv?"));
  define_libfunc("remq", 2, 2, make_remover("eq?"));

  define_libfunc("memp", 2, 2, function (ar) {
    var proc = ar[0],
      ls = ar[1];
    assert_list(ls);
    return Call.foreach(ls, {
      call: function (x) {
        return new Call(proc, [x.car]);
      },
      result: function (res, x) {
        if (res) return x;
      },
      finish: function () {
        return false;
      },
    });
  });
  var make_finder = function (key) {
    return function (ar) {
      var obj = ar[0],
        ls = ar[1];
      assert_list(ls);
      return Call.foreach(ls, {
        call: function (x) {
          return new Call(TopEnv[key] || CoreEnv[key], [obj, x.car]);
        },
        result: function (res, x) {
          if (res) return x;
        },
        finish: function () {
          return false;
        },
      });
    };
  };
  define_libfunc("member", 2, 2, make_finder("equal?"));
  define_libfunc("memv", 2, 2, make_finder("eqv?"));
  define_libfunc("memq", 2, 2, make_finder("eq?"));

  define_libfunc("assp", 2, 2, function (ar) {
    var proc = ar[0],
      als = ar[1];
    assert_list(als);
    return Call.foreach(als, {
      call: function (x) {
        if (x.car.car) return new Call(proc, [x.car.car]);
        else
          throw new BiwaError(
            "ass*: pair required but got " + to_write$1(x.car)
          );
      },
      result: function (res, x) {
        if (res) return x.car;
      },
      finish: function () {
        return false;
      },
    });
  });
  var make_assoc = function (func_name, eq_func_name) {
    return function (ar) {
      var obj = ar[0],
        list = ar[1];
      assert_list(list);
      return Call.foreach(list, {
        call: function (ls) {
          if (!isPair(ls.car))
            throw new BiwaError(
              func_name + ": pair required but got " + to_write$1(ls.car)
            );

          var equality = TopEnv[eq_func_name] || CoreEnv[eq_func_name];
          return new Call(equality, [obj, ls.car.car]);
        },
        result: function (was_equal, ls) {
          if (was_equal) return ls.car;
        },
        finish: function () {
          return false;
        },
      });
    };
  };
  define_libfunc("assoc", 2, 2, make_assoc("assoc", "equal?"));
  define_libfunc("assv", 2, 2, make_assoc("assv", "eqv?"));
  define_libfunc("assq", 2, 2, make_assoc("assq", "eq?"));

  define_libfunc("cons*", 1, null, function (ar) {
    if (ar.length == 1) return ar[0];
    else {
      var ret = null;
      each(ar.reverse(), function (x) {
        if (ret) {
          ret = new Pair(x, ret);
        } else ret = x;
      });
      return ret;
    }
  });

  //
  // Chapter 4 Sorting
  //
  (function () {
    // Destructively sort the given array
    // with scheme function `proc` as comparator
    var mergeSort = function (ary, proc, finish) {
      if (ary.length <= 1) return finish(ary);
      return mergeSort_(ary, proc, finish, [[0, ary.length, false]], false);
    };

    var mergeSort_ = function (ary, proc, finish, stack, up) {
      while (true) {
        var start = stack[stack.length - 1][0],
          end = stack[stack.length - 1][1],
          left = stack[stack.length - 1][2];
        var len = end - start;
        //console.debug("mergeSort_", ary, stack.join(' '), up?"u":"d", ""+start+".."+(end-1))

        if (len >= 2 && !up) {
          // There are parts to be sorted
          stack.push([start, start + (len >> 1), true]);
          continue;
        } else if (left) {
          // Left part sorted. Continue to the right one
          stack.pop();
          var rend = stack[stack.length - 1][1];
          stack.push([end, rend, false]);
          up = false;
          continue;
        } else {
          // Right part sorted. Merge left and right
          stack.pop();
          var lstart = stack[stack.length - 1][0];
          var ary1 = ary.slice(lstart, start),
            ary2 = ary.slice(start, end);
          return merge_(ary1, ary2, proc, [], 0, 0, function (ret) {
            //console.debug("mergeSortd", ary, stack.join(' '), up?"u":"d", ary1, ary2, ret, ""+start+".."+(start+len-1));
            for (var i = 0; i < ret.length; i++) {
              ary[lstart + i] = ret[i];
            }

            if (stack.length == 1) {
              return finish(ary);
            } else {
              return mergeSort_(ary, proc, finish, stack, true);
            }
          });
        }
      }
    };

    var merge_ = function (ary1, ary2, proc, ret, i, j, cont) {
      //console.debug("merge_", ary1, ary2, ret, i, j);
      var len1 = ary1.length,
        len2 = ary2.length;
      if (i < len1 && j < len2) {
        return new Call(proc, [ary2[j], ary1[i]], function (ar) {
          //console.debug("comp", [ary2[j], ary1[i]], ar[0]);
          if (ar[0]) {
            ret.push(ary2[j]);
            j += 1;
          } else {
            ret.push(ary1[i]);
            i += 1;
          }
          return merge_(ary1, ary2, proc, ret, i, j, cont);
        });
      } else {
        while (i < len1) {
          ret.push(ary1[i]);
          i += 1;
        }
        while (j < len2) {
          ret.push(ary2[j]);
          j += 1;
        }
        return cont(ret);
      }
    };

    var compareFn = function (a, b) {
      return lt(a, b) ? -1 : lt(b, a) ? 1 : 0;
    };

    define_libfunc("list-sort", 1, 2, function (ar) {
      if (ar[1]) {
        assert_procedure(ar[0]);
        assert_list(ar[1]);
        return mergeSort(ar[1].to_array(), ar[0], function (ret) {
          return array_to_list(ret);
        });
      } else {
        assert_list(ar[0]);
        return array_to_list(ar[0].to_array().sort(compareFn));
      }
    });

    //(vector-sort proc vector)    procedure
    define_libfunc("vector-sort", 1, 2, function (ar) {
      if (ar[1]) {
        assert_procedure(ar[0]);
        assert_vector(ar[1]);
        return mergeSort(clone(ar[1]), ar[0], function (ret) {
          return ret;
        });
      } else {
        assert_vector(ar[0]);
        return clone(ar[0]).sort(compareFn);
      }
    });

    //(vector-sort! proc vector)    procedure
    define_libfunc("vector-sort!", 1, 2, function (ar) {
      if (ar[1]) {
        assert_procedure(ar[0]);
        assert_vector(ar[1]);
        return mergeSort(ar[1], ar[0], function (ret) {
          return undef;
        });
      } else {
        assert_vector(ar[0]);
        ar[0].sort(compareFn);
        return undef;
      }
    });
  })();

  //
  // Chapter 5 Control Structures
  //
  define_syntax("when", function (x) {
    //(when test body ...)
    //=> (if test (begin body ...) #<undef>)
    var test = x.cdr.car,
      body = x.cdr.cdr;

    return new Pair(
      Sym("if"),
      new Pair(
        test,
        new Pair(new Pair(Sym("begin"), body), new Pair(undef, nil$1))
      )
    );
  });

  define_syntax("unless", function (x) {
    //(unless test body ...)
    //=> (if (not test) (begin body ...) #<undef>)
    var test = x.cdr.car,
      body = x.cdr.cdr;

    return new Pair(
      Sym("if"),
      new Pair(
        new Pair(Sym("not"), new Pair(test, nil$1)),
        new Pair(new Pair(Sym("begin"), body), new Pair(undef, nil$1))
      )
    );
  });

  define_syntax("do", function (x) {
    //(do ((var1 init1 step1)
    //     (var2 init2 step2) ...)
    //    (test expr1 expr2 ...)
    //  body1 body2 ...)
    //=> (let loop` ((var1 init1) (var2 init2) ...)
    //     (if test
    //       (begin expr1 expr2 ...)
    //       (begin body1 body2 ...
    //              (loop` step1 step2 ...)))))

    // parse arguments
    if (!isPair(x.cdr)) throw new BiwaError("do: no variables of do");
    var varsc = x.cdr.car;
    if (!isPair(varsc))
      throw new BiwaError("do: variables must be given as a list");
    if (!isPair(x.cdr.cdr)) throw new BiwaError("do: no resulting form of do");
    var resultc = x.cdr.cdr.car;
    var bodyc = x.cdr.cdr.cdr;

    // construct subforms
    var loop = gensym();

    var init_vars = array_to_list(
      varsc.map(function (var_def) {
        var a = var_def.to_array();
        return List(a[0], a[1]);
      })
    );

    var test = resultc.car;
    var result_exprs = new Pair(Sym("begin"), resultc.cdr);

    var next_loop = new Pair(
      loop,
      array_to_list(
        varsc.map(function (var_def) {
          var a = var_def.to_array();
          return a[2] || a[0];
        })
      )
    );
    var body_exprs = new Pair(Sym("begin"), bodyc).concat(List(next_loop));

    // combine subforms
    return List(
      Sym("let"),
      loop,
      init_vars,
      List(Sym("if"), test, result_exprs, body_exprs)
    );
  });

  //(case-lambda <case-lambda clause> ...)    syntax
  define_syntax("case-lambda", function (x) {
    if (!isPair(x.cdr))
      throw new BiwaError("case-lambda: at least 1 clause required");
    var clauses = x.cdr.to_array();

    var args_ = gensym();
    var exec = List(Sym("raise"), "case-lambda: no matching clause found");

    clauses.reverse().forEach(function (clause) {
      if (!isPair(clause))
        throw new BiwaError(
          "case-lambda: clause must be a pair: " + to_write$1(clause)
        );
      var formals = clause.car,
        clause_body = clause.cdr;

      if (formals === nil$1) {
        exec = List(
          Sym("if"),
          List(Sym("null?"), args_),
          new Pair(Sym("begin"), clause_body),
          exec
        );
      } else if (isPair(formals)) {
        var len = formals.length(),
          last_cdr = formals.last_cdr();
        var pred = last_cdr === nil$1 ? Sym("=") : Sym(">=");
        var lambda = new Pair(Sym("lambda"), new Pair(formals, clause_body));
        exec = List(
          Sym("if"),
          List(pred, List(Sym("length"), args_), len),
          List(Sym("apply"), lambda, args_),
          exec
        );
      } else if (isSymbol(formals)) {
        exec = new Pair(
          Sym("let1"),
          new Pair(formals, new Pair(args_, clause_body))
        );
        // Note: previous `exec` is just discarded because this is a wildcard pattern.
      } else {
        throw new BiwaError(
          "case-lambda: invalid formals: " + to_write$1(formals)
        );
      }
    });

    return List(Sym("lambda"), args_, exec);
  });

  //
  // Chapter 6 Records
  // see also: src/system/record.js
  //

  // 6.2 Records: Syntactic layer
  //eqv, eq

  //(define-record-type <name spec> <record clause>*)    syntax
  define_syntax("define-record-type", function (x) {
    // (define-record-type <name spec> <record clause>*)
    var name_spec = x.cdr.car;
    var record_clauses = x.cdr.cdr;

    // 1. parse name spec
    // <name spec>: either
    // - <record name> eg: point
    // - (<record name> <constructor name> <predicate name>)
    //   eg: (point make-point point?)
    if (isSymbol(name_spec)) {
      var record_name = name_spec;
      var constructor_name = Sym("make-" + name_spec.name);
      var predicate_name = Sym(name_spec.name + "?");
    } else {
      assert_list(name_spec);
      var record_name = name_spec.car;
      var constructor_name = name_spec.cdr.car;
      var predicate_name = name_spec.cdr.cdr.car;
      assert_symbol(record_name);
      assert_symbol(constructor_name);
      assert_symbol(predicate_name);
    }

    // 2. parse record clauses
    var sealed = false;
    var opaque = false;
    var uid = false;
    var parent_name;
    var parent_rtd = false;
    var parent_cd = false;
    var protocol = false;
    var fields = [];

    // <record clause>:
    each(record_clauses.to_array(), function (clause) {
      switch (clause.car) {
        // - (fields <field spec>*)
        case Sym("fields"):
          fields = map(clause.cdr.to_array(), function (field_spec, idx) {
            if (isSymbol(field_spec)) {
              // - <field name>
              return {
                name: field_spec,
                idx: idx,
                mutable: false,
                accessor_name: null,
                mutator_name: null,
              };
            } else {
              assert_list(field_spec);
              assert_symbol(field_spec.car);
              switch (field_spec.car) {
                case Sym("immutable"):
                  // - (immutable <field name>)
                  // - (immutable <field name> <accessor name>)
                  var field_name = field_spec.cdr.car;
                  assert_symbol(field_name);

                  if (isNil(field_spec.cdr.cdr))
                    return { name: field_name, idx: idx, mutable: false };
                  else
                    return {
                      name: field_name,
                      idx: idx,
                      mutable: false,
                      accessor_name: field_spec.cdr.cdr.car,
                    };

                case Sym("mutable"):
                  // - (mutable <field name>)
                  // - (mutable <field name> <accessor name> <mutator name>)
                  var field_name = field_spec.cdr.car;
                  assert_symbol(field_name);

                  if (isNil(field_spec.cdr.cdr))
                    return { name: field_name, idx: idx, mutable: true };
                  else
                    return {
                      name: field_name,
                      idx: idx,
                      mutable: true,
                      accessor_name: field_spec.cdr.cdr.car,
                      mutator_name: field_spec.cdr.cdr.cdr.car,
                    };
                default:
                  throw new BiwaError(
                    "define-record-type: field definition " +
                      "must start with `immutable' or `mutable' " +
                      "but got " +
                      inspect(field_spec.car)
                  );
              }
            }
          });
          break;
        // - (parent <name>)
        case Sym("parent"):
          parent_name = clause.cdr.car;
          assert_symbol(parent_name);
          break;
        // - (protocol <expr>)
        case Sym("protocol"):
          protocol = clause.cdr.car;
          break;
        // - (sealed <bool>)
        case Sym("sealed"):
          sealed = !!clause.cdr.car;
          break;
        // - (opaque <bool>)
        case Sym("opaque"):
          opaque = !!clause.cdr.car;
          break;
        // - (nongenerative <uid>?)
        case Sym("nongenerative"):
          uid = clause.cdr.car;
          break;
        // - (parent-rtd <rtd> <cd>)
        case Sym("parent-rtd"):
          parent_rtd = clause.cdr.car;
          parent_cd = clause.cdr.cdr.car;
          break;
        default:
          throw new BiwaError(
            "define-record-type: unknown clause `" +
              to_write$1(clause.car) +
              "'"
          );
      }
    });

    if (parent_name) {
      parent_rtd = [Sym("record-type-descriptor"), parent_name];
      parent_cd = [Sym("record-constructor-descriptor"), parent_name];
    }

    // 3. build the definitions
    // Note: In this implementation, rtd and cd are not bound to symbols.
    // They are referenced through record name by record-type-descriptor
    // and record-constructor-descriptor. These relation are stored in
    // the hash BiwaScheme.Record._DefinedTypes.
    var rtd = [Sym("record-type-descriptor"), record_name];
    var cd = [Sym("record-constructor-descriptor"), record_name];

    // registration
    var rtd_fields = map(fields, function (field) {
      return List(Sym(field.mutable ? "mutable" : "immutable"), field.name);
    });
    rtd_fields.is_vector = true; //tell List not to convert
    var rtd_def = [
      Sym("make-record-type-descriptor"),
      [Sym("quote"), record_name],
      parent_rtd,
      uid,
      sealed,
      opaque,
      rtd_fields,
    ];
    var cd_def = [
      Sym("make-record-constructor-descriptor"),
      Sym("__rtd"),
      parent_cd,
      protocol,
    ];
    var registration = [
      Sym("let*"),
      [
        [Sym("__rtd"), rtd_def],
        [Sym("__cd"), cd_def],
      ],
      [
        Sym("_define-record-type"),
        [Sym("quote"), record_name],
        Sym("__rtd"),
        Sym("__cd"),
      ],
    ];

    // accessors and mutators
    var accessor_defs = map(fields, function (field) {
      var name =
        field.accessor_name || Sym(record_name.name + "-" + field.name.name);

      return [Sym("define"), name, [Sym("record-accessor"), rtd, field.idx]];
    });

    var mutator_defs = filter(fields, function (field) {
      return field.mutable;
    });
    mutator_defs = map(mutator_defs, function (field) {
      var name =
        field.mutator_name ||
        Sym(record_name.name + "-" + field.name.name + "-set!");

      return [Sym("define"), name, [Sym("record-mutator"), rtd, field.idx]];
    });

    // Wrap the definitions with `begin'
    // Example:
    //   (begin
    //     (let* ((__rtd (make-record-type-descriptor 'square
    //                     (record-type-descriptor rect)
    //                     #f #f #f
    //                     #((mutable w) (mutable h))))
    //            (__cd (make-record-constructor-descriptor __rtd
    //                    (record-constructor-descriptor rect)
    //                    (lambda (n) ...))))
    //       (_define-record-type 'square __rtd __cd))
    //
    //     (define make-square
    //       (record-constructor
    //         (record-constructor-descriptor square)))
    //     (define square?
    //       (record-predicate (record-type-descriptor square)))
    //     (define square-w
    //       (record-accessor (record-type-descriptor square) 0))
    //     (define square-h
    //       (record-accessor (record-type-descriptor square) 1))
    //     (define set-square-w!
    //       (record-mutator (record-type-descriptor square) 0))
    //     (define set-square-h!
    //       (record-mutator (record-type-descriptor square) 1)))
    //
    return deep_array_to_list(
      [
        Sym("begin"),
        registration,
        [Sym("define"), constructor_name, [Sym("record-constructor"), cd]],
        [Sym("define"), predicate_name, [Sym("record-predicate"), rtd]],
      ]
        .concat(accessor_defs)
        .concat(mutator_defs)
    );
  });

  define_libfunc("_define-record-type", 3, 3, function (ar) {
    assert_symbol(ar[0]);
    assert_record_td(ar[1]);
    assert_record_cd(ar[2]);
    Record.define_type(ar[0].name, ar[1], ar[2]);
    return undef;
  });

  //(record-type-descriptor <record name>)    syntax
  define_syntax("record-type-descriptor", function (x) {
    return deep_array_to_list([
      Sym("_record-type-descriptor"),
      [Sym("quote"), x.cdr.car],
    ]);
  });
  define_libfunc("_record-type-descriptor", 1, 1, function (ar) {
    assert_symbol(ar[0]);
    var type = Record.get_type(ar[0].name);
    if (type) return type.rtd;
    else
      throw new BiwaError(
        "record-type-descriptor: unknown record type " + ar[0].name
      );
  });

  //(record-constructor-descriptor <record name>)    syntax
  define_syntax("record-constructor-descriptor", function (x) {
    return deep_array_to_list([
      Sym("_record-constructor-descriptor"),
      [Sym("quote"), x.cdr.car],
    ]);
  });
  define_libfunc("_record-constructor-descriptor", 1, 1, function (ar) {
    assert_symbol(ar[0]);
    var type = Record.get_type(ar[0].name);
    if (type) return type.cd;
    else
      throw new BiwaError(
        "record-constructor-descriptor: unknown record type " + ar[0].name
      );
  });

  // 6.3  Records: Procedural layer
  //(make-record-type-descriptor name    procedure
  define_libfunc("make-record-type-descriptor", 6, 6, function (ar) {
    var name = ar[0],
      parent_rtd = ar[1],
      uid = ar[2],
      sealed = ar[3],
      opaque = ar[4],
      fields = ar[5];

    assert_symbol(name);
    if (parent_rtd) assert_record_td(parent_rtd);
    if (uid) {
      assert_symbol(uid);
      var _rtd = Record.RTD.NongenerativeRecords[uid.name];
      if (_rtd) {
        // the record type is already defined.
        return _rtd;
        // should check equality of other arguments..
      }
    }
    sealed = !!sealed;
    opaque = !!opaque;
    assert_vector(fields);
    for (var i = 0; i < fields.length; i++) {
      var list = fields[i];
      assert_symbol(list.car, "mutability");
      assert_symbol(list.cdr.car, "field name");
      fields[i] = [list.cdr.car.name, list.car == Sym("mutable")];
    }
    var rtd = new Record.RTD(name, parent_rtd, uid, sealed, opaque, fields);
    if (uid) Record.RTD.NongenerativeRecords[uid.name] = rtd;

    return rtd;
  });

  //(record-type-descriptor? obj)    procedure
  define_libfunc("record-type-descriptor?", 1, 1, function (ar) {
    return ar[0] instanceof Record.RTD;
  });

  //(make-record-constructor-descriptor rtd    procedure
  define_libfunc("make-record-constructor-descriptor", 3, 3, function (ar) {
    var rtd = ar[0],
      parent_cd = ar[1],
      protocol = ar[2];

    assert_record_td(rtd);
    if (parent_cd) assert_record_cd(parent_cd);
    if (protocol) assert_procedure(protocol);

    return new Record.CD(rtd, parent_cd, protocol);
  });

  //(record-constructor constructor-descriptor)    procedure
  define_libfunc("record-constructor", 1, 1, function (ar) {
    var cd = ar[0];
    assert_record_cd(cd);

    return cd.record_constructor();
  });

  //(record-predicate rtd)    procedure
  define_libfunc("record-predicate", 1, 1, function (ar) {
    var rtd = ar[0];
    assert_record_td(rtd);

    return function (args) {
      var obj = args[0];

      return obj instanceof Record && obj.rtd === rtd;
    };
  });

  //(record-accessor rtd k)    procedure
  define_libfunc("record-accessor", 2, 2, function (ar) {
    var rtd = ar[0],
      k = ar[1];
    assert_record_td(rtd);
    assert_integer(k);
    for (var _rtd = rtd.parent_rtd; _rtd; _rtd = _rtd.parent_rtd)
      k += _rtd.fields.length;

    return function (args) {
      var record = args[0];
      var error_msg =
        rtd.name.name +
        "-" +
        rtd.field_name(k) +
        ": " +
        to_write$1(record) +
        " is not a " +
        rtd.name.name;
      assert(isRecord(record), error_msg);

      var descendant = false;
      for (var _rtd = record.rtd; _rtd; _rtd = _rtd.parent_rtd) {
        if (_rtd == rtd) descendant = true;
      }
      assert(descendant, error_msg);

      return record.get(k);
    };
  });

  //(record-mutator rtd k)    procedure
  define_libfunc("record-mutator", 2, 2, function (ar) {
    var rtd = ar[0],
      k = ar[1];
    assert_record_td(rtd);
    assert_integer(k);
    for (var _rtd = rtd.parent_rtd; _rtd; _rtd = _rtd.parent_rtd)
      k += _rtd.fields.length;

    return function (args) {
      var record = args[0],
        val = args[1];
      var func_name = rtd.field_name(k);

      assert_record(record);
      assert(
        record.rtd === rtd,
        func_name + ": " + to_write$1(record) + " is not a " + rtd.name.name
      );
      assert(
        !record.rtd.sealed,
        func_name + ": " + rtd.name.name + " is sealed (can't mutate)"
      );

      record.set(k, val);
    };
  });

  // 6.4  Records: Inspection
  //(record? obj)    procedure
  define_libfunc("record?", 1, 1, function (ar) {
    var obj = ar[0];
    if (isRecord(obj)) {
      if (obj.rtd.opaque) return false;
      // opaque records pretend as if it is not a record.
      else return true;
    } else return false;
  });

  //(record-rtd record)    procedure
  define_libfunc("record-rtd", 1, 1, function (ar) {
    assert_record(ar[0]);
    return ar[0].rtd;
  });

  //(record-type-name rtd)    procedure
  define_libfunc("record-type-name", 1, 1, function (ar) {
    assert_record_td(ar[0]);
    return ar[0].name;
  });

  //(record-type-parent rtd)    procedure
  define_libfunc("record-type-parent", 1, 1, function (ar) {
    assert_record_td(ar[0]);
    return ar[0].parent_rtd;
  });

  //(record-type-uid rtd)    procedure
  define_libfunc("record-type-uid", 1, 1, function (ar) {
    assert_record_td(ar[0]);
    return ar[0].uid;
  });

  //(record-type-generative? rtd)    procedure
  define_libfunc("record-type-generative?", 1, 1, function (ar) {
    assert_record_td(ar[0]);
    return ar[0].generative;
  });

  //(record-type-sealed? rtd)    procedure
  define_libfunc("record-type-sealed?", 1, 1, function (ar) {
    assert_record_td(ar[0]);
    return ar[0].sealed;
  });

  //(record-type-opaque? rtd)    procedure
  define_libfunc("record-type-opaque?", 1, 1, function (ar) {
    assert_record_td(ar[0]);
    return ar[0].opaque;
  });

  //(record-type-field-names rtd)    procedure
  define_libfunc("record-type-field-names", 1, 1, function (ar) {
    assert_record_td(ar[0]);
    return map(ar[0].fields, function (field) {
      return field.name;
    });
  });

  //(record-field-mutable? rtd k)    procedure
  define_libfunc("record-field-mutable?", 2, 2, function (ar) {
    var rtd = ar[0],
      k = ar[1];
    assert_record_td(ar[0]);
    assert_integer(k);

    for (var _rtd = rtd.parent_rtd; _rtd; _rtd = _rtd.parent_rtd)
      k += _rtd.fields.length;

    return ar[0].fields[k].mutable;
  });

  //
  // Chapter 7 Exceptions and conditions
  //
  //(with-exception-handler handler thunk)    procedure
  //(guard (<variable>    syntax
  //(raise obj)    procedure
  define_libfunc("raise", 1, 1, function (ar) {
    throw new UserError(to_write$1(ar[0]));
  });
  //(raise-continuable obj)    procedure
  //
  //&condition    condition type
  //(condition condition1 ...)    procedure
  //(simple-conditions condition)    procedure
  //(condition? obj)    procedure
  //(condition-predicate rtd)    procedure
  //(condition-accessor rtd proc)    procedure
  //
  //&message    condition type
  //&warning    condition type
  //&serious    condition type
  //&error    condition type
  //&violation    condition type
  //&assertion    condition type
  //&irritants    condition type
  //&who    condition type
  //&non-continuable    condition type
  //&implementation-restriction    condition type
  //&lexical    condition type
  //&syntax    condition type
  //&undefined    condition type

  //
  // Chapter 8 I/O
  //
  //  //    8  I/O
  //  //        8.1  Condition types
  //&i/o    condition type
  //&i/o-read    condition type
  //&i/o-write    condition type
  //&i/o-invalid-position    condition type
  //&i/o-filename    condition type
  //&i/o-file-protection    condition type
  //&i/o-file-is-read-only    condition type
  //&i/o-file-already-exists    condition type
  //&i/o-file-does-not-exist    condition type
  //&i/o-port    condition type
  //
  //  //        8.2  Port I/O
  //  //            8.2.1  File names
  //  //(no function introduced)
  //
  //  //            8.2.2  File options
  //(file-options <file-options symbol> ...)    syntax
  //
  //  //            8.2.3  Buffer modes
  //(buffer-mode <buffer-mode symbol>)    syntax
  //(buffer-mode? obj)    procedure
  //
  //  //            8.2.4  Transcoders
  //(latin-1-codec)    procedure
  //(utf-8-codec)    procedure
  //(utf-16-codec)    procedure
  //(eol-style <eol-style symbol>)    syntax
  //(native-eol-style)    procedure
  //&i/o-decoding    condition type
  //&i/o-encoding    condition type
  //(error-handling-mode <error-handling-mode symbol>)    syntax
  //(make-transcoder codec)    procedure
  //(make-transcoder codec eol-style)    procedure
  //(make-transcoder codec eol-style handling-mode)    procedure
  //(native-transcoder)    procedure
  //(transcoder-codec transcoder)    procedure
  //(transcoder-eol-style transcoder)    procedure
  //(transcoder-error-handling-mode transcoder)    procedure
  //(bytevector->string bytevector transcoder)    procedure
  //(string->bytevector string transcoder)    procedure
  //
  //            8.2.5  End-of-file object
  //-> 8.3 (eof-object)    procedure
  //-> 8.3 (eof-object? obj)    procedure

  //            8.2.6  Input and output ports
  define_libfunc("port?", 1, 1, function (ar) {
    return ar[0] instanceof Port;
  });
  //(port-transcoder port)    procedure
  define_libfunc("textual-port?", 1, 1, function (ar) {
    assert_port(ar[0]);
    return !ar[0].is_binary;
  });
  define_libfunc("binary-port?", 1, 1, function (ar) {
    assert_port(ar[0]);
    return ar[0].is_binary;
  });
  //(transcoded-port binary-port transcoder)    procedure
  //(port-has-port-position? port)    procedure
  //(port-position port)    procedure
  //(port-has-set-port-position!? port)    procedure
  //(set-port-position! port pos)    procedure
  define_libfunc("close-port", 1, 1, function (ar) {
    assert_port(ar[0]);
    ar[0].close();
    return undef;
  });
  //(call-with-port port proc)    procedure
  define_libfunc("call-with-port", 2, 2, function (ar) {
    var port = ar[0],
      proc = ar[1];
    assert_port(port);
    assert_closure(proc);

    return new Call(proc, [port], function (ar) {
      // Automatically close the port
      port.close();
      return ar[0]; // TODO: values
    });
  });

  //            8.2.7  Input ports
  //8.3 (input-port? obj)    procedure
  //(port-eof? input-port)    procedure
  //(open-file-input-port filename)    procedure
  //(open-bytevector-input-port bytevector)    procedure
  //(open-string-input-port string)    procedure
  //(standard-input-port)    procedure
  //8.3 (current-input-port)    procedure
  //(make-custom-binary-input-port id read!    procedure
  //(make-custom-textual-input-port id read!    procedure
  //
  //  //            8.2.8  Binary input
  //(get-u8 binary-input-port)    procedure
  //(lookahead-u8 binary-input-port)    procedure
  //(get-bytevector-n binary-input-port count)    procedure
  //(get-bytevector-n! binary-input-port    procedure
  //(get-bytevector-some binary-input-port)    procedure
  //(get-bytevector-all binary-input-port)    procedure
  //
  //  //            8.2.9  Textual input
  //(get-char textual-input-port)    procedure
  //(lookahead-char textual-input-port)    procedure
  //(get-string-n textual-input-port count)    procedure
  //(get-string-n! textual-input-port string start count)    procedure
  //(get-string-all textual-input-port)    procedure
  //(get-line textual-input-port)    procedure
  //(get-datum textual-input-port)    procedure
  //
  //            8.2.10  Output ports
  //8.3 (output-port? obj)    procedure
  //(flush-output-port output-port)    procedure
  //(output-port-buffer-mode output-port)    procedure
  //(open-file-output-port filename)    procedure
  //(open-bytevector-output-port)    procedure
  //(call-with-bytevector-output-port proc)    procedure
  //(open-string-output-port)    procedure
  //(call-with-string-output-port proc)    procedure
  define_libfunc("call-with-string-output-port", 1, 1, function (ar) {
    var proc = ar[0];
    assert_procedure(proc);

    var port = new Port.StringOutput();

    return new Call(proc, [port], function (ar) {
      port.close();
      return port.output_string();
    });
  });

  //(standard-output-port)    procedure
  //(standard-error-port)    procedure
  //8.3 (current-output-port)    procedure
  //8.3 (current-error-port)    procedure
  //(make-custom-binary-output-port id    procedure
  //(make-custom-textual-output-port id write! get-position set-position! close)
  //  define_libfunc("make-custom-textual-output-port", 5, 5, function(ar){
  //    assert_string(ar[0]);
  //    assert_closure(ar[1]);
  //    assert_closure(ar[2]);
  //    assert_closure(ar[3]);
  //    assert_closure(ar[4]);
  //    return new Port(ar[0], ar[1], ar[2], ar[3], ar[4]);
  //  })
  //
  //  //            8.2.11  Binary output
  //(put-u8 binary-output-port octet)    procedure
  //(put-bytevector binary-output-port bytevector)    procedure
  //
  //            8.2.12  Textual output
  define_libfunc("put-char", 2, 2, function (ar) {
    assert_port(ar[0]);
    assert_char(ar[1]);
    ar[0].put_string(ar[1].value);
    return undef;
  });
  define_libfunc("put-string", 2, 2, function (ar) {
    assert_port(ar[0]);
    assert_string(ar[1]);
    ar[0].put_string(ar[1]);
    return undef;
  });
  define_libfunc("put-datum", 2, 2, function (ar) {
    assert_port(ar[0]);
    ar[0].put_string(to_write$1(ar[1]));
    return undef;
  });
  //
  //  //            8.2.13  Input/output ports
  //(open-file-input/output-port filename)    procedure
  //(make-custom-binary-input/output-port    procedure
  //(make-custom-textual-input/output-port    procedure
  //
  //  //        8.3  Simple I/O
  define_libfunc("eof-object", 0, 0, function (ar) {
    return eof;
  });
  define_libfunc("eof-object?", 1, 1, function (ar) {
    return ar[0] === eof;
  });
  //(call-with-input-file filename proc)    procedure
  //(call-with-output-file filename proc)    procedure
  define_libfunc("input-port?", 1, 1, function (ar) {
    assert_port(ar[0]);
    return ar[0].is_input;
  });
  define_libfunc("output-port?", 1, 1, function (ar) {
    assert_port(ar[0]);
    return ar[0].is_output;
  });
  define_libfunc("current-input-port", 0, 0, function (ar) {
    return Port.current_input;
  });
  define_libfunc("current-output-port", 0, 0, function (ar) {
    return Port.current_output;
  });
  define_libfunc("current-error-port", 0, 0, function (ar) {
    return Port.current_error;
  });
  //(with-input-from-file filename thunk)    procedure
  //(with-output-to-file filename thunk)    procedure
  //(open-input-file filename)    procedure
  //(open-output-file filename)    procedure
  define_libfunc("close-input-port", 1, 1, function (ar) {
    assert_port(ar[0]);
    if (!ar[0].is_input)
      throw new BiwaError("close-input-port: port is not input port");
    ar[0].close();
    return undef;
  });
  define_libfunc("close-output-port", 1, 1, function (ar) {
    assert_port(ar[0]);
    if (!ar[0].is_output)
      throw new BiwaError("close-output-port: port is not output port");
    ar[0].close();
    return undef;
  });
  //(read-char)    procedure
  //(peek-char)    procedure
  define_libfunc("read", 0, 1, function (ar) {
    var port = ar[0] || Port.current_input;
    assert_port(port);

    return port.get_string(function (str) {
      return Interpreter.read(str);
    });
  });

  define_libfunc("write-char", 1, 2, function (ar) {
    var port = ar[1] || Port.current_output;
    assert_char(ar[0]);
    port.put_string(ar[0].value);
    return undef;
  });
  define_libfunc("newline", 0, 1, function (ar) {
    var port = ar[0] || Port.current_output;
    port.put_string("\n");
    return undef;
  });
  define_libfunc("display", 1, 2, function (ar) {
    var port = ar[1] || Port.current_output;
    port.put_string(to_display(ar[0]));
    return undef;
  });
  define_libfunc("write", 1, 2, function (ar) {
    var port = ar[1] || Port.current_output;
    assert_port(port);
    port.put_string(to_write$1(ar[0]));
    return undef;
  });

  //
  // Chapter 9 File System
  // Chapter 10 Command-line access and exit values
  //
  // see src/library/node_functions.js

  //
  // Chapter 11 Arithmetic
  //
  ////        11.1  Bitwise operations
  ////        11.2  Fixnums
  //(fixnum? obj)    procedure
  //(fixnum-width)    procedure
  //(least-fixnum)    procedure
  //(greatest-fixnum)    procedure
  //(fx=? fx1 fx2 fx3 ...)    procedure
  //(fx>? fx1 fx2 fx3 ...)    procedure
  //(fx<? fx1 fx2 fx3 ...)    procedure
  //(fx>=? fx1 fx2 fx3 ...)    procedure
  //(fx<=? fx1 fx2 fx3 ...)    procedure
  //(fxzero? fx)    procedure
  //(fxpositive? fx)    procedure
  //(fxnegative? fx)    procedure
  //(fxodd? fx)    procedure
  //(fxeven? fx)    procedure
  //(fxmax fx1 fx2 ...)    procedure
  //(fxmin fx1 fx2 ...)    procedure
  //(fx+ fx1 fx2)    procedure
  //(fx* fx1 fx2)    procedure
  //(fx- fx1 fx2)    procedure
  //(fxdiv-and-mod fx1 fx2)    procedure
  //(fxdiv fx1 fx2)    procedure
  //(fxmod fx1 fx2)    procedure
  //(fxdiv0-and-mod0 fx1 fx2)    procedure
  //(fxdiv0 fx1 fx2)    procedure
  //(fxmod0 fx1 fx2)    procedure
  //(fx+/carry fx1 fx2 fx3)    procedure
  //(fx-/carry fx1 fx2 fx3)    procedure
  //(fx*/carry fx1 fx2 fx3)    procedure
  //(fxnot fx)    procedure
  //(fxand fx1 ...)    procedure
  //(fxior fx1 ...)    procedure
  //(fxxor fx1 ...)    procedure
  //(fxif fx1 fx2 fx3)    procedure
  //(fxbit-count fx)    procedure
  //(fxlength fx)    procedure
  //(fxfirst-bit-set fx)    procedure
  //(fxbit-set? fx1 fx2)    procedure
  //(fxcopy-bit fx1 fx2 fx3)    procedure
  //(fxbit-field fx1 fx2 fx3)    procedure
  //(fxcopy-bit-field fx1 fx2 fx3 fx4)    procedure
  //(fxarithmetic-shift fx1 fx2)    procedure
  //(fxarithmetic-shift-left fx1 fx2)    procedure
  //(fxarithmetic-shift-right fx1 fx2)    procedure
  //(fxrotate-bit-field fx1 fx2 fx3 fx4)    procedure
  //(fxreverse-bit-field fx1 fx2 fx3)    procedure
  //
  ////        11.3  Flonums
  //(flonum? obj)    procedure
  //(real->flonum x)    procedure
  //(fl=? fl1 fl2 fl3 ...)    procedure
  //(fl<? fl1 fl2 fl3 ...)    procedure
  //(fl<=? fl1 fl2 fl3 ...)    procedure
  //(fl>? fl1 fl2 fl3 ...)    procedure
  //(fl>=? fl1 fl2 fl3 ...)    procedure
  //(flinteger? fl)    procedure
  //(flzero? fl)    procedure
  //(flpositive? fl)    procedure
  //(flnegative? fl)    procedure
  //(flodd? ifl)    procedure
  //(fleven? ifl)    procedure
  //(flfinite? fl)    procedure
  //(flinfinite? fl)    procedure
  //(flnan? fl)    procedure
  //(flmax fl1 fl2 ...)    procedure
  //(flmin fl1 fl2 ...)    procedure
  //(fl+ fl1 ...)    procedure
  //(fl* fl1 ...)    procedure
  //(fl- fl1 fl2 ...)    procedure
  //(fl- fl)    procedure
  //(fl/ fl1 fl2 ...)    procedure
  //(fl/ fl)    procedure
  //(flabs fl)    procedure
  //(fldiv-and-mod fl1 fl2)    procedure
  //(fldiv fl1 fl2)    procedure
  //(flmod fl1 fl2)    procedure
  //(fldiv0-and-mod0 fl1 fl2)    procedure
  //(fldiv0 fl1 fl2)    procedure
  //(flmod0 fl1 fl2)    procedure
  //(flnumerator fl)    procedure
  //(fldenominator fl)    procedure
  //(flfloor fl)    procedure
  //(flceiling fl)    procedure
  //(fltruncate fl)    procedure
  //(flround fl)    procedure
  //(flexp fl)    procedure
  //(fllog fl)    procedure
  //(fllog fl1 fl2)    procedure
  //(flsin fl)    procedure
  //(flcos fl)    procedure
  //(fltan fl)    procedure
  //(flasin fl)    procedure
  //(flacos fl)    procedure
  //(flatan fl)    procedure
  //(flatan fl1 fl2)    procedure
  //(flsqrt fl)    procedure
  //(flexpt fl1 fl2)    procedure
  //&no-infinities    condition type
  //&no-nans    condition type
  //(fixnum->flonum fx)    procedure

  ////        11.4  Exact bitwise arithmetic
  //(bitwise-not ei)    procedure
  define_libfunc("bitwise-not", 1, 1, function (ar) {
    return ~ar[0];
  });

  //(bitwise-and ei1 ...)    procedure
  define_libfunc("bitwise-and", 1, null, function (ar) {
    return reduce(ar, function (ret, item) {
      return ret & item;
    });
  });

  //(bitwise-ior ei1 ...)    procedure
  define_libfunc("bitwise-ior", 1, null, function (ar) {
    return reduce(ar, function (ret, item) {
      return ret | item;
    });
  });

  //(bitwise-xor ei1 ...)    procedure
  define_libfunc("bitwise-xor", 1, null, function (ar) {
    return reduce(ar, function (ret, item) {
      return ret ^ item;
    });
  });

  //(bitwise-if ei1 ei2 ei3)    procedure
  define_libfunc("bitwise-if", 3, 3, function (ar) {
    return (ar[0] & ar[1]) | (~ar[0] & ar[2]);
  });

  //(bitwise-bit-count ei)    procedure
  define_libfunc("bitwise-bit-count", 1, 1, function (ar) {
    var e = Math.abs(ar[0]),
      ret = 0;
    for (; e != 0; e >>= 1) {
      if (e & 1) ret++;
    }
    return ret;
  });

  //(bitwise-length ei)    procedure
  define_libfunc("bitwise-length", 1, 1, function (ar) {
    var e = Math.abs(ar[0]),
      ret = 0;
    for (; e != 0; e >>= 1) {
      ret++;
    }
    return ret;
  });

  //(bitwise-first-bit-set ei)    procedure
  define_libfunc("bitwise-first-bit-set", 1, 1, function (ar) {
    var e = Math.abs(ar[0]),
      ret = 0;
    if (e == 0) return -1;
    for (; e != 0; e >>= 1) {
      if (e & 1) return ret;
      ret++;
    }
  });

  //(bitwise-bit-set? ei1 ei2)    procedure
  define_libfunc("bitwise-bit-set?", 2, 2, function (ar) {
    return !!(ar[0] & (1 << ar[1]));
  });

  //(bitwise-copy-bit ei1 n b)    procedure
  define_libfunc("bitwise-copy-bit", 3, 3, function (ar) {
    var mask = 1 << ar[1];
    return (
      (mask & (ar[2] << ar[1])) | // Set n-th bit to b
      (~mask & ar[0])
    ); // and use ei1 for rest of the bits
  });

  //(bitwise-bit-field ei1 start end)    procedure
  define_libfunc("bitwise-bit-field", 3, 3, function (ar) {
    var mask = ~(-1 << ar[2]); // Has 1 at 0...end
    return (mask & ar[0]) >> ar[1];
  });

  //(bitwise-copy-bit-field dst start end src)    procedure
  define_libfunc("bitwise-copy-bit-field", 4, 4, function (ar) {
    var dst = ar[0],
      start = ar[1],
      end = ar[2],
      src = ar[3];
    var mask =
      ~(-1 << end) & // Has 1 at 0...end
      (-1 << start); // Clear 0...start
    return (mask & (src << start)) | (~mask & dst);
  });

  //(bitwise-arithmetic-shift ei1 ei2)    procedure
  define_libfunc("bitwise-arithmetic-shift", 2, 2, function (ar) {
    return ar[1] >= 0 ? ar[0] << ar[1] : ar[0] >> -ar[1];
  });

  //(bitwise-arithmetic-shift-left ei1 ei2)    procedure
  define_libfunc("bitwise-arithmetic-shift-left", 2, 2, function (ar) {
    return ar[0] << ar[1];
  });

  //(bitwise-arithmetic-shift-right ei1 ei2)    procedure
  define_libfunc("bitwise-arithmetic-shift-right", 2, 2, function (ar) {
    return ar[0] >> ar[1];
  });

  //(bitwise-rotate-bit-field ei1 start end count)    procedure
  define_libfunc("bitwise-rotate-bit-field", 4, 4, function (ar) {
    var n = ar[0],
      start = ar[1],
      end = ar[2],
      count = ar[3];
    var width = end - start;
    if (width <= 0) return n;

    count %= width;
    var orig_field = (~(-1 << end) & n) >> start;
    var rotated_field = (orig_field << count) | (orig_field >> (width - count));

    var mask = ~(-1 << end) & (-1 << start);
    return (mask & (rotated_field << start)) | (~mask & n);
  });

  //(bitwise-reverse-bit-field ei1 ei2 ei3)    procedure
  define_libfunc("bitwise-reverse-bit-field", 3, 3, function (ar) {
    var ret = ar[0],
      n = ar[0],
      start = ar[1],
      end = ar[2];
    var orig_field = (~(-1 << end) & n) >> start;
    for (var i = 0; i < end - start; i++, orig_field >>= 1) {
      var bit = orig_field & 1;
      var setpos = end - 1 - i;
      var mask = 1 << setpos;
      ret = (mask & (bit << setpos)) | (~mask & ret);
    }
    return ret;
  });

  //
  // Chapter 12 syntax-case
  //

  //
  // Chapter 13 Hashtables
  //

  //13.1  Constructors
  //(define h (make-eq-hashtale)
  //(define h (make-eq-hashtable 1000))
  define_libfunc("make-eq-hashtable", 0, 1, function (ar) {
    // Note: ar[1] (hashtable size) is just ignored
    return new Hashtable(Hashtable.eq_hash, Hashtable.eq_equiv);
  });
  //(make-eqv-hashtable)    procedure
  //(make-eqv-hashtable k)    procedure
  define_libfunc("make-eqv-hashtable", 0, 1, function (ar) {
    return new Hashtable(Hashtable.eqv_hash, Hashtable.eqv_equiv);
  });
  //(make-hashtable hash-function equiv)    procedure
  //(make-hashtable hash-function equiv k)    procedure
  define_libfunc("make-hashtable", 2, 3, function (ar) {
    assert_procedure(ar[0]);
    assert_procedure(ar[1]);
    return new Hashtable(ar[0], ar[1]);
  });

  //13.2  Procedures
  // (hashtable? hash)
  define_libfunc("hashtable?", 1, 1, function (ar) {
    return ar[0] instanceof Hashtable;
  });
  //(hashtable-size hash)
  define_libfunc("hashtable-size", 1, 1, function (ar) {
    assert_hashtable(ar[0]);
    return ar[0].keys().length;
  });

  // Find a pair from a hashtable with given key.
  //
  // hash      - a BiwaScheme.Hashtable
  // key       - an object
  // callbacks - an object contains callback functions
  //             .on_found     - function(pair, hashed)
  //               pair   - [Object key, Object value]
  //               hashed - Object hashed
  //             .on_not_found - function(hashed)
  //               hashed - Object hashed
  //
  // Returns an instance of BiwaScheme.Call.
  const find_hash_pair = function (hash, key, callbacks) {
    // invoke hash proc
    return new Call(hash.hash_proc, [key], function (ar) {
      var hashed = ar[0];
      var candidate_pairs = hash.candidate_pairs(hashed);

      if (!candidate_pairs) {
        // shortcut: obviously not found
        return callbacks.on_not_found(hashed);
      }

      // search the exact key from candidates
      return Call.foreach(candidate_pairs, {
        call: function (pair) {
          // invoke the equivalence proc
          return new Call(hash.equiv_proc, [key, pair[0]]);
        },
        result: function (equal, pair) {
          if (equal) {
            // found
            return callbacks.on_found(pair, hashed);
          }
        },
        finish: function () {
          // not found
          return callbacks.on_not_found(hashed);
        },
      });
    });
  };

  //(hashtable-ref hash "foo" #f)
  define_libfunc("hashtable-ref", 3, 3, function (ar) {
    var hash = ar[0],
      key = ar[1],
      ifnone = ar[2];
    assert_hashtable(hash);

    return find_hash_pair(hash, key, {
      on_found: function (pair) {
        return pair[1];
      },
      on_not_found: function (hashed) {
        return ifnone;
      },
    });
  });

  //(hashtable-set! hash "foo" '(1 2))
  define_libfunc("hashtable-set!", 3, 3, function (ar) {
    var hash = ar[0],
      key = ar[1],
      value = ar[2];
    assert_hashtable(hash);
    assert(hash.mutable, "hashtable is not mutable");

    return find_hash_pair(hash, key, {
      on_found: function (pair) {
        pair[1] = value;
        return undef;
      },
      on_not_found: function (hashed) {
        hash.add_pair(hashed, key, value);
        return undef;
      },
    });
  });

  //(hashtable-delete! hash "foo")
  define_libfunc("hashtable-delete!", 2, 2, function (ar) {
    var hash = ar[0],
      key = ar[1];
    assert_hashtable(hash);
    assert(hash.mutable, "hashtable is not mutable");

    return find_hash_pair(hash, key, {
      on_found: function (pair, hashed) {
        hash.remove_pair(hashed, pair);
        return undef;
      },
      on_not_found: function (hashed) {
        return undef;
      },
    });
  });

  //(hashtable-contains? hash "foo")
  define_libfunc("hashtable-contains?", 2, 2, function (ar) {
    var hash = ar[0],
      key = ar[1];
    assert_hashtable(hash);

    return find_hash_pair(hash, key, {
      on_found: function (pair) {
        return true;
      },
      on_not_found: function (hashed) {
        return false;
      },
    });
  });

  //(hashtable-update! hashtable key proc default)    procedure
  define_libfunc("hashtable-update!", 4, 4, function (ar) {
    var hash = ar[0],
      key = ar[1],
      proc = ar[2],
      ifnone = ar[3];
    assert_hashtable(hash);
    assert(hash.mutable, "hashtable is not mutable");
    assert_procedure(proc);

    return find_hash_pair(hash, key, {
      on_found: function (pair, hashed) {
        // invoke proc and get new value
        return new Call(proc, [pair[1]], function (ar) {
          // replace the value
          pair[1] = ar[0];
          return undef;
        });
      },
      on_not_found: function (hashed) {
        // invoke proc and get new value
        return new Call(proc, [ifnone], function (ar) {
          // create new pair
          hash.add_pair(hashed, key, ar[0]);
          return undef;
        });
      },
    });
  });
  //(hashtable-copy hashtable)    procedure
  //(hashtable-copy hashtable mutable)    procedure
  define_libfunc("hashtable-copy", 1, 2, function (ar) {
    var mutable = ar[1] === undefined ? false : !!ar[1];
    assert_hashtable(ar[0]);
    return ar[0].create_copy(mutable);
  });
  //(hashtable-clear! hashtable)    procedure
  //(hashtable-clear! hashtable k)    procedure
  define_libfunc("hashtable-clear!", 0, 1, function (ar) {
    assert_hashtable(ar[0]);
    assert(ar[0].mutable, "hashtable is not mutable");
    ar[0].clear();
    return undef;
  });
  //(hashtable-keys hash)  ; => vector
  define_libfunc("hashtable-keys", 1, 1, function (ar) {
    assert_hashtable(ar[0]);
    return ar[0].keys();
  });
  //(hashtable-entries hash)  ; => two vectors (keys, values)
  define_libfunc("hashtable-entries", 1, 1, function (ar) {
    assert_hashtable(ar[0]);
    return new Values$1([ar[0].keys(), ar[0].values()]);
  });

  //13.3  Inspection

  //(hashtable-equivalence-function hashtable)    procedure
  define_libfunc("hashtable-equivalence-function", 1, 1, function (ar) {
    assert_hashtable(ar[0]);
    return ar[0].equiv_proc;
  });
  //(hashtable-hash-function hashtable)    procedure
  define_libfunc("hashtable-hash-function", 1, 1, function (ar) {
    assert_hashtable(ar[0]);
    return ar[0].hash_proc;
  });
  //(hashtable-mutable? hashtable)    procedure
  define_libfunc("hashtable-mutable?", 1, 1, function (ar) {
    assert_hashtable(ar[0]);
    return ar[0].mutable;
  });

  //13.4  Hash functions

  //(equal-hash obj)    procedure
  define_libfunc("equal-hash", 0, 0, function (ar) {
    return Hashtable.equal_hash;
  });
  //(string-hash string)    procedure
  define_libfunc("string-hash", 0, 0, function (ar) {
    return Hashtable.string_hash;
  });
  //(string-ci-hash string)    procedure
  define_libfunc("string-ci-hash", 0, 0, function (ar) {
    return Hashtable.string_ci_hash;
  });
  //(symbol-hash symbol)    procedure
  define_libfunc("symbol-hash", 0, 0, function (ar) {
    return Hashtable.symbol_hash;
  });

  //
  // Chapter 14 Enumerators
  //
  //(make-enumeration symbol-list) -> enum-set(new type)
  define_libfunc("make-enumeration", 1, 1, function (ar) {
    assert_list(ar[0]);
    var members = ar[0].to_array();
    var enum_type = new Enumeration.EnumType(members);
    return enum_type.universe();
  });

  //(enum-set-universe enum-set) -> enum-set(same type as the argument)
  define_libfunc("enum-set-universe", 1, 1, function (ar) {
    assert_enum_set(ar[0]);
    return ar[0].enum_type.universe();
  });

  //(enum-set-indexer enum-set) -> (lambda (sym)) -> integer or #f
  define_libfunc("enum-set-indexer", 1, 1, function (ar) {
    assert_enum_set(ar[0]);
    return ar[0].enum_type.indexer();
  });

  //(enum-set-constructor enum-set) -> (lambda (syms)) -> enum-set(same type as the argument)
  define_libfunc("enum-set-constructor", 1, 1, function (ar) {
    assert_enum_set(ar[0]);
    return ar[0].enum_type.constructor();
  });

  //(enum-set->list enum-set) -> symbol-list
  define_libfunc("enum-set->list", 1, 1, function (ar) {
    assert_enum_set(ar[0]);
    return ar[0].symbol_list();
  });

  //(enum-set-member? symbol enum-set) -> bool
  define_libfunc("enum-set-member?", 2, 2, function (ar) {
    assert_symbol(ar[0]);
    assert_enum_set(ar[1]);
    return ar[1].is_member(ar[0]);
  });

  //(enum-set-subset? esa esb) -> bool
  define_libfunc("enum-set-subset?", 2, 2, function (ar) {
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    return ar[0].is_subset(ar[1]);
  });

  //(enum-set=? esa esb) -> bool
  define_libfunc("enum-set=?", 2, 2, function (ar) {
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    return ar[0].equal_to(ar[1]);
  });

  //(enum-set-union es1 es2) -> enum-set
  define_libfunc("enum-set-union", 2, 2, function (ar) {
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    assert(
      ar[0].enum_type === ar[1].enum_type,
      "two enum-sets must be the same enum-type",
      "enum-set-union"
    );
    return ar[0].union(ar[1]);
  });

  //(enum-set-intersection es1 es2) -> enum-set
  define_libfunc("enum-set-intersection", 2, 2, function (ar) {
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    return ar[0].intersection(ar[1]);
  });

  //(enum-set-difference es1 es2) -> enum-set
  define_libfunc("enum-set-difference", 2, 2, function (ar) {
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    return ar[0].difference(ar[1]);
  });

  //(enum-set-complement enum-set) -> enum-set
  define_libfunc("enum-set-complement", 1, 1, function (ar) {
    assert_enum_set(ar[0]);
    return ar[0].complement();
  });

  //(enum-set-projection esa esb) -> enum-set
  define_libfunc("enum-set-projection", 2, 2, function (ar) {
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    return ar[0].projection(ar[1]);
  });

  //(define-enumeration <type-name> (<symbol> ...) <constructor-syntax>)
  // Example:
  //   (define-enumeration color (red green black white) color-set)
  //   this defines:
  //     - an EnumType
  //     - (color red) ;=> 'red
  //     - (color-set red black) ;=> #<enum-set (red black)>
  define_syntax("define-enumeration", function (x) {
    // Extract parameters
    var type_name = x.cdr.car;
    assert(
      isSymbol(type_name),
      "expected symbol for type_name",
      "define-enumeration"
    );
    type_name = type_name.name;

    var members = x.cdr.cdr.car;
    assert(
      isList(members),
      "expected list of symbol for members",
      "define-enumeration"
    );
    members = members.to_array();

    var constructor_name = x.cdr.cdr.cdr.car;
    assert(
      isSymbol(constructor_name),
      "expected symbol for constructor_name",
      "define-enumeration"
    );
    constructor_name = constructor_name.name;

    // Define EnumType
    var enum_type = new Enumeration.EnumType(members);

    // Define (color red)
    define_syntax(type_name, function (x) {
      // (color)
      assert(!isNil(x.cdr), "an argument is needed", type_name);

      var arg = x.cdr.car;
      assert_symbol(arg, type_name);

      // Check arg is included in the universe
      assert(
        contains(enum_type.members, arg),
        arg.name +
          " is not included in the universe: " +
          to_write$1(enum_type.members),
        type_name
      );

      return List(Sym("quote"), arg);
    });

    // Define (color-set red black)
    define_syntax(constructor_name, function (x) {
      assert_list(x.cdr, constructor_name);

      var symbols = x.cdr.to_array();

      // Check each argument is included in the universe
      each(symbols, function (arg) {
        assert_symbol(arg, constructor_name);
        assert(
          contains(enum_type.members, arg),
          arg.name +
            " is not included in the universe: " +
            to_write$1(enum_type.members),
          constructor_name
        );
      });

      // Create an EnumSet
      return new Enumeration.EnumSet(enum_type, symbols);
    });
  });

  //
  // Chapter 15 Composite library
  //
  //(rnrs 6) = all - eval - mutable pairs - mutable strings - r5rs compatibility

  //
  // Chapter 16 eval
  //
  //(eval expression environment)    procedure
  define_libfunc("eval", 1, 1, function (ar, intp) {
    //TODO: environment
    //TODO: this implementation has a bug that
    //  expressions which contains #<undef>, etc. cannot be evaluated.
    var expr = ar[0];
    var intp2 = new Interpreter(intp);
    return intp2.evaluate(to_write$1(expr));
  });
  //(environment import-spec ...)    procedure

  //
  // Chapter 17 Mutable pairs
  //
  //(set-car! pair obj)    procedure
  //(set-cdr! pair obj)    procedure

  //
  // Chapter 18 Mutable strings
  //
  //(string-set! string k char)    procedure
  // (string-fill! string char)    procedure

  //
  // Chapter 19 R5RS compatibility
  //
  //(exact->inexact z)    procedure
  //(inexact->exact z)    procedure
  //
  //(quotient n1 n2)    procedure
  //(remainder n1 n2)    procedure
  //(modulo n1 n2)    procedure
  //
  //(null-environment n)    procedure
  //(scheme-report-environment n)    procedure

  //
  // R7RS (TODO: split file?)
  //

  // R7RS Promise
  //
  // (delay expression)
  define_syntax("delay", function (x) {
    if (x.cdr === nil$1) {
      throw new BiwaError("malformed delay: no argument");
    }
    if (x.cdr.cdr !== nil$1) {
      throw new BiwaError(
        "malformed delay: too many arguments: " + to_write_ss(x)
      );
    }
    var expr = x.cdr.car;
    // Expand into call of internal function
    // ( procedure->promise (lambda () (make-promise expr)))
    return new Pair(
      Sym(" procedure->promise"),
      new Pair(
        new Pair(
          Sym("lambda"),
          new Pair(
            nil$1,
            new Pair(
              new Pair(Sym("make-promise"), new Pair(expr, nil$1)),
              nil$1
            )
          )
        )
      )
    );
  });

  // (delay-force promise-expr)
  define_syntax("delay-force", function (x) {
    if (x.cdr === nil$1) {
      throw new BiwaError("malformed delay-force: no argument");
    }
    if (x.cdr.cdr !== nil$1) {
      throw new BiwaError(
        "malformed delay-force: too many arguments: " + to_write_ss(x)
      );
    }
    var expr = x.cdr.car;
    // Expand into call of internal function
    // ( procedure->promise (lambda () expr))
    return new Pair(
      Sym(" procedure->promise"),
      new Pair(
        new Pair(Sym("lambda"), new Pair(nil$1, new Pair(expr, nil$1))),
        nil$1
      )
    );
  });

  // (force promise)
  var force = function (promise) {
    if (promise.is_done()) {
      return promise.value();
    }
    return new Call(promise.thunk(), [], function (ar) {
      assert_promise(ar[0]);
      var new_promise = ar[0];
      if (promise.is_done()) {
        // reentrant!
        return promise.value();
      } else {
        promise.update_with(new_promise);
        return force(new_promise);
      }
    });
  };
  define_libfunc("force", 1, 1, function (ar, intp) {
    assert_promise(ar[0]);
    return force(ar[0]);
  });

  // (promise? obj)
  define_libfunc("promise?", 1, 1, function (ar, intp) {
    return ar[0] instanceof BiwaPromise;
  });

  // (make-promise obj)
  define_libfunc("make-promise", 1, 1, function (ar, intp) {
    var obj = ar[0];
    if (obj instanceof BiwaPromise) {
      return obj;
    } else {
      return BiwaPromise.done(obj);
    }
  });

  // internal function
  // ( procedure->promise proc)
  // proc must be a procedure with no argument and return a promise
  define_libfunc(" procedure->promise", 1, 1, function (ar, intp) {
    assert_procedure(ar[0]);
    return BiwaPromise.fresh(ar[0]);
  });

  //
  // parameterize
  //

  // (make-parameter init)
  // (make-parameter init converter)
  define_libfunc("make-parameter", 1, 2, function (ar, intp) {
    let currentValue;
    const converter = ar[1];
    // A parameter is represented by a JS function.
    // (parameterObj)   ;=> Return the current value
    // (parameterObj v) ;=> Set v as the value and return the original value
    const parameterObj = function (ar2) {
      if (ar2.length == 0) {
        // Get
        return currentValue;
      } else {
        // Set
        const origValue = currentValue;
        if (converter) {
          return new Call(converter, [ar2[0]], (ar3) => {
            currentValue = ar3[0];
            return origValue;
          });
        } else {
          currentValue = ar2[0];
          return origValue;
        }
      }
    };

    if (converter) {
      return new Call(converter, [ar[0]], (initialValue) => {
        currentValue = initialValue;
        return parameterObj;
      });
    } else {
      const initialValue = ar[0];
      currentValue = initialValue;
      return parameterObj;
    }
  });

  // (parameterize ((param val) ...) body ...)
  define_syntax("parameterize", function (x) {
    const inits = x.cdr.car.to_array();
    const body = x.cdr.cdr;
    const tmpNames = inits.map(() => gensym());
    // (let ((tmp0 val0) (tmp1 val1) ...)
    //   (dynamic-wind
    //     (lambda () (begin (set! tmp0 (param0 tmp0))) ...)
    //     (lambda () body ...)
    //     (lambda () (begin (set! tmp0 (param0 tmp0))) ...)))
    const givenValues = List(
      ...inits.map((item, i) => List(tmpNames[i], item.cdr.car))
    );
    const updateValues = Cons(
      Sym("begin"),
      List(
        ...inits.map((item, i) =>
          List(Sym("set!"), tmpNames[i], List(item.car, tmpNames[i]))
        )
      )
    );

    const before = List(Sym("lambda"), nil$1, updateValues);
    const thunk = Cons(Sym("lambda"), Cons(nil$1, body));
    const after = List(Sym("lambda"), nil$1, updateValues);
    //return List(Sym("quote"),
    return List(
      Sym("let"),
      givenValues,
      List(Sym("dynamic-wind"), before, thunk, after)
    );
    //)
  });

  //
  // srfi.js - SRFI libraries
  //
  // should be src/library/srfi/1.js, etc (in the future).
  //

  //
  // srfi-1 (list)
  //
  // (iota count start? step?)
  define_libfunc("iota", 1, 3, function (ar) {
    var count = ar[0];
    var start = ar[1] || 0;
    var step = ar[2] === undefined ? 1 : ar[2];
    assert_integer(count);
    assert_number(start);
    assert_number(step);

    var a = [],
      n = start;
    for (var i = 0; i < count; i++) {
      a.push(n);
      n += step;
    }
    return array_to_list(a);
  });

  var copy_pair = function (pair) {
    var car = isPair(pair.car) ? copy_pair(pair.car) : pair.car;
    var cdr = isPair(pair.cdr) ? copy_pair(pair.cdr) : pair.cdr;
    return new Pair(car, cdr);
  };
  // (list-copy list)
  define_libfunc("list-copy", 1, 1, function (ar) {
    if (isPair(ar[0])) {
      return copy_pair(ar[0]);
    } else {
      return nil$1;
    }
  });

  //
  // srfi-6 & Gauche (string port)
  //
  define_libfunc("open-input-string", 1, 1, function (ar) {
    assert_string(ar[0]);
    return new Port.StringInput(ar[0]);
  });
  define_libfunc("open-output-string", 0, 0, function (ar) {
    return new Port.StringOutput();
  });
  define_libfunc("get-output-string", 1, 1, function (ar) {
    assert_port(ar[0]);
    if (!(ar[0] instanceof Port.StringOutput))
      throw new Error(
        "get-output-string: port must be made by 'open-output-string'"
      );
    return ar[0].output_string();
  });

  //
  // srfi-8 (receive)
  //

  // (receive <formals> <expression> <body>...)
  // -> (call-with-values (lambda () expression)
  //                        (lambda formals body ...))
  define_syntax("receive", function (x) {
    assert(isPair(x.cdr), "missing formals", "receive");
    var formals = x.cdr.car;
    assert(isPair(x.cdr.cdr), "missing expression", "receive");
    var expression = x.cdr.cdr.car;
    var body = x.cdr.cdr.cdr;

    return deep_array_to_list([
      Sym("call-with-values"),
      [Sym("lambda"), nil$1, expression],
      new Pair(Sym("lambda"), new Pair(formals, body)),
    ]);
  });

  // srfi-19 (time)
  //
  //  // constants
  //time-duration
  //time-monotonic
  //time-process
  //time-tai
  //time-thread
  //time-utc
  // Current time and clock resolution
  // (current-date [tz-offset])
  define_libfunc("current-date", 0, 1, function (ar) {
    //todo: tz-offset (ar[1])
    return new Date();
  });
  //
  //current-julian-day -> jdn
  //current-modified-julian-day -> mjdn
  //current-time [time-type] -> time
  //time-resolution [time-type] -> integer
  //  // Time object and accessors
  //make-time type nanosecond second -> time
  //time? object -> boolean
  //time-type time -> time-type
  //time-nanosecond time -> integer
  //time-second time -> integer
  //set-time-type! time time-type
  //set-time-nanosecond! time integer
  //set-time-second! time integer
  //copy-time time1 -> time2
  //  // Time comparison procedures
  //time<=? time1 time2 -> boolean
  //time<? time1 time2 -> boolean
  //time=? time1 time2 -> boolean
  //time>=? time1 time2 -> boolean
  //time>? time1 time2 -> boolean
  //  // Time arithmetic procedures
  //time-difference time1 time2 -> time-duration
  //time-difference! time1 time2 -> time-duration
  //add-duration time1 time-duration -> time
  //add-duration! time1 time-duration -> time
  //subtract-duration time1 time-duration -> time
  //subtract-duration! time1 time-duration -> time
  // Date object and accessors
  // make-date
  define_libfunc("date?", 1, 1, function (ar) {
    return ar[0] instanceof Date;
  });
  define_libfunc("date-nanosecond", 1, 1, function (ar) {
    assert_date(ar[0]);
    return ar[0].getMilliseconds() * 1000000;
  });
  define_libfunc("date-millisecond", 1, 1, function (ar) {
    // not srfi-19
    assert_date(ar[0]);
    return ar[0].getMilliseconds();
  });
  define_libfunc("date-second", 1, 1, function (ar) {
    assert_date(ar[0]);
    return ar[0].getSeconds();
  });
  define_libfunc("date-minute", 1, 1, function (ar) {
    assert_date(ar[0]);
    return ar[0].getMinutes();
  });
  define_libfunc("date-hour", 1, 1, function (ar) {
    assert_date(ar[0]);
    return ar[0].getHours();
  });
  define_libfunc("date-day", 1, 1, function (ar) {
    assert_date(ar[0]);
    return ar[0].getDate();
  });
  define_libfunc("date-month", 1, 1, function (ar) {
    assert_date(ar[0]);
    return ar[0].getMonth() + 1; //Jan = 0 in javascript..
  });
  define_libfunc("date-year", 1, 1, function (ar) {
    assert_date(ar[0]);
    return ar[0].getFullYear();
  });
  //date-zone-offset
  //date-year-day
  define_libfunc("date-week-day", 1, 1, function (ar) {
    assert_date(ar[0]);
    return ar[0].getDay();
  });
  //date-week-number

  // Time/Date/Julian Day/Modified Julian Day Converters
  // (snipped)

  // Date to String/String to Date Converters
  // TODO: support locale
  //   * date_names
  //   * ~f 5.2 sec
  //   * ~p AM/PM
  //   * ~X 2007/01/01
  const date_names = {
    weekday: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
    full_weekday: [
      "Sunday",
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
    ],
    month: [
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec",
    ],
    full_month: [
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "Octorber",
      "November",
      "December",
    ],
  };

  const date2string = function (date, format) {
    var zeropad = function (n) {
      return n < 10 ? "0" + n : "" + n;
    };
    var spacepad = function (n) {
      return n < 10 ? " " + n : "" + n;
    };
    var isoweeknum = function (x) {
      var janFour = new Date(x.getFullYear(), 0, 4);
      var weekone = new Date(x.getFullYear(), 0, 4);

      if (janFour.getDay() >= date_names.weekday.indexOf("Thu")) {
        weekone.setDate(janFour.getDate() - (janFour.getDay() + 1));
      } else {
        weekone.setDate(janFour.getDate() + (7 - janFour.getDay() + 1));
      }

      return Math.ceil((x - weekone) / 86400000 / 7);
    };

    var getter = {
      a: function (x) {
        return date_names.weekday[x.getDay()];
      },
      A: function (x) {
        return date_names.full_weekday[x.getDay()];
      },
      b: function (x) {
        return date_names.month[x.getMonth()];
      },
      B: function (x) {
        return date_names.full_month[x.getMonth()];
      },
      c: function (x) {
        return x.toString();
      },
      d: function (x) {
        return zeropad(x.getDate());
      },
      D: function (x) {
        return getter.d(x) + getter.m(x) + getter.y(x);
      },
      e: function (x) {
        return spacepad(x.getDate());
      },
      f: function (x) {
        return x.getSeconds() + x.getMilliseconds() / 1000;
      },
      h: function (x) {
        return date_names.month[x.getMonth()];
      },
      H: function (x) {
        return zeropad(x.getHours());
      },
      I: function (x) {
        var h = x.getHours();
        return zeropad(h < 13 ? h : h - 12);
      },
      j: function (x) {
        throw new Bug("not implemented: day of year");
      },
      k: function (x) {
        return spacepad(x.getHours());
      },
      l: function (x) {
        var h = x.getHours();
        return spacepad(h < 13 ? h : h - 12);
      },
      m: function (x) {
        return zeropad(x.getMonth() + 1);
      },
      M: function (x) {
        return zeropad(x.getMinutes());
      },
      n: function (x) {
        return "\n";
      },
      N: function (x) {
        throw new Bug("not implemented: nanoseconds");
      },
      p: function (x) {
        return x.getHours() < 13 ? "AM" : "PM";
      },
      r: function (x) {
        return (
          getter.I(x) +
          ":" +
          getter.M(x) +
          ":" +
          getter.S(x) +
          " " +
          getter.p(x)
        );
      },
      s: function (x) {
        return Math.floor(x.getTime() / 1000);
      },
      S: function (x) {
        return zeropad(x.getSeconds());
      },
      t: function (x) {
        return "\t";
      },
      T: function (x) {
        return getter.H(x) + ":" + getter.M(x) + ":" + getter.S(x);
      },
      U: function (x) {
        throw new Bug("not implemented: weeknum(0~, Sun)");
      },
      V: function (x) {
        return isoweeknum(x);
      },
      w: function (x) {
        return x.getDay();
      },
      W: function (x) {
        throw new Bug("not implemented: weeknum(0~, Mon)");
      },
      x: function (x) {
        throw new Bug("not implemented: weeknum(1~, Mon)");
      },
      X: function (x) {
        return getter.Y(x) + "/" + getter.m(x) + "/" + getter.d(x);
      },
      y: function (x) {
        return x.getFullYear() % 100;
      },
      Y: function (x) {
        return x.getFullYear();
      },
      z: function (x) {
        throw new Bug("not implemented: time-zone");
      },
      Z: function (x) {
        throw new Bug("not implemented: symbol time zone");
      },
      1: function (x) {
        throw new Bug("not implemented: ISO-8601 year-month-day format");
      },
      2: function (x) {
        throw new Bug(
          "not implemented: ISO-8601 hour-minute-second-timezone format"
        );
      },
      3: function (x) {
        throw new Bug("not implemented: ISO-8601 hour-minute-second format");
      },
      4: function (x) {
        throw new Bug(
          "not implemented: ISO-8601 year-month-day-hour-minute-second-timezone format"
        );
      },
      5: function (x) {
        throw new Bug(
          "not implemented: ISO-8601 year-month-day-hour-minute-second format"
        );
      },
    };

    return format.replace(/~([\w1-5~])/g, function (str, x) {
      var func = getter[x];
      if (func) return func(date);
      else if (x == "~") return "~";
      else return x;
    });
  };

  // date->string date template
  define_libfunc("date->string", 1, 2, function (ar) {
    assert_date(ar[0]);

    if (ar[1]) {
      assert_string(ar[1]);
      return date2string(ar[0], ar[1]);
    } else return ar[0].toString();
  });
  // string->date

  // parse-date date
  define_libfunc("parse-date", 1, 1, function (ar) {
    // not srfi-19
    assert_string(ar[0]);
    return new Date(Date.parse(ar[0]));
  });

  //
  // srfi-27
  //
  define_libfunc("random-integer", 1, 1, function (ar) {
    var n = ar[0];
    assert_integer(n);
    if (n < 0) throw new Error("random-integer: the argument must be >= 0");
    else return Math.floor(Math.random() * ar[0]);
  });
  define_libfunc("random-real", 0, 0, function (ar) {
    return Math.random();
  });

  //
  // srfi-28 (format)
  //

  // (format format-str obj1 obj2 ...) -> string
  // (format #f format-str ...) -> string
  // (format #t format-str ...) -> output to current port
  // (format port format-str ...) -> output to the port
  //   ~a: display
  //   ~s: write
  //   ~%: newline
  //   ~~: tilde
  define_libfunc("format", 1, null, function (ar) {
    if (isString$1(ar[0])) {
      var port = null,
        format_str = ar.shift();
    } else if (ar[0] === false) {
      ar.shift();
      var port = null,
        format_str = ar.shift();
    } else if (ar[0] === true) {
      ar.shift();
      var port = Port.current_output,
        format_str = ar.shift();
    } else {
      var port = ar.shift(),
        format_str = ar.shift();
      assert_port(port);
    }

    var str = format_str
      .replace(/~[as]/g, function (matched) {
        assert(ar.length > 0, "insufficient number of arguments", "format");
        if (matched == "~a") return to_display(ar.shift());
        else return to_write$1(ar.shift());
      })
      .replace(/~%/, "\n")
      .replace(/~~/, "~");
    if (port) {
      port.put_string(str);
      return undef;
    } else {
      return str;
    }
  });

  //
  // srfi-38 (write/ss)
  //
  const user_write_ss = function (ar) {
    Console.puts(write_ss(ar[0]), true);
    return undef;
  };
  define_libfunc("write/ss", 1, 2, user_write_ss);
  define_libfunc("write-with-shared-structure", 1, 2, user_write_ss);
  define_libfunc("write*", 1, 2, user_write_ss); //from Gauche(STklos)

  //
  // srfi-43 (vector library)
  //
  define_libfunc("vector-append", 2, null, function (ar) {
    var vec = [];
    return vec.concat.apply(vec, ar);
  });

  // (vector-copy vector)
  define_libfunc("vector-copy", 1, 1, function (ar) {
    assert_vector(ar[0]);
    return clone(ar[0]);
  });

  //
  // see src/library/node_functions.js for:
  // - srfi-98 (get-environment-variable)
  //

  // Avoid circular dependency
  nil$1.to_set = function () {
    return new BiwaSet();
  };

  var BiwaScheme$1 = {
    TopEnv,
    CoreEnv,
    nil: nil$1,
    undef,
    max_trace_size,
    suppress_deprecation_warning,
    Version: VERSION,
    VERSION,
    GitCommit,
    isNil,
    isUndef,
    isBoolean,
    isString,
    isChar,
    isSymbol,
    isPort,
    isPair,
    isList,
    isVector,
    isHashtable,
    isMutableHashtable,
    isClosure,
    makeClosure,
    isProcedure,
    isSelfEvaluating,
    eq,
    eqv,
    equal,
    lt,
    to_write: to_write$1,
    to_display,
    inspect,
    write_ss,
    to_write_ss,
    Call,
    Char,
    Compiler,
    Enumeration,
    isEnumSet,
    Error: BiwaError,
    Bug: Bug$1,
    UserError,
    Hashtable,
    Interpreter,
    Complex,
    Rational,
    isNumber: isNumber$1,
    isComplex,
    isReal,
    isRational,
    isInteger,
    Pair,
    List,
    array_to_list,
    deep_array_to_list,
    Cons,
    Parser,
    Pause,
    Port,
    eof,
    Promise: BiwaPromise,
    isPromise,
    Record,
    isRecord,
    isRecordTD,
    isRecordCD,
    Set: BiwaSet,
    Symbol: BiwaSymbol,
    Sym,
    gensym,
    Syntax,
    Values: Values$1,

    reduce_cyclic_info,
    find_cyclic,
    define_libfunc,
    define_scmfunc,
    parse_fraction,
    is_valid_integer_notation,
    parse_integer,
    is_valid_float_notation,
    parse_float,
  };

  Console.puts = function (str, no_newline) {
    Port.current_output.put_string(str + (no_newline ? "" : "\n"));
  };

  Console.p = function (/*ARGS*/) {
    Port.current_output.put_string(
      "p> " + map(toArray(arguments), inspect).join(" ")
    );
  };

  const current_input = new Port.CustomInput(function (callback) {
    const out = document.querySelector("#bs-console");
    const form = document.createElement("form");
    form.innerHTML =
      "<input id='webscheme-read-line' type='text'><input type='submit' value='ok'>";
    out.appendChild(form);
    form.addEventListener("submit", function () {
      const input = document.querySelector("#webscheme-read-line").value;
      form.remove();
      callback(input);
      return false;
    });
  });

  const current_output = new Port.CustomOutput(function (str) {
    const out = document.querySelector("#bs-console");
    if (!out) return;
    const span = document.createElement("span");
    span.innerHTML = escape(str).replace(/\n/g, "<br>").replace(/ /g, "&nbsp;");
    out.appendChild(span);
  });

  const current_error = current_output;

  // To use webscheme_lib, jQuery must be loaded beforehand
  const $$1 = window.jQuery;

  define_libfunc("read-line", 0, 1, function (ar) {
    var port = ar[0] || Port.current_input;
    assert_port(port);
    return port.get_string((str) => str);
  });

  //
  // element
  //
  define_libfunc("element-empty!", 1, 1, function (ar) {
    if ($$1(ar[0]).prop("value")) {
      return $$1(ar[0]).val("");
    } else {
      return $$1(ar[0]).empty();
    }
  });
  alias_libfunc("element-empty!", "element-clear!");
  define_libfunc("element-visible?", 1, 1, function (ar) {
    return $$1(ar[0]).is(":visible");
  });
  define_libfunc("element-toggle!", 1, 1, function (ar) {
    return $$1(ar[0]).toggle();
  });
  define_libfunc("element-hide!", 1, 1, function (ar) {
    return $$1(ar[0]).hide();
  });
  define_libfunc("element-show!", 1, 1, function (ar) {
    return $$1(ar[0]).show();
  });
  define_libfunc("element-remove!", 1, 1, function (ar) {
    return $$1(ar[0]).remove();
  });
  define_libfunc("element-update!", 2, 2, function (ar) {
    return $$1(ar[0]).html(ar[1]);
  });
  define_libfunc("element-replace!", 2, 2, function (ar) {
    return $$1(ar[0]).replaceWith(ar[1]);
  });
  define_libfunc("element-insert!", 2, 2, function (ar) {
    return $$1(ar[0]).append(ar[1]);
  });
  define_libfunc("element-wrap!", 3, 3, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-ancestors", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-descendants", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-first-descendant", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-immediate-descendants", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-previous-sibling", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-next-sibling", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-siblings", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-match?", 2, 2, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-up", 3, 3, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-down", 3, 3, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-previous", 3, 3, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-next", 3, 3, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-select", 1, 1, function (ar) {
    $$1(ar[0]).select();
  });
  define_libfunc("element-adjacent", 0, 0, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-identify", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-read-attribute", 2, 2, function (ar) {
    assert_string(ar[1]);
    return $$1(ar[0]).prop(ar[1]);
  });

  var element_write_attribute = function (ar) {
    assert_string(ar[1]);
    return $$1(ar[0]).prop(ar[1], ar[2]);
  };
  define_libfunc("element-write-attribute", 3, 3, function (ar) {
    deprecate("element-write-attribute", "1.0", "element-write-attribute!");
    return element_write_attribute(ar);
  });
  define_libfunc("element-write-attribute!", 3, 3, element_write_attribute);

  define_libfunc("element-height", 1, 1, function (ar) {
    return $$1(ar[0]).height();
  });
  define_libfunc("element-width", 1, 1, function (ar) {
    return $$1(ar[0]).width();
  });

  define_libfunc("element-class-names", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-has-class-name?", 2, 2, function (ar) {
    assert_string(ar[1]);
    return $$1(ar[0]).hasClass(ar[1]);
  });

  var element_add_class_name = function (ar) {
    assert_string(ar[1]);
    return $$1(ar[0]).addClass(ar[1]);
  };
  define_libfunc("element-add-class-name", 2, 2, function (ar) {
    deprecate("element-add-class-name", "1.0", "element-add-class-name!");
    return element_add_class_name(ar);
  });
  define_libfunc("element-add-class-name!", 2, 2, element_add_class_name);

  var element_remove_class_name = function (ar) {
    assert_string(ar[1]);
    return $$1(ar[0]).removeClass(ar[1]);
  };
  define_libfunc("element-remove-class-name", 2, 2, function (ar) {
    deprecate("element-remove-class-name", "1.0", "element-remove-class-name!");
    return element_remove_class_name(ar);
  });
  define_libfunc("element-remove-class-name!", 2, 2, element_remove_class_name);

  var element_toggle_class_name = function (ar) {
    assert_string(ar[1]);
    return $$1(ar[0]).toggleClass(ar[1]);
  };
  define_libfunc("element-toggle-class-name", 2, 2, function (ar) {
    deprecate("element-toggle-class-name", "1.0", "element-toggle-class-name!");
    return element_toggle_class_name(ar);
  });
  define_libfunc("element-toggle-class-name!", 2, 2, element_toggle_class_name);

  define_libfunc("element-clean-whitespace!", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-empty?", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-descendant-of!", 2, 2, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("scroll-to-element!", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-style", 2, 2, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-opacity", 2, 2, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-style-set!", 2, 2, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-opacity-set!", 2, 2, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-dimensions", 1, 1, function (ar) {
    return new Values($$1(ar[0]).width(), $$1(ar[0]).height());
  });
  define_libfunc("element-make-positioned!", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-undo-positioned!", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-make-clipping!", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-undo-clipping!", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-cumulative-offset", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-positioned-offset", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-absolutize!", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-relativize!", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-cumulative-scroll-offset", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-offset-parent", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-viewport-offset", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-clone-position!", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-absolutize!", 1, 1, function (ar) {
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-focus!", 1, 1, function (ar) {
    return $$1(ar[0]).focus();
  });

  // usage:
  //  (element-new '(div "foo"))        => <div>foo</div>
  //  (element-new '("div#main" "foo")) => <div id='main'>foo</div>
  //  (element-new '("div.red" "foo"))  => <div class='red'>foo</div>
  //  (element-new '(div align "right" "foo"))  => <div align='right'>foo</div>
  //  (element-new '(div (span "foo"))  => <div><span>foo</span></div>
  //

  const create_elements_by_string = function (spec) {
    spec = spec.to_array();
    var name = spec.shift();
    if (name instanceof BiwaSymbol) name = name.name;
    var m = name.match(/(.*)\.(.*)/);
    if (m) {
      name = m[1];
      spec.unshift(Sym("class"), m[2]);
    }
    m = name.match(/(.*)\#(.*)/);
    if (m) {
      name = m[1];
      spec.unshift(Sym("id"), m[2]);
    }
    var children = [];
    var s = ["<" + name];
    for (var i = 0; i < spec.length; i++) {
      if (spec[i] instanceof BiwaSymbol) {
        s.push(" " + spec[i].name + '="' + spec[i + 1] + '"');
        i++;
      } else {
        if (spec[i] instanceof Pair)
          children.push(create_elements_by_string(spec[i]));
        else children.push(spec[i]); // String
      }
    }
    s.push(">");
    s.push(children.join(""));
    s.push("</" + name + ">");
    return s.join("");
  };

  const tree_all = function (tree, pred) {
    if (tree === nil) return true;
    else if (pred(tree.car) === false) return false;
    else return tree_all(tree.cdr, pred);
  };
  define_libfunc("element-new", 1, 1, function (ar) {
    var string_or_symbol = function (item) {
      return (
        isString$1(item) || item instanceof BiwaSymbol || item instanceof Pair
      );
    };
    if (tree_all(ar[0], string_or_symbol)) {
      return $$1(create_elements_by_string(ar[0]))[0];
    } else {
      return nil;
    }
  });

  const element_content = function (selector) {
    if ($$1(selector).prop("value")) {
      return $$1(selector).val();
    } else {
      return escape($$1(selector).html());
    }
  };
  define_libfunc("element-content", 1, 1, function (ar) {
    return element_content(ar[0]);
  });

  //
  // load from network
  //
  define_libfunc("load", 1, 1, function (ar, intp) {
    var path = ar[0];
    assert_string(path);
    var intp2 = new Interpreter(intp);
    return new Pause(function (pause) {
      $$1.ajax(path, {
        dataType: "text",
        mimeType: "text/plain; charset=UTF-8", // For Firefox (#88)
        success: function (data) {
          // create new interpreter not to destroy current stack.
          intp2.evaluate(data, function () {
            return pause.resume(undef);
          });
        },
        error: function () {
          throw new Error("load: network error: failed to load " + path);
        },
      });
    });
  });

  // Load javascript file on the server
  const _require = function (src, check, proc) {
    var script = $$1("<script/>", { src: src });
    $$1("body").append(script);

    var checker = new Function("return !!(" + check + ")");

    if (checker()) proc();
    else
      setTimeout(function () {
        checker() ? proc() : setTimeout(arguments.callee, 10);
      }, 10);
  };
  // (js-load "lib/foo.js" "Foo")
  define_libfunc("js-load", 2, 2, function (ar) {
    var path = ar[0];
    var check = ar[1];
    assert_string(path);
    assert_string(check);

    return new Pause(function (pause) {
      _require(path, "window." + check, function () {
        pause.resume(undef);
      });
    });
  });

  //
  // html modification
  //

  const getelem = function (ar) {
    // account for getelem returning false when no results (and that getting passed back in)
    if (ar.length > 1 && ar[1] === false) {
      ar[1] = [];
    }

    var x = $$1.apply(this, ar);
    if (x.length > 0) {
      return x;
    } else {
      return false;
    }
  };
  define_libfunc("$", 1, 2, getelem);
  define_libfunc("getelem", 1, 2, getelem);
  define_libfunc("dom-element", 1, 1, function (ar) {
    return $$1(ar[0])[0];
  });

  define_libfunc("set-style!", 3, 3, function (ar) {
    assert_string(ar[1]);
    $$1(ar[0]).css(ar[1], ar[2]);
    return undef;
  });
  define_libfunc("get-style", 2, 2, function (ar) {
    assert_string(ar[1]);
    return $$1(ar[0]).css(ar[1]);
  });
  define_libfunc("set-content!", 2, 2, function (ar) {
    assert_string(ar[1]);
    var str = ar[1].replace(/\n/g, "<br>").replace(/\t/g, "&nbsp;&nbsp;&nbsp;");
    $$1(ar[0]).html(str);
    return undef;
  });
  define_libfunc("get-content", 1, 1, function (ar) {
    return element_content(ar[0]);
  });

  //
  // handlers
  //
  define_libfunc("set-handler!", 3, 3, function (ar, intp) {
    throw new Error(
      "set-handler! is obsolete, please use add-handler! instead"
    );
  });
  define_libfunc("add-handler!", 3, 3, function (ar, intp) {
    var selector = ar[0],
      evtype = ar[1],
      proc = ar[2];
    var intp2 = new Interpreter(intp);
    var handler = function (event) {
      return clone(intp2).invoke_closure(proc, [event]);
    };
    $$1(selector).on(evtype, handler);
    return handler;
  });
  define_libfunc("remove-handler!", 3, 3, function (ar, intp) {
    var selector = ar[0],
      evtype = ar[1],
      handler = ar[2];
    $$1(selector).off(evtype, handler);
    return undef;
  });
  define_libfunc("wait-for", 2, 2, function (ar) {
    var selector = ar[0],
      evtype = ar[1];
    var elem = $$1(selector);
    elem.biwascheme_wait_for = elem.biwascheme_wait_for || {};

    var prev_handler = elem.biwascheme_wait_for[evtype];
    if (prev_handler) {
      // Maybe a wait-for is called more than once,
      // and previous handler is not consumed.
      elem.off(evtype, prev_handler);
    }

    return new Pause(function (pause) {
      var handler = function (event) {
        elem.biwascheme_wait_for[evtype] = undefined;
        elem.off(evtype, handler);
        return pause.resume(event);
      };

      elem.biwascheme_wait_for[evtype] = handler;
      elem.on(evtype, handler);
    });
  });

  //
  // dom
  //
  define_libfunc("domelem", 1, null, function (ar) {
    throw new Error("obsolete");
  });
  define_libfunc("dom-remove-children!", 1, 1, function (ar) {
    Console.puts(
      "warning: dom-remove-children! is obsolete. use element-empty! instead"
    );
    $$1(ar[0]).empty();
    return undef;
  });
  define_libfunc("dom-create-element", 1, 1, function (ar) {
    throw new Error("obsolete");
  });
  define_libfunc("element-append-child!", 2, 2, function (ar) {
    return $$1(ar[0]).append(ar[1]);
  });
  define_libfunc("dom-remove-child!", 2, 2, function (ar) {
    throw new Error("obsolete");
  });
  //  define_libfunc("dom-get-attribute", 2, 2, function(ar){
  //  });
  //  define_libfunc("dom-remove-child!", 2, 2, function(ar){
  //  });

  //
  // communication to server
  //
  define_libfunc("http-request", 1, 1, function (ar) {
    var path = ar[0];
    assert_string(path);

    return new Pause(function (pause) {
      $$1.get(
        path,
        function (data) {
          pause.resume(data);
        },
        "text"
      );
    });
  });
  // (http-post "/foo" '(("x" . 1) ("y" . 2)))
  define_libfunc("http-post", 2, 2, function (ar) {
    var path = ar[0];
    assert_string(path);
    var alist = ar[1];
    assert_list(alist);
    var h = alist_to_js_obj(alist);

    return new Pause(function (pause) {
      $$1.post(
        path,
        h,
        function (data) {
          pause.resume(data);
        },
        "text"
      );
    });
  });

  const jsonp_receiver = [];
  define_libfunc("receive-jsonp", 1, 1, function (ar) {
    var url = ar[0];
    assert_string(url);

    var receives = jsonp_receiver;
    for (var i = 0; i < receives.length; i++) if (receives[i] === null) break;
    var receiver_id = i;
    url += "?callback=BiwaScheme.jsonp_receiver[" + receiver_id + "]";

    return new Pause(function (pause) {
      receives[receiver_id] = function (data) {
        pause.resume(data);
        receives[receiver_id] = null;
      };
      var script = $$1("<script/>", { src: url });
      $$1("body").append(script);
    });
  });

  //
  // dialog, debug
  //
  define_libfunc("alert", 1, 1, function (ar) {
    alert(ar[0]);
    return undef;
  });
  define_libfunc("confirm", 1, 1, function (ar) {
    return confirm(ar[0]);
  });

  //
  // Dumper - graphical state dumper
  //

  const Dumper = Class.create({
    initialize: function (dumparea) {
      this.dumparea = dumparea || $("#dumparea")[0] || null;
      this.reset();
    },

    reset: function () {
      if (this.dumparea) {
        // Note: this is for repl.html (needs refactoring..)
        $(this.dumparea).empty();
      }
      this.n_folds = 0;
      this.closures = [];
      this.n_dumps = 0;
      this.cur = -1;
      this.is_folded = true;
    },

    is_opc: function (obj) {
      return obj instanceof Array && typeof obj[0] == "string";
    },

    dump_pad: "&nbsp;&nbsp;&nbsp;",
    dump_opc: function (obj, level, nested) {
      var s = "";
      var pad1 = "",
        pad2 = "";
      var level = level || 0;
      var nested = nested || false;
      times(
        level,
        bind(function () {
          pad1 += this.dump_pad;
        }, this)
      );
      times(
        level + 1,
        bind(function () {
          pad2 += this.dump_pad;
        }, this)
      );

      s += pad1 + '[<span class="dump_opecode">' + obj[0] + "</span>";
      var i = 1;
      while (!(obj[i] instanceof Array) && i < obj.length) {
        if (obj[0] == "constant")
          s +=
            "&nbsp;<span class='dump_constant'>" +
            this.dump_obj(obj[i]) +
            "</span>";
        else s += "&nbsp;" + this.dump_obj(obj[i]);
        i++;
      }
      if (i < obj.length) s += "<br>\n";
      for (; i < obj.length; i++) {
        if (this.is_opc(obj[i])) {
          s += this.dump_opc(
            obj[i],
            i == obj.length - 1 ? level : level + 1,
            true
          );
        } else {
          s += i == obj.length - 1 ? pad1 : pad2;
          s += this.dump_obj(obj[i]);
        }
        if (i != obj.length - 1) s += "<br>\n";
      }
      s += "]";
      return nested ? s : this.add_fold(s);
    },

    fold_limit: 20,
    add_fold: function (s) {
      var lines = s.split(/<br>/gim);

      if (lines.length > this.fold_limit) {
        var fold_btn =
          " <span style='text-decoration:underline; color:blue; cursor:pointer;'" +
          "onclick='BiwaScheme.Dumper.toggle_fold(" +
          this.n_folds +
          ")'>more</span>";
        var fold_start =
          "<div style='display:none' class='fold" + this.n_folds + "'>";
        var fold_end = "</div>";
        this.n_folds++;
        return [
          lines.slice(0, this.fold_limit).join("<br>"),
          fold_btn,
          fold_start,
          lines.slice(this.fold_limit).join("<br>"),
          fold_end,
        ].join("");
      } else {
        return s;
      }
    },

    stack_max_len: 80,
    dump_stack: function (stk, size) {
      if (stk === null || stk === undefined) return inspect(stk);
      var s = "<table>";

      // show the 'physical' stack top
      if (stk.length == 0) {
        s += "<tr><td class='dump_dead'>(stack is empty)</td></tr>";
      } else if (size < stk.length) {
        var l = stk.length - 1;
        s +=
          "<tr><td class='dump_dead'>[" +
          l +
          "]</td>" +
          "<td class='dump_dead'>" +
          truncate(this.dump_obj(stk[l]), this.stack_max_len) +
          "</td></tr>";
      }

      // show the element in the stack
      for (var i = size - 1; i >= 0; i--) {
        s +=
          "<tr><td class='dump_stknum'>[" +
          i +
          "]</td>" +
          "<td>" +
          truncate(this.dump_obj(stk[i]), this.stack_max_len) +
          "</td></tr>";
      }
      return s + "</table>";
    },

    dump_object: function (obj) {
      var a = [];
      for (var k in obj) {
        //if(this.prototype[k]) continue;
        a.push(k.toString()); //+" => "+this[k].toString() );
      }
      return "#<Object{" + a.join(",") + "}>";
    },

    dump_closure: function (cls) {
      if (!cls) return "**BROKEN**";
      if (cls.length == 0) return "[]";

      var cls_num = null;
      for (var i = 0; i < this.closures.length; i++) {
        if (this.closures[i] == cls) cls_num = i;
      }
      if (cls_num == null) {
        cls_num = this.closures.length;
        this.closures.push(cls);
      }

      var c = clone(cls);
      var body = c.shift && c.shift();
      return [
        "c",
        cls_num,
        " <span class='dump_closure'>free vars :</span> ",
        this.dump_obj(c),
        " <span class='dump_closure'>body :</span> ",
        truncate(this.dump_obj(body), 100),
      ].join("");
    },

    dump_obj: function (obj) {
      if (obj && typeof obj.to_html == "function") return obj.to_html();
      else {
        var s = write_ss(obj, true); //true=Array mode
        if (s == "[object Object]") s = this.dump_object(obj);
        return escape(s);
      }
    },

    dump: function (obj) {
      var s = "";
      if (obj instanceof Object) {
        s += "<table>";

        // header
        s +=
          "<tr><td colspan='4'>" +
          "<a href='#' class='header'>" +
          "#" +
          this.n_dumps +
          "</a></td></tr>";

        // registers
        each(
          keys(obj),
          bind(function (key) {
            var value = obj[key];
            if (key != "x" && key != "stack") {
              value =
                key == "c" ? this.dump_closure(value) : this.dump_obj(value);
              s +=
                "<tr><td>" +
                key +
                ": </td>" +
                "<td colspan='3'>" +
                value +
                "</td></tr>";
            }
          }, this)
        );
        s +=
          "<tr><td>x:</td><td>" +
          (this.is_opc(obj["x"])
            ? this.dump_opc(obj["x"])
            : this.dump_obj(obj["x"])) +
          "</td>";

        // stack
        s +=
          "<td style='border-left: 1px solid black'>stack:</td><td>" +
          this.dump_stack(obj["stack"], obj["s"]) +
          "</td></tr>";
        s += "</table>";
      } else {
        s = escape(inspect(obj)) + "<br>\n";
      }
      var dumpitem = $("<div/>", { class: "dump" + this.n_dumps });
      dumpitem.html(s);
      $(this.dumparea).append(dumpitem);
      bind(function (n) {
        $(".header", this.dump_el(this.n_dumps)).click(
          bind(function () {
            this.dump_move_to(n);
            this.dump_fold();
          }, this)
        );
      }, this)(this.n_dumps);
      dumpitem.hide();
      this.n_dumps++;
    },

    //
    // UI
    //
    dump_el: function (n) {
      return $(".dump" + n, this.dumparea);
    },
    dump_move_to: function (n) {
      if (n < 0) n = this.n_dumps + n;

      if (0 <= n && n <= this.n_dumps) {
        this.dump_el(this.cur).hide();
        this.cur = n;
        this.dump_el(this.cur).show();
      }
    },

    dump_move: function (dir) {
      if (0 <= this.cur && this.cur < this.n_dumps)
        this.dump_el(this.cur).hide();

      if (0 <= this.cur + dir && this.cur + dir < this.n_dumps) this.cur += dir;

      this.dump_el(this.cur).show();
    },

    dump_fold: function () {
      for (var i = 0; i < this.n_dumps; i++)
        if (i != this.cur) this.dump_el(i).hide();

      this.is_folded = true;
    },

    dump_unfold: function () {
      for (var i = 0; i < this.n_dumps; i++) this.dump_el(i).show();

      this.is_folded = false;
    },

    dump_toggle_fold: function () {
      if (this.is_folded) this.dump_unfold();
      else this.dump_fold();
    },
  });

  Dumper.toggle_fold = function (n) {
    $(".fold" + n, this.dumparea).toggle();
  };

  //
  // release_initializer.js - read user's program and eval it (if it exists)
  //

  const execute_user_program = function () {
    const dumper = null;
    const debug_area = document.querySelector("#biwascheme-debugger");
    if (debug_area) {
      dumper = new Dumper(debug_area);
    }

    // Error handler (show message to console div)
    const onError = function (e, state) {
      BiwaScheme.Port.current_error.put_string(e.message + "\n");
      if (dumper) {
        dumper.dump(state);
        dumper.dump_move(1);
      } else if (typeof console !== "undefined" && console.error) {
        console.error(e.message);
      } else {
        throw e;
      }
    };

    const run = function (script) {
      const intp = new BiwaScheme.Interpreter(onError);
      try {
        intp.evaluate(script, function () {});
      } catch (e) {
        onError(e);
      }
    };

    // Start user's program (old style)
    let script = "";
    for (const s of document.querySelectorAll("script[src$='biwascheme.js']")) {
      script += s.innerHTML;
    }
    for (const s of document.querySelectorAll(
      "script[src$='biwascheme-min.js']"
    )) {
      script += s.innerHTML;
    }

    if (script.length > 0) run(script);

    // Start user's program (new style)
    window.addEventListener("DOMContentLoaded", function () {
      for (const s of document.querySelectorAll(
        "script[type='text/biwascheme']"
      )) {
        run(s.innerHTML);
      }
    });
  };

  BiwaScheme$1.on_node = false;
  BiwaScheme$1.Console = Console;
  BiwaScheme$1.Port.current_input = current_input;
  BiwaScheme$1.Port.current_output = current_output;
  BiwaScheme$1.Port.current_error = current_error;
  BiwaScheme$1.jsonp_receiver = jsonp_receiver;
  BiwaScheme$1.Dumper = Dumper;

  // TODO: ideally this should just be `window.BiwaScheme = BiwaScheme` but it will break test/spec.html (grep with `register_tests`)
  window.BiwaScheme = window.BiwaScheme || {};
  Object.assign(window.BiwaScheme, BiwaScheme$1);
  execute_user_program();

  return BiwaScheme$1;
})();
