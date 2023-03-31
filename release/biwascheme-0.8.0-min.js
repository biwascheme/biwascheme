/*
 * BiwaScheme 0.7.5 - R6RS/R7RS Scheme in JavaScript
 *
 * Copyright (c) 2007-2023 Yutaka HARA (http://www.biwascheme.org/)
 * Licensed under the MIT license.
 */
/*!
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
!(function (e, t) {
  "use strict";
  "object" == typeof module && "object" == typeof module.exports
    ? (module.exports = e.document
        ? t(e, !0)
        : function (e) {
            if (!e.document)
              throw new Error("jQuery requires a window with a document");
            return t(e);
          })
    : t(e);
})("undefined" != typeof window ? window : this, function (e, t) {
  "use strict";
  var n = [],
    r = Object.getPrototypeOf,
    i = n.slice,
    a = n.flat
      ? function (e) {
          return n.flat.call(e);
        }
      : function (e) {
          return n.concat.apply([], e);
        },
    s = n.push,
    o = n.indexOf,
    u = {},
    c = u.toString,
    l = u.hasOwnProperty,
    f = l.toString,
    d = f.call(Object),
    h = {},
    p = function (e) {
      return (
        "function" == typeof e &&
        "number" != typeof e.nodeType &&
        "function" != typeof e.item
      );
    },
    m = function (e) {
      return null != e && e === e.window;
    },
    _ = e.document,
    b = { type: !0, src: !0, nonce: !0, noModule: !0 };
  function g(e, t, n) {
    var r,
      i,
      a = (n = n || _).createElement("script");
    if (((a.text = e), t))
      for (r in b)
        (i = t[r] || (t.getAttribute && t.getAttribute(r))) &&
          a.setAttribute(r, i);
    n.head.appendChild(a).parentNode.removeChild(a);
  }
  function y(e) {
    return null == e
      ? e + ""
      : "object" == typeof e || "function" == typeof e
      ? u[c.call(e)] || "object"
      : typeof e;
  }
  var v = "3.6.0",
    w = function (e, t) {
      return new w.fn.init(e, t);
    };
  function x(e) {
    var t = !!e && "length" in e && e.length,
      n = y(e);
    return (
      !p(e) &&
      !m(e) &&
      ("array" === n ||
        0 === t ||
        ("number" == typeof t && t > 0 && t - 1 in e))
    );
  }
  (w.fn = w.prototype = {
    jquery: v,
    constructor: w,
    length: 0,
    toArray: function () {
      return i.call(this);
    },
    get: function (e) {
      return null == e ? i.call(this) : e < 0 ? this[e + this.length] : this[e];
    },
    pushStack: function (e) {
      var t = w.merge(this.constructor(), e);
      return (t.prevObject = this), t;
    },
    each: function (e) {
      return w.each(this, e);
    },
    map: function (e) {
      return this.pushStack(
        w.map(this, function (t, n) {
          return e.call(t, n, t);
        })
      );
    },
    slice: function () {
      return this.pushStack(i.apply(this, arguments));
    },
    first: function () {
      return this.eq(0);
    },
    last: function () {
      return this.eq(-1);
    },
    even: function () {
      return this.pushStack(
        w.grep(this, function (e, t) {
          return (t + 1) % 2;
        })
      );
    },
    odd: function () {
      return this.pushStack(
        w.grep(this, function (e, t) {
          return t % 2;
        })
      );
    },
    eq: function (e) {
      var t = this.length,
        n = +e + (e < 0 ? t : 0);
      return this.pushStack(n >= 0 && n < t ? [this[n]] : []);
    },
    end: function () {
      return this.prevObject || this.constructor();
    },
    push: s,
    sort: n.sort,
    splice: n.splice,
  }),
    (w.extend = w.fn.extend = function () {
      var e,
        t,
        n,
        r,
        i,
        a,
        s = arguments[0] || {},
        o = 1,
        u = arguments.length,
        c = !1;
      for (
        "boolean" == typeof s && ((c = s), (s = arguments[o] || {}), o++),
          "object" == typeof s || p(s) || (s = {}),
          o === u && ((s = this), o--);
        o < u;
        o++
      )
        if (null != (e = arguments[o]))
          for (t in e)
            (r = e[t]),
              "__proto__" !== t &&
                s !== r &&
                (c && r && (w.isPlainObject(r) || (i = Array.isArray(r)))
                  ? ((n = s[t]),
                    (a =
                      i && !Array.isArray(n)
                        ? []
                        : i || w.isPlainObject(n)
                        ? n
                        : {}),
                    (i = !1),
                    (s[t] = w.extend(c, a, r)))
                  : void 0 !== r && (s[t] = r));
      return s;
    }),
    w.extend({
      expando: "jQuery" + (v + Math.random()).replace(/\D/g, ""),
      isReady: !0,
      error: function (e) {
        throw new Error(e);
      },
      noop: function () {},
      isPlainObject: function (e) {
        var t, n;
        return (
          !(!e || "[object Object]" !== c.call(e)) &&
          (!(t = r(e)) ||
            ("function" ==
              typeof (n = l.call(t, "constructor") && t.constructor) &&
              f.call(n) === d))
        );
      },
      isEmptyObject: function (e) {
        var t;
        for (t in e) return !1;
        return !0;
      },
      globalEval: function (e, t, n) {
        g(e, { nonce: t && t.nonce }, n);
      },
      each: function (e, t) {
        var n,
          r = 0;
        if (x(e))
          for (n = e.length; r < n && !1 !== t.call(e[r], r, e[r]); r++);
        else for (r in e) if (!1 === t.call(e[r], r, e[r])) break;
        return e;
      },
      makeArray: function (e, t) {
        var n = t || [];
        return (
          null != e &&
            (x(Object(e))
              ? w.merge(n, "string" == typeof e ? [e] : e)
              : s.call(n, e)),
          n
        );
      },
      inArray: function (e, t, n) {
        return null == t ? -1 : o.call(t, e, n);
      },
      merge: function (e, t) {
        for (var n = +t.length, r = 0, i = e.length; r < n; r++) e[i++] = t[r];
        return (e.length = i), e;
      },
      grep: function (e, t, n) {
        for (var r = [], i = 0, a = e.length, s = !n; i < a; i++)
          !t(e[i], i) !== s && r.push(e[i]);
        return r;
      },
      map: function (e, t, n) {
        var r,
          i,
          s = 0,
          o = [];
        if (x(e))
          for (r = e.length; s < r; s++)
            null != (i = t(e[s], s, n)) && o.push(i);
        else for (s in e) null != (i = t(e[s], s, n)) && o.push(i);
        return a(o);
      },
      guid: 1,
      support: h,
    }),
    "function" == typeof Symbol && (w.fn[Symbol.iterator] = n[Symbol.iterator]),
    w.each(
      "Boolean Number String Function Array Date RegExp Object Error Symbol".split(
        " "
      ),
      function (e, t) {
        u["[object " + t + "]"] = t.toLowerCase();
      }
    );
  var S =
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
    (function (e) {
      var t,
        n,
        r,
        i,
        a,
        s,
        o,
        u,
        c,
        l,
        f,
        d,
        h,
        p,
        m,
        _,
        b,
        g,
        y,
        v = "sizzle" + 1 * new Date(),
        w = e.document,
        x = 0,
        S = 0,
        C = ue(),
        E = ue(),
        P = ue(),
        k = ue(),
        T = function (e, t) {
          return e === t && (f = !0), 0;
        },
        j = {}.hasOwnProperty,
        B = [],
        q = B.pop,
        D = B.push,
        A = B.push,
        $ = B.slice,
        N = function (e, t) {
          for (var n = 0, r = e.length; n < r; n++) if (e[n] === t) return n;
          return -1;
        },
        L =
          "checked|selected|async|autofocus|autoplay|controls|defer|disabled|hidden|ismap|loop|multiple|open|readonly|required|scoped",
        R = "[\\x20\\t\\r\\n\\f]",
        O =
          "(?:\\\\[\\da-fA-F]{1,6}[\\x20\\t\\r\\n\\f]?|\\\\[^\\r\\n\\f]|[\\w-]|[^\0-\\x7f])+",
        M =
          "\\[[\\x20\\t\\r\\n\\f]*(" +
          O +
          ")(?:" +
          R +
          "*([*^$|!~]?=)" +
          R +
          "*(?:'((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\"|(" +
          O +
          "))|)" +
          R +
          "*\\]",
        I =
          ":(" +
          O +
          ")(?:\\((('((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\")|((?:\\\\.|[^\\\\()[\\]]|" +
          M +
          ")*)|.*)\\)|)",
        H = new RegExp(R + "+", "g"),
        F = new RegExp(
          "^[\\x20\\t\\r\\n\\f]+|((?:^|[^\\\\])(?:\\\\.)*)[\\x20\\t\\r\\n\\f]+$",
          "g"
        ),
        W = new RegExp("^[\\x20\\t\\r\\n\\f]*,[\\x20\\t\\r\\n\\f]*"),
        z = new RegExp(
          "^[\\x20\\t\\r\\n\\f]*([>+~]|[\\x20\\t\\r\\n\\f])[\\x20\\t\\r\\n\\f]*"
        ),
        U = new RegExp(R + "|>"),
        V = new RegExp(I),
        X = new RegExp("^" + O + "$"),
        G = {
          ID: new RegExp("^#(" + O + ")"),
          CLASS: new RegExp("^\\.(" + O + ")"),
          TAG: new RegExp("^(" + O + "|[*])"),
          ATTR: new RegExp("^" + M),
          PSEUDO: new RegExp("^" + I),
          CHILD: new RegExp(
            "^:(only|first|last|nth|nth-last)-(child|of-type)(?:\\([\\x20\\t\\r\\n\\f]*(even|odd|(([+-]|)(\\d*)n|)[\\x20\\t\\r\\n\\f]*(?:([+-]|)[\\x20\\t\\r\\n\\f]*(\\d+)|))[\\x20\\t\\r\\n\\f]*\\)|)",
            "i"
          ),
          bool: new RegExp("^(?:" + L + ")$", "i"),
          needsContext: new RegExp(
            "^[\\x20\\t\\r\\n\\f]*[>+~]|:(even|odd|eq|gt|lt|nth|first|last)(?:\\([\\x20\\t\\r\\n\\f]*((?:-\\d)?\\d*)[\\x20\\t\\r\\n\\f]*\\)|)(?=[^-]|$)",
            "i"
          ),
        },
        Y = /HTML$/i,
        Q = /^(?:input|select|textarea|button)$/i,
        J = /^h\d$/i,
        K = /^[^{]+\{\s*\[native \w/,
        Z = /^(?:#([\w-]+)|(\w+)|\.([\w-]+))$/,
        ee = /[+~]/,
        te = new RegExp(
          "\\\\[\\da-fA-F]{1,6}[\\x20\\t\\r\\n\\f]?|\\\\([^\\r\\n\\f])",
          "g"
        ),
        ne = function (e, t) {
          var n = "0x" + e.slice(1) - 65536;
          return (
            t ||
            (n < 0
              ? String.fromCharCode(n + 65536)
              : String.fromCharCode((n >> 10) | 55296, (1023 & n) | 56320))
          );
        },
        re = /([\0-\x1f\x7f]|^-?\d)|^-$|[^\0-\x1f\x7f-\uFFFF\w-]/g,
        ie = function (e, t) {
          return t
            ? "\0" === e
              ? "�"
              : e.slice(0, -1) +
                "\\" +
                e.charCodeAt(e.length - 1).toString(16) +
                " "
            : "\\" + e;
        },
        ae = function () {
          d();
        },
        se = ve(
          function (e) {
            return !0 === e.disabled && "fieldset" === e.nodeName.toLowerCase();
          },
          { dir: "parentNode", next: "legend" }
        );
      try {
        A.apply((B = $.call(w.childNodes)), w.childNodes),
          B[w.childNodes.length].nodeType;
      } catch (e) {
        A = {
          apply: B.length
            ? function (e, t) {
                D.apply(e, $.call(t));
              }
            : function (e, t) {
                for (var n = e.length, r = 0; (e[n++] = t[r++]); );
                e.length = n - 1;
              },
        };
      }
      function oe(e, t, r, i) {
        var a,
          o,
          c,
          l,
          f,
          p,
          b,
          g = t && t.ownerDocument,
          w = t ? t.nodeType : 9;
        if (
          ((r = r || []),
          "string" != typeof e || !e || (1 !== w && 9 !== w && 11 !== w))
        )
          return r;
        if (!i && (d(t), (t = t || h), m)) {
          if (11 !== w && (f = Z.exec(e)))
            if ((a = f[1])) {
              if (9 === w) {
                if (!(c = t.getElementById(a))) return r;
                if (c.id === a) return r.push(c), r;
              } else if (
                g &&
                (c = g.getElementById(a)) &&
                y(t, c) &&
                c.id === a
              )
                return r.push(c), r;
            } else {
              if (f[2]) return A.apply(r, t.getElementsByTagName(e)), r;
              if (
                (a = f[3]) &&
                n.getElementsByClassName &&
                t.getElementsByClassName
              )
                return A.apply(r, t.getElementsByClassName(a)), r;
            }
          if (
            n.qsa &&
            !k[e + " "] &&
            (!_ || !_.test(e)) &&
            (1 !== w || "object" !== t.nodeName.toLowerCase())
          ) {
            if (((b = e), (g = t), 1 === w && (U.test(e) || z.test(e)))) {
              for (
                ((g = (ee.test(e) && be(t.parentNode)) || t) === t &&
                  n.scope) ||
                  ((l = t.getAttribute("id"))
                    ? (l = l.replace(re, ie))
                    : t.setAttribute("id", (l = v))),
                  o = (p = s(e)).length;
                o--;

              )
                p[o] = (l ? "#" + l : ":scope") + " " + ye(p[o]);
              b = p.join(",");
            }
            try {
              return A.apply(r, g.querySelectorAll(b)), r;
            } catch (t) {
              k(e, !0);
            } finally {
              l === v && t.removeAttribute("id");
            }
          }
        }
        return u(e.replace(F, "$1"), t, r, i);
      }
      function ue() {
        var e = [];
        return function t(n, i) {
          return (
            e.push(n + " ") > r.cacheLength && delete t[e.shift()],
            (t[n + " "] = i)
          );
        };
      }
      function ce(e) {
        return (e[v] = !0), e;
      }
      function le(e) {
        var t = h.createElement("fieldset");
        try {
          return !!e(t);
        } catch (e) {
          return !1;
        } finally {
          t.parentNode && t.parentNode.removeChild(t), (t = null);
        }
      }
      function fe(e, t) {
        for (var n = e.split("|"), i = n.length; i--; ) r.attrHandle[n[i]] = t;
      }
      function de(e, t) {
        var n = t && e,
          r =
            n &&
            1 === e.nodeType &&
            1 === t.nodeType &&
            e.sourceIndex - t.sourceIndex;
        if (r) return r;
        if (n) for (; (n = n.nextSibling); ) if (n === t) return -1;
        return e ? 1 : -1;
      }
      function he(e) {
        return function (t) {
          return "input" === t.nodeName.toLowerCase() && t.type === e;
        };
      }
      function pe(e) {
        return function (t) {
          var n = t.nodeName.toLowerCase();
          return ("input" === n || "button" === n) && t.type === e;
        };
      }
      function me(e) {
        return function (t) {
          return "form" in t
            ? t.parentNode && !1 === t.disabled
              ? "label" in t
                ? "label" in t.parentNode
                  ? t.parentNode.disabled === e
                  : t.disabled === e
                : t.isDisabled === e || (t.isDisabled !== !e && se(t) === e)
              : t.disabled === e
            : "label" in t && t.disabled === e;
        };
      }
      function _e(e) {
        return ce(function (t) {
          return (
            (t = +t),
            ce(function (n, r) {
              for (var i, a = e([], n.length, t), s = a.length; s--; )
                n[(i = a[s])] && (n[i] = !(r[i] = n[i]));
            })
          );
        });
      }
      function be(e) {
        return e && void 0 !== e.getElementsByTagName && e;
      }
      for (t in ((n = oe.support = {}),
      (a = oe.isXML = function (e) {
        var t = e && e.namespaceURI,
          n = e && (e.ownerDocument || e).documentElement;
        return !Y.test(t || (n && n.nodeName) || "HTML");
      }),
      (d = oe.setDocument = function (e) {
        var t,
          i,
          s = e ? e.ownerDocument || e : w;
        return s != h && 9 === s.nodeType && s.documentElement
          ? ((p = (h = s).documentElement),
            (m = !a(h)),
            w != h &&
              (i = h.defaultView) &&
              i.top !== i &&
              (i.addEventListener
                ? i.addEventListener("unload", ae, !1)
                : i.attachEvent && i.attachEvent("onunload", ae)),
            (n.scope = le(function (e) {
              return (
                p.appendChild(e).appendChild(h.createElement("div")),
                void 0 !== e.querySelectorAll &&
                  !e.querySelectorAll(":scope fieldset div").length
              );
            })),
            (n.attributes = le(function (e) {
              return (e.className = "i"), !e.getAttribute("className");
            })),
            (n.getElementsByTagName = le(function (e) {
              return (
                e.appendChild(h.createComment("")),
                !e.getElementsByTagName("*").length
              );
            })),
            (n.getElementsByClassName = K.test(h.getElementsByClassName)),
            (n.getById = le(function (e) {
              return (
                (p.appendChild(e).id = v),
                !h.getElementsByName || !h.getElementsByName(v).length
              );
            })),
            n.getById
              ? ((r.filter.ID = function (e) {
                  var t = e.replace(te, ne);
                  return function (e) {
                    return e.getAttribute("id") === t;
                  };
                }),
                (r.find.ID = function (e, t) {
                  if (void 0 !== t.getElementById && m) {
                    var n = t.getElementById(e);
                    return n ? [n] : [];
                  }
                }))
              : ((r.filter.ID = function (e) {
                  var t = e.replace(te, ne);
                  return function (e) {
                    var n =
                      void 0 !== e.getAttributeNode && e.getAttributeNode("id");
                    return n && n.value === t;
                  };
                }),
                (r.find.ID = function (e, t) {
                  if (void 0 !== t.getElementById && m) {
                    var n,
                      r,
                      i,
                      a = t.getElementById(e);
                    if (a) {
                      if ((n = a.getAttributeNode("id")) && n.value === e)
                        return [a];
                      for (i = t.getElementsByName(e), r = 0; (a = i[r++]); )
                        if ((n = a.getAttributeNode("id")) && n.value === e)
                          return [a];
                    }
                    return [];
                  }
                })),
            (r.find.TAG = n.getElementsByTagName
              ? function (e, t) {
                  return void 0 !== t.getElementsByTagName
                    ? t.getElementsByTagName(e)
                    : n.qsa
                    ? t.querySelectorAll(e)
                    : void 0;
                }
              : function (e, t) {
                  var n,
                    r = [],
                    i = 0,
                    a = t.getElementsByTagName(e);
                  if ("*" === e) {
                    for (; (n = a[i++]); ) 1 === n.nodeType && r.push(n);
                    return r;
                  }
                  return a;
                }),
            (r.find.CLASS =
              n.getElementsByClassName &&
              function (e, t) {
                if (void 0 !== t.getElementsByClassName && m)
                  return t.getElementsByClassName(e);
              }),
            (b = []),
            (_ = []),
            (n.qsa = K.test(h.querySelectorAll)) &&
              (le(function (e) {
                var t;
                (p.appendChild(e).innerHTML =
                  "<a id='" +
                  v +
                  "'></a><select id='" +
                  v +
                  "-\r\\' msallowcapture=''><option selected=''></option></select>"),
                  e.querySelectorAll("[msallowcapture^='']").length &&
                    _.push("[*^$]=[\\x20\\t\\r\\n\\f]*(?:''|\"\")"),
                  e.querySelectorAll("[selected]").length ||
                    _.push("\\[[\\x20\\t\\r\\n\\f]*(?:value|" + L + ")"),
                  e.querySelectorAll("[id~=" + v + "-]").length || _.push("~="),
                  (t = h.createElement("input")).setAttribute("name", ""),
                  e.appendChild(t),
                  e.querySelectorAll("[name='']").length ||
                    _.push(
                      "\\[[\\x20\\t\\r\\n\\f]*name[\\x20\\t\\r\\n\\f]*=[\\x20\\t\\r\\n\\f]*(?:''|\"\")"
                    ),
                  e.querySelectorAll(":checked").length || _.push(":checked"),
                  e.querySelectorAll("a#" + v + "+*").length ||
                    _.push(".#.+[+~]"),
                  e.querySelectorAll("\\\f"),
                  _.push("[\\r\\n\\f]");
              }),
              le(function (e) {
                e.innerHTML =
                  "<a href='' disabled='disabled'></a><select disabled='disabled'><option/></select>";
                var t = h.createElement("input");
                t.setAttribute("type", "hidden"),
                  e.appendChild(t).setAttribute("name", "D"),
                  e.querySelectorAll("[name=d]").length &&
                    _.push("name[\\x20\\t\\r\\n\\f]*[*^$|!~]?="),
                  2 !== e.querySelectorAll(":enabled").length &&
                    _.push(":enabled", ":disabled"),
                  (p.appendChild(e).disabled = !0),
                  2 !== e.querySelectorAll(":disabled").length &&
                    _.push(":enabled", ":disabled"),
                  e.querySelectorAll("*,:x"),
                  _.push(",.*:");
              })),
            (n.matchesSelector = K.test(
              (g =
                p.matches ||
                p.webkitMatchesSelector ||
                p.mozMatchesSelector ||
                p.oMatchesSelector ||
                p.msMatchesSelector)
            )) &&
              le(function (e) {
                (n.disconnectedMatch = g.call(e, "*")),
                  g.call(e, "[s!='']:x"),
                  b.push("!=", I);
              }),
            (_ = _.length && new RegExp(_.join("|"))),
            (b = b.length && new RegExp(b.join("|"))),
            (t = K.test(p.compareDocumentPosition)),
            (y =
              t || K.test(p.contains)
                ? function (e, t) {
                    var n = 9 === e.nodeType ? e.documentElement : e,
                      r = t && t.parentNode;
                    return (
                      e === r ||
                      !(
                        !r ||
                        1 !== r.nodeType ||
                        !(n.contains
                          ? n.contains(r)
                          : e.compareDocumentPosition &&
                            16 & e.compareDocumentPosition(r))
                      )
                    );
                  }
                : function (e, t) {
                    if (t) for (; (t = t.parentNode); ) if (t === e) return !0;
                    return !1;
                  }),
            (T = t
              ? function (e, t) {
                  if (e === t) return (f = !0), 0;
                  var r =
                    !e.compareDocumentPosition - !t.compareDocumentPosition;
                  return (
                    r ||
                    (1 &
                      (r =
                        (e.ownerDocument || e) == (t.ownerDocument || t)
                          ? e.compareDocumentPosition(t)
                          : 1) ||
                    (!n.sortDetached && t.compareDocumentPosition(e) === r)
                      ? e == h || (e.ownerDocument == w && y(w, e))
                        ? -1
                        : t == h || (t.ownerDocument == w && y(w, t))
                        ? 1
                        : l
                        ? N(l, e) - N(l, t)
                        : 0
                      : 4 & r
                      ? -1
                      : 1)
                  );
                }
              : function (e, t) {
                  if (e === t) return (f = !0), 0;
                  var n,
                    r = 0,
                    i = e.parentNode,
                    a = t.parentNode,
                    s = [e],
                    o = [t];
                  if (!i || !a)
                    return e == h
                      ? -1
                      : t == h
                      ? 1
                      : i
                      ? -1
                      : a
                      ? 1
                      : l
                      ? N(l, e) - N(l, t)
                      : 0;
                  if (i === a) return de(e, t);
                  for (n = e; (n = n.parentNode); ) s.unshift(n);
                  for (n = t; (n = n.parentNode); ) o.unshift(n);
                  for (; s[r] === o[r]; ) r++;
                  return r
                    ? de(s[r], o[r])
                    : s[r] == w
                    ? -1
                    : o[r] == w
                    ? 1
                    : 0;
                }),
            h)
          : h;
      }),
      (oe.matches = function (e, t) {
        return oe(e, null, null, t);
      }),
      (oe.matchesSelector = function (e, t) {
        if (
          (d(e),
          n.matchesSelector &&
            m &&
            !k[t + " "] &&
            (!b || !b.test(t)) &&
            (!_ || !_.test(t)))
        )
          try {
            var r = g.call(e, t);
            if (
              r ||
              n.disconnectedMatch ||
              (e.document && 11 !== e.document.nodeType)
            )
              return r;
          } catch (e) {
            k(t, !0);
          }
        return oe(t, h, null, [e]).length > 0;
      }),
      (oe.contains = function (e, t) {
        return (e.ownerDocument || e) != h && d(e), y(e, t);
      }),
      (oe.attr = function (e, t) {
        (e.ownerDocument || e) != h && d(e);
        var i = r.attrHandle[t.toLowerCase()],
          a = i && j.call(r.attrHandle, t.toLowerCase()) ? i(e, t, !m) : void 0;
        return void 0 !== a
          ? a
          : n.attributes || !m
          ? e.getAttribute(t)
          : (a = e.getAttributeNode(t)) && a.specified
          ? a.value
          : null;
      }),
      (oe.escape = function (e) {
        return (e + "").replace(re, ie);
      }),
      (oe.error = function (e) {
        throw new Error("Syntax error, unrecognized expression: " + e);
      }),
      (oe.uniqueSort = function (e) {
        var t,
          r = [],
          i = 0,
          a = 0;
        if (
          ((f = !n.detectDuplicates),
          (l = !n.sortStable && e.slice(0)),
          e.sort(T),
          f)
        ) {
          for (; (t = e[a++]); ) t === e[a] && (i = r.push(a));
          for (; i--; ) e.splice(r[i], 1);
        }
        return (l = null), e;
      }),
      (i = oe.getText = function (e) {
        var t,
          n = "",
          r = 0,
          a = e.nodeType;
        if (a) {
          if (1 === a || 9 === a || 11 === a) {
            if ("string" == typeof e.textContent) return e.textContent;
            for (e = e.firstChild; e; e = e.nextSibling) n += i(e);
          } else if (3 === a || 4 === a) return e.nodeValue;
        } else for (; (t = e[r++]); ) n += i(t);
        return n;
      }),
      (r = oe.selectors = {
        cacheLength: 50,
        createPseudo: ce,
        match: G,
        attrHandle: {},
        find: {},
        relative: {
          ">": { dir: "parentNode", first: !0 },
          " ": { dir: "parentNode" },
          "+": { dir: "previousSibling", first: !0 },
          "~": { dir: "previousSibling" },
        },
        preFilter: {
          ATTR: function (e) {
            return (
              (e[1] = e[1].replace(te, ne)),
              (e[3] = (e[3] || e[4] || e[5] || "").replace(te, ne)),
              "~=" === e[2] && (e[3] = " " + e[3] + " "),
              e.slice(0, 4)
            );
          },
          CHILD: function (e) {
            return (
              (e[1] = e[1].toLowerCase()),
              "nth" === e[1].slice(0, 3)
                ? (e[3] || oe.error(e[0]),
                  (e[4] = +(e[4]
                    ? e[5] + (e[6] || 1)
                    : 2 * ("even" === e[3] || "odd" === e[3]))),
                  (e[5] = +(e[7] + e[8] || "odd" === e[3])))
                : e[3] && oe.error(e[0]),
              e
            );
          },
          PSEUDO: function (e) {
            var t,
              n = !e[6] && e[2];
            return G.CHILD.test(e[0])
              ? null
              : (e[3]
                  ? (e[2] = e[4] || e[5] || "")
                  : n &&
                    V.test(n) &&
                    (t = s(n, !0)) &&
                    (t = n.indexOf(")", n.length - t) - n.length) &&
                    ((e[0] = e[0].slice(0, t)), (e[2] = n.slice(0, t))),
                e.slice(0, 3));
          },
        },
        filter: {
          TAG: function (e) {
            var t = e.replace(te, ne).toLowerCase();
            return "*" === e
              ? function () {
                  return !0;
                }
              : function (e) {
                  return e.nodeName && e.nodeName.toLowerCase() === t;
                };
          },
          CLASS: function (e) {
            var t = C[e + " "];
            return (
              t ||
              ((t = new RegExp(
                "(^|[\\x20\\t\\r\\n\\f])" + e + "(" + R + "|$)"
              )) &&
                C(e, function (e) {
                  return t.test(
                    ("string" == typeof e.className && e.className) ||
                      (void 0 !== e.getAttribute && e.getAttribute("class")) ||
                      ""
                  );
                }))
            );
          },
          ATTR: function (e, t, n) {
            return function (r) {
              var i = oe.attr(r, e);
              return null == i
                ? "!=" === t
                : !t ||
                    ((i += ""),
                    "=" === t
                      ? i === n
                      : "!=" === t
                      ? i !== n
                      : "^=" === t
                      ? n && 0 === i.indexOf(n)
                      : "*=" === t
                      ? n && i.indexOf(n) > -1
                      : "$=" === t
                      ? n && i.slice(-n.length) === n
                      : "~=" === t
                      ? (" " + i.replace(H, " ") + " ").indexOf(n) > -1
                      : "|=" === t &&
                        (i === n || i.slice(0, n.length + 1) === n + "-"));
            };
          },
          CHILD: function (e, t, n, r, i) {
            var a = "nth" !== e.slice(0, 3),
              s = "last" !== e.slice(-4),
              o = "of-type" === t;
            return 1 === r && 0 === i
              ? function (e) {
                  return !!e.parentNode;
                }
              : function (t, n, u) {
                  var c,
                    l,
                    f,
                    d,
                    h,
                    p,
                    m = a !== s ? "nextSibling" : "previousSibling",
                    _ = t.parentNode,
                    b = o && t.nodeName.toLowerCase(),
                    g = !u && !o,
                    y = !1;
                  if (_) {
                    if (a) {
                      for (; m; ) {
                        for (d = t; (d = d[m]); )
                          if (
                            o
                              ? d.nodeName.toLowerCase() === b
                              : 1 === d.nodeType
                          )
                            return !1;
                        p = m = "only" === e && !p && "nextSibling";
                      }
                      return !0;
                    }
                    if (((p = [s ? _.firstChild : _.lastChild]), s && g)) {
                      for (
                        y =
                          (h =
                            (c =
                              (l =
                                (f = (d = _)[v] || (d[v] = {}))[d.uniqueID] ||
                                (f[d.uniqueID] = {}))[e] || [])[0] === x &&
                            c[1]) && c[2],
                          d = h && _.childNodes[h];
                        (d = (++h && d && d[m]) || (y = h = 0) || p.pop());

                      )
                        if (1 === d.nodeType && ++y && d === t) {
                          l[e] = [x, h, y];
                          break;
                        }
                    } else if (
                      (g &&
                        (y = h =
                          (c =
                            (l =
                              (f = (d = t)[v] || (d[v] = {}))[d.uniqueID] ||
                              (f[d.uniqueID] = {}))[e] || [])[0] === x && c[1]),
                      !1 === y)
                    )
                      for (
                        ;
                        (d = (++h && d && d[m]) || (y = h = 0) || p.pop()) &&
                        ((o
                          ? d.nodeName.toLowerCase() !== b
                          : 1 !== d.nodeType) ||
                          !++y ||
                          (g &&
                            ((l =
                              (f = d[v] || (d[v] = {}))[d.uniqueID] ||
                              (f[d.uniqueID] = {}))[e] = [x, y]),
                          d !== t));

                      );
                    return (y -= i) === r || (y % r == 0 && y / r >= 0);
                  }
                };
          },
          PSEUDO: function (e, t) {
            var n,
              i =
                r.pseudos[e] ||
                r.setFilters[e.toLowerCase()] ||
                oe.error("unsupported pseudo: " + e);
            return i[v]
              ? i(t)
              : i.length > 1
              ? ((n = [e, e, "", t]),
                r.setFilters.hasOwnProperty(e.toLowerCase())
                  ? ce(function (e, n) {
                      for (var r, a = i(e, t), s = a.length; s--; )
                        e[(r = N(e, a[s]))] = !(n[r] = a[s]);
                    })
                  : function (e) {
                      return i(e, 0, n);
                    })
              : i;
          },
        },
        pseudos: {
          not: ce(function (e) {
            var t = [],
              n = [],
              r = o(e.replace(F, "$1"));
            return r[v]
              ? ce(function (e, t, n, i) {
                  for (var a, s = r(e, null, i, []), o = e.length; o--; )
                    (a = s[o]) && (e[o] = !(t[o] = a));
                })
              : function (e, i, a) {
                  return (t[0] = e), r(t, null, a, n), (t[0] = null), !n.pop();
                };
          }),
          has: ce(function (e) {
            return function (t) {
              return oe(e, t).length > 0;
            };
          }),
          contains: ce(function (e) {
            return (
              (e = e.replace(te, ne)),
              function (t) {
                return (t.textContent || i(t)).indexOf(e) > -1;
              }
            );
          }),
          lang: ce(function (e) {
            return (
              X.test(e || "") || oe.error("unsupported lang: " + e),
              (e = e.replace(te, ne).toLowerCase()),
              function (t) {
                var n;
                do {
                  if (
                    (n = m
                      ? t.lang
                      : t.getAttribute("xml:lang") || t.getAttribute("lang"))
                  )
                    return (
                      (n = n.toLowerCase()) === e || 0 === n.indexOf(e + "-")
                    );
                } while ((t = t.parentNode) && 1 === t.nodeType);
                return !1;
              }
            );
          }),
          target: function (t) {
            var n = e.location && e.location.hash;
            return n && n.slice(1) === t.id;
          },
          root: function (e) {
            return e === p;
          },
          focus: function (e) {
            return (
              e === h.activeElement &&
              (!h.hasFocus || h.hasFocus()) &&
              !!(e.type || e.href || ~e.tabIndex)
            );
          },
          enabled: me(!1),
          disabled: me(!0),
          checked: function (e) {
            var t = e.nodeName.toLowerCase();
            return (
              ("input" === t && !!e.checked) || ("option" === t && !!e.selected)
            );
          },
          selected: function (e) {
            return (
              e.parentNode && e.parentNode.selectedIndex, !0 === e.selected
            );
          },
          empty: function (e) {
            for (e = e.firstChild; e; e = e.nextSibling)
              if (e.nodeType < 6) return !1;
            return !0;
          },
          parent: function (e) {
            return !r.pseudos.empty(e);
          },
          header: function (e) {
            return J.test(e.nodeName);
          },
          input: function (e) {
            return Q.test(e.nodeName);
          },
          button: function (e) {
            var t = e.nodeName.toLowerCase();
            return ("input" === t && "button" === e.type) || "button" === t;
          },
          text: function (e) {
            var t;
            return (
              "input" === e.nodeName.toLowerCase() &&
              "text" === e.type &&
              (null == (t = e.getAttribute("type")) ||
                "text" === t.toLowerCase())
            );
          },
          first: _e(function () {
            return [0];
          }),
          last: _e(function (e, t) {
            return [t - 1];
          }),
          eq: _e(function (e, t, n) {
            return [n < 0 ? n + t : n];
          }),
          even: _e(function (e, t) {
            for (var n = 0; n < t; n += 2) e.push(n);
            return e;
          }),
          odd: _e(function (e, t) {
            for (var n = 1; n < t; n += 2) e.push(n);
            return e;
          }),
          lt: _e(function (e, t, n) {
            for (var r = n < 0 ? n + t : n > t ? t : n; --r >= 0; ) e.push(r);
            return e;
          }),
          gt: _e(function (e, t, n) {
            for (var r = n < 0 ? n + t : n; ++r < t; ) e.push(r);
            return e;
          }),
        },
      }),
      (r.pseudos.nth = r.pseudos.eq),
      { radio: !0, checkbox: !0, file: !0, password: !0, image: !0 }))
        r.pseudos[t] = he(t);
      for (t in { submit: !0, reset: !0 }) r.pseudos[t] = pe(t);
      function ge() {}
      function ye(e) {
        for (var t = 0, n = e.length, r = ""; t < n; t++) r += e[t].value;
        return r;
      }
      function ve(e, t, n) {
        var r = t.dir,
          i = t.next,
          a = i || r,
          s = n && "parentNode" === a,
          o = S++;
        return t.first
          ? function (t, n, i) {
              for (; (t = t[r]); ) if (1 === t.nodeType || s) return e(t, n, i);
              return !1;
            }
          : function (t, n, u) {
              var c,
                l,
                f,
                d = [x, o];
              if (u) {
                for (; (t = t[r]); )
                  if ((1 === t.nodeType || s) && e(t, n, u)) return !0;
              } else
                for (; (t = t[r]); )
                  if (1 === t.nodeType || s)
                    if (
                      ((l =
                        (f = t[v] || (t[v] = {}))[t.uniqueID] ||
                        (f[t.uniqueID] = {})),
                      i && i === t.nodeName.toLowerCase())
                    )
                      t = t[r] || t;
                    else {
                      if ((c = l[a]) && c[0] === x && c[1] === o)
                        return (d[2] = c[2]);
                      if (((l[a] = d), (d[2] = e(t, n, u)))) return !0;
                    }
              return !1;
            };
      }
      function we(e) {
        return e.length > 1
          ? function (t, n, r) {
              for (var i = e.length; i--; ) if (!e[i](t, n, r)) return !1;
              return !0;
            }
          : e[0];
      }
      function xe(e, t, n, r, i) {
        for (var a, s = [], o = 0, u = e.length, c = null != t; o < u; o++)
          (a = e[o]) && ((n && !n(a, r, i)) || (s.push(a), c && t.push(o)));
        return s;
      }
      function Se(e, t, n, r, i, a) {
        return (
          r && !r[v] && (r = Se(r)),
          i && !i[v] && (i = Se(i, a)),
          ce(function (a, s, o, u) {
            var c,
              l,
              f,
              d = [],
              h = [],
              p = s.length,
              m =
                a ||
                (function (e, t, n) {
                  for (var r = 0, i = t.length; r < i; r++) oe(e, t[r], n);
                  return n;
                })(t || "*", o.nodeType ? [o] : o, []),
              _ = !e || (!a && t) ? m : xe(m, d, e, o, u),
              b = n ? (i || (a ? e : p || r) ? [] : s) : _;
            if ((n && n(_, b, o, u), r))
              for (c = xe(b, h), r(c, [], o, u), l = c.length; l--; )
                (f = c[l]) && (b[h[l]] = !(_[h[l]] = f));
            if (a) {
              if (i || e) {
                if (i) {
                  for (c = [], l = b.length; l--; )
                    (f = b[l]) && c.push((_[l] = f));
                  i(null, (b = []), c, u);
                }
                for (l = b.length; l--; )
                  (f = b[l]) &&
                    (c = i ? N(a, f) : d[l]) > -1 &&
                    (a[c] = !(s[c] = f));
              }
            } else (b = xe(b === s ? b.splice(p, b.length) : b)), i ? i(null, s, b, u) : A.apply(s, b);
          })
        );
      }
      function Ce(e) {
        for (
          var t,
            n,
            i,
            a = e.length,
            s = r.relative[e[0].type],
            o = s || r.relative[" "],
            u = s ? 1 : 0,
            l = ve(
              function (e) {
                return e === t;
              },
              o,
              !0
            ),
            f = ve(
              function (e) {
                return N(t, e) > -1;
              },
              o,
              !0
            ),
            d = [
              function (e, n, r) {
                var i =
                  (!s && (r || n !== c)) ||
                  ((t = n).nodeType ? l(e, n, r) : f(e, n, r));
                return (t = null), i;
              },
            ];
          u < a;
          u++
        )
          if ((n = r.relative[e[u].type])) d = [ve(we(d), n)];
          else {
            if ((n = r.filter[e[u].type].apply(null, e[u].matches))[v]) {
              for (i = ++u; i < a && !r.relative[e[i].type]; i++);
              return Se(
                u > 1 && we(d),
                u > 1 &&
                  ye(
                    e
                      .slice(0, u - 1)
                      .concat({ value: " " === e[u - 2].type ? "*" : "" })
                  ).replace(F, "$1"),
                n,
                u < i && Ce(e.slice(u, i)),
                i < a && Ce((e = e.slice(i))),
                i < a && ye(e)
              );
            }
            d.push(n);
          }
        return we(d);
      }
      return (
        (ge.prototype = r.filters = r.pseudos),
        (r.setFilters = new ge()),
        (s = oe.tokenize = function (e, t) {
          var n,
            i,
            a,
            s,
            o,
            u,
            c,
            l = E[e + " "];
          if (l) return t ? 0 : l.slice(0);
          for (o = e, u = [], c = r.preFilter; o; ) {
            for (s in ((n && !(i = W.exec(o))) ||
              (i && (o = o.slice(i[0].length) || o), u.push((a = []))),
            (n = !1),
            (i = z.exec(o)) &&
              ((n = i.shift()),
              a.push({ value: n, type: i[0].replace(F, " ") }),
              (o = o.slice(n.length))),
            r.filter))
              !(i = G[s].exec(o)) ||
                (c[s] && !(i = c[s](i))) ||
                ((n = i.shift()),
                a.push({ value: n, type: s, matches: i }),
                (o = o.slice(n.length)));
            if (!n) break;
          }
          return t ? o.length : o ? oe.error(e) : E(e, u).slice(0);
        }),
        (o = oe.compile = function (e, t) {
          var n,
            i = [],
            a = [],
            o = P[e + " "];
          if (!o) {
            for (t || (t = s(e)), n = t.length; n--; )
              (o = Ce(t[n]))[v] ? i.push(o) : a.push(o);
            (o = P(
              e,
              (function (e, t) {
                var n = t.length > 0,
                  i = e.length > 0,
                  a = function (a, s, o, u, l) {
                    var f,
                      p,
                      _,
                      b = 0,
                      g = "0",
                      y = a && [],
                      v = [],
                      w = c,
                      S = a || (i && r.find.TAG("*", l)),
                      C = (x += null == w ? 1 : Math.random() || 0.1),
                      E = S.length;
                    for (
                      l && (c = s == h || s || l);
                      g !== E && null != (f = S[g]);
                      g++
                    ) {
                      if (i && f) {
                        for (
                          p = 0, s || f.ownerDocument == h || (d(f), (o = !m));
                          (_ = e[p++]);

                        )
                          if (_(f, s || h, o)) {
                            u.push(f);
                            break;
                          }
                        l && (x = C);
                      }
                      n && ((f = !_ && f) && b--, a && y.push(f));
                    }
                    if (((b += g), n && g !== b)) {
                      for (p = 0; (_ = t[p++]); ) _(y, v, s, o);
                      if (a) {
                        if (b > 0)
                          for (; g--; ) y[g] || v[g] || (v[g] = q.call(u));
                        v = xe(v);
                      }
                      A.apply(u, v),
                        l &&
                          !a &&
                          v.length > 0 &&
                          b + t.length > 1 &&
                          oe.uniqueSort(u);
                    }
                    return l && ((x = C), (c = w)), y;
                  };
                return n ? ce(a) : a;
              })(a, i)
            )),
              (o.selector = e);
          }
          return o;
        }),
        (u = oe.select = function (e, t, n, i) {
          var a,
            u,
            c,
            l,
            f,
            d = "function" == typeof e && e,
            h = !i && s((e = d.selector || e));
          if (((n = n || []), 1 === h.length)) {
            if (
              (u = h[0] = h[0].slice(0)).length > 2 &&
              "ID" === (c = u[0]).type &&
              9 === t.nodeType &&
              m &&
              r.relative[u[1].type]
            ) {
              if (!(t = (r.find.ID(c.matches[0].replace(te, ne), t) || [])[0]))
                return n;
              d && (t = t.parentNode), (e = e.slice(u.shift().value.length));
            }
            for (
              a = G.needsContext.test(e) ? 0 : u.length;
              a-- && ((c = u[a]), !r.relative[(l = c.type)]);

            )
              if (
                (f = r.find[l]) &&
                (i = f(
                  c.matches[0].replace(te, ne),
                  (ee.test(u[0].type) && be(t.parentNode)) || t
                ))
              ) {
                if ((u.splice(a, 1), !(e = i.length && ye(u))))
                  return A.apply(n, i), n;
                break;
              }
          }
          return (
            (d || o(e, h))(
              i,
              t,
              !m,
              n,
              !t || (ee.test(e) && be(t.parentNode)) || t
            ),
            n
          );
        }),
        (n.sortStable = v.split("").sort(T).join("") === v),
        (n.detectDuplicates = !!f),
        d(),
        (n.sortDetached = le(function (e) {
          return 1 & e.compareDocumentPosition(h.createElement("fieldset"));
        })),
        le(function (e) {
          return (
            (e.innerHTML = "<a href='#'></a>"),
            "#" === e.firstChild.getAttribute("href")
          );
        }) ||
          fe("type|href|height|width", function (e, t, n) {
            if (!n)
              return e.getAttribute(t, "type" === t.toLowerCase() ? 1 : 2);
          }),
        (n.attributes &&
          le(function (e) {
            return (
              (e.innerHTML = "<input/>"),
              e.firstChild.setAttribute("value", ""),
              "" === e.firstChild.getAttribute("value")
            );
          })) ||
          fe("value", function (e, t, n) {
            if (!n && "input" === e.nodeName.toLowerCase())
              return e.defaultValue;
          }),
        le(function (e) {
          return null == e.getAttribute("disabled");
        }) ||
          fe(L, function (e, t, n) {
            var r;
            if (!n)
              return !0 === e[t]
                ? t.toLowerCase()
                : (r = e.getAttributeNode(t)) && r.specified
                ? r.value
                : null;
          }),
        oe
      );
    })(e);
  (w.find = S),
    (w.expr = S.selectors),
    (w.expr[":"] = w.expr.pseudos),
    (w.uniqueSort = w.unique = S.uniqueSort),
    (w.text = S.getText),
    (w.isXMLDoc = S.isXML),
    (w.contains = S.contains),
    (w.escapeSelector = S.escape);
  var C = function (e, t, n) {
      for (var r = [], i = void 0 !== n; (e = e[t]) && 9 !== e.nodeType; )
        if (1 === e.nodeType) {
          if (i && w(e).is(n)) break;
          r.push(e);
        }
      return r;
    },
    E = function (e, t) {
      for (var n = []; e; e = e.nextSibling)
        1 === e.nodeType && e !== t && n.push(e);
      return n;
    },
    P = w.expr.match.needsContext;
  function k(e, t) {
    return e.nodeName && e.nodeName.toLowerCase() === t.toLowerCase();
  }
  var T = /^<([a-z][^\/\0>:\x20\t\r\n\f]*)[\x20\t\r\n\f]*\/?>(?:<\/\1>|)$/i;
  function j(e, t, n) {
    return p(t)
      ? w.grep(e, function (e, r) {
          return !!t.call(e, r, e) !== n;
        })
      : t.nodeType
      ? w.grep(e, function (e) {
          return (e === t) !== n;
        })
      : "string" != typeof t
      ? w.grep(e, function (e) {
          return o.call(t, e) > -1 !== n;
        })
      : w.filter(t, e, n);
  }
  (w.filter = function (e, t, n) {
    var r = t[0];
    return (
      n && (e = ":not(" + e + ")"),
      1 === t.length && 1 === r.nodeType
        ? w.find.matchesSelector(r, e)
          ? [r]
          : []
        : w.find.matches(
            e,
            w.grep(t, function (e) {
              return 1 === e.nodeType;
            })
          )
    );
  }),
    w.fn.extend({
      find: function (e) {
        var t,
          n,
          r = this.length,
          i = this;
        if ("string" != typeof e)
          return this.pushStack(
            w(e).filter(function () {
              for (t = 0; t < r; t++) if (w.contains(i[t], this)) return !0;
            })
          );
        for (n = this.pushStack([]), t = 0; t < r; t++) w.find(e, i[t], n);
        return r > 1 ? w.uniqueSort(n) : n;
      },
      filter: function (e) {
        return this.pushStack(j(this, e || [], !1));
      },
      not: function (e) {
        return this.pushStack(j(this, e || [], !0));
      },
      is: function (e) {
        return !!j(this, "string" == typeof e && P.test(e) ? w(e) : e || [], !1)
          .length;
      },
    });
  var B,
    q = /^(?:\s*(<[\w\W]+>)[^>]*|#([\w-]+))$/;
  ((w.fn.init = function (e, t, n) {
    var r, i;
    if (!e) return this;
    if (((n = n || B), "string" == typeof e)) {
      if (
        !(r =
          "<" === e[0] && ">" === e[e.length - 1] && e.length >= 3
            ? [null, e, null]
            : q.exec(e)) ||
        (!r[1] && t)
      )
        return !t || t.jquery ? (t || n).find(e) : this.constructor(t).find(e);
      if (r[1]) {
        if (
          ((t = t instanceof w ? t[0] : t),
          w.merge(
            this,
            w.parseHTML(r[1], t && t.nodeType ? t.ownerDocument || t : _, !0)
          ),
          T.test(r[1]) && w.isPlainObject(t))
        )
          for (r in t) p(this[r]) ? this[r](t[r]) : this.attr(r, t[r]);
        return this;
      }
      return (
        (i = _.getElementById(r[2])) && ((this[0] = i), (this.length = 1)), this
      );
    }
    return e.nodeType
      ? ((this[0] = e), (this.length = 1), this)
      : p(e)
      ? void 0 !== n.ready
        ? n.ready(e)
        : e(w)
      : w.makeArray(e, this);
  }).prototype = w.fn),
    (B = w(_));
  var D = /^(?:parents|prev(?:Until|All))/,
    A = { children: !0, contents: !0, next: !0, prev: !0 };
  function $(e, t) {
    for (; (e = e[t]) && 1 !== e.nodeType; );
    return e;
  }
  w.fn.extend({
    has: function (e) {
      var t = w(e, this),
        n = t.length;
      return this.filter(function () {
        for (var e = 0; e < n; e++) if (w.contains(this, t[e])) return !0;
      });
    },
    closest: function (e, t) {
      var n,
        r = 0,
        i = this.length,
        a = [],
        s = "string" != typeof e && w(e);
      if (!P.test(e))
        for (; r < i; r++)
          for (n = this[r]; n && n !== t; n = n.parentNode)
            if (
              n.nodeType < 11 &&
              (s
                ? s.index(n) > -1
                : 1 === n.nodeType && w.find.matchesSelector(n, e))
            ) {
              a.push(n);
              break;
            }
      return this.pushStack(a.length > 1 ? w.uniqueSort(a) : a);
    },
    index: function (e) {
      return e
        ? "string" == typeof e
          ? o.call(w(e), this[0])
          : o.call(this, e.jquery ? e[0] : e)
        : this[0] && this[0].parentNode
        ? this.first().prevAll().length
        : -1;
    },
    add: function (e, t) {
      return this.pushStack(w.uniqueSort(w.merge(this.get(), w(e, t))));
    },
    addBack: function (e) {
      return this.add(null == e ? this.prevObject : this.prevObject.filter(e));
    },
  }),
    w.each(
      {
        parent: function (e) {
          var t = e.parentNode;
          return t && 11 !== t.nodeType ? t : null;
        },
        parents: function (e) {
          return C(e, "parentNode");
        },
        parentsUntil: function (e, t, n) {
          return C(e, "parentNode", n);
        },
        next: function (e) {
          return $(e, "nextSibling");
        },
        prev: function (e) {
          return $(e, "previousSibling");
        },
        nextAll: function (e) {
          return C(e, "nextSibling");
        },
        prevAll: function (e) {
          return C(e, "previousSibling");
        },
        nextUntil: function (e, t, n) {
          return C(e, "nextSibling", n);
        },
        prevUntil: function (e, t, n) {
          return C(e, "previousSibling", n);
        },
        siblings: function (e) {
          return E((e.parentNode || {}).firstChild, e);
        },
        children: function (e) {
          return E(e.firstChild);
        },
        contents: function (e) {
          return null != e.contentDocument && r(e.contentDocument)
            ? e.contentDocument
            : (k(e, "template") && (e = e.content || e),
              w.merge([], e.childNodes));
        },
      },
      function (e, t) {
        w.fn[e] = function (n, r) {
          var i = w.map(this, t, n);
          return (
            "Until" !== e.slice(-5) && (r = n),
            r && "string" == typeof r && (i = w.filter(r, i)),
            this.length > 1 &&
              (A[e] || w.uniqueSort(i), D.test(e) && i.reverse()),
            this.pushStack(i)
          );
        };
      }
    );
  var N = /[^\x20\t\r\n\f]+/g;
  function L(e) {
    return e;
  }
  function R(e) {
    throw e;
  }
  function O(e, t, n, r) {
    var i;
    try {
      e && p((i = e.promise))
        ? i.call(e).done(t).fail(n)
        : e && p((i = e.then))
        ? i.call(e, t, n)
        : t.apply(void 0, [e].slice(r));
    } catch (e) {
      n.apply(void 0, [e]);
    }
  }
  (w.Callbacks = function (e) {
    e =
      "string" == typeof e
        ? (function (e) {
            var t = {};
            return (
              w.each(e.match(N) || [], function (e, n) {
                t[n] = !0;
              }),
              t
            );
          })(e)
        : w.extend({}, e);
    var t,
      n,
      r,
      i,
      a = [],
      s = [],
      o = -1,
      u = function () {
        for (i = i || e.once, r = t = !0; s.length; o = -1)
          for (n = s.shift(); ++o < a.length; )
            !1 === a[o].apply(n[0], n[1]) &&
              e.stopOnFalse &&
              ((o = a.length), (n = !1));
        e.memory || (n = !1), (t = !1), i && (a = n ? [] : "");
      },
      c = {
        add: function () {
          return (
            a &&
              (n && !t && ((o = a.length - 1), s.push(n)),
              (function t(n) {
                w.each(n, function (n, r) {
                  p(r)
                    ? (e.unique && c.has(r)) || a.push(r)
                    : r && r.length && "string" !== y(r) && t(r);
                });
              })(arguments),
              n && !t && u()),
            this
          );
        },
        remove: function () {
          return (
            w.each(arguments, function (e, t) {
              for (var n; (n = w.inArray(t, a, n)) > -1; )
                a.splice(n, 1), n <= o && o--;
            }),
            this
          );
        },
        has: function (e) {
          return e ? w.inArray(e, a) > -1 : a.length > 0;
        },
        empty: function () {
          return a && (a = []), this;
        },
        disable: function () {
          return (i = s = []), (a = n = ""), this;
        },
        disabled: function () {
          return !a;
        },
        lock: function () {
          return (i = s = []), n || t || (a = n = ""), this;
        },
        locked: function () {
          return !!i;
        },
        fireWith: function (e, n) {
          return (
            i ||
              ((n = [e, (n = n || []).slice ? n.slice() : n]),
              s.push(n),
              t || u()),
            this
          );
        },
        fire: function () {
          return c.fireWith(this, arguments), this;
        },
        fired: function () {
          return !!r;
        },
      };
    return c;
  }),
    w.extend({
      Deferred: function (t) {
        var n = [
            [
              "notify",
              "progress",
              w.Callbacks("memory"),
              w.Callbacks("memory"),
              2,
            ],
            [
              "resolve",
              "done",
              w.Callbacks("once memory"),
              w.Callbacks("once memory"),
              0,
              "resolved",
            ],
            [
              "reject",
              "fail",
              w.Callbacks("once memory"),
              w.Callbacks("once memory"),
              1,
              "rejected",
            ],
          ],
          r = "pending",
          i = {
            state: function () {
              return r;
            },
            always: function () {
              return a.done(arguments).fail(arguments), this;
            },
            catch: function (e) {
              return i.then(null, e);
            },
            pipe: function () {
              var e = arguments;
              return w
                .Deferred(function (t) {
                  w.each(n, function (n, r) {
                    var i = p(e[r[4]]) && e[r[4]];
                    a[r[1]](function () {
                      var e = i && i.apply(this, arguments);
                      e && p(e.promise)
                        ? e
                            .promise()
                            .progress(t.notify)
                            .done(t.resolve)
                            .fail(t.reject)
                        : t[r[0] + "With"](this, i ? [e] : arguments);
                    });
                  }),
                    (e = null);
                })
                .promise();
            },
            then: function (t, r, i) {
              var a = 0;
              function s(t, n, r, i) {
                return function () {
                  var o = this,
                    u = arguments,
                    c = function () {
                      var e, c;
                      if (!(t < a)) {
                        if ((e = r.apply(o, u)) === n.promise())
                          throw new TypeError("Thenable self-resolution");
                        (c =
                          e &&
                          ("object" == typeof e || "function" == typeof e) &&
                          e.then),
                          p(c)
                            ? i
                              ? c.call(e, s(a, n, L, i), s(a, n, R, i))
                              : (a++,
                                c.call(
                                  e,
                                  s(a, n, L, i),
                                  s(a, n, R, i),
                                  s(a, n, L, n.notifyWith)
                                ))
                            : (r !== L && ((o = void 0), (u = [e])),
                              (i || n.resolveWith)(o, u));
                      }
                    },
                    l = i
                      ? c
                      : function () {
                          try {
                            c();
                          } catch (e) {
                            w.Deferred.exceptionHook &&
                              w.Deferred.exceptionHook(e, l.stackTrace),
                              t + 1 >= a &&
                                (r !== R && ((o = void 0), (u = [e])),
                                n.rejectWith(o, u));
                          }
                        };
                  t
                    ? l()
                    : (w.Deferred.getStackHook &&
                        (l.stackTrace = w.Deferred.getStackHook()),
                      e.setTimeout(l));
                };
              }
              return w
                .Deferred(function (e) {
                  n[0][3].add(s(0, e, p(i) ? i : L, e.notifyWith)),
                    n[1][3].add(s(0, e, p(t) ? t : L)),
                    n[2][3].add(s(0, e, p(r) ? r : R));
                })
                .promise();
            },
            promise: function (e) {
              return null != e ? w.extend(e, i) : i;
            },
          },
          a = {};
        return (
          w.each(n, function (e, t) {
            var s = t[2],
              o = t[5];
            (i[t[1]] = s.add),
              o &&
                s.add(
                  function () {
                    r = o;
                  },
                  n[3 - e][2].disable,
                  n[3 - e][3].disable,
                  n[0][2].lock,
                  n[0][3].lock
                ),
              s.add(t[3].fire),
              (a[t[0]] = function () {
                return (
                  a[t[0] + "With"](this === a ? void 0 : this, arguments), this
                );
              }),
              (a[t[0] + "With"] = s.fireWith);
          }),
          i.promise(a),
          t && t.call(a, a),
          a
        );
      },
      when: function (e) {
        var t = arguments.length,
          n = t,
          r = Array(n),
          a = i.call(arguments),
          s = w.Deferred(),
          o = function (e) {
            return function (n) {
              (r[e] = this),
                (a[e] = arguments.length > 1 ? i.call(arguments) : n),
                --t || s.resolveWith(r, a);
            };
          };
        if (
          t <= 1 &&
          (O(e, s.done(o(n)).resolve, s.reject, !t),
          "pending" === s.state() || p(a[n] && a[n].then))
        )
          return s.then();
        for (; n--; ) O(a[n], o(n), s.reject);
        return s.promise();
      },
    });
  var M = /^(Eval|Internal|Range|Reference|Syntax|Type|URI)Error$/;
  (w.Deferred.exceptionHook = function (t, n) {
    e.console &&
      e.console.warn &&
      t &&
      M.test(t.name) &&
      e.console.warn("jQuery.Deferred exception: " + t.message, t.stack, n);
  }),
    (w.readyException = function (t) {
      e.setTimeout(function () {
        throw t;
      });
    });
  var I = w.Deferred();
  function H() {
    _.removeEventListener("DOMContentLoaded", H),
      e.removeEventListener("load", H),
      w.ready();
  }
  (w.fn.ready = function (e) {
    return (
      I.then(e).catch(function (e) {
        w.readyException(e);
      }),
      this
    );
  }),
    w.extend({
      isReady: !1,
      readyWait: 1,
      ready: function (e) {
        (!0 === e ? --w.readyWait : w.isReady) ||
          ((w.isReady = !0),
          (!0 !== e && --w.readyWait > 0) || I.resolveWith(_, [w]));
      },
    }),
    (w.ready.then = I.then),
    "complete" === _.readyState ||
    ("loading" !== _.readyState && !_.documentElement.doScroll)
      ? e.setTimeout(w.ready)
      : (_.addEventListener("DOMContentLoaded", H),
        e.addEventListener("load", H));
  var F = function (e, t, n, r, i, a, s) {
      var o = 0,
        u = e.length,
        c = null == n;
      if ("object" === y(n))
        for (o in ((i = !0), n)) F(e, t, o, n[o], !0, a, s);
      else if (
        void 0 !== r &&
        ((i = !0),
        p(r) || (s = !0),
        c &&
          (s
            ? (t.call(e, r), (t = null))
            : ((c = t),
              (t = function (e, t, n) {
                return c.call(w(e), n);
              }))),
        t)
      )
        for (; o < u; o++) t(e[o], n, s ? r : r.call(e[o], o, t(e[o], n)));
      return i ? e : c ? t.call(e) : u ? t(e[0], n) : a;
    },
    W = /^-ms-/,
    z = /-([a-z])/g;
  function U(e, t) {
    return t.toUpperCase();
  }
  function V(e) {
    return e.replace(W, "ms-").replace(z, U);
  }
  var X = function (e) {
    return 1 === e.nodeType || 9 === e.nodeType || !+e.nodeType;
  };
  function G() {
    this.expando = w.expando + G.uid++;
  }
  (G.uid = 1),
    (G.prototype = {
      cache: function (e) {
        var t = e[this.expando];
        return (
          t ||
            ((t = {}),
            X(e) &&
              (e.nodeType
                ? (e[this.expando] = t)
                : Object.defineProperty(e, this.expando, {
                    value: t,
                    configurable: !0,
                  }))),
          t
        );
      },
      set: function (e, t, n) {
        var r,
          i = this.cache(e);
        if ("string" == typeof t) i[V(t)] = n;
        else for (r in t) i[V(r)] = t[r];
        return i;
      },
      get: function (e, t) {
        return void 0 === t
          ? this.cache(e)
          : e[this.expando] && e[this.expando][V(t)];
      },
      access: function (e, t, n) {
        return void 0 === t || (t && "string" == typeof t && void 0 === n)
          ? this.get(e, t)
          : (this.set(e, t, n), void 0 !== n ? n : t);
      },
      remove: function (e, t) {
        var n,
          r = e[this.expando];
        if (void 0 !== r) {
          if (void 0 !== t) {
            n = (t = Array.isArray(t)
              ? t.map(V)
              : (t = V(t)) in r
              ? [t]
              : t.match(N) || []).length;
            for (; n--; ) delete r[t[n]];
          }
          (void 0 === t || w.isEmptyObject(r)) &&
            (e.nodeType ? (e[this.expando] = void 0) : delete e[this.expando]);
        }
      },
      hasData: function (e) {
        var t = e[this.expando];
        return void 0 !== t && !w.isEmptyObject(t);
      },
    });
  var Y = new G(),
    Q = new G(),
    J = /^(?:\{[\w\W]*\}|\[[\w\W]*\])$/,
    K = /[A-Z]/g;
  function Z(e, t, n) {
    var r;
    if (void 0 === n && 1 === e.nodeType)
      if (
        ((r = "data-" + t.replace(K, "-$&").toLowerCase()),
        "string" == typeof (n = e.getAttribute(r)))
      ) {
        try {
          n = (function (e) {
            return (
              "true" === e ||
              ("false" !== e &&
                ("null" === e
                  ? null
                  : e === +e + ""
                  ? +e
                  : J.test(e)
                  ? JSON.parse(e)
                  : e))
            );
          })(n);
        } catch (e) {}
        Q.set(e, t, n);
      } else n = void 0;
    return n;
  }
  w.extend({
    hasData: function (e) {
      return Q.hasData(e) || Y.hasData(e);
    },
    data: function (e, t, n) {
      return Q.access(e, t, n);
    },
    removeData: function (e, t) {
      Q.remove(e, t);
    },
    _data: function (e, t, n) {
      return Y.access(e, t, n);
    },
    _removeData: function (e, t) {
      Y.remove(e, t);
    },
  }),
    w.fn.extend({
      data: function (e, t) {
        var n,
          r,
          i,
          a = this[0],
          s = a && a.attributes;
        if (void 0 === e) {
          if (
            this.length &&
            ((i = Q.get(a)), 1 === a.nodeType && !Y.get(a, "hasDataAttrs"))
          ) {
            for (n = s.length; n--; )
              s[n] &&
                0 === (r = s[n].name).indexOf("data-") &&
                ((r = V(r.slice(5))), Z(a, r, i[r]));
            Y.set(a, "hasDataAttrs", !0);
          }
          return i;
        }
        return "object" == typeof e
          ? this.each(function () {
              Q.set(this, e);
            })
          : F(
              this,
              function (t) {
                var n;
                if (a && void 0 === t)
                  return void 0 !== (n = Q.get(a, e)) ||
                    void 0 !== (n = Z(a, e))
                    ? n
                    : void 0;
                this.each(function () {
                  Q.set(this, e, t);
                });
              },
              null,
              t,
              arguments.length > 1,
              null,
              !0
            );
      },
      removeData: function (e) {
        return this.each(function () {
          Q.remove(this, e);
        });
      },
    }),
    w.extend({
      queue: function (e, t, n) {
        var r;
        if (e)
          return (
            (t = (t || "fx") + "queue"),
            (r = Y.get(e, t)),
            n &&
              (!r || Array.isArray(n)
                ? (r = Y.access(e, t, w.makeArray(n)))
                : r.push(n)),
            r || []
          );
      },
      dequeue: function (e, t) {
        t = t || "fx";
        var n = w.queue(e, t),
          r = n.length,
          i = n.shift(),
          a = w._queueHooks(e, t);
        "inprogress" === i && ((i = n.shift()), r--),
          i &&
            ("fx" === t && n.unshift("inprogress"),
            delete a.stop,
            i.call(
              e,
              function () {
                w.dequeue(e, t);
              },
              a
            )),
          !r && a && a.empty.fire();
      },
      _queueHooks: function (e, t) {
        var n = t + "queueHooks";
        return (
          Y.get(e, n) ||
          Y.access(e, n, {
            empty: w.Callbacks("once memory").add(function () {
              Y.remove(e, [t + "queue", n]);
            }),
          })
        );
      },
    }),
    w.fn.extend({
      queue: function (e, t) {
        var n = 2;
        return (
          "string" != typeof e && ((t = e), (e = "fx"), n--),
          arguments.length < n
            ? w.queue(this[0], e)
            : void 0 === t
            ? this
            : this.each(function () {
                var n = w.queue(this, e, t);
                w._queueHooks(this, e),
                  "fx" === e && "inprogress" !== n[0] && w.dequeue(this, e);
              })
        );
      },
      dequeue: function (e) {
        return this.each(function () {
          w.dequeue(this, e);
        });
      },
      clearQueue: function (e) {
        return this.queue(e || "fx", []);
      },
      promise: function (e, t) {
        var n,
          r = 1,
          i = w.Deferred(),
          a = this,
          s = this.length,
          o = function () {
            --r || i.resolveWith(a, [a]);
          };
        for (
          "string" != typeof e && ((t = e), (e = void 0)), e = e || "fx";
          s--;

        )
          (n = Y.get(a[s], e + "queueHooks")) &&
            n.empty &&
            (r++, n.empty.add(o));
        return o(), i.promise(t);
      },
    });
  var ee = /[+-]?(?:\d*\.|)\d+(?:[eE][+-]?\d+|)/.source,
    te = new RegExp("^(?:([+-])=|)(" + ee + ")([a-z%]*)$", "i"),
    ne = ["Top", "Right", "Bottom", "Left"],
    re = _.documentElement,
    ie = function (e) {
      return w.contains(e.ownerDocument, e);
    },
    ae = { composed: !0 };
  re.getRootNode &&
    (ie = function (e) {
      return (
        w.contains(e.ownerDocument, e) || e.getRootNode(ae) === e.ownerDocument
      );
    });
  var se = function (e, t) {
    return (
      "none" === (e = t || e).style.display ||
      ("" === e.style.display && ie(e) && "none" === w.css(e, "display"))
    );
  };
  function oe(e, t, n, r) {
    var i,
      a,
      s = 20,
      o = r
        ? function () {
            return r.cur();
          }
        : function () {
            return w.css(e, t, "");
          },
      u = o(),
      c = (n && n[3]) || (w.cssNumber[t] ? "" : "px"),
      l =
        e.nodeType &&
        (w.cssNumber[t] || ("px" !== c && +u)) &&
        te.exec(w.css(e, t));
    if (l && l[3] !== c) {
      for (u /= 2, c = c || l[3], l = +u || 1; s--; )
        w.style(e, t, l + c),
          (1 - a) * (1 - (a = o() / u || 0.5)) <= 0 && (s = 0),
          (l /= a);
      (l *= 2), w.style(e, t, l + c), (n = n || []);
    }
    return (
      n &&
        ((l = +l || +u || 0),
        (i = n[1] ? l + (n[1] + 1) * n[2] : +n[2]),
        r && ((r.unit = c), (r.start = l), (r.end = i))),
      i
    );
  }
  var ue = {};
  function ce(e) {
    var t,
      n = e.ownerDocument,
      r = e.nodeName,
      i = ue[r];
    return (
      i ||
      ((t = n.body.appendChild(n.createElement(r))),
      (i = w.css(t, "display")),
      t.parentNode.removeChild(t),
      "none" === i && (i = "block"),
      (ue[r] = i),
      i)
    );
  }
  function le(e, t) {
    for (var n, r, i = [], a = 0, s = e.length; a < s; a++)
      (r = e[a]).style &&
        ((n = r.style.display),
        t
          ? ("none" === n &&
              ((i[a] = Y.get(r, "display") || null),
              i[a] || (r.style.display = "")),
            "" === r.style.display && se(r) && (i[a] = ce(r)))
          : "none" !== n && ((i[a] = "none"), Y.set(r, "display", n)));
    for (a = 0; a < s; a++) null != i[a] && (e[a].style.display = i[a]);
    return e;
  }
  w.fn.extend({
    show: function () {
      return le(this, !0);
    },
    hide: function () {
      return le(this);
    },
    toggle: function (e) {
      return "boolean" == typeof e
        ? e
          ? this.show()
          : this.hide()
        : this.each(function () {
            se(this) ? w(this).show() : w(this).hide();
          });
    },
  });
  var fe,
    de,
    he = /^(?:checkbox|radio)$/i,
    pe = /<([a-z][^\/\0>\x20\t\r\n\f]*)/i,
    me = /^$|^module$|\/(?:java|ecma)script/i;
  (fe = _.createDocumentFragment().appendChild(_.createElement("div"))),
    (de = _.createElement("input")).setAttribute("type", "radio"),
    de.setAttribute("checked", "checked"),
    de.setAttribute("name", "t"),
    fe.appendChild(de),
    (h.checkClone = fe.cloneNode(!0).cloneNode(!0).lastChild.checked),
    (fe.innerHTML = "<textarea>x</textarea>"),
    (h.noCloneChecked = !!fe.cloneNode(!0).lastChild.defaultValue),
    (fe.innerHTML = "<option></option>"),
    (h.option = !!fe.lastChild);
  var _e = {
    thead: [1, "<table>", "</table>"],
    col: [2, "<table><colgroup>", "</colgroup></table>"],
    tr: [2, "<table><tbody>", "</tbody></table>"],
    td: [3, "<table><tbody><tr>", "</tr></tbody></table>"],
    _default: [0, "", ""],
  };
  function be(e, t) {
    var n;
    return (
      (n =
        void 0 !== e.getElementsByTagName
          ? e.getElementsByTagName(t || "*")
          : void 0 !== e.querySelectorAll
          ? e.querySelectorAll(t || "*")
          : []),
      void 0 === t || (t && k(e, t)) ? w.merge([e], n) : n
    );
  }
  function ge(e, t) {
    for (var n = 0, r = e.length; n < r; n++)
      Y.set(e[n], "globalEval", !t || Y.get(t[n], "globalEval"));
  }
  (_e.tbody = _e.tfoot = _e.colgroup = _e.caption = _e.thead),
    (_e.th = _e.td),
    h.option ||
      (_e.optgroup = _e.option = [
        1,
        "<select multiple='multiple'>",
        "</select>",
      ]);
  var ye = /<|&#?\w+;/;
  function ve(e, t, n, r, i) {
    for (
      var a,
        s,
        o,
        u,
        c,
        l,
        f = t.createDocumentFragment(),
        d = [],
        h = 0,
        p = e.length;
      h < p;
      h++
    )
      if ((a = e[h]) || 0 === a)
        if ("object" === y(a)) w.merge(d, a.nodeType ? [a] : a);
        else if (ye.test(a)) {
          for (
            s = s || f.appendChild(t.createElement("div")),
              o = (pe.exec(a) || ["", ""])[1].toLowerCase(),
              u = _e[o] || _e._default,
              s.innerHTML = u[1] + w.htmlPrefilter(a) + u[2],
              l = u[0];
            l--;

          )
            s = s.lastChild;
          w.merge(d, s.childNodes), ((s = f.firstChild).textContent = "");
        } else d.push(t.createTextNode(a));
    for (f.textContent = "", h = 0; (a = d[h++]); )
      if (r && w.inArray(a, r) > -1) i && i.push(a);
      else if (
        ((c = ie(a)), (s = be(f.appendChild(a), "script")), c && ge(s), n)
      )
        for (l = 0; (a = s[l++]); ) me.test(a.type || "") && n.push(a);
    return f;
  }
  var we = /^([^.]*)(?:\.(.+)|)/;
  function xe() {
    return !0;
  }
  function Se() {
    return !1;
  }
  function Ce(e, t) {
    return (
      (e ===
        (function () {
          try {
            return _.activeElement;
          } catch (e) {}
        })()) ==
      ("focus" === t)
    );
  }
  function Ee(e, t, n, r, i, a) {
    var s, o;
    if ("object" == typeof t) {
      for (o in ("string" != typeof n && ((r = r || n), (n = void 0)), t))
        Ee(e, o, n, r, t[o], a);
      return e;
    }
    if (
      (null == r && null == i
        ? ((i = n), (r = n = void 0))
        : null == i &&
          ("string" == typeof n
            ? ((i = r), (r = void 0))
            : ((i = r), (r = n), (n = void 0))),
      !1 === i)
    )
      i = Se;
    else if (!i) return e;
    return (
      1 === a &&
        ((s = i),
        (i = function (e) {
          return w().off(e), s.apply(this, arguments);
        }),
        (i.guid = s.guid || (s.guid = w.guid++))),
      e.each(function () {
        w.event.add(this, t, i, r, n);
      })
    );
  }
  function Pe(e, t, n) {
    n
      ? (Y.set(e, t, !1),
        w.event.add(e, t, {
          namespace: !1,
          handler: function (e) {
            var r,
              a,
              s = Y.get(this, t);
            if (1 & e.isTrigger && this[t]) {
              if (s.length)
                (w.event.special[t] || {}).delegateType && e.stopPropagation();
              else if (
                ((s = i.call(arguments)),
                Y.set(this, t, s),
                (r = n(this, t)),
                this[t](),
                s !== (a = Y.get(this, t)) || r ? Y.set(this, t, !1) : (a = {}),
                s !== a)
              )
                return (
                  e.stopImmediatePropagation(), e.preventDefault(), a && a.value
                );
            } else
              s.length &&
                (Y.set(this, t, {
                  value: w.event.trigger(
                    w.extend(s[0], w.Event.prototype),
                    s.slice(1),
                    this
                  ),
                }),
                e.stopImmediatePropagation());
          },
        }))
      : void 0 === Y.get(e, t) && w.event.add(e, t, xe);
  }
  (w.event = {
    global: {},
    add: function (e, t, n, r, i) {
      var a,
        s,
        o,
        u,
        c,
        l,
        f,
        d,
        h,
        p,
        m,
        _ = Y.get(e);
      if (X(e))
        for (
          n.handler && ((n = (a = n).handler), (i = a.selector)),
            i && w.find.matchesSelector(re, i),
            n.guid || (n.guid = w.guid++),
            (u = _.events) || (u = _.events = Object.create(null)),
            (s = _.handle) ||
              (s = _.handle = function (t) {
                return void 0 !== w && w.event.triggered !== t.type
                  ? w.event.dispatch.apply(e, arguments)
                  : void 0;
              }),
            c = (t = (t || "").match(N) || [""]).length;
          c--;

        )
          (h = m = (o = we.exec(t[c]) || [])[1]),
            (p = (o[2] || "").split(".").sort()),
            h &&
              ((f = w.event.special[h] || {}),
              (h = (i ? f.delegateType : f.bindType) || h),
              (f = w.event.special[h] || {}),
              (l = w.extend(
                {
                  type: h,
                  origType: m,
                  data: r,
                  handler: n,
                  guid: n.guid,
                  selector: i,
                  needsContext: i && w.expr.match.needsContext.test(i),
                  namespace: p.join("."),
                },
                a
              )),
              (d = u[h]) ||
                (((d = u[h] = []).delegateCount = 0),
                (f.setup && !1 !== f.setup.call(e, r, p, s)) ||
                  (e.addEventListener && e.addEventListener(h, s))),
              f.add &&
                (f.add.call(e, l), l.handler.guid || (l.handler.guid = n.guid)),
              i ? d.splice(d.delegateCount++, 0, l) : d.push(l),
              (w.event.global[h] = !0));
    },
    remove: function (e, t, n, r, i) {
      var a,
        s,
        o,
        u,
        c,
        l,
        f,
        d,
        h,
        p,
        m,
        _ = Y.hasData(e) && Y.get(e);
      if (_ && (u = _.events)) {
        for (c = (t = (t || "").match(N) || [""]).length; c--; )
          if (
            ((h = m = (o = we.exec(t[c]) || [])[1]),
            (p = (o[2] || "").split(".").sort()),
            h)
          ) {
            for (
              f = w.event.special[h] || {},
                d = u[(h = (r ? f.delegateType : f.bindType) || h)] || [],
                o =
                  o[2] &&
                  new RegExp("(^|\\.)" + p.join("\\.(?:.*\\.|)") + "(\\.|$)"),
                s = a = d.length;
              a--;

            )
              (l = d[a]),
                (!i && m !== l.origType) ||
                  (n && n.guid !== l.guid) ||
                  (o && !o.test(l.namespace)) ||
                  (r && r !== l.selector && ("**" !== r || !l.selector)) ||
                  (d.splice(a, 1),
                  l.selector && d.delegateCount--,
                  f.remove && f.remove.call(e, l));
            s &&
              !d.length &&
              ((f.teardown && !1 !== f.teardown.call(e, p, _.handle)) ||
                w.removeEvent(e, h, _.handle),
              delete u[h]);
          } else for (h in u) w.event.remove(e, h + t[c], n, r, !0);
        w.isEmptyObject(u) && Y.remove(e, "handle events");
      }
    },
    dispatch: function (e) {
      var t,
        n,
        r,
        i,
        a,
        s,
        o = new Array(arguments.length),
        u = w.event.fix(e),
        c = (Y.get(this, "events") || Object.create(null))[u.type] || [],
        l = w.event.special[u.type] || {};
      for (o[0] = u, t = 1; t < arguments.length; t++) o[t] = arguments[t];
      if (
        ((u.delegateTarget = this),
        !l.preDispatch || !1 !== l.preDispatch.call(this, u))
      ) {
        for (
          s = w.event.handlers.call(this, u, c), t = 0;
          (i = s[t++]) && !u.isPropagationStopped();

        )
          for (
            u.currentTarget = i.elem, n = 0;
            (a = i.handlers[n++]) && !u.isImmediatePropagationStopped();

          )
            (u.rnamespace &&
              !1 !== a.namespace &&
              !u.rnamespace.test(a.namespace)) ||
              ((u.handleObj = a),
              (u.data = a.data),
              void 0 !==
                (r = (
                  (w.event.special[a.origType] || {}).handle || a.handler
                ).apply(i.elem, o)) &&
                !1 === (u.result = r) &&
                (u.preventDefault(), u.stopPropagation()));
        return l.postDispatch && l.postDispatch.call(this, u), u.result;
      }
    },
    handlers: function (e, t) {
      var n,
        r,
        i,
        a,
        s,
        o = [],
        u = t.delegateCount,
        c = e.target;
      if (u && c.nodeType && !("click" === e.type && e.button >= 1))
        for (; c !== this; c = c.parentNode || this)
          if (1 === c.nodeType && ("click" !== e.type || !0 !== c.disabled)) {
            for (a = [], s = {}, n = 0; n < u; n++)
              void 0 === s[(i = (r = t[n]).selector + " ")] &&
                (s[i] = r.needsContext
                  ? w(i, this).index(c) > -1
                  : w.find(i, this, null, [c]).length),
                s[i] && a.push(r);
            a.length && o.push({ elem: c, handlers: a });
          }
      return (
        (c = this), u < t.length && o.push({ elem: c, handlers: t.slice(u) }), o
      );
    },
    addProp: function (e, t) {
      Object.defineProperty(w.Event.prototype, e, {
        enumerable: !0,
        configurable: !0,
        get: p(t)
          ? function () {
              if (this.originalEvent) return t(this.originalEvent);
            }
          : function () {
              if (this.originalEvent) return this.originalEvent[e];
            },
        set: function (t) {
          Object.defineProperty(this, e, {
            enumerable: !0,
            configurable: !0,
            writable: !0,
            value: t,
          });
        },
      });
    },
    fix: function (e) {
      return e[w.expando] ? e : new w.Event(e);
    },
    special: {
      load: { noBubble: !0 },
      click: {
        setup: function (e) {
          var t = this || e;
          return (
            he.test(t.type) && t.click && k(t, "input") && Pe(t, "click", xe),
            !1
          );
        },
        trigger: function (e) {
          var t = this || e;
          return (
            he.test(t.type) && t.click && k(t, "input") && Pe(t, "click"), !0
          );
        },
        _default: function (e) {
          var t = e.target;
          return (
            (he.test(t.type) &&
              t.click &&
              k(t, "input") &&
              Y.get(t, "click")) ||
            k(t, "a")
          );
        },
      },
      beforeunload: {
        postDispatch: function (e) {
          void 0 !== e.result &&
            e.originalEvent &&
            (e.originalEvent.returnValue = e.result);
        },
      },
    },
  }),
    (w.removeEvent = function (e, t, n) {
      e.removeEventListener && e.removeEventListener(t, n);
    }),
    (w.Event = function (e, t) {
      if (!(this instanceof w.Event)) return new w.Event(e, t);
      e && e.type
        ? ((this.originalEvent = e),
          (this.type = e.type),
          (this.isDefaultPrevented =
            e.defaultPrevented ||
            (void 0 === e.defaultPrevented && !1 === e.returnValue)
              ? xe
              : Se),
          (this.target =
            e.target && 3 === e.target.nodeType
              ? e.target.parentNode
              : e.target),
          (this.currentTarget = e.currentTarget),
          (this.relatedTarget = e.relatedTarget))
        : (this.type = e),
        t && w.extend(this, t),
        (this.timeStamp = (e && e.timeStamp) || Date.now()),
        (this[w.expando] = !0);
    }),
    (w.Event.prototype = {
      constructor: w.Event,
      isDefaultPrevented: Se,
      isPropagationStopped: Se,
      isImmediatePropagationStopped: Se,
      isSimulated: !1,
      preventDefault: function () {
        var e = this.originalEvent;
        (this.isDefaultPrevented = xe),
          e && !this.isSimulated && e.preventDefault();
      },
      stopPropagation: function () {
        var e = this.originalEvent;
        (this.isPropagationStopped = xe),
          e && !this.isSimulated && e.stopPropagation();
      },
      stopImmediatePropagation: function () {
        var e = this.originalEvent;
        (this.isImmediatePropagationStopped = xe),
          e && !this.isSimulated && e.stopImmediatePropagation(),
          this.stopPropagation();
      },
    }),
    w.each(
      {
        altKey: !0,
        bubbles: !0,
        cancelable: !0,
        changedTouches: !0,
        ctrlKey: !0,
        detail: !0,
        eventPhase: !0,
        metaKey: !0,
        pageX: !0,
        pageY: !0,
        shiftKey: !0,
        view: !0,
        char: !0,
        code: !0,
        charCode: !0,
        key: !0,
        keyCode: !0,
        button: !0,
        buttons: !0,
        clientX: !0,
        clientY: !0,
        offsetX: !0,
        offsetY: !0,
        pointerId: !0,
        pointerType: !0,
        screenX: !0,
        screenY: !0,
        targetTouches: !0,
        toElement: !0,
        touches: !0,
        which: !0,
      },
      w.event.addProp
    ),
    w.each({ focus: "focusin", blur: "focusout" }, function (e, t) {
      w.event.special[e] = {
        setup: function () {
          return Pe(this, e, Ce), !1;
        },
        trigger: function () {
          return Pe(this, e), !0;
        },
        _default: function () {
          return !0;
        },
        delegateType: t,
      };
    }),
    w.each(
      {
        mouseenter: "mouseover",
        mouseleave: "mouseout",
        pointerenter: "pointerover",
        pointerleave: "pointerout",
      },
      function (e, t) {
        w.event.special[e] = {
          delegateType: t,
          bindType: t,
          handle: function (e) {
            var n,
              r = this,
              i = e.relatedTarget,
              a = e.handleObj;
            return (
              (i && (i === r || w.contains(r, i))) ||
                ((e.type = a.origType),
                (n = a.handler.apply(this, arguments)),
                (e.type = t)),
              n
            );
          },
        };
      }
    ),
    w.fn.extend({
      on: function (e, t, n, r) {
        return Ee(this, e, t, n, r);
      },
      one: function (e, t, n, r) {
        return Ee(this, e, t, n, r, 1);
      },
      off: function (e, t, n) {
        var r, i;
        if (e && e.preventDefault && e.handleObj)
          return (
            (r = e.handleObj),
            w(e.delegateTarget).off(
              r.namespace ? r.origType + "." + r.namespace : r.origType,
              r.selector,
              r.handler
            ),
            this
          );
        if ("object" == typeof e) {
          for (i in e) this.off(i, t, e[i]);
          return this;
        }
        return (
          (!1 !== t && "function" != typeof t) || ((n = t), (t = void 0)),
          !1 === n && (n = Se),
          this.each(function () {
            w.event.remove(this, e, n, t);
          })
        );
      },
    });
  var ke = /<script|<style|<link/i,
    Te = /checked\s*(?:[^=]|=\s*.checked.)/i,
    je = /^\s*<!(?:\[CDATA\[|--)|(?:\]\]|--)>\s*$/g;
  function Be(e, t) {
    return (
      (k(e, "table") &&
        k(11 !== t.nodeType ? t : t.firstChild, "tr") &&
        w(e).children("tbody")[0]) ||
      e
    );
  }
  function qe(e) {
    return (e.type = (null !== e.getAttribute("type")) + "/" + e.type), e;
  }
  function De(e) {
    return (
      "true/" === (e.type || "").slice(0, 5)
        ? (e.type = e.type.slice(5))
        : e.removeAttribute("type"),
      e
    );
  }
  function Ae(e, t) {
    var n, r, i, a, s, o;
    if (1 === t.nodeType) {
      if (Y.hasData(e) && (o = Y.get(e).events))
        for (i in (Y.remove(t, "handle events"), o))
          for (n = 0, r = o[i].length; n < r; n++) w.event.add(t, i, o[i][n]);
      Q.hasData(e) && ((a = Q.access(e)), (s = w.extend({}, a)), Q.set(t, s));
    }
  }
  function $e(e, t) {
    var n = t.nodeName.toLowerCase();
    "input" === n && he.test(e.type)
      ? (t.checked = e.checked)
      : ("input" !== n && "textarea" !== n) ||
        (t.defaultValue = e.defaultValue);
  }
  function Ne(e, t, n, r) {
    t = a(t);
    var i,
      s,
      o,
      u,
      c,
      l,
      f = 0,
      d = e.length,
      m = d - 1,
      _ = t[0],
      b = p(_);
    if (b || (d > 1 && "string" == typeof _ && !h.checkClone && Te.test(_)))
      return e.each(function (i) {
        var a = e.eq(i);
        b && (t[0] = _.call(this, i, a.html())), Ne(a, t, n, r);
      });
    if (
      d &&
      ((s = (i = ve(t, e[0].ownerDocument, !1, e, r)).firstChild),
      1 === i.childNodes.length && (i = s),
      s || r)
    ) {
      for (u = (o = w.map(be(i, "script"), qe)).length; f < d; f++)
        (c = i),
          f !== m &&
            ((c = w.clone(c, !0, !0)), u && w.merge(o, be(c, "script"))),
          n.call(e[f], c, f);
      if (u)
        for (l = o[o.length - 1].ownerDocument, w.map(o, De), f = 0; f < u; f++)
          (c = o[f]),
            me.test(c.type || "") &&
              !Y.access(c, "globalEval") &&
              w.contains(l, c) &&
              (c.src && "module" !== (c.type || "").toLowerCase()
                ? w._evalUrl &&
                  !c.noModule &&
                  w._evalUrl(
                    c.src,
                    { nonce: c.nonce || c.getAttribute("nonce") },
                    l
                  )
                : g(c.textContent.replace(je, ""), c, l));
    }
    return e;
  }
  function Le(e, t, n) {
    for (var r, i = t ? w.filter(t, e) : e, a = 0; null != (r = i[a]); a++)
      n || 1 !== r.nodeType || w.cleanData(be(r)),
        r.parentNode &&
          (n && ie(r) && ge(be(r, "script")), r.parentNode.removeChild(r));
    return e;
  }
  w.extend({
    htmlPrefilter: function (e) {
      return e;
    },
    clone: function (e, t, n) {
      var r,
        i,
        a,
        s,
        o = e.cloneNode(!0),
        u = ie(e);
      if (
        !(
          h.noCloneChecked ||
          (1 !== e.nodeType && 11 !== e.nodeType) ||
          w.isXMLDoc(e)
        )
      )
        for (s = be(o), r = 0, i = (a = be(e)).length; r < i; r++)
          $e(a[r], s[r]);
      if (t)
        if (n)
          for (a = a || be(e), s = s || be(o), r = 0, i = a.length; r < i; r++)
            Ae(a[r], s[r]);
        else Ae(e, o);
      return (
        (s = be(o, "script")).length > 0 && ge(s, !u && be(e, "script")), o
      );
    },
    cleanData: function (e) {
      for (var t, n, r, i = w.event.special, a = 0; void 0 !== (n = e[a]); a++)
        if (X(n)) {
          if ((t = n[Y.expando])) {
            if (t.events)
              for (r in t.events)
                i[r] ? w.event.remove(n, r) : w.removeEvent(n, r, t.handle);
            n[Y.expando] = void 0;
          }
          n[Q.expando] && (n[Q.expando] = void 0);
        }
    },
  }),
    w.fn.extend({
      detach: function (e) {
        return Le(this, e, !0);
      },
      remove: function (e) {
        return Le(this, e);
      },
      text: function (e) {
        return F(
          this,
          function (e) {
            return void 0 === e
              ? w.text(this)
              : this.empty().each(function () {
                  (1 !== this.nodeType &&
                    11 !== this.nodeType &&
                    9 !== this.nodeType) ||
                    (this.textContent = e);
                });
          },
          null,
          e,
          arguments.length
        );
      },
      append: function () {
        return Ne(this, arguments, function (e) {
          (1 !== this.nodeType &&
            11 !== this.nodeType &&
            9 !== this.nodeType) ||
            Be(this, e).appendChild(e);
        });
      },
      prepend: function () {
        return Ne(this, arguments, function (e) {
          if (
            1 === this.nodeType ||
            11 === this.nodeType ||
            9 === this.nodeType
          ) {
            var t = Be(this, e);
            t.insertBefore(e, t.firstChild);
          }
        });
      },
      before: function () {
        return Ne(this, arguments, function (e) {
          this.parentNode && this.parentNode.insertBefore(e, this);
        });
      },
      after: function () {
        return Ne(this, arguments, function (e) {
          this.parentNode && this.parentNode.insertBefore(e, this.nextSibling);
        });
      },
      empty: function () {
        for (var e, t = 0; null != (e = this[t]); t++)
          1 === e.nodeType && (w.cleanData(be(e, !1)), (e.textContent = ""));
        return this;
      },
      clone: function (e, t) {
        return (
          (e = null != e && e),
          (t = null == t ? e : t),
          this.map(function () {
            return w.clone(this, e, t);
          })
        );
      },
      html: function (e) {
        return F(
          this,
          function (e) {
            var t = this[0] || {},
              n = 0,
              r = this.length;
            if (void 0 === e && 1 === t.nodeType) return t.innerHTML;
            if (
              "string" == typeof e &&
              !ke.test(e) &&
              !_e[(pe.exec(e) || ["", ""])[1].toLowerCase()]
            ) {
              e = w.htmlPrefilter(e);
              try {
                for (; n < r; n++)
                  1 === (t = this[n] || {}).nodeType &&
                    (w.cleanData(be(t, !1)), (t.innerHTML = e));
                t = 0;
              } catch (e) {}
            }
            t && this.empty().append(e);
          },
          null,
          e,
          arguments.length
        );
      },
      replaceWith: function () {
        var e = [];
        return Ne(
          this,
          arguments,
          function (t) {
            var n = this.parentNode;
            w.inArray(this, e) < 0 &&
              (w.cleanData(be(this)), n && n.replaceChild(t, this));
          },
          e
        );
      },
    }),
    w.each(
      {
        appendTo: "append",
        prependTo: "prepend",
        insertBefore: "before",
        insertAfter: "after",
        replaceAll: "replaceWith",
      },
      function (e, t) {
        w.fn[e] = function (e) {
          for (var n, r = [], i = w(e), a = i.length - 1, o = 0; o <= a; o++)
            (n = o === a ? this : this.clone(!0)),
              w(i[o])[t](n),
              s.apply(r, n.get());
          return this.pushStack(r);
        };
      }
    );
  var Re = new RegExp("^(" + ee + ")(?!px)[a-z%]+$", "i"),
    Oe = function (t) {
      var n = t.ownerDocument.defaultView;
      return (n && n.opener) || (n = e), n.getComputedStyle(t);
    },
    Me = function (e, t, n) {
      var r,
        i,
        a = {};
      for (i in t) (a[i] = e.style[i]), (e.style[i] = t[i]);
      for (i in ((r = n.call(e)), t)) e.style[i] = a[i];
      return r;
    },
    Ie = new RegExp(ne.join("|"), "i");
  function He(e, t, n) {
    var r,
      i,
      a,
      s,
      o = e.style;
    return (
      (n = n || Oe(e)) &&
        ("" !== (s = n.getPropertyValue(t) || n[t]) ||
          ie(e) ||
          (s = w.style(e, t)),
        !h.pixelBoxStyles() &&
          Re.test(s) &&
          Ie.test(t) &&
          ((r = o.width),
          (i = o.minWidth),
          (a = o.maxWidth),
          (o.minWidth = o.maxWidth = o.width = s),
          (s = n.width),
          (o.width = r),
          (o.minWidth = i),
          (o.maxWidth = a))),
      void 0 !== s ? s + "" : s
    );
  }
  function Fe(e, t) {
    return {
      get: function () {
        if (!e()) return (this.get = t).apply(this, arguments);
        delete this.get;
      },
    };
  }
  !(function () {
    function t() {
      if (l) {
        (c.style.cssText =
          "position:absolute;left:-11111px;width:60px;margin-top:1px;padding:0;border:0"),
          (l.style.cssText =
            "position:relative;display:block;box-sizing:border-box;overflow:scroll;margin:auto;border:1px;padding:1px;width:60%;top:1%"),
          re.appendChild(c).appendChild(l);
        var t = e.getComputedStyle(l);
        (r = "1%" !== t.top),
          (u = 12 === n(t.marginLeft)),
          (l.style.right = "60%"),
          (s = 36 === n(t.right)),
          (i = 36 === n(t.width)),
          (l.style.position = "absolute"),
          (a = 12 === n(l.offsetWidth / 3)),
          re.removeChild(c),
          (l = null);
      }
    }
    function n(e) {
      return Math.round(parseFloat(e));
    }
    var r,
      i,
      a,
      s,
      o,
      u,
      c = _.createElement("div"),
      l = _.createElement("div");
    l.style &&
      ((l.style.backgroundClip = "content-box"),
      (l.cloneNode(!0).style.backgroundClip = ""),
      (h.clearCloneStyle = "content-box" === l.style.backgroundClip),
      w.extend(h, {
        boxSizingReliable: function () {
          return t(), i;
        },
        pixelBoxStyles: function () {
          return t(), s;
        },
        pixelPosition: function () {
          return t(), r;
        },
        reliableMarginLeft: function () {
          return t(), u;
        },
        scrollboxSize: function () {
          return t(), a;
        },
        reliableTrDimensions: function () {
          var t, n, r, i;
          return (
            null == o &&
              ((t = _.createElement("table")),
              (n = _.createElement("tr")),
              (r = _.createElement("div")),
              (t.style.cssText =
                "position:absolute;left:-11111px;border-collapse:separate"),
              (n.style.cssText = "border:1px solid"),
              (n.style.height = "1px"),
              (r.style.height = "9px"),
              (r.style.display = "block"),
              re.appendChild(t).appendChild(n).appendChild(r),
              (i = e.getComputedStyle(n)),
              (o =
                parseInt(i.height, 10) +
                  parseInt(i.borderTopWidth, 10) +
                  parseInt(i.borderBottomWidth, 10) ===
                n.offsetHeight),
              re.removeChild(t)),
            o
          );
        },
      }));
  })();
  var We = ["Webkit", "Moz", "ms"],
    ze = _.createElement("div").style,
    Ue = {};
  function Ve(e) {
    var t = w.cssProps[e] || Ue[e];
    return (
      t ||
      (e in ze
        ? e
        : (Ue[e] =
            (function (e) {
              for (
                var t = e[0].toUpperCase() + e.slice(1), n = We.length;
                n--;

              )
                if ((e = We[n] + t) in ze) return e;
            })(e) || e))
    );
  }
  var Xe = /^(none|table(?!-c[ea]).+)/,
    Ge = /^--/,
    Ye = { position: "absolute", visibility: "hidden", display: "block" },
    Qe = { letterSpacing: "0", fontWeight: "400" };
  function Je(e, t, n) {
    var r = te.exec(t);
    return r ? Math.max(0, r[2] - (n || 0)) + (r[3] || "px") : t;
  }
  function Ke(e, t, n, r, i, a) {
    var s = "width" === t ? 1 : 0,
      o = 0,
      u = 0;
    if (n === (r ? "border" : "content")) return 0;
    for (; s < 4; s += 2)
      "margin" === n && (u += w.css(e, n + ne[s], !0, i)),
        r
          ? ("content" === n && (u -= w.css(e, "padding" + ne[s], !0, i)),
            "margin" !== n &&
              (u -= w.css(e, "border" + ne[s] + "Width", !0, i)))
          : ((u += w.css(e, "padding" + ne[s], !0, i)),
            "padding" !== n
              ? (u += w.css(e, "border" + ne[s] + "Width", !0, i))
              : (o += w.css(e, "border" + ne[s] + "Width", !0, i)));
    return (
      !r &&
        a >= 0 &&
        (u +=
          Math.max(
            0,
            Math.ceil(
              e["offset" + t[0].toUpperCase() + t.slice(1)] - a - u - o - 0.5
            )
          ) || 0),
      u
    );
  }
  function Ze(e, t, n) {
    var r = Oe(e),
      i =
        (!h.boxSizingReliable() || n) &&
        "border-box" === w.css(e, "boxSizing", !1, r),
      a = i,
      s = He(e, t, r),
      o = "offset" + t[0].toUpperCase() + t.slice(1);
    if (Re.test(s)) {
      if (!n) return s;
      s = "auto";
    }
    return (
      ((!h.boxSizingReliable() && i) ||
        (!h.reliableTrDimensions() && k(e, "tr")) ||
        "auto" === s ||
        (!parseFloat(s) && "inline" === w.css(e, "display", !1, r))) &&
        e.getClientRects().length &&
        ((i = "border-box" === w.css(e, "boxSizing", !1, r)),
        (a = o in e) && (s = e[o])),
      (s = parseFloat(s) || 0) +
        Ke(e, t, n || (i ? "border" : "content"), a, r, s) +
        "px"
    );
  }
  function et(e, t, n, r, i) {
    return new et.prototype.init(e, t, n, r, i);
  }
  w.extend({
    cssHooks: {
      opacity: {
        get: function (e, t) {
          if (t) {
            var n = He(e, "opacity");
            return "" === n ? "1" : n;
          }
        },
      },
    },
    cssNumber: {
      animationIterationCount: !0,
      columnCount: !0,
      fillOpacity: !0,
      flexGrow: !0,
      flexShrink: !0,
      fontWeight: !0,
      gridArea: !0,
      gridColumn: !0,
      gridColumnEnd: !0,
      gridColumnStart: !0,
      gridRow: !0,
      gridRowEnd: !0,
      gridRowStart: !0,
      lineHeight: !0,
      opacity: !0,
      order: !0,
      orphans: !0,
      widows: !0,
      zIndex: !0,
      zoom: !0,
    },
    cssProps: {},
    style: function (e, t, n, r) {
      if (e && 3 !== e.nodeType && 8 !== e.nodeType && e.style) {
        var i,
          a,
          s,
          o = V(t),
          u = Ge.test(t),
          c = e.style;
        if (
          (u || (t = Ve(o)), (s = w.cssHooks[t] || w.cssHooks[o]), void 0 === n)
        )
          return s && "get" in s && void 0 !== (i = s.get(e, !1, r)) ? i : c[t];
        "string" === (a = typeof n) &&
          (i = te.exec(n)) &&
          i[1] &&
          ((n = oe(e, t, i)), (a = "number")),
          null != n &&
            n == n &&
            ("number" !== a ||
              u ||
              (n += (i && i[3]) || (w.cssNumber[o] ? "" : "px")),
            h.clearCloneStyle ||
              "" !== n ||
              0 !== t.indexOf("background") ||
              (c[t] = "inherit"),
            (s && "set" in s && void 0 === (n = s.set(e, n, r))) ||
              (u ? c.setProperty(t, n) : (c[t] = n)));
      }
    },
    css: function (e, t, n, r) {
      var i,
        a,
        s,
        o = V(t);
      return (
        Ge.test(t) || (t = Ve(o)),
        (s = w.cssHooks[t] || w.cssHooks[o]) &&
          "get" in s &&
          (i = s.get(e, !0, n)),
        void 0 === i && (i = He(e, t, r)),
        "normal" === i && t in Qe && (i = Qe[t]),
        "" === n || n
          ? ((a = parseFloat(i)), !0 === n || isFinite(a) ? a || 0 : i)
          : i
      );
    },
  }),
    w.each(["height", "width"], function (e, t) {
      w.cssHooks[t] = {
        get: function (e, n, r) {
          if (n)
            return !Xe.test(w.css(e, "display")) ||
              (e.getClientRects().length && e.getBoundingClientRect().width)
              ? Ze(e, t, r)
              : Me(e, Ye, function () {
                  return Ze(e, t, r);
                });
        },
        set: function (e, n, r) {
          var i,
            a = Oe(e),
            s = !h.scrollboxSize() && "absolute" === a.position,
            o = (s || r) && "border-box" === w.css(e, "boxSizing", !1, a),
            u = r ? Ke(e, t, r, o, a) : 0;
          return (
            o &&
              s &&
              (u -= Math.ceil(
                e["offset" + t[0].toUpperCase() + t.slice(1)] -
                  parseFloat(a[t]) -
                  Ke(e, t, "border", !1, a) -
                  0.5
              )),
            u &&
              (i = te.exec(n)) &&
              "px" !== (i[3] || "px") &&
              ((e.style[t] = n), (n = w.css(e, t))),
            Je(0, n, u)
          );
        },
      };
    }),
    (w.cssHooks.marginLeft = Fe(h.reliableMarginLeft, function (e, t) {
      if (t)
        return (
          (parseFloat(He(e, "marginLeft")) ||
            e.getBoundingClientRect().left -
              Me(e, { marginLeft: 0 }, function () {
                return e.getBoundingClientRect().left;
              })) + "px"
        );
    })),
    w.each({ margin: "", padding: "", border: "Width" }, function (e, t) {
      (w.cssHooks[e + t] = {
        expand: function (n) {
          for (
            var r = 0, i = {}, a = "string" == typeof n ? n.split(" ") : [n];
            r < 4;
            r++
          )
            i[e + ne[r] + t] = a[r] || a[r - 2] || a[0];
          return i;
        },
      }),
        "margin" !== e && (w.cssHooks[e + t].set = Je);
    }),
    w.fn.extend({
      css: function (e, t) {
        return F(
          this,
          function (e, t, n) {
            var r,
              i,
              a = {},
              s = 0;
            if (Array.isArray(t)) {
              for (r = Oe(e), i = t.length; s < i; s++)
                a[t[s]] = w.css(e, t[s], !1, r);
              return a;
            }
            return void 0 !== n ? w.style(e, t, n) : w.css(e, t);
          },
          e,
          t,
          arguments.length > 1
        );
      },
    }),
    (w.Tween = et),
    (et.prototype = {
      constructor: et,
      init: function (e, t, n, r, i, a) {
        (this.elem = e),
          (this.prop = n),
          (this.easing = i || w.easing._default),
          (this.options = t),
          (this.start = this.now = this.cur()),
          (this.end = r),
          (this.unit = a || (w.cssNumber[n] ? "" : "px"));
      },
      cur: function () {
        var e = et.propHooks[this.prop];
        return e && e.get ? e.get(this) : et.propHooks._default.get(this);
      },
      run: function (e) {
        var t,
          n = et.propHooks[this.prop];
        return (
          this.options.duration
            ? (this.pos = t = w.easing[this.easing](
                e,
                this.options.duration * e,
                0,
                1,
                this.options.duration
              ))
            : (this.pos = t = e),
          (this.now = (this.end - this.start) * t + this.start),
          this.options.step &&
            this.options.step.call(this.elem, this.now, this),
          n && n.set ? n.set(this) : et.propHooks._default.set(this),
          this
        );
      },
    }),
    (et.prototype.init.prototype = et.prototype),
    (et.propHooks = {
      _default: {
        get: function (e) {
          var t;
          return 1 !== e.elem.nodeType ||
            (null != e.elem[e.prop] && null == e.elem.style[e.prop])
            ? e.elem[e.prop]
            : (t = w.css(e.elem, e.prop, "")) && "auto" !== t
            ? t
            : 0;
        },
        set: function (e) {
          w.fx.step[e.prop]
            ? w.fx.step[e.prop](e)
            : 1 !== e.elem.nodeType ||
              (!w.cssHooks[e.prop] && null == e.elem.style[Ve(e.prop)])
            ? (e.elem[e.prop] = e.now)
            : w.style(e.elem, e.prop, e.now + e.unit);
        },
      },
    }),
    (et.propHooks.scrollTop = et.propHooks.scrollLeft = {
      set: function (e) {
        e.elem.nodeType && e.elem.parentNode && (e.elem[e.prop] = e.now);
      },
    }),
    (w.easing = {
      linear: function (e) {
        return e;
      },
      swing: function (e) {
        return 0.5 - Math.cos(e * Math.PI) / 2;
      },
      _default: "swing",
    }),
    (w.fx = et.prototype.init),
    (w.fx.step = {});
  var tt,
    nt,
    rt = /^(?:toggle|show|hide)$/,
    it = /queueHooks$/;
  function at() {
    nt &&
      (!1 === _.hidden && e.requestAnimationFrame
        ? e.requestAnimationFrame(at)
        : e.setTimeout(at, w.fx.interval),
      w.fx.tick());
  }
  function st() {
    return (
      e.setTimeout(function () {
        tt = void 0;
      }),
      (tt = Date.now())
    );
  }
  function ot(e, t) {
    var n,
      r = 0,
      i = { height: e };
    for (t = t ? 1 : 0; r < 4; r += 2 - t)
      i["margin" + (n = ne[r])] = i["padding" + n] = e;
    return t && (i.opacity = i.width = e), i;
  }
  function ut(e, t, n) {
    for (
      var r,
        i = (ct.tweeners[t] || []).concat(ct.tweeners["*"]),
        a = 0,
        s = i.length;
      a < s;
      a++
    )
      if ((r = i[a].call(n, t, e))) return r;
  }
  function ct(e, t, n) {
    var r,
      i,
      a = 0,
      s = ct.prefilters.length,
      o = w.Deferred().always(function () {
        delete u.elem;
      }),
      u = function () {
        if (i) return !1;
        for (
          var t = tt || st(),
            n = Math.max(0, c.startTime + c.duration - t),
            r = 1 - (n / c.duration || 0),
            a = 0,
            s = c.tweens.length;
          a < s;
          a++
        )
          c.tweens[a].run(r);
        return (
          o.notifyWith(e, [c, r, n]),
          r < 1 && s
            ? n
            : (s || o.notifyWith(e, [c, 1, 0]), o.resolveWith(e, [c]), !1)
        );
      },
      c = o.promise({
        elem: e,
        props: w.extend({}, t),
        opts: w.extend(!0, { specialEasing: {}, easing: w.easing._default }, n),
        originalProperties: t,
        originalOptions: n,
        startTime: tt || st(),
        duration: n.duration,
        tweens: [],
        createTween: function (t, n) {
          var r = w.Tween(
            e,
            c.opts,
            t,
            n,
            c.opts.specialEasing[t] || c.opts.easing
          );
          return c.tweens.push(r), r;
        },
        stop: function (t) {
          var n = 0,
            r = t ? c.tweens.length : 0;
          if (i) return this;
          for (i = !0; n < r; n++) c.tweens[n].run(1);
          return (
            t
              ? (o.notifyWith(e, [c, 1, 0]), o.resolveWith(e, [c, t]))
              : o.rejectWith(e, [c, t]),
            this
          );
        },
      }),
      l = c.props;
    for (
      !(function (e, t) {
        var n, r, i, a, s;
        for (n in e)
          if (
            ((i = t[(r = V(n))]),
            (a = e[n]),
            Array.isArray(a) && ((i = a[1]), (a = e[n] = a[0])),
            n !== r && ((e[r] = a), delete e[n]),
            (s = w.cssHooks[r]) && ("expand" in s))
          )
            for (n in ((a = s.expand(a)), delete e[r], a))
              (n in e) || ((e[n] = a[n]), (t[n] = i));
          else t[r] = i;
      })(l, c.opts.specialEasing);
      a < s;
      a++
    )
      if ((r = ct.prefilters[a].call(c, e, l, c.opts)))
        return (
          p(r.stop) &&
            (w._queueHooks(c.elem, c.opts.queue).stop = r.stop.bind(r)),
          r
        );
    return (
      w.map(l, ut, c),
      p(c.opts.start) && c.opts.start.call(e, c),
      c
        .progress(c.opts.progress)
        .done(c.opts.done, c.opts.complete)
        .fail(c.opts.fail)
        .always(c.opts.always),
      w.fx.timer(w.extend(u, { elem: e, anim: c, queue: c.opts.queue })),
      c
    );
  }
  (w.Animation = w.extend(ct, {
    tweeners: {
      "*": [
        function (e, t) {
          var n = this.createTween(e, t);
          return oe(n.elem, e, te.exec(t), n), n;
        },
      ],
    },
    tweener: function (e, t) {
      p(e) ? ((t = e), (e = ["*"])) : (e = e.match(N));
      for (var n, r = 0, i = e.length; r < i; r++)
        (n = e[r]),
          (ct.tweeners[n] = ct.tweeners[n] || []),
          ct.tweeners[n].unshift(t);
    },
    prefilters: [
      function (e, t, n) {
        var r,
          i,
          a,
          s,
          o,
          u,
          c,
          l,
          f = "width" in t || "height" in t,
          d = this,
          h = {},
          p = e.style,
          m = e.nodeType && se(e),
          _ = Y.get(e, "fxshow");
        for (r in (n.queue ||
          (null == (s = w._queueHooks(e, "fx")).unqueued &&
            ((s.unqueued = 0),
            (o = s.empty.fire),
            (s.empty.fire = function () {
              s.unqueued || o();
            })),
          s.unqueued++,
          d.always(function () {
            d.always(function () {
              s.unqueued--, w.queue(e, "fx").length || s.empty.fire();
            });
          })),
        t))
          if (((i = t[r]), rt.test(i))) {
            if (
              (delete t[r],
              (a = a || "toggle" === i),
              i === (m ? "hide" : "show"))
            ) {
              if ("show" !== i || !_ || void 0 === _[r]) continue;
              m = !0;
            }
            h[r] = (_ && _[r]) || w.style(e, r);
          }
        if ((u = !w.isEmptyObject(t)) || !w.isEmptyObject(h))
          for (r in (f &&
            1 === e.nodeType &&
            ((n.overflow = [p.overflow, p.overflowX, p.overflowY]),
            null == (c = _ && _.display) && (c = Y.get(e, "display")),
            "none" === (l = w.css(e, "display")) &&
              (c
                ? (l = c)
                : (le([e], !0),
                  (c = e.style.display || c),
                  (l = w.css(e, "display")),
                  le([e]))),
            ("inline" === l || ("inline-block" === l && null != c)) &&
              "none" === w.css(e, "float") &&
              (u ||
                (d.done(function () {
                  p.display = c;
                }),
                null == c && ((l = p.display), (c = "none" === l ? "" : l))),
              (p.display = "inline-block"))),
          n.overflow &&
            ((p.overflow = "hidden"),
            d.always(function () {
              (p.overflow = n.overflow[0]),
                (p.overflowX = n.overflow[1]),
                (p.overflowY = n.overflow[2]);
            })),
          (u = !1),
          h))
            u ||
              (_
                ? "hidden" in _ && (m = _.hidden)
                : (_ = Y.access(e, "fxshow", { display: c })),
              a && (_.hidden = !m),
              m && le([e], !0),
              d.done(function () {
                for (r in (m || le([e]), Y.remove(e, "fxshow"), h))
                  w.style(e, r, h[r]);
              })),
              (u = ut(m ? _[r] : 0, r, d)),
              r in _ ||
                ((_[r] = u.start), m && ((u.end = u.start), (u.start = 0)));
      },
    ],
    prefilter: function (e, t) {
      t ? ct.prefilters.unshift(e) : ct.prefilters.push(e);
    },
  })),
    (w.speed = function (e, t, n) {
      var r =
        e && "object" == typeof e
          ? w.extend({}, e)
          : {
              complete: n || (!n && t) || (p(e) && e),
              duration: e,
              easing: (n && t) || (t && !p(t) && t),
            };
      return (
        w.fx.off
          ? (r.duration = 0)
          : "number" != typeof r.duration &&
            (r.duration in w.fx.speeds
              ? (r.duration = w.fx.speeds[r.duration])
              : (r.duration = w.fx.speeds._default)),
        (null != r.queue && !0 !== r.queue) || (r.queue = "fx"),
        (r.old = r.complete),
        (r.complete = function () {
          p(r.old) && r.old.call(this), r.queue && w.dequeue(this, r.queue);
        }),
        r
      );
    }),
    w.fn.extend({
      fadeTo: function (e, t, n, r) {
        return this.filter(se)
          .css("opacity", 0)
          .show()
          .end()
          .animate({ opacity: t }, e, n, r);
      },
      animate: function (e, t, n, r) {
        var i = w.isEmptyObject(e),
          a = w.speed(t, n, r),
          s = function () {
            var t = ct(this, w.extend({}, e), a);
            (i || Y.get(this, "finish")) && t.stop(!0);
          };
        return (
          (s.finish = s),
          i || !1 === a.queue ? this.each(s) : this.queue(a.queue, s)
        );
      },
      stop: function (e, t, n) {
        var r = function (e) {
          var t = e.stop;
          delete e.stop, t(n);
        };
        return (
          "string" != typeof e && ((n = t), (t = e), (e = void 0)),
          t && this.queue(e || "fx", []),
          this.each(function () {
            var t = !0,
              i = null != e && e + "queueHooks",
              a = w.timers,
              s = Y.get(this);
            if (i) s[i] && s[i].stop && r(s[i]);
            else for (i in s) s[i] && s[i].stop && it.test(i) && r(s[i]);
            for (i = a.length; i--; )
              a[i].elem !== this ||
                (null != e && a[i].queue !== e) ||
                (a[i].anim.stop(n), (t = !1), a.splice(i, 1));
            (!t && n) || w.dequeue(this, e);
          })
        );
      },
      finish: function (e) {
        return (
          !1 !== e && (e = e || "fx"),
          this.each(function () {
            var t,
              n = Y.get(this),
              r = n[e + "queue"],
              i = n[e + "queueHooks"],
              a = w.timers,
              s = r ? r.length : 0;
            for (
              n.finish = !0,
                w.queue(this, e, []),
                i && i.stop && i.stop.call(this, !0),
                t = a.length;
              t--;

            )
              a[t].elem === this &&
                a[t].queue === e &&
                (a[t].anim.stop(!0), a.splice(t, 1));
            for (t = 0; t < s; t++)
              r[t] && r[t].finish && r[t].finish.call(this);
            delete n.finish;
          })
        );
      },
    }),
    w.each(["toggle", "show", "hide"], function (e, t) {
      var n = w.fn[t];
      w.fn[t] = function (e, r, i) {
        return null == e || "boolean" == typeof e
          ? n.apply(this, arguments)
          : this.animate(ot(t, !0), e, r, i);
      };
    }),
    w.each(
      {
        slideDown: ot("show"),
        slideUp: ot("hide"),
        slideToggle: ot("toggle"),
        fadeIn: { opacity: "show" },
        fadeOut: { opacity: "hide" },
        fadeToggle: { opacity: "toggle" },
      },
      function (e, t) {
        w.fn[e] = function (e, n, r) {
          return this.animate(t, e, n, r);
        };
      }
    ),
    (w.timers = []),
    (w.fx.tick = function () {
      var e,
        t = 0,
        n = w.timers;
      for (tt = Date.now(); t < n.length; t++)
        (e = n[t])() || n[t] !== e || n.splice(t--, 1);
      n.length || w.fx.stop(), (tt = void 0);
    }),
    (w.fx.timer = function (e) {
      w.timers.push(e), w.fx.start();
    }),
    (w.fx.interval = 13),
    (w.fx.start = function () {
      nt || ((nt = !0), at());
    }),
    (w.fx.stop = function () {
      nt = null;
    }),
    (w.fx.speeds = { slow: 600, fast: 200, _default: 400 }),
    (w.fn.delay = function (t, n) {
      return (
        (t = (w.fx && w.fx.speeds[t]) || t),
        (n = n || "fx"),
        this.queue(n, function (n, r) {
          var i = e.setTimeout(n, t);
          r.stop = function () {
            e.clearTimeout(i);
          };
        })
      );
    }),
    (function () {
      var e = _.createElement("input"),
        t = _.createElement("select").appendChild(_.createElement("option"));
      (e.type = "checkbox"),
        (h.checkOn = "" !== e.value),
        (h.optSelected = t.selected),
        ((e = _.createElement("input")).value = "t"),
        (e.type = "radio"),
        (h.radioValue = "t" === e.value);
    })();
  var lt,
    ft = w.expr.attrHandle;
  w.fn.extend({
    attr: function (e, t) {
      return F(this, w.attr, e, t, arguments.length > 1);
    },
    removeAttr: function (e) {
      return this.each(function () {
        w.removeAttr(this, e);
      });
    },
  }),
    w.extend({
      attr: function (e, t, n) {
        var r,
          i,
          a = e.nodeType;
        if (3 !== a && 8 !== a && 2 !== a)
          return void 0 === e.getAttribute
            ? w.prop(e, t, n)
            : ((1 === a && w.isXMLDoc(e)) ||
                (i =
                  w.attrHooks[t.toLowerCase()] ||
                  (w.expr.match.bool.test(t) ? lt : void 0)),
              void 0 !== n
                ? null === n
                  ? void w.removeAttr(e, t)
                  : i && "set" in i && void 0 !== (r = i.set(e, n, t))
                  ? r
                  : (e.setAttribute(t, n + ""), n)
                : i && "get" in i && null !== (r = i.get(e, t))
                ? r
                : null == (r = w.find.attr(e, t))
                ? void 0
                : r);
      },
      attrHooks: {
        type: {
          set: function (e, t) {
            if (!h.radioValue && "radio" === t && k(e, "input")) {
              var n = e.value;
              return e.setAttribute("type", t), n && (e.value = n), t;
            }
          },
        },
      },
      removeAttr: function (e, t) {
        var n,
          r = 0,
          i = t && t.match(N);
        if (i && 1 === e.nodeType) for (; (n = i[r++]); ) e.removeAttribute(n);
      },
    }),
    (lt = {
      set: function (e, t, n) {
        return !1 === t ? w.removeAttr(e, n) : e.setAttribute(n, n), n;
      },
    }),
    w.each(w.expr.match.bool.source.match(/\w+/g), function (e, t) {
      var n = ft[t] || w.find.attr;
      ft[t] = function (e, t, r) {
        var i,
          a,
          s = t.toLowerCase();
        return (
          r ||
            ((a = ft[s]),
            (ft[s] = i),
            (i = null != n(e, t, r) ? s : null),
            (ft[s] = a)),
          i
        );
      };
    });
  var dt = /^(?:input|select|textarea|button)$/i,
    ht = /^(?:a|area)$/i;
  function pt(e) {
    return (e.match(N) || []).join(" ");
  }
  function mt(e) {
    return (e.getAttribute && e.getAttribute("class")) || "";
  }
  function _t(e) {
    return Array.isArray(e) ? e : ("string" == typeof e && e.match(N)) || [];
  }
  w.fn.extend({
    prop: function (e, t) {
      return F(this, w.prop, e, t, arguments.length > 1);
    },
    removeProp: function (e) {
      return this.each(function () {
        delete this[w.propFix[e] || e];
      });
    },
  }),
    w.extend({
      prop: function (e, t, n) {
        var r,
          i,
          a = e.nodeType;
        if (3 !== a && 8 !== a && 2 !== a)
          return (
            (1 === a && w.isXMLDoc(e)) ||
              ((t = w.propFix[t] || t), (i = w.propHooks[t])),
            void 0 !== n
              ? i && "set" in i && void 0 !== (r = i.set(e, n, t))
                ? r
                : (e[t] = n)
              : i && "get" in i && null !== (r = i.get(e, t))
              ? r
              : e[t]
          );
      },
      propHooks: {
        tabIndex: {
          get: function (e) {
            var t = w.find.attr(e, "tabindex");
            return t
              ? parseInt(t, 10)
              : dt.test(e.nodeName) || (ht.test(e.nodeName) && e.href)
              ? 0
              : -1;
          },
        },
      },
      propFix: { for: "htmlFor", class: "className" },
    }),
    h.optSelected ||
      (w.propHooks.selected = {
        get: function (e) {
          var t = e.parentNode;
          return t && t.parentNode && t.parentNode.selectedIndex, null;
        },
        set: function (e) {
          var t = e.parentNode;
          t && (t.selectedIndex, t.parentNode && t.parentNode.selectedIndex);
        },
      }),
    w.each(
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
        w.propFix[this.toLowerCase()] = this;
      }
    ),
    w.fn.extend({
      addClass: function (e) {
        var t,
          n,
          r,
          i,
          a,
          s,
          o,
          u = 0;
        if (p(e))
          return this.each(function (t) {
            w(this).addClass(e.call(this, t, mt(this)));
          });
        if ((t = _t(e)).length)
          for (; (n = this[u++]); )
            if (((i = mt(n)), (r = 1 === n.nodeType && " " + pt(i) + " "))) {
              for (s = 0; (a = t[s++]); )
                r.indexOf(" " + a + " ") < 0 && (r += a + " ");
              i !== (o = pt(r)) && n.setAttribute("class", o);
            }
        return this;
      },
      removeClass: function (e) {
        var t,
          n,
          r,
          i,
          a,
          s,
          o,
          u = 0;
        if (p(e))
          return this.each(function (t) {
            w(this).removeClass(e.call(this, t, mt(this)));
          });
        if (!arguments.length) return this.attr("class", "");
        if ((t = _t(e)).length)
          for (; (n = this[u++]); )
            if (((i = mt(n)), (r = 1 === n.nodeType && " " + pt(i) + " "))) {
              for (s = 0; (a = t[s++]); )
                for (; r.indexOf(" " + a + " ") > -1; )
                  r = r.replace(" " + a + " ", " ");
              i !== (o = pt(r)) && n.setAttribute("class", o);
            }
        return this;
      },
      toggleClass: function (e, t) {
        var n = typeof e,
          r = "string" === n || Array.isArray(e);
        return "boolean" == typeof t && r
          ? t
            ? this.addClass(e)
            : this.removeClass(e)
          : p(e)
          ? this.each(function (n) {
              w(this).toggleClass(e.call(this, n, mt(this), t), t);
            })
          : this.each(function () {
              var t, i, a, s;
              if (r)
                for (i = 0, a = w(this), s = _t(e); (t = s[i++]); )
                  a.hasClass(t) ? a.removeClass(t) : a.addClass(t);
              else
                (void 0 !== e && "boolean" !== n) ||
                  ((t = mt(this)) && Y.set(this, "__className__", t),
                  this.setAttribute &&
                    this.setAttribute(
                      "class",
                      t || !1 === e ? "" : Y.get(this, "__className__") || ""
                    ));
            });
      },
      hasClass: function (e) {
        var t,
          n,
          r = 0;
        for (t = " " + e + " "; (n = this[r++]); )
          if (1 === n.nodeType && (" " + pt(mt(n)) + " ").indexOf(t) > -1)
            return !0;
        return !1;
      },
    });
  var bt = /\r/g;
  w.fn.extend({
    val: function (e) {
      var t,
        n,
        r,
        i = this[0];
      return arguments.length
        ? ((r = p(e)),
          this.each(function (n) {
            var i;
            1 === this.nodeType &&
              (null == (i = r ? e.call(this, n, w(this).val()) : e)
                ? (i = "")
                : "number" == typeof i
                ? (i += "")
                : Array.isArray(i) &&
                  (i = w.map(i, function (e) {
                    return null == e ? "" : e + "";
                  })),
              ((t =
                w.valHooks[this.type] ||
                w.valHooks[this.nodeName.toLowerCase()]) &&
                "set" in t &&
                void 0 !== t.set(this, i, "value")) ||
                (this.value = i));
          }))
        : i
        ? (t = w.valHooks[i.type] || w.valHooks[i.nodeName.toLowerCase()]) &&
          "get" in t &&
          void 0 !== (n = t.get(i, "value"))
          ? n
          : "string" == typeof (n = i.value)
          ? n.replace(bt, "")
          : null == n
          ? ""
          : n
        : void 0;
    },
  }),
    w.extend({
      valHooks: {
        option: {
          get: function (e) {
            var t = w.find.attr(e, "value");
            return null != t ? t : pt(w.text(e));
          },
        },
        select: {
          get: function (e) {
            var t,
              n,
              r,
              i = e.options,
              a = e.selectedIndex,
              s = "select-one" === e.type,
              o = s ? null : [],
              u = s ? a + 1 : i.length;
            for (r = a < 0 ? u : s ? a : 0; r < u; r++)
              if (
                ((n = i[r]).selected || r === a) &&
                !n.disabled &&
                (!n.parentNode.disabled || !k(n.parentNode, "optgroup"))
              ) {
                if (((t = w(n).val()), s)) return t;
                o.push(t);
              }
            return o;
          },
          set: function (e, t) {
            for (
              var n, r, i = e.options, a = w.makeArray(t), s = i.length;
              s--;

            )
              ((r = i[s]).selected =
                w.inArray(w.valHooks.option.get(r), a) > -1) && (n = !0);
            return n || (e.selectedIndex = -1), a;
          },
        },
      },
    }),
    w.each(["radio", "checkbox"], function () {
      (w.valHooks[this] = {
        set: function (e, t) {
          if (Array.isArray(t))
            return (e.checked = w.inArray(w(e).val(), t) > -1);
        },
      }),
        h.checkOn ||
          (w.valHooks[this].get = function (e) {
            return null === e.getAttribute("value") ? "on" : e.value;
          });
    }),
    (h.focusin = "onfocusin" in e);
  var gt = /^(?:focusinfocus|focusoutblur)$/,
    yt = function (e) {
      e.stopPropagation();
    };
  w.extend(w.event, {
    trigger: function (t, n, r, i) {
      var a,
        s,
        o,
        u,
        c,
        f,
        d,
        h,
        b = [r || _],
        g = l.call(t, "type") ? t.type : t,
        y = l.call(t, "namespace") ? t.namespace.split(".") : [];
      if (
        ((s = h = o = r = r || _),
        3 !== r.nodeType &&
          8 !== r.nodeType &&
          !gt.test(g + w.event.triggered) &&
          (g.indexOf(".") > -1 &&
            ((y = g.split(".")), (g = y.shift()), y.sort()),
          (c = g.indexOf(":") < 0 && "on" + g),
          ((t = t[w.expando]
            ? t
            : new w.Event(g, "object" == typeof t && t)).isTrigger = i ? 2 : 3),
          (t.namespace = y.join(".")),
          (t.rnamespace = t.namespace
            ? new RegExp("(^|\\.)" + y.join("\\.(?:.*\\.|)") + "(\\.|$)")
            : null),
          (t.result = void 0),
          t.target || (t.target = r),
          (n = null == n ? [t] : w.makeArray(n, [t])),
          (d = w.event.special[g] || {}),
          i || !d.trigger || !1 !== d.trigger.apply(r, n)))
      ) {
        if (!i && !d.noBubble && !m(r)) {
          for (
            u = d.delegateType || g, gt.test(u + g) || (s = s.parentNode);
            s;
            s = s.parentNode
          )
            b.push(s), (o = s);
          o === (r.ownerDocument || _) &&
            b.push(o.defaultView || o.parentWindow || e);
        }
        for (a = 0; (s = b[a++]) && !t.isPropagationStopped(); )
          (h = s),
            (t.type = a > 1 ? u : d.bindType || g),
            (f =
              (Y.get(s, "events") || Object.create(null))[t.type] &&
              Y.get(s, "handle")) && f.apply(s, n),
            (f = c && s[c]) &&
              f.apply &&
              X(s) &&
              ((t.result = f.apply(s, n)),
              !1 === t.result && t.preventDefault());
        return (
          (t.type = g),
          i ||
            t.isDefaultPrevented() ||
            (d._default && !1 !== d._default.apply(b.pop(), n)) ||
            !X(r) ||
            (c &&
              p(r[g]) &&
              !m(r) &&
              ((o = r[c]) && (r[c] = null),
              (w.event.triggered = g),
              t.isPropagationStopped() && h.addEventListener(g, yt),
              r[g](),
              t.isPropagationStopped() && h.removeEventListener(g, yt),
              (w.event.triggered = void 0),
              o && (r[c] = o))),
          t.result
        );
      }
    },
    simulate: function (e, t, n) {
      var r = w.extend(new w.Event(), n, { type: e, isSimulated: !0 });
      w.event.trigger(r, null, t);
    },
  }),
    w.fn.extend({
      trigger: function (e, t) {
        return this.each(function () {
          w.event.trigger(e, t, this);
        });
      },
      triggerHandler: function (e, t) {
        var n = this[0];
        if (n) return w.event.trigger(e, t, n, !0);
      },
    }),
    h.focusin ||
      w.each({ focus: "focusin", blur: "focusout" }, function (e, t) {
        var n = function (e) {
          w.event.simulate(t, e.target, w.event.fix(e));
        };
        w.event.special[t] = {
          setup: function () {
            var r = this.ownerDocument || this.document || this,
              i = Y.access(r, t);
            i || r.addEventListener(e, n, !0), Y.access(r, t, (i || 0) + 1);
          },
          teardown: function () {
            var r = this.ownerDocument || this.document || this,
              i = Y.access(r, t) - 1;
            i
              ? Y.access(r, t, i)
              : (r.removeEventListener(e, n, !0), Y.remove(r, t));
          },
        };
      });
  var vt = e.location,
    wt = { guid: Date.now() },
    xt = /\?/;
  w.parseXML = function (t) {
    var n, r;
    if (!t || "string" != typeof t) return null;
    try {
      n = new e.DOMParser().parseFromString(t, "text/xml");
    } catch (e) {}
    return (
      (r = n && n.getElementsByTagName("parsererror")[0]),
      (n && !r) ||
        w.error(
          "Invalid XML: " +
            (r
              ? w
                  .map(r.childNodes, function (e) {
                    return e.textContent;
                  })
                  .join("\n")
              : t)
        ),
      n
    );
  };
  var St = /\[\]$/,
    Ct = /\r?\n/g,
    Et = /^(?:submit|button|image|reset|file)$/i,
    Pt = /^(?:input|select|textarea|keygen)/i;
  function kt(e, t, n, r) {
    var i;
    if (Array.isArray(t))
      w.each(t, function (t, i) {
        n || St.test(e)
          ? r(e, i)
          : kt(
              e + "[" + ("object" == typeof i && null != i ? t : "") + "]",
              i,
              n,
              r
            );
      });
    else if (n || "object" !== y(t)) r(e, t);
    else for (i in t) kt(e + "[" + i + "]", t[i], n, r);
  }
  (w.param = function (e, t) {
    var n,
      r = [],
      i = function (e, t) {
        var n = p(t) ? t() : t;
        r[r.length] =
          encodeURIComponent(e) + "=" + encodeURIComponent(null == n ? "" : n);
      };
    if (null == e) return "";
    if (Array.isArray(e) || (e.jquery && !w.isPlainObject(e)))
      w.each(e, function () {
        i(this.name, this.value);
      });
    else for (n in e) kt(n, e[n], t, i);
    return r.join("&");
  }),
    w.fn.extend({
      serialize: function () {
        return w.param(this.serializeArray());
      },
      serializeArray: function () {
        return this.map(function () {
          var e = w.prop(this, "elements");
          return e ? w.makeArray(e) : this;
        })
          .filter(function () {
            var e = this.type;
            return (
              this.name &&
              !w(this).is(":disabled") &&
              Pt.test(this.nodeName) &&
              !Et.test(e) &&
              (this.checked || !he.test(e))
            );
          })
          .map(function (e, t) {
            var n = w(this).val();
            return null == n
              ? null
              : Array.isArray(n)
              ? w.map(n, function (e) {
                  return { name: t.name, value: e.replace(Ct, "\r\n") };
                })
              : { name: t.name, value: n.replace(Ct, "\r\n") };
          })
          .get();
      },
    });
  var Tt = /%20/g,
    jt = /#.*$/,
    Bt = /([?&])_=[^&]*/,
    qt = /^(.*?):[ \t]*([^\r\n]*)$/gm,
    Dt = /^(?:GET|HEAD)$/,
    At = /^\/\//,
    $t = {},
    Nt = {},
    Lt = "*/".concat("*"),
    Rt = _.createElement("a");
  function Ot(e) {
    return function (t, n) {
      "string" != typeof t && ((n = t), (t = "*"));
      var r,
        i = 0,
        a = t.toLowerCase().match(N) || [];
      if (p(n))
        for (; (r = a[i++]); )
          "+" === r[0]
            ? ((r = r.slice(1) || "*"), (e[r] = e[r] || []).unshift(n))
            : (e[r] = e[r] || []).push(n);
    };
  }
  function Mt(e, t, n, r) {
    var i = {},
      a = e === Nt;
    function s(o) {
      var u;
      return (
        (i[o] = !0),
        w.each(e[o] || [], function (e, o) {
          var c = o(t, n, r);
          return "string" != typeof c || a || i[c]
            ? a
              ? !(u = c)
              : void 0
            : (t.dataTypes.unshift(c), s(c), !1);
        }),
        u
      );
    }
    return s(t.dataTypes[0]) || (!i["*"] && s("*"));
  }
  function It(e, t) {
    var n,
      r,
      i = w.ajaxSettings.flatOptions || {};
    for (n in t) void 0 !== t[n] && ((i[n] ? e : r || (r = {}))[n] = t[n]);
    return r && w.extend(!0, e, r), e;
  }
  (Rt.href = vt.href),
    w.extend({
      active: 0,
      lastModified: {},
      etag: {},
      ajaxSettings: {
        url: vt.href,
        type: "GET",
        isLocal: /^(?:about|app|app-storage|.+-extension|file|res|widget):$/.test(
          vt.protocol
        ),
        global: !0,
        processData: !0,
        async: !0,
        contentType: "application/x-www-form-urlencoded; charset=UTF-8",
        accepts: {
          "*": Lt,
          text: "text/plain",
          html: "text/html",
          xml: "application/xml, text/xml",
          json: "application/json, text/javascript",
        },
        contents: { xml: /\bxml\b/, html: /\bhtml/, json: /\bjson\b/ },
        responseFields: {
          xml: "responseXML",
          text: "responseText",
          json: "responseJSON",
        },
        converters: {
          "* text": String,
          "text html": !0,
          "text json": JSON.parse,
          "text xml": w.parseXML,
        },
        flatOptions: { url: !0, context: !0 },
      },
      ajaxSetup: function (e, t) {
        return t ? It(It(e, w.ajaxSettings), t) : It(w.ajaxSettings, e);
      },
      ajaxPrefilter: Ot($t),
      ajaxTransport: Ot(Nt),
      ajax: function (t, n) {
        "object" == typeof t && ((n = t), (t = void 0)), (n = n || {});
        var r,
          i,
          a,
          s,
          o,
          u,
          c,
          l,
          f,
          d,
          h = w.ajaxSetup({}, n),
          p = h.context || h,
          m = h.context && (p.nodeType || p.jquery) ? w(p) : w.event,
          b = w.Deferred(),
          g = w.Callbacks("once memory"),
          y = h.statusCode || {},
          v = {},
          x = {},
          S = "canceled",
          C = {
            readyState: 0,
            getResponseHeader: function (e) {
              var t;
              if (c) {
                if (!s)
                  for (s = {}; (t = qt.exec(a)); )
                    s[t[1].toLowerCase() + " "] = (
                      s[t[1].toLowerCase() + " "] || []
                    ).concat(t[2]);
                t = s[e.toLowerCase() + " "];
              }
              return null == t ? null : t.join(", ");
            },
            getAllResponseHeaders: function () {
              return c ? a : null;
            },
            setRequestHeader: function (e, t) {
              return (
                null == c &&
                  ((e = x[e.toLowerCase()] = x[e.toLowerCase()] || e),
                  (v[e] = t)),
                this
              );
            },
            overrideMimeType: function (e) {
              return null == c && (h.mimeType = e), this;
            },
            statusCode: function (e) {
              var t;
              if (e)
                if (c) C.always(e[C.status]);
                else for (t in e) y[t] = [y[t], e[t]];
              return this;
            },
            abort: function (e) {
              var t = e || S;
              return r && r.abort(t), E(0, t), this;
            },
          };
        if (
          (b.promise(C),
          (h.url = ((t || h.url || vt.href) + "").replace(
            At,
            vt.protocol + "//"
          )),
          (h.type = n.method || n.type || h.method || h.type),
          (h.dataTypes = (h.dataType || "*").toLowerCase().match(N) || [""]),
          null == h.crossDomain)
        ) {
          u = _.createElement("a");
          try {
            (u.href = h.url),
              (u.href = u.href),
              (h.crossDomain =
                Rt.protocol + "//" + Rt.host != u.protocol + "//" + u.host);
          } catch (e) {
            h.crossDomain = !0;
          }
        }
        if (
          (h.data &&
            h.processData &&
            "string" != typeof h.data &&
            (h.data = w.param(h.data, h.traditional)),
          Mt($t, h, n, C),
          c)
        )
          return C;
        for (f in ((l = w.event && h.global) &&
          0 == w.active++ &&
          w.event.trigger("ajaxStart"),
        (h.type = h.type.toUpperCase()),
        (h.hasContent = !Dt.test(h.type)),
        (i = h.url.replace(jt, "")),
        h.hasContent
          ? h.data &&
            h.processData &&
            0 ===
              (h.contentType || "").indexOf(
                "application/x-www-form-urlencoded"
              ) &&
            (h.data = h.data.replace(Tt, "+"))
          : ((d = h.url.slice(i.length)),
            h.data &&
              (h.processData || "string" == typeof h.data) &&
              ((i += (xt.test(i) ? "&" : "?") + h.data), delete h.data),
            !1 === h.cache &&
              ((i = i.replace(Bt, "$1")),
              (d = (xt.test(i) ? "&" : "?") + "_=" + wt.guid++ + d)),
            (h.url = i + d)),
        h.ifModified &&
          (w.lastModified[i] &&
            C.setRequestHeader("If-Modified-Since", w.lastModified[i]),
          w.etag[i] && C.setRequestHeader("If-None-Match", w.etag[i])),
        ((h.data && h.hasContent && !1 !== h.contentType) || n.contentType) &&
          C.setRequestHeader("Content-Type", h.contentType),
        C.setRequestHeader(
          "Accept",
          h.dataTypes[0] && h.accepts[h.dataTypes[0]]
            ? h.accepts[h.dataTypes[0]] +
                ("*" !== h.dataTypes[0] ? ", " + Lt + "; q=0.01" : "")
            : h.accepts["*"]
        ),
        h.headers))
          C.setRequestHeader(f, h.headers[f]);
        if (h.beforeSend && (!1 === h.beforeSend.call(p, C, h) || c))
          return C.abort();
        if (
          ((S = "abort"),
          g.add(h.complete),
          C.done(h.success),
          C.fail(h.error),
          (r = Mt(Nt, h, n, C)))
        ) {
          if (((C.readyState = 1), l && m.trigger("ajaxSend", [C, h]), c))
            return C;
          h.async &&
            h.timeout > 0 &&
            (o = e.setTimeout(function () {
              C.abort("timeout");
            }, h.timeout));
          try {
            (c = !1), r.send(v, E);
          } catch (e) {
            if (c) throw e;
            E(-1, e);
          }
        } else E(-1, "No Transport");
        function E(t, n, s, u) {
          var f,
            d,
            _,
            v,
            x,
            S = n;
          c ||
            ((c = !0),
            o && e.clearTimeout(o),
            (r = void 0),
            (a = u || ""),
            (C.readyState = t > 0 ? 4 : 0),
            (f = (t >= 200 && t < 300) || 304 === t),
            s &&
              (v = (function (e, t, n) {
                for (
                  var r, i, a, s, o = e.contents, u = e.dataTypes;
                  "*" === u[0];

                )
                  u.shift(),
                    void 0 === r &&
                      (r = e.mimeType || t.getResponseHeader("Content-Type"));
                if (r)
                  for (i in o)
                    if (o[i] && o[i].test(r)) {
                      u.unshift(i);
                      break;
                    }
                if (u[0] in n) a = u[0];
                else {
                  for (i in n) {
                    if (!u[0] || e.converters[i + " " + u[0]]) {
                      a = i;
                      break;
                    }
                    s || (s = i);
                  }
                  a = a || s;
                }
                if (a) return a !== u[0] && u.unshift(a), n[a];
              })(h, C, s)),
            !f &&
              w.inArray("script", h.dataTypes) > -1 &&
              w.inArray("json", h.dataTypes) < 0 &&
              (h.converters["text script"] = function () {}),
            (v = (function (e, t, n, r) {
              var i,
                a,
                s,
                o,
                u,
                c = {},
                l = e.dataTypes.slice();
              if (l[1])
                for (s in e.converters) c[s.toLowerCase()] = e.converters[s];
              for (a = l.shift(); a; )
                if (
                  (e.responseFields[a] && (n[e.responseFields[a]] = t),
                  !u && r && e.dataFilter && (t = e.dataFilter(t, e.dataType)),
                  (u = a),
                  (a = l.shift()))
                )
                  if ("*" === a) a = u;
                  else if ("*" !== u && u !== a) {
                    if (!(s = c[u + " " + a] || c["* " + a]))
                      for (i in c)
                        if (
                          (o = i.split(" "))[1] === a &&
                          (s = c[u + " " + o[0]] || c["* " + o[0]])
                        ) {
                          !0 === s
                            ? (s = c[i])
                            : !0 !== c[i] && ((a = o[0]), l.unshift(o[1]));
                          break;
                        }
                    if (!0 !== s)
                      if (s && e.throws) t = s(t);
                      else
                        try {
                          t = s(t);
                        } catch (e) {
                          return {
                            state: "parsererror",
                            error: s
                              ? e
                              : "No conversion from " + u + " to " + a,
                          };
                        }
                  }
              return { state: "success", data: t };
            })(h, v, C, f)),
            f
              ? (h.ifModified &&
                  ((x = C.getResponseHeader("Last-Modified")) &&
                    (w.lastModified[i] = x),
                  (x = C.getResponseHeader("etag")) && (w.etag[i] = x)),
                204 === t || "HEAD" === h.type
                  ? (S = "nocontent")
                  : 304 === t
                  ? (S = "notmodified")
                  : ((S = v.state), (d = v.data), (f = !(_ = v.error))))
              : ((_ = S), (!t && S) || ((S = "error"), t < 0 && (t = 0))),
            (C.status = t),
            (C.statusText = (n || S) + ""),
            f ? b.resolveWith(p, [d, S, C]) : b.rejectWith(p, [C, S, _]),
            C.statusCode(y),
            (y = void 0),
            l && m.trigger(f ? "ajaxSuccess" : "ajaxError", [C, h, f ? d : _]),
            g.fireWith(p, [C, S]),
            l &&
              (m.trigger("ajaxComplete", [C, h]),
              --w.active || w.event.trigger("ajaxStop")));
        }
        return C;
      },
      getJSON: function (e, t, n) {
        return w.get(e, t, n, "json");
      },
      getScript: function (e, t) {
        return w.get(e, void 0, t, "script");
      },
    }),
    w.each(["get", "post"], function (e, t) {
      w[t] = function (e, n, r, i) {
        return (
          p(n) && ((i = i || r), (r = n), (n = void 0)),
          w.ajax(
            w.extend(
              { url: e, type: t, dataType: i, data: n, success: r },
              w.isPlainObject(e) && e
            )
          )
        );
      };
    }),
    w.ajaxPrefilter(function (e) {
      var t;
      for (t in e.headers)
        "content-type" === t.toLowerCase() &&
          (e.contentType = e.headers[t] || "");
    }),
    (w._evalUrl = function (e, t, n) {
      return w.ajax({
        url: e,
        type: "GET",
        dataType: "script",
        cache: !0,
        async: !1,
        global: !1,
        converters: { "text script": function () {} },
        dataFilter: function (e) {
          w.globalEval(e, t, n);
        },
      });
    }),
    w.fn.extend({
      wrapAll: function (e) {
        var t;
        return (
          this[0] &&
            (p(e) && (e = e.call(this[0])),
            (t = w(e, this[0].ownerDocument).eq(0).clone(!0)),
            this[0].parentNode && t.insertBefore(this[0]),
            t
              .map(function () {
                for (var e = this; e.firstElementChild; )
                  e = e.firstElementChild;
                return e;
              })
              .append(this)),
          this
        );
      },
      wrapInner: function (e) {
        return p(e)
          ? this.each(function (t) {
              w(this).wrapInner(e.call(this, t));
            })
          : this.each(function () {
              var t = w(this),
                n = t.contents();
              n.length ? n.wrapAll(e) : t.append(e);
            });
      },
      wrap: function (e) {
        var t = p(e);
        return this.each(function (n) {
          w(this).wrapAll(t ? e.call(this, n) : e);
        });
      },
      unwrap: function (e) {
        return (
          this.parent(e)
            .not("body")
            .each(function () {
              w(this).replaceWith(this.childNodes);
            }),
          this
        );
      },
    }),
    (w.expr.pseudos.hidden = function (e) {
      return !w.expr.pseudos.visible(e);
    }),
    (w.expr.pseudos.visible = function (e) {
      return !!(e.offsetWidth || e.offsetHeight || e.getClientRects().length);
    }),
    (w.ajaxSettings.xhr = function () {
      try {
        return new e.XMLHttpRequest();
      } catch (e) {}
    });
  var Ht = { 0: 200, 1223: 204 },
    Ft = w.ajaxSettings.xhr();
  (h.cors = !!Ft && "withCredentials" in Ft),
    (h.ajax = Ft = !!Ft),
    w.ajaxTransport(function (t) {
      var n, r;
      if (h.cors || (Ft && !t.crossDomain))
        return {
          send: function (i, a) {
            var s,
              o = t.xhr();
            if (
              (o.open(t.type, t.url, t.async, t.username, t.password),
              t.xhrFields)
            )
              for (s in t.xhrFields) o[s] = t.xhrFields[s];
            for (s in (t.mimeType &&
              o.overrideMimeType &&
              o.overrideMimeType(t.mimeType),
            t.crossDomain ||
              i["X-Requested-With"] ||
              (i["X-Requested-With"] = "XMLHttpRequest"),
            i))
              o.setRequestHeader(s, i[s]);
            (n = function (e) {
              return function () {
                n &&
                  ((n = r = o.onload = o.onerror = o.onabort = o.ontimeout = o.onreadystatechange = null),
                  "abort" === e
                    ? o.abort()
                    : "error" === e
                    ? "number" != typeof o.status
                      ? a(0, "error")
                      : a(o.status, o.statusText)
                    : a(
                        Ht[o.status] || o.status,
                        o.statusText,
                        "text" !== (o.responseType || "text") ||
                          "string" != typeof o.responseText
                          ? { binary: o.response }
                          : { text: o.responseText },
                        o.getAllResponseHeaders()
                      ));
              };
            }),
              (o.onload = n()),
              (r = o.onerror = o.ontimeout = n("error")),
              void 0 !== o.onabort
                ? (o.onabort = r)
                : (o.onreadystatechange = function () {
                    4 === o.readyState &&
                      e.setTimeout(function () {
                        n && r();
                      });
                  }),
              (n = n("abort"));
            try {
              o.send((t.hasContent && t.data) || null);
            } catch (e) {
              if (n) throw e;
            }
          },
          abort: function () {
            n && n();
          },
        };
    }),
    w.ajaxPrefilter(function (e) {
      e.crossDomain && (e.contents.script = !1);
    }),
    w.ajaxSetup({
      accepts: {
        script:
          "text/javascript, application/javascript, application/ecmascript, application/x-ecmascript",
      },
      contents: { script: /\b(?:java|ecma)script\b/ },
      converters: {
        "text script": function (e) {
          return w.globalEval(e), e;
        },
      },
    }),
    w.ajaxPrefilter("script", function (e) {
      void 0 === e.cache && (e.cache = !1), e.crossDomain && (e.type = "GET");
    }),
    w.ajaxTransport("script", function (e) {
      var t, n;
      if (e.crossDomain || e.scriptAttrs)
        return {
          send: function (r, i) {
            (t = w("<script>")
              .attr(e.scriptAttrs || {})
              .prop({ charset: e.scriptCharset, src: e.url })
              .on(
                "load error",
                (n = function (e) {
                  t.remove(),
                    (n = null),
                    e && i("error" === e.type ? 404 : 200, e.type);
                })
              )),
              _.head.appendChild(t[0]);
          },
          abort: function () {
            n && n();
          },
        };
    });
  var Wt,
    zt = [],
    Ut = /(=)\?(?=&|$)|\?\?/;
  w.ajaxSetup({
    jsonp: "callback",
    jsonpCallback: function () {
      var e = zt.pop() || w.expando + "_" + wt.guid++;
      return (this[e] = !0), e;
    },
  }),
    w.ajaxPrefilter("json jsonp", function (t, n, r) {
      var i,
        a,
        s,
        o =
          !1 !== t.jsonp &&
          (Ut.test(t.url)
            ? "url"
            : "string" == typeof t.data &&
              0 ===
                (t.contentType || "").indexOf(
                  "application/x-www-form-urlencoded"
                ) &&
              Ut.test(t.data) &&
              "data");
      if (o || "jsonp" === t.dataTypes[0])
        return (
          (i = t.jsonpCallback = p(t.jsonpCallback)
            ? t.jsonpCallback()
            : t.jsonpCallback),
          o
            ? (t[o] = t[o].replace(Ut, "$1" + i))
            : !1 !== t.jsonp &&
              (t.url += (xt.test(t.url) ? "&" : "?") + t.jsonp + "=" + i),
          (t.converters["script json"] = function () {
            return s || w.error(i + " was not called"), s[0];
          }),
          (t.dataTypes[0] = "json"),
          (a = e[i]),
          (e[i] = function () {
            s = arguments;
          }),
          r.always(function () {
            void 0 === a ? w(e).removeProp(i) : (e[i] = a),
              t[i] && ((t.jsonpCallback = n.jsonpCallback), zt.push(i)),
              s && p(a) && a(s[0]),
              (s = a = void 0);
          }),
          "script"
        );
    }),
    (h.createHTMLDocument =
      (((Wt = _.implementation.createHTMLDocument("").body).innerHTML =
        "<form></form><form></form>"),
      2 === Wt.childNodes.length)),
    (w.parseHTML = function (e, t, n) {
      return "string" != typeof e
        ? []
        : ("boolean" == typeof t && ((n = t), (t = !1)),
          t ||
            (h.createHTMLDocument
              ? (((r = (t = _.implementation.createHTMLDocument(
                  ""
                )).createElement("base")).href = _.location.href),
                t.head.appendChild(r))
              : (t = _)),
          (a = !n && []),
          (i = T.exec(e))
            ? [t.createElement(i[1])]
            : ((i = ve([e], t, a)),
              a && a.length && w(a).remove(),
              w.merge([], i.childNodes)));
      var r, i, a;
    }),
    (w.fn.load = function (e, t, n) {
      var r,
        i,
        a,
        s = this,
        o = e.indexOf(" ");
      return (
        o > -1 && ((r = pt(e.slice(o))), (e = e.slice(0, o))),
        p(t)
          ? ((n = t), (t = void 0))
          : t && "object" == typeof t && (i = "POST"),
        s.length > 0 &&
          w
            .ajax({ url: e, type: i || "GET", dataType: "html", data: t })
            .done(function (e) {
              (a = arguments),
                s.html(r ? w("<div>").append(w.parseHTML(e)).find(r) : e);
            })
            .always(
              n &&
                function (e, t) {
                  s.each(function () {
                    n.apply(this, a || [e.responseText, t, e]);
                  });
                }
            ),
        this
      );
    }),
    (w.expr.pseudos.animated = function (e) {
      return w.grep(w.timers, function (t) {
        return e === t.elem;
      }).length;
    }),
    (w.offset = {
      setOffset: function (e, t, n) {
        var r,
          i,
          a,
          s,
          o,
          u,
          c = w.css(e, "position"),
          l = w(e),
          f = {};
        "static" === c && (e.style.position = "relative"),
          (o = l.offset()),
          (a = w.css(e, "top")),
          (u = w.css(e, "left")),
          ("absolute" === c || "fixed" === c) && (a + u).indexOf("auto") > -1
            ? ((s = (r = l.position()).top), (i = r.left))
            : ((s = parseFloat(a) || 0), (i = parseFloat(u) || 0)),
          p(t) && (t = t.call(e, n, w.extend({}, o))),
          null != t.top && (f.top = t.top - o.top + s),
          null != t.left && (f.left = t.left - o.left + i),
          "using" in t ? t.using.call(e, f) : l.css(f);
      },
    }),
    w.fn.extend({
      offset: function (e) {
        if (arguments.length)
          return void 0 === e
            ? this
            : this.each(function (t) {
                w.offset.setOffset(this, e, t);
              });
        var t,
          n,
          r = this[0];
        return r
          ? r.getClientRects().length
            ? ((t = r.getBoundingClientRect()),
              (n = r.ownerDocument.defaultView),
              { top: t.top + n.pageYOffset, left: t.left + n.pageXOffset })
            : { top: 0, left: 0 }
          : void 0;
      },
      position: function () {
        if (this[0]) {
          var e,
            t,
            n,
            r = this[0],
            i = { top: 0, left: 0 };
          if ("fixed" === w.css(r, "position")) t = r.getBoundingClientRect();
          else {
            for (
              t = this.offset(),
                n = r.ownerDocument,
                e = r.offsetParent || n.documentElement;
              e &&
              (e === n.body || e === n.documentElement) &&
              "static" === w.css(e, "position");

            )
              e = e.parentNode;
            e &&
              e !== r &&
              1 === e.nodeType &&
              (((i = w(e).offset()).top += w.css(e, "borderTopWidth", !0)),
              (i.left += w.css(e, "borderLeftWidth", !0)));
          }
          return {
            top: t.top - i.top - w.css(r, "marginTop", !0),
            left: t.left - i.left - w.css(r, "marginLeft", !0),
          };
        }
      },
      offsetParent: function () {
        return this.map(function () {
          for (
            var e = this.offsetParent;
            e && "static" === w.css(e, "position");

          )
            e = e.offsetParent;
          return e || re;
        });
      },
    }),
    w.each({ scrollLeft: "pageXOffset", scrollTop: "pageYOffset" }, function (
      e,
      t
    ) {
      var n = "pageYOffset" === t;
      w.fn[e] = function (r) {
        return F(
          this,
          function (e, r, i) {
            var a;
            if (
              (m(e) ? (a = e) : 9 === e.nodeType && (a = e.defaultView),
              void 0 === i)
            )
              return a ? a[t] : e[r];
            a
              ? a.scrollTo(n ? a.pageXOffset : i, n ? i : a.pageYOffset)
              : (e[r] = i);
          },
          e,
          r,
          arguments.length
        );
      };
    }),
    w.each(["top", "left"], function (e, t) {
      w.cssHooks[t] = Fe(h.pixelPosition, function (e, n) {
        if (n)
          return (n = He(e, t)), Re.test(n) ? w(e).position()[t] + "px" : n;
      });
    }),
    w.each({ Height: "height", Width: "width" }, function (e, t) {
      w.each({ padding: "inner" + e, content: t, "": "outer" + e }, function (
        n,
        r
      ) {
        w.fn[r] = function (i, a) {
          var s = arguments.length && (n || "boolean" != typeof i),
            o = n || (!0 === i || !0 === a ? "margin" : "border");
          return F(
            this,
            function (t, n, i) {
              var a;
              return m(t)
                ? 0 === r.indexOf("outer")
                  ? t["inner" + e]
                  : t.document.documentElement["client" + e]
                : 9 === t.nodeType
                ? ((a = t.documentElement),
                  Math.max(
                    t.body["scroll" + e],
                    a["scroll" + e],
                    t.body["offset" + e],
                    a["offset" + e],
                    a["client" + e]
                  ))
                : void 0 === i
                ? w.css(t, n, o)
                : w.style(t, n, i, o);
            },
            t,
            s ? i : void 0,
            s
          );
        };
      });
    }),
    w.each(
      [
        "ajaxStart",
        "ajaxStop",
        "ajaxComplete",
        "ajaxError",
        "ajaxSuccess",
        "ajaxSend",
      ],
      function (e, t) {
        w.fn[t] = function (e) {
          return this.on(t, e);
        };
      }
    ),
    w.fn.extend({
      bind: function (e, t, n) {
        return this.on(e, null, t, n);
      },
      unbind: function (e, t) {
        return this.off(e, null, t);
      },
      delegate: function (e, t, n, r) {
        return this.on(t, e, n, r);
      },
      undelegate: function (e, t, n) {
        return 1 === arguments.length
          ? this.off(e, "**")
          : this.off(t, e || "**", n);
      },
      hover: function (e, t) {
        return this.mouseenter(e).mouseleave(t || e);
      },
    }),
    w.each(
      "blur focus focusin focusout resize scroll click dblclick mousedown mouseup mousemove mouseover mouseout mouseenter mouseleave change select submit keydown keypress keyup contextmenu".split(
        " "
      ),
      function (e, t) {
        w.fn[t] = function (e, n) {
          return arguments.length > 0
            ? this.on(t, null, e, n)
            : this.trigger(t);
        };
      }
    );
  var Vt = /^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g;
  (w.proxy = function (e, t) {
    var n, r, a;
    if (("string" == typeof t && ((n = e[t]), (t = e), (e = n)), p(e)))
      return (
        (r = i.call(arguments, 2)),
        (a = function () {
          return e.apply(t || this, r.concat(i.call(arguments)));
        }),
        (a.guid = e.guid = e.guid || w.guid++),
        a
      );
  }),
    (w.holdReady = function (e) {
      e ? w.readyWait++ : w.ready(!0);
    }),
    (w.isArray = Array.isArray),
    (w.parseJSON = JSON.parse),
    (w.nodeName = k),
    (w.isFunction = p),
    (w.isWindow = m),
    (w.camelCase = V),
    (w.type = y),
    (w.now = Date.now),
    (w.isNumeric = function (e) {
      var t = w.type(e);
      return ("number" === t || "string" === t) && !isNaN(e - parseFloat(e));
    }),
    (w.trim = function (e) {
      return null == e ? "" : (e + "").replace(Vt, "");
    }),
    "function" == typeof define &&
      define.amd &&
      define("jquery", [], function () {
        return w;
      });
  var Xt = e.jQuery,
    Gt = e.$;
  return (
    (w.noConflict = function (t) {
      return e.$ === w && (e.$ = Gt), t && e.jQuery === w && (e.jQuery = Xt), w;
    }),
    void 0 === t && (e.jQuery = e.$ = w),
    w
  );
});
var BiwaScheme = (function () {
  const TopEnv = {},
    CoreEnv = {},
    nil = {
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
    },
    undef = new Object();
  undef.toString = function () {
    return "#<undef>";
  };
  const max_trace_size = 40,
    suppress_deprecation_warning = !1,
    VERSION = "0.7.5",
    GitCommit = "7a92b7c3cd5abbb1212309bef72874414996e4e7";
  var idCounter = 0;
  function uniqueId(e) {
    var t = ++idCounter + "";
    return e ? e + t : t;
  }
  const escapeMap = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': "&quot;",
    "'": "&#x27;",
    "`": "&#x60;",
  };
  function escape(e) {
    return Object.entries(escapeMap).reduce(
      (e, [t, n]) => e.replaceAll(t, n),
      e
    );
  }
  const Symbols = {};
  class BiwaSymbol {
    constructor(e) {
      (this.name = e), (Symbols[e] = this);
    }
    inspect() {
      return "'" + this.name;
    }
    toString() {
      return "'" + this.name;
    }
    to_write() {
      return this.name;
    }
  }
  const Sym = function (e, t) {
      return void 0 === Symbols[e]
        ? new BiwaSymbol(e)
        : Symbols[e] instanceof BiwaSymbol
        ? Symbols[e]
        : new BiwaSymbol(e);
    },
    gensym = function (e = "__gensym") {
      return Sym(uniqueId(e));
    };
  class Pause {
    constructor(e) {
      this.on_pause = e;
    }
    set_state(e, t, n, r, i) {
      (this.interpreter = e),
        (this.x = t),
        (this.f = n),
        (this.c = r),
        (this.s = i);
    }
    ready() {
      this.on_pause(this);
    }
    resume(e) {
      return this.interpreter.resume(!0, e, this.x, this.f, this.c, this.s);
    }
  }
  const eof = new Object();
  class Port {
    constructor(e, t) {
      (this.is_open = !0),
        (this.is_binary = !1),
        (this.is_input = e),
        (this.is_output = t);
    }
    close() {
      this.is_open = !1;
    }
    inspect() {
      return "#<Port>";
    }
    to_write() {
      return "#<Port>";
    }
  }
  (Port.StringOutput = class extends Port {
    constructor() {
      super(!1, !0), (this.buffer = []);
    }
    put_string(e) {
      this.buffer.push(e);
    }
    output_string(e) {
      return this.buffer.join("");
    }
  }),
    (Port.StringInput = class extends Port {
      constructor(e) {
        super(!0, !1), (this.str = e);
      }
      get_string(e) {
        return e(this.str);
      }
    }),
    (Port.NullInput = class extends Port {
      constructor() {
        super(!0, !1);
      }
      get_string(e) {
        return e("");
      }
    }),
    (Port.NullOutput = class extends Port {
      constructor(e) {
        super(!1, !0), (this.output_function = e);
      }
      put_string(e) {}
    }),
    (Port.CustomOutput = class extends Port {
      constructor(e) {
        super(!1, !0), (this.output_function = e);
      }
      put_string(e) {
        this.output_function(e);
      }
    }),
    (Port.CustomInput = class extends Port {
      constructor(e) {
        super(!0, !1), (this.input_function = e);
      }
      get_string(e) {
        var t = this.input_function;
        return new Pause(function (n) {
          t(function (t) {
            n.resume(e(t));
          });
        });
      }
    }),
    (Port.current_input = new Port.NullInput()),
    (Port.current_output = new Port.NullOutput()),
    (Port.current_error = new Port.NullOutput());
  class Closure {
    constructor(e, t, n, r) {
      (this.body = e),
        (this.freevars = t),
        (this.dotpos = n),
        (this.expected_args = r);
    }
    to_write() {
      return "#<Closure>";
    }
  }
  const isClosure = function (e) {
      return e instanceof Closure;
    },
    isNil = function (e) {
      return e === nil;
    },
    isUndef = function (e) {
      return e === undef;
    },
    isBoolean = (e) => "boolean" == typeof e,
    isString = (e) => "string" == typeof e,
    isFunction = (e) => "function" == typeof e,
    isSymbol = function (e) {
      return e instanceof BiwaSymbol;
    },
    isPort = function (e) {
      return e instanceof Port;
    },
    isVector = function (e) {
      return e instanceof Array;
    },
    isProcedure = function (e) {
      return isClosure(e) || "function" == typeof e;
    },
    lt = function (e, t) {
      return typeof e != typeof t ? compareFn(typeof e, typeof t) : e < t;
    };
  class BiwaError extends Error {
    constructor(e, t = null) {
      super(`${e}${null === t ? "" : `: ${to_write$1(t)}`}`), (this.form = t);
    }
    toString() {
      return this.message;
    }
  }
  class Bug extends BiwaError {
    constructor(e) {
      super("[BUG] " + e);
    }
  }
  class UserError extends BiwaError {
    constructor(e) {
      this.message = e;
    }
  }
  class BiwaSet {
    constructor() {
      var e;
      for (this.arr = [], e = 0; e < arguments.length; e++)
        this.arr[e] = arguments[e];
    }
    equals(e) {
      if (this.arr.length != e.arr.length) return !1;
      var t = [...this.arr],
        n = [...e.arr];
      t.sort(), n.sort();
      for (var r = 0; r < this.arr.length; r++) if (t[r] != n[r]) return !1;
      return !0;
    }
    set_cons(e) {
      var t = new BiwaSet(e);
      return (t.arr = [...this.arr]), t.arr.push(e), t;
    }
    set_union() {
      var e = new BiwaSet();
      e.arr = [...this.arr];
      for (var t = 0; t < arguments.length; t++) {
        var n = arguments[t];
        if (!(n instanceof BiwaSet))
          throw new BiwaError("set_union: arguments must be a set");
        for (var r = 0; r < n.arr.length; r++) e.add(n.arr[r]);
      }
      return e;
    }
    set_intersect(e) {
      if (!(e instanceof BiwaSet))
        throw new BiwaError("set_intersect: arguments must be a set");
      for (var t = new BiwaSet(), n = 0; n < this.arr.length; n++)
        e.member(this.arr[n]) && t.add(this.arr[n]);
      return t;
    }
    set_minus(e) {
      if (!(e instanceof BiwaSet))
        throw new BiwaError("set_minus: arguments must be a set");
      for (var t = new BiwaSet(), n = 0; n < this.arr.length; n++)
        e.member(this.arr[n]) || t.add(this.arr[n]);
      return t;
    }
    add(e) {
      this.member(e) || this.arr.push(e);
    }
    member(e) {
      for (var t = 0; t < this.arr.length; t++) if (this.arr[t] == e) return !0;
      return !1;
    }
    rindex(e) {
      for (var t = this.arr.length - 1; t >= 0; t--)
        if (this.arr[t] == e) return this.arr.length - 1 - t;
      return null;
    }
    index(e) {
      for (var t = 0; t < this.arr.length; t++) if (this.arr[t] == e) return t;
      return null;
    }
    inspect() {
      return "Set(" + this.arr.join(", ") + ")";
    }
    toString() {
      return this.inspect();
    }
    size() {
      return this.arr.length;
    }
  }
  class Pair {
    constructor(e, t) {
      (this.car = e), (this.cdr = t);
    }
    caar(e) {
      return this._get(["car", "car"], e);
    }
    cadr(e) {
      return this._get(["cdr", "car"], e);
    }
    cdar(e) {
      return this._get(["car", "cdr"], e);
    }
    cddr(e) {
      return this._get(["cdr", "cdr"], e);
    }
    _get(e, t = "unexpected") {
      let n = this;
      for (const r of e) {
        if (!n.hasOwnProperty(r)) throw t;
        n = n[r];
      }
      return n;
    }
    first() {
      return this.car;
    }
    second() {
      return this.cdr.car;
    }
    third() {
      return this.cdr.cdr.car;
    }
    fourth() {
      return this.cdr.cdr.cdr.car;
    }
    fifth() {
      return this.cdr.cdr.cdr.cdr.car;
    }
    to_array() {
      for (var e = [], t = this; t instanceof Pair; t = t.cdr) e.push(t.car);
      return e;
    }
    length() {
      for (var e = 0, t = this; t instanceof Pair; t = t.cdr) e++;
      return e;
    }
    last_cdr() {
      var e;
      for (e = this; e instanceof Pair; e = e.cdr);
      return e;
    }
    forEach(e) {
      for (var t = this; t instanceof Pair; t = t.cdr) e(t.car);
      return t;
    }
    foreach(e) {
      for (var t = this; t instanceof Pair; t = t.cdr) e(t.car);
      return t;
    }
    map(e) {
      for (var t = [], n = this; isPair(n); n = n.cdr) t.push(e(n.car));
      return t;
    }
    mapList(e) {
      return array_to_list(this.map(e));
    }
    async mapAsync(e) {
      const t = [];
      for (var n = this; isPair(n); n = n.cdr) t.push(await e(n.car));
      return array_to_list(t);
    }
    concat(e) {
      for (var t = this; t instanceof Pair && t.cdr != nil; ) t = t.cdr;
      return (t.cdr = e), this;
    }
    inspect(e) {
      e || (e = inspect);
      var t = [],
        n = this.foreach(function (n) {
          t.push(e(n));
        });
      return n != nil && (t.push("."), t.push(e(n))), "(" + t.join(" ") + ")";
    }
    toString() {
      return this.inspect();
    }
    to_display(e) {
      return this.inspect(e);
    }
    to_write() {
      return this.inspect(to_write$1);
    }
  }
  const isPair = function (e) {
      return e instanceof Pair;
    },
    isList = function (e) {
      if (e === nil) return !0;
      if (!(e instanceof Pair)) return !1;
      for (var t = e, n = e.cdr; ; ) {
        if (n === nil) return !0;
        if (n === t) return !1;
        if (!(n instanceof Pair)) return !1;
        if (n.cdr === nil) return !0;
        if (!(n.cdr instanceof Pair)) return !1;
        (n = n.cdr.cdr), (t = t.cdr);
      }
    },
    array_to_list_ = function (e, t) {
      for (var n = nil, r = e.length - 1; r >= 0; r--) {
        var i = e[r];
        t && Array.isArray(i) && !i.is_vector && (i = array_to_list_(i, t)),
          (n = new Pair(i, n));
      }
      return n;
    },
    List = function () {
      var e = Array.from(arguments);
      return array_to_list_(e, !1);
    },
    array_to_list = function (e) {
      return array_to_list_(e, !1);
    },
    deep_array_to_list = function (e) {
      return array_to_list_(e, !0);
    },
    Cons = function (e, t) {
      return new Pair(e, t);
    },
    js_obj_to_alist = function (e) {
      if (void 0 === e) return nil;
      var t = [];
      return (
        Object.keys(e).forEach(function (n) {
          t.push(new Pair(n, e[n]));
        }),
        array_to_list(t)
      );
    },
    alist_to_js_obj = function (e) {
      if (e === nil) return {};
      var t = {};
      return (
        e.foreach(function (e) {
          t[e.car] = e.cdr;
        }),
        t
      );
    },
    truncate = function (e, t) {
      return e.length > t ? e.slice(0, t) + "..." : e;
    },
    write_simple = function (e) {
      if (void 0 === e) return "undefined";
      if (null === e) return "null";
      if ("function" == typeof e)
        return (
          "#<Function " +
          (e.fname ? e.fname : e.toSource ? truncate(e.toSource(), 40) : "") +
          ">"
        );
      if ("string" == typeof e)
        return (
          '"' +
          e
            .replace(/\\|\"/g, function (e) {
              return "\\" + e;
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
      if (Array.isArray(e))
        return (
          "#(" +
          e
            .map(function (e) {
              return write_simple(e);
            })
            .join(" ") +
          ")"
        );
      if ("function" == typeof e.to_write) return e.to_write();
      if (isNaN(e) && "number" == typeof e) return "+nan.0";
      switch (e) {
        case !0:
          return "#t";
        case !1:
          return "#f";
        case 1 / 0:
          return "+inf.0";
        case -1 / 0:
          return "-inf.0";
      }
      return inspect(e);
    },
    to_display = function (e) {
      return void 0 === e
        ? "undefined"
        : null === e
        ? "null"
        : e.to_display
        ? e.to_display(to_display)
        : "string" == typeof e.valueOf()
        ? e
        : e instanceof BiwaSymbol
        ? e.name
        : e instanceof Array
        ? "#(" + e.map(to_display).join(" ") + ")"
        : write_simple(e);
    },
    inspect = function (e, t) {
      try {
        return void 0 === e
          ? "undefined"
          : null === e
          ? "null"
          : !0 === e
          ? "#t"
          : !1 === e
          ? "#f"
          : e.inspect
          ? e.inspect()
          : "string" == typeof e
          ? '"' + e.replace(/"/g, '\\"') + '"'
          : Array.isArray(e)
          ? "[" + e.map(inspect).join(", ") + "]"
          : t && t.fallback
          ? t.fallback
          : e.toString();
      } catch (e) {
        if (e instanceof RangeError) return "...";
        throw e;
      }
    };
  function write(e) {
    const t = _preprocess(e);
    return t.cyclic ? _write_shared(e, t) : write_simple(e);
  }
  function _preprocess(e) {
    const t = {
      objs: new Set(),
      shared_objs: new Set(),
      parents: new Set(),
      cyclic: !1,
    };
    _gather_information(e, t);
    const n = new Map();
    for (const e of t.shared_objs) n.set(e, null);
    return { ids: n, last_id: -1, cyclic: t.cyclic };
  }
  function _gather_information(e, t) {
    t.parents.has(e) && (t.cyclic = !0),
      t.shared_objs.has(e) ||
        (t.objs.has(e)
          ? t.shared_objs.add(e)
          : (t.objs.add(e),
            isPair(e)
              ? (t.parents.add(e),
                _gather_information(e.car, t),
                _gather_information(e.cdr, t),
                t.parents.delete(e))
              : isVector(e) &&
                (t.parents.add(e),
                e.forEach((e) => {
                  _gather_information(e, t);
                }),
                t.parents.delete(e))));
  }
  function write_shared(e) {
    return _write_shared(e, _preprocess(e));
  }
  function _write_shared(e, t) {
    let n = "";
    if (t.ids.has(e)) {
      const r = t.ids.get(e);
      if (null !== r) return `#${r}#`;
      {
        const r = t.last_id + 1;
        t.ids.set(e, r), (t.last_id = r), (n += `#${r}=`);
      }
    }
    if (isPair(e)) {
      const r = [];
      r.push(_write_shared(e.car, t));
      for (let n = e.cdr; n !== nil; n = n.cdr) {
        if (!isPair(n) || t.ids.has(n)) {
          r.push("."), r.push(_write_shared(n, t));
          break;
        }
        r.push(_write_shared(n.car, t));
      }
      n += "(" + r.join(" ") + ")";
    } else if (isVector(e)) {
      n += "#(" + e.map((e) => _write_shared(e, t)).join(" ") + ")";
    } else n += write_simple(e);
    return n;
  }
  const to_write$1 = write,
    eq = function (e, t) {
      return e === t;
    },
    eqv = function (e, t) {
      return e == t && typeof e == typeof t;
    },
    equal = function (e, t) {
      return to_write$1(e) == to_write$1(t);
    },
    Chars = {};
  class Char {
    constructor(e) {
      Chars[(this.value = e)] = this;
    }
    to_write() {
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
    }
    to_display() {
      return this.value;
    }
    inspect() {
      return this.to_write();
    }
  }
  Char.get = function (e) {
    if ("string" != typeof e)
      throw new Bug("Char.get: " + inspect(e) + " is not a string");
    return void 0 === Chars[e] ? new Char(e) : Chars[e];
  };
  const isChar = function (e) {
    return e instanceof Char;
  };
  class Call {
    constructor(e, t, n) {
      (this.proc = e),
        (this.args = t),
        (this.after =
          n ||
          function (e) {
            return e[0];
          });
    }
    inspect() {
      return "#<Call args=" + this.args.inspect() + ">";
    }
    toString() {
      return "#<Call>";
    }
    to_write() {
      return "#<Call>";
    }
  }
  const Iterator = {
    ForArray: class {
      constructor(e) {
        (this.arr = e), (this.i = 0);
      }
      has_next() {
        return this.i < this.arr.length;
      }
      next() {
        return this.arr[this.i++];
      }
    },
    ForString: class {
      constructor(e) {
        (this.str = e), (this.i = 0);
      }
      has_next() {
        return this.i < this.str.length;
      }
      next() {
        return Char.get(this.str.charAt(this.i++));
      }
    },
    ForList: class {
      constructor(e) {
        this.ls = e;
      }
      has_next() {
        return this.ls instanceof Pair && this.ls != nil;
      }
      next() {
        var e = this.ls;
        return (this.ls = this.ls.cdr), e;
      }
    },
    ForMulti: class {
      constructor(e) {
        (this.objs = e),
          (this.size = e.length),
          (this.iterators = e.map(function (e) {
            return Iterator.of(e);
          }));
      }
      has_next() {
        for (var e = 0; e < this.size; e++)
          if (!this.iterators[e].has_next()) return !1;
        return !0;
      }
      next() {
        return this.iterators.map(function (e) {
          return e.next();
        });
      }
    },
    of: function (e) {
      switch (!0) {
        case e instanceof Array:
          return new this.ForArray(e);
        case "string" == typeof e:
          return new this.ForString(e);
        case e instanceof Pair:
        case e === nil:
          return new this.ForList(e);
        default:
          throw new Bug("Iterator.of: unknown class: " + inspect(e));
      }
    },
  };
  (Call.default_callbacks = {
    call: function (e) {
      return new Call(this.proc, [e]);
    },
    result: function () {},
    finish: function () {},
  }),
    (Call.foreach = function (e, t, n) {
      n || (n = !1),
        ["call", "result", "finish"].forEach(function (e) {
          t[e] || (t[e] = Call.default_callbacks[e]);
        });
      var r = null,
        i = null,
        a = function (s) {
          if (r) {
            var o = t.result(s[0], i);
            if (void 0 !== o) return o;
          } else r = n ? new Iterator.ForMulti(e) : Iterator.of(e);
          if (r.has_next()) {
            i = r.next();
            var u = t.call(i);
            return (u.after = a), u;
          }
          return t.finish();
        };
      return a(null);
    }),
    (Call.multi_foreach = function (e, t) {
      return Call.foreach(e, t, !0);
    });
  class Syntax {
    constructor(e, t) {
      (this.sname = e), (this.func = t);
    }
    transform(e) {
      if (!this.func)
        throw new Bug(
          "sorry, syntax " + this.sname + " is a pseudo syntax now"
        );
      return this.func(e);
    }
    inspect() {
      return "#<Syntax " + this.sname + ">";
    }
  }
  (CoreEnv.define = new Syntax("define")),
    (CoreEnv.begin = new Syntax("begin")),
    (CoreEnv.quote = new Syntax("quote")),
    (CoreEnv.lambda = new Syntax("lambda")),
    (CoreEnv.if = new Syntax("if")),
    (CoreEnv["set!"] = new Syntax("set!"));
  class VMCode {
    constructor(e) {
      if (!isVector(e)) throw (console.error(e), "not array");
      this.il = e;
    }
    to_write() {
      return "#<VMCode>";
    }
  }
  class Compiler {
    constructor() {}
    is_tail(e) {
      return "return" == e[0];
    }
    collect_free(e, t, n) {
      for (var r = n, i = e.arr, a = 0; a < i.length; a++)
        r = this.compile_refer(i[a], t, ["argument", r]);
      return r;
    }
    compile_refer(e, t, n) {
      return this.compile_lookup(
        e,
        t,
        function (e) {
          return ["refer-local", e, n];
        },
        function (e) {
          return ["refer-free", e, n];
        },
        function (e) {
          return ["refer-global", e, n];
        }
      );
    }
    compile_lookup(e, t, n, r, i) {
      var a,
        s = t[0],
        o = t[1];
      return null != (a = s.index(e))
        ? n(a)
        : null != (a = o.index(e))
        ? r(a)
        : i(e.name);
    }
    make_boxes(e, t, n) {
      for (var r = 0, i = []; t instanceof Pair; )
        e.member(t.car) && i.push(r), r++, (t = t.cdr);
      for (var a = n, s = i.length - 1; s >= 0; s--) a = ["box", i[s], a];
      return a;
    }
    find_sets(e, t) {
      var n = null;
      if (e instanceof BiwaSymbol) n = new BiwaSet();
      else if (e instanceof Pair)
        switch (e.first()) {
          case Sym("define"):
            var r = e.third();
            n = this.find_sets(r, t);
          case Sym("begin"):
            n = this.find_sets(e.cdr, t);
            break;
          case Sym("quote"):
            n = new BiwaSet();
            break;
          case Sym("lambda"):
            var i = e.second(),
              a = e.cdr.cdr;
            n =
              i instanceof Pair
                ? this.find_sets(a, t.set_minus(to_set(i)))
                : this.find_sets(a, t.set_minus(new BiwaSet(i)));
            break;
          case Sym("if"):
            var s = e.second(),
              o = e.third(),
              u = e.fourth();
            n = this.find_sets(s, t).set_union(
              this.find_sets(o, t),
              this.find_sets(u, t)
            );
            break;
          case Sym("set!"):
            var c = e.second(),
              l = e.third();
            n = t.member(c)
              ? this.find_sets(l, t).set_cons(c)
              : this.find_sets(l, t);
            break;
          case Sym("call/cc"):
            r = e.second();
            n = this.find_sets(r, t);
            break;
          default:
            for (var f = new BiwaSet(), d = e; d instanceof Pair; d = d.cdr)
              f = f.set_union(this.find_sets(d.car, t));
            n = f;
        }
      else n = new BiwaSet();
      if (null == n) throw new Bug("find_sets() exited in unusual way");
      return n;
    }
    find_free(e, t, n) {
      var r = null;
      if (e instanceof BiwaSymbol)
        r = n.member(e) ? new BiwaSet(e) : new BiwaSet();
      else if (e instanceof Pair)
        switch (e.first()) {
          case Sym("define"):
            var i = e.third();
            r = this.find_free(i, t, n);
            break;
          case Sym("begin"):
            r = this.find_free(e.cdr, t, n);
            break;
          case Sym("quote"):
            r = new BiwaSet();
            break;
          case Sym("lambda"):
            var a = e.second(),
              s = e.cdr.cdr;
            r =
              a instanceof Pair
                ? this.find_free(s, t.set_union(to_set(a)), n)
                : this.find_free(s, t.set_cons(a), n);
            break;
          case Sym("if"):
            var o = e.second(),
              u = e.third(),
              c = e.fourth();
            r = this.find_free(o, t, n).set_union(
              this.find_free(u, t, n),
              this.find_free(c, t, n)
            );
            break;
          case Sym("set!"):
            var l = e.second();
            i = e.third();
            r = n.member(l)
              ? this.find_free(i, t, n).set_cons(l)
              : this.find_free(i, t, n);
            break;
          case Sym("call/cc"):
            i = e.second();
            r = this.find_free(i, t, n);
            break;
          default:
            for (var f = new BiwaSet(), d = e; d instanceof Pair; d = d.cdr)
              f = f.set_union(this.find_free(d.car, t, n));
            r = f;
        }
      else r = new BiwaSet();
      if (null == r) throw new Bug("find_free() exited in unusual way");
      return r;
    }
    find_dot_pos(e) {
      for (var t = 0; e instanceof Pair; e = e.cdr, ++t);
      return e != nil ? t : -1;
    }
    last_pair(e) {
      if (e instanceof Pair) for (; e.cdr instanceof Pair; e = e.cdr);
      return e;
    }
    dotted2proper(e) {
      if (e === nil) return nil;
      if (e instanceof Pair) {
        var t = this.last_pair(e);
        if (t instanceof Pair && t.cdr === nil) return e;
        var n = (function (e) {
          for (var t = nil; e instanceof Pair; e = e.cdr)
            t = new Pair(e.car, t);
          return (function (e) {
            for (var t = nil; e instanceof Pair; ) {
              var n = e.cdr;
              (e.cdr = t), (t = e), (e = n);
            }
            return t;
          })(t);
        })(e);
        return (this.last_pair(n).cdr = new Pair(t.cdr, nil)), n;
      }
      return new Pair(e, nil);
    }
    compile(e, t, n, r, i) {
      for (var a = null; ; ) {
        if (e instanceof BiwaSymbol)
          return this.compile_refer(e, t, n.member(e) ? ["indirect", i] : i);
        if (!(e instanceof Pair)) return ["constant", e, i];
        switch (e.first()) {
          case Sym("define"):
            (e = (a = this._compile_define(e, i))[0]), (i = a[1]);
            break;
          case Sym("begin"):
            for (var s = [], o = e.cdr; o instanceof Pair; o = o.cdr)
              s.push(o.car);
            for (var u = i, c = s.length - 1; c >= 0; c--)
              u = this.compile(s[c], t, n, r, u);
            return u;
          case Sym("quote"):
            if (e.length() < 2)
              throw new BiwaError("Invalid quote: " + e.to_write());
            return ["constant", e.second(), i];
          case Sym("lambda"):
            return this._compile_lambda(e, t, n, r, i);
          case Sym("if"):
            if (e.length() < 3 || e.length() > 4)
              throw new BiwaError("Invalid if: " + e.to_write());
            var l = e.second(),
              f = e.third(),
              d = e.fourth();
            (f = this.compile(f, t, n, r, i)),
              (d = this.compile(d, t, n, r, i));
            (e = l), (i = ["test", f, d]);
            break;
          case Sym("set!"):
            if (3 != e.length())
              throw new BiwaError("Invalid set!: " + e.to_write());
            var h = e.second(),
              p =
                ((e = e.third()),
                this.compile_lookup(
                  h,
                  t,
                  function (e) {
                    return ["assign-local", e, i];
                  },
                  function (e) {
                    return ["assign-free", e, i];
                  },
                  function (e) {
                    return ["assign-global", e, i];
                  }
                ));
            i = p;
            break;
          case Sym("call/cc"):
            (e = e.second()),
              (u = [
                "conti",
                this.is_tail(i) ? t[0].size() + 1 : 0,
                [
                  "argument",
                  [
                    "constant",
                    1,
                    [
                      "argument",
                      this.compile(
                        e,
                        t,
                        n,
                        r,
                        this.is_tail(i)
                          ? ["shift", 1, ["tco_hinted_apply"]]
                          : ["apply"]
                      ),
                    ],
                  ],
                ],
              ]);
            return this.is_tail(i) ? u : ["frame", u, i];
          default:
            var m = e.car,
              _ = e.cdr;
            u = this.compile(
              m,
              t,
              n,
              r,
              this.is_tail(i)
                ? ["shift", _.length(), ["tco_hinted_apply"]]
                : ["apply"]
            );
            u = this.compile(_.length(), t, n, r, ["argument", u]);
            for (o = _; o instanceof Pair; o = o.cdr)
              u = this.compile(o.car, t, n, r, ["argument", u]);
            return this.is_tail(i) ? u : ["frame", u, i];
        }
      }
    }
    _compile_define(e, t) {
      if (1 == e.length())
        throw new BiwaError("Invalid `define': " + e.to_write());
      var n = e.cdr.car,
        r = e.cdr.cdr;
      if (n instanceof BiwaSymbol) {
        if (r === nil) e = undef;
        else {
          if (r.cdr !== nil)
            throw new BiwaError("Invalid `define': " + e.to_write());
          e = r.car;
        }
        TopEnv.hasOwnProperty(n.name) || (TopEnv[n.name] = undef),
          (t = ["assign-global", n.name, t]);
      } else {
        if (!(n instanceof Pair))
          throw new BiwaError("define: symbol or pair expected but got " + n);
        var i = n.car,
          a = n.cdr;
        (e = new Pair(Sym("lambda"), new Pair(a, r))),
          TopEnv.hasOwnProperty(n.name) || (TopEnv[i.name] = undef),
          (t = ["assign-global", i.name, t]);
      }
      return [e, t];
    }
    _compile_lambda(e, t, n, r, i) {
      if (e.length() < 3)
        throw new BiwaError("Invalid lambda: " + e.to_write());
      var a = e.cdr.car,
        s = e.cdr.cdr,
        o = Compiler.transform_internal_define(s);
      if (isPair(o) && isSymbol(o.car) && "letrec*" == o.car.name)
        var u = Compiler.expand(o);
      else u = new Pair(Sym("begin"), e.cdr.cdr);
      var c = this.find_dot_pos(a),
        l = this.dotted2proper(a),
        f = this.find_free(u, to_set(l), r),
        d = this.find_sets(u, to_set(l)),
        h = this.compile(
          u,
          [to_set(l), f],
          d.set_union(n.set_intersect(f)),
          r.set_union(to_set(l)),
          ["return"]
        ),
        p = [
          "close",
          a instanceof Pair ? a.length() : 0,
          f.size(),
          this.make_boxes(d, l, h),
          i,
          c,
        ];
      return this.collect_free(f, t, p);
    }
    run(e) {
      const t = this.compile(
        e,
        [new BiwaSet(), new BiwaSet()],
        new BiwaSet(),
        new BiwaSet(),
        ["halt"]
      );
      return new VMCode(t);
    }
  }
  (Compiler.compile = function (e, t) {
    return (e = Compiler.expand(e)), new Compiler().run(e, t);
  }),
    (Compiler.expand = function (e, t) {
      var n = Compiler.expand;
      t || (t = {});
      var r = null;
      if (e instanceof Pair)
        switch (e.car) {
          case Sym("define"):
            var i = e.cdr.car,
              a = e.cdr.cdr;
            r = new Pair(Sym("define"), new Pair(i, n(a, t)));
            break;
          case Sym("begin"):
            r = new Pair(Sym("begin"), n(e.cdr, t));
            break;
          case Sym("quote"):
            r = e;
            break;
          case Sym("lambda"):
            var s = e.cdr.car,
              o = e.cdr.cdr;
            r = new Pair(Sym("lambda"), new Pair(s, n(o, t)));
            break;
          case Sym("if"):
            var u = e.second(),
              c = e.third(),
              l = e.fourth();
            r = List(Sym("if"), n(u, t), n(c, t), n(l, t));
            break;
          case Sym("set!"):
            var f = e.second();
            e = e.third();
            r = List(Sym("set!"), f, n(e, t));
            break;
          case Sym("call-with-current-continuation"):
          case Sym("call/cc"):
            e = e.second();
            r = List(Sym("call/cc"), n(e, t));
            break;
          default:
            var d = null;
            if (
              (isSymbol(e.car) &&
                (TopEnv[e.car.name] instanceof Syntax
                  ? (d = TopEnv[e.car.name])
                  : CoreEnv[e.car.name] instanceof Syntax &&
                    (d = CoreEnv[e.car.name])),
              d)
            ) {
              var h;
              for (
                t.modified = !0, r = d.transform(e);
                (r = n(r, (h = {}))), h.modified;

              );
            } else {
              var p,
                m = n(e.car, t);
              if (!(e.cdr instanceof Pair) && e.cdr !== nil)
                throw new BiwaError(
                  "proper list required for function application or macro use: " +
                    to_write(e)
                );
              (p = array_to_list(
                e.cdr.to_array().map(function (e) {
                  return n(e, t);
                })
              )),
                (r = new Pair(m, p));
            }
        }
      else r = e;
      return r;
    });
  var is_definition = function (e) {
      return isPair(e) && Sym("define") === e.car;
    },
    define_to_lambda_bind = function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr;
      if (isSymbol(t)) return new Pair((r = t), n);
      var r = t.car,
        i = new Pair(Sym("lambda"), new Pair(t.cdr, n));
      return List(r, i);
    };
  function to_set(e) {
    if (e === nil) return new BiwaSet();
    for (var t = new BiwaSet(), n = e; n instanceof Pair; n = n.cdr)
      t.add(n.car);
    return t;
  }
  Compiler.transform_internal_define = function (e) {
    for (var t = [], n = e; is_definition(n.car); ) t.push(n.car), (n = n.cdr);
    var r = n;
    if (0 == t.length) return e;
    var i = List.apply(null, t.map(define_to_lambda_bind));
    return new Pair(Sym("letrec*"), new Pair(i, r));
  };
  const make_assert = function (e) {
      return function () {
        const t = "";
        e.apply(this, [t].concat(Array.from(arguments)));
      };
    },
    make_simple_assert = function (e, t, n) {
      return make_assert(function (n, r, i) {
        const a = i ? "(" + i + "): " : "";
        if (!t(r))
          throw new BiwaError(a + e + " required, but got " + to_write$1(r));
      });
    };
  class Hashtable {
    constructor(e, t, n) {
      (this.mutable = void 0 === n || !!n),
        (this.hash_proc = e),
        (this.equiv_proc = t),
        (this.pairs_of = {});
    }
    clear() {
      this.pairs_of = {};
    }
    candidate_pairs(e) {
      return this.pairs_of[e];
    }
    add_pair(e, t, n) {
      var r = this.pairs_of[e];
      r ? r.push([t, n]) : (this.pairs_of[e] = [[t, n]]);
    }
    remove_pair(e, t) {
      var n = this.pairs_of[e],
        r = n.indexOf(t);
      if (-1 == r) throw new Bug("Hashtable#remove_pair: pair not found!");
      n.splice(r, 1);
    }
    create_copy(e) {
      var t = new Hashtable(this.hash_proc, this.equiv_proc, e);
      return (
        Object.keys(this.pairs_of).forEach((e) => {
          let n = this.pairs_of[e].map((e) => [...e]);
          t.pairs_of[e] = n;
        }),
        t
      );
    }
    size() {
      var e = 0;
      return (
        this._apply_pair(function (t) {
          e++;
        }),
        e
      );
    }
    keys() {
      return this._apply_pair(function (e) {
        return e[0];
      });
    }
    values() {
      return this._apply_pair(function (e) {
        return e[1];
      });
    }
    _apply_pair(e) {
      var t = [];
      return (
        Object.values(this.pairs_of).forEach(function (n) {
          n.forEach(function (n) {
            t.push(e(n));
          });
        }),
        t
      );
    }
    to_write() {
      return "#<Hashtable size=" + this.size() + ">";
    }
  }
  const isHashtable = function (e) {
      return e instanceof Hashtable;
    },
    isMutableHashtable = function (e) {
      return e instanceof Hashtable && e.mutable;
    };
  (Hashtable.equal_hash = function (e) {
    return to_write$1(e[0]);
  }),
    (Hashtable.eq_hash = Hashtable.equal_hash),
    (Hashtable.eqv_hash = Hashtable.equal_hash),
    (Hashtable.string_hash = function (e) {
      return e[0];
    }),
    (Hashtable.string_ci_hash = function (e) {
      return "string" == typeof e[0] ? e[0].toLowerCase() : e[0];
    }),
    (Hashtable.symbol_hash = function (e) {
      return e[0] instanceof BiwaSymbol ? e[0].name : e[0];
    }),
    (Hashtable.eq_equiv = function (e) {
      return eq(e[0], e[1]);
    }),
    (Hashtable.eqv_equiv = function (e) {
      return eqv(e[0], e[1]);
    });
  class Complex {
    constructor(e, t) {
      (this.real = e), (this.imag = t);
    }
    magnitude() {
      return Math.sqrt(this.real * this.real + this.imag * this.imag);
    }
    angle() {
      return Math.atan2(this.imag, this.real);
    }
    isReal() {
      return 0 == this.imag;
    }
    isRational() {
      return 0 == this.imag && isRational(this.real);
    }
    isInteger() {
      return 0 == this.imag && isInteger(this.real);
    }
    toString(e) {
      if (0 === this.real && 0 === this.imag) return "0";
      var t = "";
      if (0 !== this.imag) {
        switch ((this.imag > 0 && 0 !== this.real && (t += "+"), this.imag)) {
          case 1:
            break;
          case -1:
            t += "-";
            break;
          default:
            t += this.imag.toString(e);
        }
        t += "i";
      }
      var n = "";
      return 0 !== this.real && (n += this.real.toString(e)), n + t;
    }
  }
  (Complex.from_polar = function (e, t) {
    var n = e * Math.cos(t),
      r = e * Math.sin(t);
    return new Complex(n, r);
  }),
    (Complex.assure = function (e) {
      return e instanceof Complex ? e : new Complex(e, 0);
    });
  class Rational$1 {
    constructor(e, t) {
      (this.numerator = e), (this.denominator = t);
    }
    isInteger() {}
  }
  const isNumber = function (e) {
      return (
        e instanceof Complex || e instanceof Rational$1 || "number" == typeof e
      );
    },
    isComplex = isNumber,
    isReal = function (e) {
      return e instanceof Complex || e instanceof Rational$1
        ? e.isReal()
        : "number" == typeof e;
    },
    isRational = function (e) {
      return e instanceof Complex
        ? e.isRational()
        : e instanceof Rational$1 || "number" == typeof e;
    },
    isInteger = function (e) {
      return e instanceof Complex || e instanceof Rational$1
        ? e.isInteger()
        : "number" == typeof e && e % 1 == 0;
    };
  class Unterminated extends BiwaError {}
  const DIGITS = {
      2: /^[01]+/,
      8: /^[0-7]+/,
      10: /^[0-9]+/,
      16: /^[0-9a-fA-F]+/,
    },
    PAREN = { "(": ")", "{": "}", "[": "]" },
    NAMED_CHARS = {
      alarm: "",
      backspace: "\b",
      delete: "",
      escape: "",
      newline: "\n",
      null: "\0",
      return: "\r",
      space: " ",
      tab: "\t",
    },
    NAMED_CHARS_REXP = new RegExp(
      "^(" + Object.keys(NAMED_CHARS).join("|") + ")\\b"
    ),
    ESCAPE_SEQUENCES = { a: "", b: "\b", t: "\t", n: "\n", r: "\r" };
  class Parser {
    constructor(e) {
      (this.txt = e),
        (this.i = 0),
        (this.foldCase = !1),
        (this.labelledData = []);
    }
    insert(e) {
      const t = this.txt;
      this.txt = t.slice(0, this.i) + e + t.slice(this.i);
    }
    inspect() {
      return `#<Parser (${this.i}/${this.txt.length})>`;
    }
    getObject() {
      let e;
      if ((this._skipAtmosphere(), this.done())) e = Parser.EOS;
      else
        switch (this.txt[this.i]) {
          case "#":
            this.i++, (e = this._parseSharp());
            break;
          case "(":
          case "[":
          case "{":
            e = this._parseList();
            break;
          case '"':
            e = this._parseString();
            break;
          case "|":
            e = this._parseEnclosedSymbol();
            break;
          case "'":
            this.i++, (e = this._parseQuote("quote"));
            break;
          case "`":
            this.i++, (e = this._parseQuote("quasiquote"));
            break;
          case ",":
            this.i++,
              "@" == this.txt[this.i]
                ? (this.i++, (e = this._parseQuote("unquote-splicing")))
                : (e = this._parseQuote("unquote"));
            break;
          default:
            e = this._parseDecimalNumberOrIdentifier();
        }
      return e;
    }
    _getObjectOrThrowUnterminated(e) {
      const t = this.getObject();
      if (t === Parser.EOS) throw new Unterminated(e);
      return t;
    }
    _skipWhitespace() {
      for (; this.i < this.txt.length; )
        switch (this.txt[i]) {
          case " ":
          case "\t":
          case "\n":
            i++;
            break;
          default:
            return;
        }
    }
    _skipAtmosphere() {
      for (; this.i < this.txt.length; )
        switch (this.txt[this.i]) {
          case " ":
          case "\t":
          case "\n":
            this.i++;
            break;
          case ";":
            const e = this.match(/^;[^\n]*(\n|$)/);
            this.i += e[0].length;
            break;
          case "#":
            if (";" == this.txt[this.i + 1])
              (this.i += "#;".length),
                this._skipAtmosphere(),
                this._getObjectOrThrowUnterminated("missing argument for `#;`");
            else if ("|" == this.txt[this.i + 1])
              (this.i += "#|".length), this._skipBlockComment();
            else if (this.match(/^#!fold-case/))
              (this.i += "#!fold-case".length), (this.foldCase = !0);
            else {
              if (!this.match(/^#!no-fold-case/)) return;
              (this.i += "#!no-fold-case".length), (this.foldCase = !1);
            }
            break;
          default:
            return;
        }
    }
    _skipBlockComment() {
      let e = 1;
      for (; this.i < this.txt.length; ) {
        const t = this.match(/\|#/);
        if (null === t) break;
        const n = /#\|/.exec(this.txt.slice(this.i, t.index));
        if (n) e++, (this.i += n.index + "#|".length);
        else if (((this.i += t.index + "|#".length), e--, 0 == e)) return;
      }
      throw new Unterminated(
        "unterminated block comment (`|#` not found)",
        this.rest()
      );
    }
    _parseQuote(e) {
      this._skipAtmosphere();
      const t = this._getObjectOrThrowUnterminated(`unterminated ${e}`);
      return List(Sym(e), t);
    }
    _parseSharp() {
      switch (this.txt[this.i]) {
        case "t":
          return this.match(/^true/) ? (this.i += "true".length) : this.i++, !0;
        case "f":
          return (
            this.match(/^false/) ? (this.i += "false".length) : this.i++, !1
          );
        case "\\":
          return this.i++, this._parseChar();
        case "(":
          return this.i++, this._parseVector();
        case "u":
          if (this.match(/^u8\(/))
            throw new BiwaError(
              "bytevectors are not supported yet",
              this.rest(-1)
            );
          break;
        case "e":
        case "i":
        case "b":
        case "o":
        case "d":
        case "x":
          return this.i--, this._parsePrefixedNumber();
        default:
          if (this.match(/^\d/)) return this._parseDatumLabel();
      }
      throw new BiwaError("unknown #-syntax", this.rest(-1));
    }
    _parseChar() {
      let e = this.match(NAMED_CHARS_REXP);
      if (e) return (this.i += e[0].length), Char.get(NAMED_CHARS[e[1]]);
      if (((e = this.match(/^x([a-zA-Z0-9]+)/)), e))
        return (
          (this.i += e[0].length),
          Char.get(String.fromCharCode(parseInt(e[1], 16)))
        );
      if (this.done())
        throw new Unterminated("got EOS on char literal", this.rest(-2));
      {
        const e = this.txt[this.i];
        return this.i++, Char.get(e);
      }
    }
    _parseVector() {
      this.i;
      const e = [];
      e: for (; this.i < this.txt.length; )
        switch ((this._skipAtmosphere(), this.txt[this.i])) {
          case ")":
            this.i++;
            break e;
          case "]":
          case "}":
            throw new BiwaError("extra close paren", this.rest());
          default:
            e.push(this.getObject());
        }
      return e;
    }
    _parsePrefixedNumber() {
      let e = 10;
      return (
        this.match(/^#[iIeE]/) && (this.i += 2),
        this.match(/^#[bB]/)
          ? ((e = 2), (this.i += 2))
          : this.match(/^#[oO]/)
          ? ((e = 8), (this.i += 2))
          : this.match(/^#[dD]/)
          ? ((e = 10), (this.i += 2))
          : this.match(/^#[xX]/) && ((e = 16), (this.i += 2)),
        this.match(/^#[iIeE]/) && (this.i += 2),
        this._parseComplexNumber(e)
      );
    }
    _parseComplexNumber(e) {
      const t = this._parseRealNumber(e),
        n = this.txt[this.i];
      return "@" == n
        ? (this.i++, this._parsePolarComplexNumber(t, e))
        : "+" == n || "-" == n
        ? (this.i--, this._parseOrthoComplexNumber(t, e))
        : t;
    }
    _parsePolarComplexNumber(e, t) {
      const n = this._parseRealNumber(t);
      return Complex.from_polar(e, n);
    }
    _parseOrthoComplexNumber(e, t) {
      const n = this._parseRealNumber(t);
      if (this.match(/^[iI]/)) return this.i++, new Complex(e, n);
      throw new BiwaError(
        "invalid complex number format (missing `i`)",
        this.rest()
      );
    }
    _parseRealNumber(e, t = !1) {
      if (t && 10 != e) throw new Bug("base must be 10 if maybeSymbol");
      let n = "";
      const r = this.match(/^(\+|-)(inf.0|nan.0)/);
      if (r)
        return (
          (this.i += "+inf.0".length),
          ("inf.0" == r[2] ? 1 / 0 : NaN) * ("+" == r[1] ? 1 : -1)
        );
      let i = 1;
      this.match(/^\+/)
        ? (this.i++, (n += "+"))
        : this.match(/^\-/) && (this.i++, (n += "-"), (i = -1));
      let a = null;
      const s = this.match(DIGITS[e]);
      if (s) (this.i += s[0].length), (n += s[0]), (a = parseInt(s[0], e) * i);
      else if (10 == e && "." == this.txt[this.i]);
      else if (!t)
        throw new BiwaError("invalid char in number literal", this.rest());
      if ("/" == this.txt[this.i]) {
        this.i++;
        const r = this.match(DIGITS[e]);
        if (r) {
          this.i += r[0].length;
          const t = parseInt(r[0], e);
          return new Rational(a, t);
        }
        if (!t)
          throw new BiwaError(
            "invalid char in rational number literal",
            this.rest()
          );
        n += "/";
      }
      if (10 == e) {
        this.i -= n.length;
        const e = this.match(/^[+-]?(\d+\.\d*|\.?\d+)([eE][+-]?\d+)?/);
        if (e) return (this.i += e[0].length), parseFloat(e[0]);
        this.i += n.length;
      }
      if (t) return n;
      throw new BiwaError(
        `invalid chars in number literal (${n})`,
        this.rest()
      );
    }
    _parseDatumLabel() {
      const e = this.match(/^(\d+)(=|#)/);
      if (e) {
        this.i += e[0].length;
        const t = parseInt(e[1]);
        if ("=" == e[2]) {
          const e = this._getObjectOrThrowUnterminated(
            "got EOS for labelled datum"
          );
          return (this.labelledData[t] = e), e;
        }
        if (this.labelledData.hasOwnProperty(t)) return this.labelledData[t];
        throw new BiwaError("undefined datum reference", this.rest(-1));
      }
      throw new BiwaError("unknown #-syntax", this.rest(-1));
    }
    _parseList() {
      const e = this.i,
        t = this.txt[this.i];
      this.i++;
      const n = PAREN[t];
      let r = !1,
        i = nil,
        a = i;
      for (; this.i < this.txt.length; ) {
        this._skipAtmosphere();
        const t = this.txt[this.i];
        if (t == n) return this.i++, i;
        if (")" == t || "]" == t || "}" == t)
          throw new BiwaError("extra close paren", this.rest());
        if ("." == t && this.match(/^\.[\s]/)) {
          if (i === nil)
            throw new BiwaError("no list element before `.`", this.from(e));
          (r = !0), this.i++;
          const t = this.getObject();
          if (t === Parser.EOS)
            throw new Unterminated("found EOS after `.` in list", this.from(e));
          a.cdr = t;
        } else {
          if (r)
            throw new BiwaError(
              "more than one element after `.`",
              this.from(e)
            );
          {
            const t = this.getObject();
            if (t === Parser.EOS)
              throw (
                ((this.i = e),
                new Unterminated("found EOS in list", this.rest()))
              );
            const n = new Pair(t, nil);
            i === nil ? (i = n) : (a.cdr = n), (a = n);
          }
        }
      }
      throw ((this.i = e), new Unterminated("found EOS in list", this.rest()));
    }
    _parseString() {
      const e = this.match(/^"((\\"|[^"])*)"/);
      if (e) {
        this.i += e[0].length;
        const t = e[1].replaceAll(/\\\s*\n\s*/g, "");
        return this._replaceEscapedChars(t);
      }
      throw new Unterminated("invalid string literal", this.rest());
    }
    _parseEnclosedSymbol() {
      const e = this.match(/^\|((\\\||[^\|])*)\|/);
      if (e)
        return (this.i += e[0].length), Sym(this._replaceEscapedChars(e[1]));
      throw new Unterminated("invalid symbol literal", this.rest());
    }
    _replaceEscapedChars(e) {
      return e
        .replaceAll(/\\x([0-9a-f]+);/gi, (e, t) =>
          String.fromCharCode(parseInt(t, 16))
        )
        .replaceAll(/\\(.)/g, (e, t) => ESCAPE_SEQUENCES[t] || t);
    }
    _parseDecimalNumberOrIdentifier() {
      const e = this.txt[this.i];
      if ("#" == e) throw new Bug("#-syntax must be parsed beforehand");
      if (void 0 === e) throw new Bug("EOS must be handled beforehand");
      let t = this._parseRealNumber(10, !0);
      if (isString(t)) {
        const e = this.match(/^[^\s)}\]]+/);
        return (
          e && ((this.i += e[0].length), (t += e[0])),
          this.foldCase && (t = t.toLowerCase()),
          Sym(t)
        );
      }
      return t;
    }
    rest(e = 0) {
      return this.txt.slice(this.i + e);
    }
    from(e) {
      return this.txt.slice(e);
    }
    match(e, t = 0) {
      return e.exec(this.rest(t));
    }
    done() {
      return this.i >= this.txt.length;
    }
  }
  (Parser.EOS = new Object()),
    (Parser.EOS.toString = () => "#<BiwaScheme.Parser.EOS>"),
    (Parser.parse = (e) => {
      const t = new Parser(e),
        n = [];
      for (;;) {
        var r = t.getObject();
        if (r === Parser.EOS) break;
        n.push(r);
      }
      return n;
    }),
    (Parser.Unterminated = Unterminated);
  class Interpreter {
    constructor() {
      var e = null,
        t = null;
      2 == arguments.length
        ? ((e = arguments[0]), (t = arguments[1]))
        : 1 == arguments.length && arguments[0] instanceof Interpreter
        ? (e = arguments[0])
        : 1 == arguments.length &&
          "function" == typeof arguments[0] &&
          (t = arguments[0]),
        (this.stack = []),
        (this.on_error = t || (e ? e.on_error : function (e) {})),
        (this.after_evaluate = function () {}),
        (this.last_refer = e ? e.last_refer : null),
        (this.call_stack = e ? [...e.call_stack] : []),
        (this.tco_counter = []),
        (this.max_trace_size = e ? e.max_trace_size : max_trace_size),
        (this.current_dynamic_winder = Interpreter.DynamicWind.ROOT);
    }
    inspect() {
      return [
        "#<Interpreter: stack size=>",
        this.stack.length,
        " ",
        "after_evaluate=",
        inspect(this.after_evaluate),
        ">",
      ].join("");
    }
    push(e, t) {
      return (this.stack[t] = e), t + 1;
    }
    save_stack(e) {
      for (var t = [], n = 0; n < e; n++) t[n] = this.stack[n];
      return {
        stack: t,
        last_refer: this.last_refer,
        call_stack: [...this.call_stack],
        tco_counter: [...this.tco_counter],
      };
    }
    restore_stack(e) {
      const t = e.stack,
        n = t.length;
      for (var r = 0; r < n; r++) this.stack[r] = t[r];
      return (
        (this.last_refer = e.last_refer),
        (this.call_stack = [...e.call_stack]),
        (this.tco_counter = [...e.tco_counter]),
        n
      );
    }
    capture_continuation(e, t) {
      var n = this.push(t, e);
      return this.closure(
        ["nuate1", this.save_stack(n), this.current_dynamic_winder],
        1,
        0,
        null,
        -1
      );
    }
    shift_args(e, t, n) {
      for (var r = e; r >= 0; r--)
        this.index_set(n, r + t + 1, this.index(n, r));
      return n - t - 1;
    }
    index(e, t) {
      return this.stack[e - 1 - t];
    }
    index_set(e, t, n) {
      this.stack[e - 1 - t] = n;
    }
    closure(e, t, n, r, i) {
      const a = [];
      for (var s = 0; s < n; s++) a[s] = this.index(r, s);
      return new Closure(e, a, i, -1 == i ? t : void 0);
    }
    run_dump_hook(e, t, n, r, i) {
      var a, s;
      if (this.dumper) a = this.dumper;
      else {
        if (!Interpreter.dumper) return;
        a = Interpreter.dumper;
      }
      a &&
        ((s = { a: e, f: n, c: r, s: i, x: t, stack: this.stack }), a.dump(s));
    }
    _execute(e, t, n, r, i) {
      for (var a = null; ; )
        switch ((this.run_dump_hook(e, t, n, r, i), t[0])) {
          case "halt":
            return e;
          case "refer-local":
            var s = t[1];
            t = t[2];
            (e = this.index(n, s + 1)), (this.last_refer = "(anon)");
            break;
          case "refer-free":
            (s = t[1]), (t = t[2]);
            (e = r.freevars[s]), (this.last_refer = "(anon)");
            break;
          case "refer-global":
            var o = t[1];
            t = t[2];
            if (TopEnv.hasOwnProperty(o)) var u = TopEnv[o];
            else {
              if (!CoreEnv.hasOwnProperty(o))
                throw new BiwaError("execute: unbound symbol: " + inspect(o));
              u = CoreEnv[o];
            }
            (e = u), (this.last_refer = o || "(anon)");
            break;
          case "indirect":
            t = t[1];
            e = e[0];
            break;
          case "constant":
            var c = t[1];
            t = t[2];
            (e = c), (this.last_refer = "(anon)");
            break;
          case "close":
            var l = t,
              f = l[1],
              d = ((s = l[2]), l[3]),
              h = ((t = l[4]), l[5]);
            (e = this.closure(d, f, s, i, h)), (i -= s);
            break;
          case "box":
            (s = t[1]), (t = t[2]);
            this.index_set(i, s + 1, [this.index(i, s + 1)]);
            break;
          case "test":
            var p = t[1],
              m = t[2];
            t = !1 !== e ? p : m;
            break;
          case "assign-global":
            var _ = t[1];
            t = t[2];
            if (!TopEnv.hasOwnProperty(_) && !CoreEnv.hasOwnProperty(_))
              throw new BiwaError("global variable '" + _ + "' is not defined");
            (TopEnv[_] = e), (e = undef);
            break;
          case "assign-local":
            (s = t[1]), (t = t[2]);
            (this.index(n, s + 1)[0] = e), (e = undef);
            break;
          case "assign-free":
            (s = t[1]), (t = t[2]);
            (r.freevars[s][0] = e), (e = undef);
            break;
          case "conti":
            (s = t[1]), (t = t[2]);
            e = this.capture_continuation(i, s);
            break;
          case "nuate1":
            var b = t[1],
              g = t[2],
              y = this.current_dynamic_winder,
              v = Interpreter.DynamicWind.listWinders(y, g);
            t = Interpreter.DynamicWind.joinWinders(v, [
              "refer-local",
              0,
              ["nuate2", b],
            ]);
            break;
          case "nuate2":
            (b = t[1]), (t = ["return"]);
            i = this.restore_stack(b);
            break;
          case "frame":
            a = t[2];
            (t = t[1]),
              (i = this.push(a, this.push(n, this.push(r, i)))),
              (this.tco_counter[this.tco_counter.length] = 0);
            break;
          case "argument":
            t = t[1];
            i = this.push(e, i);
            break;
          case "shift":
            (s = t[1]), (t = t[2]);
            var w = this.index(i, s + 1);
            i = this.shift_args(s, w, i);
            break;
          case "tco_hinted_apply":
            this.tco_counter[this.tco_counter.length - 1]++,
              (t = ["apply"].concat(t.slice(1)));
            break;
          case "apply":
            var x = e;
            this.call_stack.push(this.last_refer),
              this.call_stack.length > this.max_trace_size &&
                this.call_stack.shift();
            w = this.index(i, 0);
            if (isClosure(x)) {
              (e = x), (t = x.body);
              const a = x.dotpos;
              if (a >= 0) {
                for (var S = nil, C = w; --C >= a; )
                  S = new Pair(this.index(i, C + 1), S);
                if (a >= w) {
                  for (C = 0; C < w + 1; C++)
                    this.index_set(i, C - 1, this.index(i, C));
                  i++, this.index_set(i, 0, this.index(i, 0) + 1);
                }
                this.index_set(i, a + 1, S);
              } else if (void 0 !== x.expected_args && w != x.expected_args) {
                var E =
                  "Function call error: got " +
                  w +
                  " but wanted " +
                  x.expected_args;
                throw new BiwaError(E);
              }
              (n = i), (r = x);
            } else {
              if (!(x instanceof Function))
                throw new BiwaError(inspect(x) + " is not a function");
              var P = [];
              for (C = 0; C < w; C++) P.push(this.index(i, C + 1));
              var k = x(P, this);
              if (k instanceof Pause) {
                var T = k;
                return T.set_state(this, ["return"], n, r, i), T.ready(), T;
              }
              if (k instanceof Call) {
                var j = [
                    "frame",
                    [
                      "argument",
                      [
                        "constant",
                        1,
                        ["argument", ["constant", k.after, ["apply"]]],
                      ],
                    ],
                    ["return"],
                  ],
                  B = [
                    "constant",
                    k.args.length,
                    [
                      "argument",
                      ["constant", k.proc, ["apply", k.args.length]],
                    ],
                  ];
                t = [
                  "frame",
                  k.args.reduce(function (e, t) {
                    return ["constant", t, ["argument", e]];
                  }, B),
                  j,
                ];
              } else (e = k), (t = ["return"]);
            }
            break;
          case "return":
            var q = i - (s = this.index(i, 0));
            (t = this.index(q, 1)),
              (n = this.index(q, 2)),
              (r = this.index(q, 3)),
              (i = q - 3 - 1);
            var D = 1 + this.tco_counter[this.tco_counter.length - 1];
            this.call_stack.splice(-D), this.tco_counter.pop();
            break;
          default:
            throw new Bug("unknown opecode type: " + t[0]);
        }
      return e;
    }
    evaluate(e, t) {
      (this.call_stack = []),
        (this.parser = new Parser(e)),
        (this.compiler = new Compiler()),
        t && (this.after_evaluate = t),
        (this.is_top = !0),
        (this.file_stack = []);
      try {
        return this.resume(!1);
      } catch (e) {
        return (
          (e.message = e.message + " [" + this.call_stack.join(", ") + "]"),
          this.on_error(e)
        );
      }
    }
    resume(e, t, n, r, i, a) {
      for (var s = undef; ; ) {
        if (e) (s = this._execute(t, n, r, i, a)), (e = !1);
        else {
          if (!this.parser) break;
          var o = this.parser.getObject();
          if (o === Parser.EOS) break;
          o = Compiler.expand(o);
          const e = this.compiler.run(o);
          s = this._execute(o, e.il, 0, [], 0);
        }
        if (s instanceof Pause) return s;
      }
      return this.after_evaluate(s), s;
    }
    evaluate_vmcode(e) {
      (this.call_stack = []), (this.is_top = !0), (this.file_stack = []);
      try {
        const t = this._execute(undef, e.il, 0, [], 0);
        return t instanceof Pause || this.after_evaluate(t), t;
      } catch (e) {
        return (
          (e.message = e.message + " [" + this.call_stack.join(", ") + "]"),
          this.on_error(e)
        );
      }
    }
    invoke_closure(e, t) {
      t || (t = []);
      for (
        var n = t.length,
          r = ["constant", n, ["argument", ["constant", e, ["apply"]]]],
          i = 0;
        i < n;
        i++
      )
        r = ["constant", t[i], ["argument", r]];
      return this._execute(e, ["frame", r, ["halt"]], 0, e, 0);
    }
    compile(e) {
      var t = Interpreter.read(e);
      return Compiler.compile(t);
    }
    push_dynamic_winder(e, t) {
      this.current_dynamic_winder = new Interpreter.DynamicWind(
        this.current_dynamic_winder,
        e,
        t
      );
    }
    pop_dynamic_winder(e, t) {
      this.current_dynamic_winder = this.current_dynamic_winder.parent;
    }
  }
  (Interpreter.read = function (e) {
    var t = new Parser(e).getObject();
    return t == Parser.EOS ? eof : t;
  }),
    (Interpreter.expand = function () {
      throw "Interpreter.expand is moved to Compiler.expand";
    }),
    (Interpreter.DynamicWind = class {
      constructor(e, t, n) {
        (this.parent = e), (this.before = t), (this.after = n);
      }
    }),
    (Interpreter.DynamicWind.ROOT = { _: "this is ROOT." }),
    (Interpreter.DynamicWind.listWinders = function (e, t) {
      for (var n = [e]; e !== Interpreter.DynamicWind.ROOT; )
        (e = e.parent), n.push(e);
      for (var r, i = []; ; ) {
        var a = n.find(function (e) {
          return e === t;
        });
        if (a) {
          r = a;
          break;
        }
        i.push(t), (t = t.parent);
      }
      for (var s = [], o = 0; o < n.length && n[o] !== r; o++)
        s.push(n[o].after);
      return (
        i.reverse(),
        i.forEach(function (e) {
          s.push(e.before);
        }),
        s
      );
    }),
    (Interpreter.DynamicWind.joinWinders = function (e, t) {
      return e.reduceRight(function (e, t) {
        return [
          "frame",
          ["constant", 0, ["argument", ["constant", t, ["apply"]]]],
          e,
        ];
      }, t);
    });
  class BiwaPromise {
    constructor(e, t) {
      this.box = [e, t];
    }
    is_done() {
      return this.box[0];
    }
    value() {
      if (!this.is_done()) throw new Bug("this promise is not calculated yet");
      return this.box[1];
    }
    thunk() {
      if (this.is_done()) throw new Bug("this promise does not know the thunk");
      return this.box[1];
    }
    update_with(e) {
      (this.box[0] = e.box[0]), (this.box[1] = e.box[1]), (e.box = this.box);
    }
  }
  const isPromise = function (e) {
    return e instanceof BiwaPromise;
  };
  (BiwaPromise.fresh = function (e) {
    return new BiwaPromise(!1, e);
  }),
    (BiwaPromise.done = function (e) {
      return new BiwaPromise(!0, e);
    });
  const check_arity = function (e, t, n, r) {
      if (t < n)
        throw new BiwaError(
          r && r == n
            ? e +
              ": wrong number of arguments (expected: " +
              n +
              " got: " +
              t +
              ")"
            : e + ": too few arguments (at least: " + n + " got: " + t + ")"
        );
      if (r && r < t)
        throw new BiwaError(
          e + ": too many arguments (at most: " + r + " got: " + t + ")"
        );
    },
    define_libfunc = function (e, t, n, r) {
      var i = function (i, a) {
        return check_arity(e, i.length, t, n), r(i, a);
      };
      (r.fname = e),
        (i.inspect = function () {
          return this.fname;
        }),
        (CoreEnv[e] = i);
    },
    alias_libfunc = function (e, t) {
      CoreEnv[e]
        ? Array.isArray(t)
          ? t.map(function (t) {
              alias_libfunc(e, t);
            })
          : "string" == typeof t
          ? (CoreEnv[t] = CoreEnv[e])
          : console.error(
              "[BUG] bad alias for library function `" +
                e +
                "': " +
                t.toString()
            )
        : console.error(
            "[BUG] library function `" +
              e +
              "' does not exist, so can't alias it."
          );
    },
    define_syntax = function (e, t) {
      var n = new Syntax(e, t);
      CoreEnv[e] = n;
    },
    define_scmfunc = function (e, t, n, r) {
      new Interpreter().evaluate("(define " + e + " " + r + "\n)");
    },
    assert_number = make_simple_assert("number", function (e) {
      return "number" == typeof e || e instanceof Complex;
    }),
    assert_integer = make_simple_assert("integer", function (e) {
      return "number" == typeof e && e % 1 == 0;
    }),
    assert_real = make_simple_assert("real number", function (e) {
      return "number" == typeof e;
    }),
    assert_between = make_assert(function (e, t, n, r) {
      if ("number" != typeof t || t != Math.round(t))
        throw new BiwaError(e + ": number required, but got " + to_write$1(t));
      if (t < n || r < t)
        throw new BiwaError(
          e +
            ": number must be between " +
            n +
            " and " +
            r +
            ", but got " +
            to_write$1(t)
        );
    }),
    assert_string = make_simple_assert("string", isString),
    assert_char = make_simple_assert("character", isChar),
    assert_symbol = make_simple_assert("symbol", isSymbol),
    assert_port = make_simple_assert("port", isPort),
    assert_pair = make_simple_assert("pair", isPair),
    assert_list = make_simple_assert("list", isList),
    assert_vector = make_simple_assert("vector", isVector),
    assert_hashtable = make_simple_assert("hashtable", isHashtable),
    assert_mutable_hashtable = make_simple_assert(
      "mutable hashtable",
      isMutableHashtable
    ),
    assert_promise = make_simple_assert("promise", isPromise),
    assert_function = make_simple_assert("JavaScript function", isFunction),
    assert_closure = make_simple_assert("scheme closure", isClosure),
    assert_procedure = make_simple_assert("scheme/js function", function (e) {
      return isClosure(e) || isFunction(e);
    }),
    assert_date = make_simple_assert("date", function (e) {
      return e instanceof Date;
    }),
    assert = make_assert(function (e, t, n, r) {
      if (!t) throw new BiwaError((r || e) + ": " + n);
    }),
    deprecate = function (e, t, n) {
      var r =
        e +
        " is deprecated and will be removed in BiwaScheme " +
        t +
        ". Please use " +
        n +
        " instead";
      console.warn(r);
    },
    parse_fraction = function (e) {
      assert_string(e);
      var t = e.split("/");
      if (2 !== t.length) return !1;
      var n = t[0],
        r = t[1],
        i = parse_integer(n, 10),
        a = parse_integer(r, 10);
      return !1 !== i && !1 !== a && !(a <= 0) && i / a;
    },
    is_valid_integer_notation = function (e, t) {
      if ((assert_string(e), assert_integer(t), t < 2 || t > 36)) return !1;
      var n = "0123456789abcdefghijklmnopqrstuvwxyz".slice(0, t);
      return new RegExp("^[+-]?[" + n + "]+$", "ig").test(e);
    },
    parse_integer = function (e, t) {
      if ((assert_string(e), assert_integer(t), t < 2 || t > 36)) return !1;
      if (!is_valid_integer_notation(e, t)) return !1;
      var n = parseInt(e, t);
      return !Number.isNaN(n) && n;
    },
    is_valid_float_notation = function (e) {
      assert_string(e);
      return (
        !(
          !/^[+-]?[0-9]+[.]?[0-9]*e[+-]?[0-9]+$/i.test(e) &&
          !/(^[+-]?[0-9]*[.][0-9]+$)|(^[+-]?[0-9]+[.][0-9]*$)/.test(e)
        ) || is_valid_integer_notation(e, 10)
      );
    },
    parse_float = function (e) {
      if ((assert_string(e), !is_valid_float_notation(e))) return !1;
      var t = new Number(e).valueOf();
      return !Number.isNaN(t) && !!Number.isFinite(t) && t;
    },
    Enumeration = {
      EnumType: class {
        constructor(e) {
          this.members = [...new Set(e)];
        }
        universe() {
          return new Enumeration.EnumSet(this, this.members);
        }
        indexer() {
          return (e) => {
            assert_symbol(e[0], "(enum-set indexer)");
            var t = this.members.indexOf(e[0]);
            return -1 !== t && t;
          };
        }
        constructor_() {
          return (e) => {
            assert_list(e[0], "(enum-set constructor)");
            var t = e[0].to_array();
            return (
              t.forEach(function (e) {
                assert_symbol(e, "(enum-set constructor)");
              }),
              new Enumeration.EnumSet(this, t)
            );
          };
        }
      },
      EnumSet: class {
        constructor(e, t) {
          (this.enum_type = e),
            (this.symbols = e.members.filter((e) => t.includes(e)));
        }
        symbol_list() {
          return array_to_list(this.symbols);
        }
        is_member(e) {
          return this.symbols.includes(e);
        }
        is_subset(e) {
          return (
            !this.symbols.some((t) => !e.symbols.includes(t)) &&
            (this.enum_type === e.enum_type ||
              this.enum_type.members.every((t) =>
                e.enum_type.members.includes(t)
              ))
          );
        }
        equal_to(e) {
          return this.is_subset(e) && e.is_subset(this);
        }
        union(e) {
          var t = this.enum_type.members.filter(
            (t) => this.symbols.includes(t) || e.symbols.includes(t)
          );
          return new Enumeration.EnumSet(this.enum_type, t);
        }
        intersection(e) {
          var t = this.symbols.filter((t) => e.symbols.includes(t));
          return new Enumeration.EnumSet(this.enum_type, t);
        }
        difference(e) {
          var t = this.symbols.filter((t) => !e.symbols.includes(t));
          return new Enumeration.EnumSet(this.enum_type, t);
        }
        complement() {
          var e = this.enum_type.members.filter(
            (e) => !this.symbols.includes(e)
          );
          return new Enumeration.EnumSet(this.enum_type, e);
        }
        projection(e) {
          var t = this.symbols.filter((t) => e.enum_type.members.includes(t));
          return new Enumeration.EnumSet(e.enum_type, t);
        }
        toString() {
          return "#<EnumSet " + inspect(this.symbols) + ">";
        }
      },
    },
    isEnumSet = function (e) {
      return e instanceof Enumeration.EnumSet;
    },
    assert_enum_set = make_simple_assert("enum_set", isEnumSet),
    memoize = function (e, t) {
      const n = e.prototype;
      t.forEach((e) => {
        (n["compute_" + e] = n[e]),
          (n[e] = function () {
            return (
              this.hasOwnProperty("cached_" + e) ||
                (this["cached_" + e] = this["compute_" + e].apply(
                  this,
                  Array.from(arguments)
                )),
              this["cached_" + e]
            );
          });
      });
    };
  memoize(Enumeration.EnumSet, ["symbol_list"]),
    memoize(Enumeration.EnumType, ["universe", "indexer", "constructor_"]);
  class Record {
    constructor(e, t) {
      assert_record_td(e, "new Record"), (this.rtd = e), (this.fields = t);
    }
    get(e) {
      return this.fields[e];
    }
    set(e, t) {
      this.fields[e] = t;
    }
    toString() {
      var e = to_write$1(this.fields);
      return "#<Record " + this.rtd.name + " " + e + ">";
    }
  }
  const isRecord = function (e) {
    return e instanceof Record;
  };
  (Record._DefinedTypes = {}),
    (Record.define_type = function (e, t, n) {
      return (Record._DefinedTypes[e] = { rtd: t, cd: n });
    }),
    (Record.get_type = function (e) {
      return Record._DefinedTypes[e];
    }),
    (Record.RTD = class {
      constructor(e, t, n, r, i, a) {
        (this.name = e),
          (this.parent_rtd = t),
          (this.is_base_type = !t),
          n
            ? ((this.uid = n), (this.generative = !1))
            : ((this.uid = this._generate_new_uid()), (this.generative = !0)),
          (this.sealed = !!r),
          (this.opaque = t.opaque || !!i),
          (this.fields = a.map(function (e) {
            return { name: e[0], mutable: !!e[1] };
          }));
      }
      field_name(e) {
        for (
          var t = this._field_names(), n = this.parent_rtd;
          n;
          n = n.parent_rtd
        )
          t = n._field_names() + t;
        return t[e];
      }
      _field_names() {
        return this.fields.map(function (e) {
          return e.name;
        });
      }
      _generate_new_uid() {
        return Sym(uniqueId("__record_td_uid"));
      }
      toString() {
        return "#<RecordTD " + name + ">";
      }
    }),
    (Record.RTD.NongenerativeRecords = {});
  const isRecordTD = function (e) {
    return e instanceof Record.RTD;
  };
  Record.CD = class {
    constructor(e, t, n) {
      this._check(e, t, n),
        (this.rtd = e),
        (this.parent_cd = t),
        n
          ? ((this.has_custom_protocol = !0), (this.protocol = n))
          : ((this.has_custom_protocol = !1),
            e.parent_rtd
              ? (this.protocol = this._default_protocol_for_derived_types())
              : (this.protocol = this._default_protocol_for_base_types()));
    }
    _check(e, t, n) {
      if (e.is_base_type && t)
        throw new Error(
          "Record.CD.new: cannot specify parent cd of a base type"
        );
      if (t && e.parent_rtd && t.rtd != e.parent_rtd)
        throw new Error(
          "Record.CD.new: mismatched parents between rtd and parent_cd"
        );
      if (e.parent_rtd && !t && n)
        throw new Error(
          "Record.CD.new: protocol must be #f when parent_cd is not given"
        );
      if (t && t.has_custom_protocol && !n)
        throw new Error(
          "Record.CD.new: protocol must be specified when parent_cd has a custom protocol"
        );
    }
    _default_protocol_for_base_types() {
      return function (e) {
        var t = e[0];
        return assert_procedure(t, "_default_protocol/base"), t;
      };
    }
    _default_protocol_for_derived_types() {
      var e = this.rtd;
      return function (t) {
        var n = t[0];
        assert_procedure(n, "_default_protocol/n");
        return function (t) {
          var r = e.fields.length,
            i = t.length - r,
            a = t.slice(0, i),
            s = t.slice(i);
          return new Call(n, a, function (e) {
            var t = e[0];
            return (
              assert_procedure(t, "_default_protocol/p"),
              new Call(t, s, function (e) {
                var t = e[0];
                return assert_record(t, "_default_protocol/result"), t;
              })
            );
          });
        };
      };
    }
    toString() {
      return "#<RecordCD " + this.rtd.name + ">";
    }
    record_constructor() {
      var e = this.parent_cd ? this._make_n([], this.rtd) : this._make_p();
      return (
        (e = e.bind(this)),
        new Call(this.protocol, [e], function (e) {
          var t = e[0];
          return assert_procedure(t, "record_constructor"), t;
        })
      );
    }
    _make_p() {
      return function (e) {
        return new Record(this.rtd, e);
      };
    }
    _make_n(e, t) {
      var n = this.parent_cd;
      return n
        ? function (r) {
            return function (i) {
              var a = [].concat(i[0]).concat(e),
                s = n._make_n(a, t);
              return new Call(n.protocol, [s], function (e) {
                var t = e[0];
                return (
                  assert_procedure(t, "_make_n"),
                  new Call(t, r, function (e) {
                    var t = e[0];
                    return assert_record(t), t;
                  })
                );
              });
            };
          }
        : function (n) {
            var r = n.concat(e);
            return new Record(t, r);
          };
    }
  };
  const isRecordCD = function (e) {
      return e instanceof Record.CD;
    },
    assert_record = make_simple_assert("record", isRecord),
    assert_record_td = make_simple_assert("record type descriptor", isRecordTD),
    assert_record_cd = make_simple_assert(
      "record constructor descriptor",
      isRecordCD
    );
  class Values$1 {
    constructor(e) {
      this.content = e;
    }
    to_write() {
      return "#<Values " + this.content.map(to_write$1).join(" ") + ">";
    }
  }
  const Console = {};
  define_libfunc("html-escape", 1, 1, function (e) {
    return assert_string(e[0]), escape(e[0]);
  });
  const inspect_objs = function (e) {
    return e.map(inspect).join(", ");
  };
  define_libfunc("inspect", 1, null, function (e) {
    return inspect_objs(e);
  }),
    define_libfunc("inspect!", 1, null, function (e) {
      return Console.puts(inspect_objs(e)), undef;
    });
  const json2sexp = function (e) {
    switch (!0) {
      case "number" == typeof e || "string" == typeof e || !0 === e || !1 === e:
        return e;
      case Array.isArray(e):
        return array_to_list(e.map(json2sexp));
      case "object" == typeof e:
        var t = nil;
        for (key in e) t = new Pair(new Pair(key, json2sexp(e[key])), t);
        return t;
      default:
        throw new Error(
          "json->sexp: detected invalid value for json: " + inspect(e)
        );
    }
  };
  define_libfunc("json->sexp", 1, 1, function (e) {
    return json2sexp(e[0]);
  }),
    define_libfunc("vector-push!", 2, null, function (e) {
      assert_vector(e[0]);
      for (var t = 1; t < e.length; t++) e[0].push(e[t]);
      return e[0];
    }),
    define_libfunc("identity", 1, 1, function (e) {
      return e[0];
    }),
    define_syntax("inc!", function (e) {
      var t = e.cdr.car;
      return List(Sym("begin"), List(Sym("set!"), t, List(Sym("+"), t, 1)), t);
    }),
    define_syntax("dec!", function (e) {
      var t = e.cdr.car;
      return List(Sym("begin"), List(Sym("set!"), t, List(Sym("-"), t, 1)), t);
    }),
    define_libfunc("string-concat", 1, 1, function (e) {
      return assert_list(e[0]), e[0].to_array().join("");
    }),
    define_libfunc("string-split", 2, 2, function (e) {
      return (
        assert_string(e[0]),
        assert_string(e[1]),
        array_to_list(e[0].split(e[1]))
      );
    }),
    define_libfunc("string-join", 1, 2, function (e) {
      assert_list(e[0]);
      var t = "";
      return e[1] && (assert_string(e[1]), (t = e[1])), e[0].to_array().join(t);
    }),
    define_libfunc("intersperse", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      assert_list(n);
      var r = [];
      return (
        n
          .to_array()
          .reverse()
          .forEach(function (e) {
            r.push(e), r.push(t);
          }),
        r.pop(),
        array_to_list(r)
      );
    }),
    define_libfunc("map-with-index", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      n.forEach(assert_list);
      var r = [],
        i = 0;
      return Call.multi_foreach(n, {
        call: function (e) {
          var n = e.map(function (e) {
            return e.car;
          });
          return n.unshift(i), i++, new Call(t, n);
        },
        result: function (e) {
          r.push(e);
        },
        finish: function () {
          return array_to_list(r);
        },
      });
    }),
    define_syntax("dotimes", function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr,
        r = t.car,
        i = t.cdr.car,
        a = t.cdr.cdr.car,
        s = gensym(),
        o = deep_array_to_list([
          [s, i],
          [r, 0, [Sym("+"), r, 1]],
        ]),
        u = deep_array_to_list([[Sym(">="), r, s], a]);
      return new Pair(Sym("do"), new Pair(o, new Pair(u, n)));
    });
  var sort_with_comp = function (e, t, n) {
    return e.sort(function (e, r) {
      return new Interpreter(n).invoke_closure(t, [e, r]);
    });
  };
  define_libfunc("list-sort/comp", 1, 2, function (e, t) {
    return (
      assert_procedure(e[0]),
      assert_list(e[1]),
      array_to_list(sort_with_comp(e[1].to_array(), e[0], t))
    );
  }),
    define_libfunc("vector-sort/comp", 1, 2, function (e, t) {
      return (
        assert_procedure(e[0]),
        assert_vector(e[1]),
        sort_with_comp([...e[1]], e[0], t)
      );
    }),
    define_libfunc("vector-sort/comp!", 1, 2, function (e, t) {
      return (
        assert_procedure(e[0]),
        assert_vector(e[1]),
        sort_with_comp(e[1], e[0], t),
        undef
      );
    });
  var rearrange_args = function (e, t) {
    var n = [],
      r = new Compiler().find_dot_pos(e);
    if (-1 == r) n = t;
    else {
      for (var i = 0; i < r; i++) n[i] = t[i];
      n[i] = array_to_list(t.slice(i));
    }
    return n;
  };
  define_syntax("define-macro", function (e) {
    var t,
      n = e.cdr.car;
    if (n instanceof Pair) {
      var r = n.car;
      t = n.cdr;
      var i = e.cdr.cdr,
        a = new Pair(Sym("lambda"), new Pair(t, i));
    } else {
      (r = n), (a = e.cdr.cdr.car);
      t = a.cdr.car;
    }
    var s = Compiler.compile(a).il;
    if (0 != s[2])
      throw new Bug(
        "you cannot use free variables in macro expander (or define-macro must be on toplevel)"
      );
    var o = new Closure(s[3], [], -1, void 0);
    return (
      (TopEnv[r.name] = new Syntax(r.name, function (e) {
        var n = e.to_array();
        n.shift();
        var r = new Interpreter(),
          i = rearrange_args(t, n);
        return r.invoke_closure(o, i);
      })),
      undef
    );
  });
  var macroexpand_1 = function (e) {
    if (e instanceof Pair) {
      if (
        !(e.car instanceof BiwaSymbol && TopEnv[e.car.name] instanceof Syntax)
      )
        throw new Error(
          "macroexpand-1: `" + to_write$1(e) + "' is not a macro"
        );
      e = TopEnv[e.car.name].transform(e);
    }
    return e;
  };
  define_syntax("%macroexpand", function (e) {
    var t = Compiler.expand(e.cdr.car);
    return List(Sym("quote"), t);
  }),
    define_syntax("%macroexpand-1", function (e) {
      var t = macroexpand_1(e.cdr.car);
      return List(Sym("quote"), t);
    }),
    define_libfunc("macroexpand", 1, 1, function (e) {
      return Compiler.expand(e[0]);
    }),
    define_libfunc("macroexpand-1", 1, 1, function (e) {
      return macroexpand_1(e[0]);
    }),
    define_libfunc("gensym", 0, 1, function (e) {
      return null == e[0] ? gensym() : (assert_string(e[0]), gensym(e[0]));
    }),
    define_libfunc("print", 1, null, function (e) {
      return (
        e.map(function (e) {
          Console.puts(to_display(e), !0);
        }),
        Console.puts(""),
        undef
      );
    }),
    define_libfunc("write-to-string", 1, 1, function (e) {
      return to_write$1(e[0]);
    }),
    define_libfunc("read-from-string", 1, 1, function (e) {
      return assert_string(e[0]), Interpreter.read(e[0]);
    }),
    define_libfunc("port-closed?", 1, 1, function (e) {
      return assert_port(e[0]), !e[0].is_open;
    }),
    define_libfunc("with-output-to-port", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      assert_port(t), assert_procedure(n);
      var r = Port.current_output;
      return (
        (Port.current_output = t),
        new Call(n, [t], function (e) {
          return t.close(), (Port.current_output = r), e[0];
        })
      );
    }),
    define_syntax("let1", function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr.car,
        r = e.cdr.cdr.cdr;
      return new Pair(
        new Pair(Sym("lambda"), new Pair(new Pair(t, nil), r)),
        new Pair(n, nil)
      );
    });
  var assert_regexp = function (e, t) {
    if (!(e instanceof RegExp))
      throw new Error(t + ": regexp required, but got " + to_write$1(e));
  };
  define_libfunc("string->regexp", 1, 1, function (e) {
    return assert_string(e[0], "string->regexp"), new RegExp(e[0]);
  }),
    define_libfunc("regexp?", 1, 1, function (e) {
      return e[0] instanceof RegExp;
    }),
    define_libfunc("regexp->string", 1, 1, function (e) {
      return (
        assert_regexp(e[0], "regexp->string"), e[0].toString().slice(1, -1)
      );
    }),
    define_libfunc("regexp-exec", 2, 2, function (e) {
      var t = e[0];
      "string" == typeof e[0] && (t = new RegExp(e[0])),
        assert_regexp(t, "regexp-exec"),
        assert_string(e[1], "regexp-exec");
      var n = t.exec(e[1]);
      return null !== n && array_to_list(n);
    }),
    define_libfunc("regexp-replace-all", 3, 3, function (e) {
      var t = e[0];
      if ("string" == typeof t) var n = new RegExp(t, "g");
      else {
        assert_regexp(t);
        n = new RegExp(t.source, "g");
      }
      return assert_string(e[1]), assert_string(e[2]), e[1].replace(n, e[2]);
    }),
    define_libfunc("js-eval", 1, 1, function (ar) {
      return eval(ar[0]);
    }),
    define_libfunc("js-ref", 2, 2, function (e) {
      return "string" == typeof e[1] || Number.isInteger(e[1])
        ? e[0][e[1]]
        : (assert_symbol(e[1]), e[0][e[1].name]);
    }),
    define_libfunc("js-set!", 3, 3, function (e) {
      return assert_string(e[1]), (e[0][e[1]] = e[2]), undef;
    }),
    define_libfunc("js-call", 1, null, function (e) {
      var t = e.shift();
      assert_function(t);
      return t.apply(null, e);
    }),
    define_libfunc("js-invoke", 2, null, function (e) {
      var t = e.shift(),
        n = e.shift();
      if (t[n]) return t[n].apply(t, e);
      throw new Error("js-invoke: function " + n + " is not defined");
    }),
    define_libfunc("js-invocation", 2, null, function (ar, intp) {
      var receiver = ar.shift();
      isSymbol(receiver) && (receiver = eval(receiver.name));
      var v = receiver;
      return (
        ar.forEach(function (e) {
          if (isSymbol(e)) v = v[e.name];
          else {
            if (!isList(e))
              throw new BiwaError(
                "js-invocation: expected list or symbol for callspec but got " +
                  inspect(e)
              );
            var t = e.to_array();
            assert_symbol(t[0]);
            var n = t.shift().name;
            if (
              ((t = t.map(function (e) {
                if (isClosure(e)) return js_closure(e, intp);
                if (isList(e)) {
                  var t = {};
                  return (
                    e.foreach(function (e) {
                      assert_symbol(e.car), (t[e.car.name] = e.cdr);
                    }),
                    t
                  );
                }
                return e;
              })),
              "function" === (v[n], !1))
            )
              throw new BiwaError(
                "js-invocation: the method `" + n + "' not found"
              );
            v = v[n].apply(v, t);
          }
        }),
        v
      );
    }),
    define_syntax("..", function (e) {
      if (e.cdr == nil) throw new Error("malformed ..");
      return new Pair(Sym("js-invocation"), e.cdr);
    }),
    define_libfunc("js-new", 1, null, function (ar, intp) {
      var array_to_obj = function (e) {
          if (e.length % 2 != 0)
            throw new Error("js-new: odd number of key-value pair");
          for (var t = {}, n = 0; n < e.length; n += 2) {
            var r = e[n],
              i = e[n + 1];
            assert_symbol(r),
              isClosure(i) && (i = js_closure(i, intp)),
              (t[r.name] = i);
          }
          return t;
        },
        ctor = ar.shift();
      if (("string" == typeof ctor && (ctor = eval(ctor)), 0 == ar.length))
        return new ctor();
      for (var args = [], i = 0; i < ar.length; i++) {
        if (ar[i] instanceof BiwaSymbol) {
          args.push(array_to_obj(ar.slice(i)));
          break;
        }
        args.push(ar[i]);
      }
      return new (Function.prototype.bind.apply(ctor, [null].concat(args)))();
    }),
    define_libfunc("js-obj", 0, null, function (e) {
      if (e.length % 2 != 0)
        throw new Error("js-obj: number of arguments must be even");
      var t = {};
      for (let n = 0; n < e.length / 2; n++)
        assert_string(e[2 * n]), (t[e[2 * n]] = e[2 * n + 1]);
      return t;
    });
  const js_closure = function (e, t) {
    var n = new Interpreter(t);
    return function () {
      return n.invoke_closure(e, Array.from(arguments));
    };
  };
  define_libfunc("js-closure", 1, 1, function (e, t) {
    return assert_closure(e[0]), js_closure(e[0], t);
  }),
    define_libfunc("js-null?", 1, 1, function (e) {
      return null === e[0];
    }),
    define_libfunc("js-undefined?", 1, 1, function (e) {
      return void 0 === e[0];
    }),
    define_libfunc("js-function?", 1, 1, function (e) {
      return "function" == typeof e[0];
    }),
    define_libfunc("js-array-to-list", 1, 1, function (e) {
      return (
        deprecate("js-array-to-list", "1.0", "js-array->list"),
        array_to_list(e[0])
      );
    }),
    define_libfunc("js-array->list", 1, 1, function (e) {
      return array_to_list(e[0]);
    }),
    define_libfunc("list-to-js-array", 1, 1, function (e) {
      return (
        deprecate("list-to-js-array", "1.0", "list->js-array"), e[0].to_array()
      );
    }),
    define_libfunc("list->js-array", 1, 1, function (e) {
      return e[0].to_array();
    }),
    define_libfunc("alist-to-js-obj", 1, 1, function (e) {
      return (
        deprecate("alist-to-js-obj", "1.0", "alist->js-obj"),
        alist_to_js_obj(e[0])
      );
    }),
    define_libfunc("alist->js-obj", 1, 1, function (e) {
      return assert_list(e[0]), alist_to_js_obj(e[0]);
    }),
    define_libfunc("js-obj-to-alist", 1, 1, function (e) {
      return (
        deprecate("js-obj-to-alist", "1.0", "js-obj->alist"),
        js_obj_to_alist(e[0])
      );
    }),
    define_libfunc("js-obj->alist", 1, 1, function (e) {
      return js_obj_to_alist(e[0]);
    }),
    define_libfunc("timer", 2, 2, function (e, t) {
      var n = e[0],
        r = e[1];
      assert_closure(n), assert_real(r);
      var i = new Interpreter(t);
      return (
        setTimeout(function () {
          i.invoke_closure(n);
        }, 1e3 * r),
        undef
      );
    }),
    define_libfunc("set-timer!", 2, 2, function (e, t) {
      var n = e[0],
        r = e[1];
      assert_closure(n), assert_real(r);
      var i = new Interpreter(t);
      return setInterval(function () {
        i.invoke_closure(n);
      }, 1e3 * r);
    }),
    define_libfunc("clear-timer!", 1, 1, function (e) {
      var t = e[0];
      return clearInterval(t), undef;
    }),
    define_libfunc("sleep", 1, 1, function (e) {
      var t = e[0];
      return (
        assert_real(t),
        new Pause(function (e) {
          setTimeout(function () {
            e.resume(nil);
          }, 1e3 * t);
        })
      );
    });
  var define_console_func = function (e) {
    define_libfunc("console-" + e, 1, null, function (t) {
      var n = window.console;
      if (n) {
        var r = t.map(function (e) {
          return inspect(e, { fallback: e });
        });
        n[e].apply(n, r);
      }
      return t[0];
    });
  };
  define_console_func("debug"),
    define_console_func("log"),
    define_console_func("info"),
    define_console_func("warn"),
    define_console_func("error"),
    define_syntax("cond", function (e) {
      var t = e.cdr;
      if (!(t instanceof Pair) || t === nil)
        throw new BiwaError(
          "malformed cond: cond needs list but got " + write_ss(t)
        );
      var n = null;
      return (
        t
          .to_array()
          .reverse()
          .forEach(function (e) {
            if (!(e instanceof Pair))
              throw new BiwaError("bad clause in cond: " + write_ss(e));
            if (e.car === Sym("else")) {
              if (null !== n)
                throw new BiwaError(
                  "'else' clause of cond followed by more clauses: " +
                    write_ss(t)
                );
              n =
                e.cdr !== nil &&
                (e.cdr.cdr === nil ? e.cdr.car : new Pair(Sym("begin"), e.cdr));
            } else {
              var r = e.car;
              if (e.cdr === nil) n = List(Sym("or"), r, n);
              else if (e.cdr.cdr === nil) n = List(Sym("if"), r, e.cdr.car, n);
              else if (e.cdr.car === Sym("=>")) {
                r = e.car;
                var i = e.cdr.cdr.car,
                  a = gensym();
                n = List(
                  Sym("let"),
                  List(List(a, r)),
                  List(Sym("if"), r, List(i, a), n)
                );
              } else n = List(Sym("if"), r, new Pair(Sym("begin"), e.cdr), n);
            }
          }),
        n
      );
    }),
    define_syntax("case", function (e) {
      var t = gensym();
      if (e.cdr === nil)
        throw new BiwaError("case: at least one clause is required");
      if (e.cdr instanceof Pair) {
        var n = e.cdr.car,
          r = e.cdr.cdr,
          i = void 0;
        return (
          r
            .to_array()
            .reverse()
            .forEach(function (e) {
              if (e.car === Sym("else")) {
                if (void 0 !== i)
                  throw new BiwaError(
                    "case: 'else' clause followed by more clauses: " +
                      write_ss(r)
                  );
                i = new Pair(Sym("begin"), e.cdr);
              } else
                i = List(
                  Sym("if"),
                  new Pair(
                    Sym("or"),
                    array_to_list(
                      e.car.to_array().map(function (e) {
                        return List(Sym("eqv?"), t, List(Sym("quote"), e));
                      })
                    )
                  ),
                  new Pair(Sym("begin"), e.cdr),
                  i
                );
            }),
          new Pair(Sym("let1"), new Pair(t, new Pair(n, new Pair(i, nil))))
        );
      }
      throw new BiwaError("case: proper list is required");
    }),
    define_syntax("and", function (e) {
      if (e.cdr == nil) return !0;
      var t = e.cdr.to_array(),
        n = t.length - 1,
        r = t[n];
      for (n -= 1; n >= 0; n--) r = List(Sym("if"), t[n], r, !1);
      return r;
    }),
    define_syntax("or", function (e) {
      for (var t = e.cdr.to_array(), n = !1, r = t.length - 1; r >= 0; r--)
        n = List(Sym("if"), t[r], t[r], n);
      return n;
    }),
    define_syntax("let", function (e) {
      var t = null;
      e.cdr.car instanceof BiwaSymbol && ((t = e.cdr.car), (e = e.cdr));
      var n = e.cdr.car,
        r = e.cdr.cdr;
      if (!(n instanceof Pair) && n != nil)
        throw new BiwaError(
          "let: need a pair for bindings: got " + to_write$1(n)
        );
      for (var i = nil, a = nil, s = n; s instanceof Pair; s = s.cdr) {
        if (!(s.car instanceof Pair))
          throw new BiwaError(
            "let: need a pair for bindings: got " + to_write$1(s.car)
          );
        (i = new Pair(s.car.car, i)), (a = new Pair(s.car.cdr.car, a));
      }
      var o = null;
      if (t) {
        (i = array_to_list(i.to_array().reverse())),
          (a = array_to_list(a.to_array().reverse()));
        var u = new Pair(Sym("lambda"), new Pair(i, r)),
          c = new Pair(t, a);
        o = List(Sym("letrec"), new Pair(List(t, u), nil), c);
      } else o = new Pair(new Pair(Sym("lambda"), new Pair(i, r)), a);
      return o;
    }),
    define_syntax("let*", function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr;
      if (t === nil) return new Pair(Sym("let"), new Pair(nil, n));
      if (!(t instanceof Pair))
        throw new BiwaError(
          "let*: need a pair for bindings: got " + to_write$1(t)
        );
      var r = null;
      return (
        t
          .to_array()
          .reverse()
          .forEach(function (e) {
            r = new Pair(
              Sym("let"),
              new Pair(new Pair(e, nil), null == r ? n : new Pair(r, nil))
            );
          }),
        r
      );
    });
  var expand_letrec_star = function (e) {
    var t = e.cdr.car,
      n = e.cdr.cdr;
    if (!(t instanceof Pair))
      throw new BiwaError(
        "letrec*: need a pair for bindings: got " + to_write$1(t)
      );
    var r = n;
    t.to_array()
      .reverse()
      .forEach(function (e) {
        r = new Pair(new Pair(Sym("set!"), e), r);
      });
    var i = nil;
    return (
      t
        .to_array()
        .reverse()
        .forEach(function (e) {
          i = new Pair(new Pair(e.car, new Pair(undef, nil)), i);
        }),
      new Pair(Sym("let"), new Pair(i, r))
    );
  };
  define_syntax("letrec", expand_letrec_star),
    define_syntax("letrec*", expand_letrec_star),
    define_syntax("let-values", function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr,
        r = nil,
        i = nil;
      t.to_array()
        .reverse()
        .forEach(function (e) {
          var t = e.cdr.car,
            n = gensym(),
            a = new Pair(
              n,
              new Pair(
                new Pair(Sym("lambda"), new Pair(nil, new Pair(t, nil))),
                nil
              )
            );
          r = new Pair(a, r);
          var s = e.car;
          i = new Pair(new Pair(s, new Pair(new Pair(n, nil), nil)), i);
        });
      var a = new Pair(Sym("let*-values"), new Pair(i, n));
      return new Pair(Sym("let"), new Pair(r, new Pair(a, nil)));
    }),
    define_syntax("let*-values", function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr,
        r = null;
      return (
        t
          .to_array()
          .reverse()
          .forEach(function (e) {
            var t = e.car,
              i = e.cdr.car;
            r = new Pair(
              Sym("call-with-values"),
              new Pair(
                new Pair(Sym("lambda"), new Pair(nil, new Pair(i, nil))),
                new Pair(
                  new Pair(
                    Sym("lambda"),
                    new Pair(t, null == r ? n : new Pair(r, nil))
                  ),
                  nil
                )
              )
            );
          }),
        r
      );
    }),
    define_libfunc("eqv?", 2, 2, function (e) {
      return eqv(e[0], e[1]);
    }),
    define_libfunc("eq?", 2, 2, function (e) {
      return eq(e[0], e[1]);
    }),
    define_libfunc("equal?", 2, 2, function (e) {
      return equal(e[0], e[1]);
    }),
    define_libfunc("procedure?", 1, 1, function (e) {
      return isProcedure(e[0]);
    }),
    define_libfunc("number?", 1, 1, function (e) {
      return isNumber(e[0]);
    }),
    define_libfunc("complex?", 1, 1, function (e) {
      return isComplex(e[0]);
    }),
    define_libfunc("real?", 1, 1, function (e) {
      return isReal(e[0]);
    }),
    define_libfunc("rational?", 1, 1, function (e) {
      return isRational(e[0]);
    }),
    define_libfunc("integer?", 1, 1, function (e) {
      return isInteger(e[0]);
    }),
    define_libfunc("=", 2, null, function (e) {
      var t = e[0];
      assert_number(e[0]);
      for (var n = 1; n < e.length; n++) {
        if ((assert_number(e[n]), real_part(e[n]) != real_part(t))) return !1;
        if (imag_part(e[n]) != imag_part(t)) return !1;
      }
      return !0;
    }),
    define_libfunc("<", 2, null, function (e) {
      assert_number(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((assert_number(e[t]), !(e[t - 1] < e[t]))) return !1;
      return !0;
    }),
    define_libfunc(">", 2, null, function (e) {
      assert_number(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((assert_number(e[t]), !(e[t - 1] > e[t]))) return !1;
      return !0;
    }),
    define_libfunc("<=", 2, null, function (e) {
      assert_number(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((assert_number(e[t]), !(e[t - 1] <= e[t]))) return !1;
      return !0;
    }),
    define_libfunc(">=", 2, null, function (e) {
      assert_number(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((assert_number(e[t]), !(e[t - 1] >= e[t]))) return !1;
      return !0;
    }),
    define_libfunc("zero?", 1, 1, function (e) {
      return assert_number(e[0]), 0 === e[0];
    }),
    define_libfunc("positive?", 1, 1, function (e) {
      return assert_number(e[0]), e[0] > 0;
    }),
    define_libfunc("negative?", 1, 1, function (e) {
      return assert_number(e[0]), e[0] < 0;
    }),
    define_libfunc("odd?", 1, 1, function (e) {
      return assert_number(e[0]), e[0] % 2 == 1 || e[0] % 2 == -1;
    }),
    define_libfunc("even?", 1, 1, function (e) {
      return assert_number(e[0]), e[0] % 2 == 0;
    }),
    define_libfunc("finite?", 1, 1, function (e) {
      return (
        assert_number(e[0]), e[0] != 1 / 0 && e[0] != -1 / 0 && !isNaN(e[0])
      );
    }),
    define_libfunc("infinite?", 1, 1, function (e) {
      return assert_number(e[0]), e[0] == 1 / 0 || e[0] == -1 / 0;
    }),
    define_libfunc("nan?", 1, 1, function (e) {
      return assert_number(e[0]), isNaN(e[0]);
    }),
    define_libfunc("max", 2, null, function (e) {
      for (var t = 0; t < e.length; t++) assert_number(e[t]);
      return Math.max.apply(null, e);
    }),
    define_libfunc("min", 2, null, function (e) {
      for (var t = 0; t < e.length; t++) assert_number(e[t]);
      return Math.min.apply(null, e);
    });
  var complex_or_real = function (e, t) {
      return 0 === t ? e : new Complex(e, t);
    },
    polar_or_real = function (e, t) {
      return 0 === t ? e : Complex.from_polar(e, t);
    };
  define_libfunc("+", 0, null, function (e) {
    for (var t = 0, n = 0, r = 0; r < e.length; r++)
      assert_number(e[r]), (t += real_part(e[r])), (n += imag_part(e[r]));
    return complex_or_real(t, n);
  });
  var the_magnitude = function (e) {
      return e instanceof Complex ? e.magnitude() : e;
    },
    the_angle = function (e) {
      return e instanceof Complex ? e.angle() : 0;
    };
  define_libfunc("*", 0, null, function (e) {
    for (var t = 1, n = 0, r = 0; r < e.length; r++)
      assert_number(e[r]), (t *= the_magnitude(e[r])), (n += the_angle(e[r]));
    return polar_or_real(t, n);
  }),
    define_libfunc("-", 1, null, function (e) {
      var t = e.length;
      if ((assert_number(e[0]), 1 == t))
        return e[0] instanceof Complex
          ? new Complex(-real_part(e[0]), -imag_part(e[0]))
          : -e[0];
      for (var n = real_part(e[0]), r = imag_part(e[0]), i = 1; i < t; i++)
        assert_number(e[i]), (n -= real_part(e[i])), (r -= imag_part(e[i]));
      return complex_or_real(n, r);
    }),
    define_libfunc("/", 1, null, function (e) {
      var t = e.length;
      if ((assert_number(e[0]), 1 == t))
        return e[0] instanceof Complex
          ? Complex.from_polar(1 / the_magnitude(e[0]), -the_angle(e[0]))
          : 1 / e[0];
      for (var n = the_magnitude(e[0]), r = the_angle(e[0]), i = 1; i < t; i++)
        assert_number(e[i]), (n /= the_magnitude(e[i])), (r -= the_angle(e[i]));
      return polar_or_real(n, r);
    }),
    define_libfunc("abs", 1, 1, function (e) {
      return assert_number(e[0]), Math.abs(e[0]);
    });
  var div = function (e, t) {
      return Math.floor(e / t);
    },
    mod = function (e, t) {
      return e - Math.floor(e / t) * t;
    },
    div0 = function (e, t) {
      return e > 0 ? Math.floor(e / t) : Math.ceil(e / t);
    },
    mod0 = function (e, t) {
      return e > 0 ? e - Math.floor(e / t) * t : e - Math.ceil(e / t) * t;
    };
  define_libfunc("div-and-mod", 2, 2, function (e) {
    return (
      assert_number(e[0]),
      assert_number(e[1]),
      new Values$1([div(e[0], e[1]), mod(e[0], e[1])])
    );
  }),
    define_libfunc("div", 2, 2, function (e) {
      return assert_number(e[0]), assert_number(e[1]), div(e[0], e[1]);
    }),
    define_libfunc("mod", 2, 2, function (e) {
      return assert_number(e[0]), assert_number(e[1]), mod(e[0], e[1]);
    }),
    define_libfunc("div0-and-mod0", 2, 2, function (e) {
      return (
        assert_number(e[0]),
        assert_number(e[1]),
        new Values$1([div0(e[0], e[1]), mod0(e[0], e[1])])
      );
    }),
    define_libfunc("div0", 2, 2, function (e) {
      return assert_number(e[0]), assert_number(e[1]), div0(e[0], e[1]);
    }),
    define_libfunc("mod0", 2, 2, function (e) {
      return assert_number(e[0]), assert_number(e[1]), mod0(e[0], e[1]);
    }),
    alias_libfunc("div-and-mod", "floor/"),
    alias_libfunc("div", "floor-quotient"),
    alias_libfunc("mod", "floor-remainder");
  var truncate_q = function (e, t) {
      return Math.trunc(e / t);
    },
    truncate_r = function (e, t) {
      return e - Math.trunc(e / t) * t;
    };
  define_libfunc("truncate/", 2, 2, function (e) {
    return (
      assert_number(e[0]),
      assert_number(e[1]),
      new Values$1([truncate_q(e[0], e[1]), truncate_r(e[0], e[1])])
    );
  }),
    define_libfunc("truncate-quotient", 2, 2, function (e) {
      return assert_number(e[0]), assert_number(e[1]), truncate_q(e[0], e[1]);
    }),
    define_libfunc("truncate-remainder", 2, 2, function (e) {
      return assert_number(e[0]), assert_number(e[1]), truncate_r(e[0], e[1]);
    }),
    alias_libfunc("truncate-quotient", "quotient"),
    alias_libfunc("truncate-remainder", "remainder"),
    alias_libfunc("floor-remainder", "modulo"),
    define_libfunc("numerator", 1, 1, function (e) {
      if ((assert_number(e[0]), e[0] instanceof Rational$1))
        return e[0].numerator;
      throw new Bug("todo");
    }),
    define_libfunc("denominator", 1, 1, function (e) {
      if ((assert_number(e[0]), e[0] instanceof Rational$1))
        return e[0].denominator;
      throw new Bug("todo");
    }),
    define_libfunc("floor", 1, 1, function (e) {
      return assert_number(e[0]), Math.floor(e[0]);
    }),
    define_libfunc("ceiling", 1, 1, function (e) {
      return assert_number(e[0]), Math.ceil(e[0]);
    }),
    define_libfunc("truncate", 1, 1, function (e) {
      return assert_number(e[0]), e[0] < 0 ? Math.ceil(e[0]) : Math.floor(e[0]);
    }),
    define_libfunc("round", 1, 1, function (e) {
      return assert_number(e[0]), Math.round(e[0]);
    }),
    define_libfunc("exp", 1, 1, function (e) {
      return assert_number(e[0]), Math.exp(e[0]);
    }),
    define_libfunc("log", 1, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        assert_number(t),
        n ? (assert_number(n), Math.log(t) / Math.log(n)) : Math.log(t)
      );
    }),
    define_libfunc("sin", 1, 1, function (e) {
      return assert_number(e[0]), Math.sin(e[0]);
    }),
    define_libfunc("cos", 1, 1, function (e) {
      return assert_number(e[0]), Math.cos(e[0]);
    }),
    define_libfunc("tan", 1, 1, function (e) {
      return assert_number(e[0]), Math.tan(e[0]);
    }),
    define_libfunc("asin", 1, 1, function (e) {
      return assert_number(e[0]), Math.asin(e[0]);
    }),
    define_libfunc("acos", 1, 1, function (e) {
      return assert_number(e[0]), Math.acos(e[0]);
    }),
    define_libfunc("atan", 1, 2, function (e) {
      return (
        assert_number(e[0]),
        2 == e.length
          ? (assert_number(e[1]), Math.atan2(e[0], e[1]))
          : Math.atan(e[0])
      );
    }),
    define_libfunc("sqrt", 1, 1, function (e) {
      return assert_number(e[0]), Math.sqrt(e[0]);
    }),
    define_libfunc("exact-integer-sqrt", 1, 1, function (e) {
      assert_number(e[0]);
      var t = Math.sqrt(e[0]),
        n = t - (t % 1),
        r = e[0] - n * n;
      return new Values$1([n, r]);
    }),
    define_libfunc("expt", 2, 2, function (e) {
      return assert_number(e[0]), assert_number(e[1]), Math.pow(e[0], e[1]);
    }),
    define_libfunc("make-rectangular", 2, 2, function (e) {
      return assert_number(e[0]), assert_number(e[1]), new Complex(e[0], e[1]);
    }),
    define_libfunc("make-polar", 2, 2, function (e) {
      return (
        assert_number(e[0]), assert_number(e[1]), Complex.from_polar(e[0], e[1])
      );
    });
  var real_part = function (e) {
      return Complex.assure(e).real;
    },
    imag_part = function (e) {
      return Complex.assure(e).imag;
    },
    get;
  define_libfunc("real-part", 1, 1, function (e) {
    return assert_number(e[0]), real_part(e[0]);
  }),
    define_libfunc("imag-part", 1, 1, function (e) {
      return assert_number(e[0]), Complex.assure(e[0]).imag;
    }),
    define_libfunc("magnitude", 1, 1, function (e) {
      return assert_number(e[0]), Complex.assure(e[0]).magnitude();
    }),
    define_libfunc("angle", 1, 1, function (e) {
      return assert_number(e[0]), Complex.assure(e[0]).angle();
    }),
    define_libfunc("number->string", 1, 3, function (e) {
      var t = e[0],
        n = e[1];
      if (e[2])
        throw new Bug("number->string: precision is not yet implemented");
      return (n = n || 10), t.toString(n);
    }),
    define_libfunc("string->number", 1, 3, function (e) {
      var t = e[0];
      if ("+inf.0" === t) return 1 / 0;
      if ("-inf.0" === t) return -1 / 0;
      if ("+nan.0" === t) return NaN;
      var n = e[1],
        r = parse_integer(t, 0 === n ? 0 : n || 10);
      if (!1 !== r) return r;
      if (void 0 !== n && 10 !== n) return !1;
      var i = parse_float(t);
      if (!1 !== i) return i;
      var a = parse_fraction(t);
      return !1 !== a && a;
    }),
    define_libfunc("not", 1, 1, function (e) {
      return !1 === e[0];
    }),
    define_libfunc("boolean?", 1, 1, function (e) {
      return !1 === e[0] || !0 === e[0];
    }),
    define_libfunc("boolean=?", 2, null, function (e) {
      for (var t = e.length, n = 1; n < t; n++) if (e[n] != e[0]) return !1;
      return !0;
    }),
    define_libfunc("pair?", 1, 1, function (e) {
      return e[0] instanceof Pair;
    }),
    define_libfunc("cons", 2, 2, function (e) {
      return new Pair(e[0], e[1]);
    }),
    define_libfunc("car", 1, 1, function (e) {
      if (!(e[0] instanceof Pair))
        throw new BiwaError("Attempt to apply car on " + e[0]);
      return e[0].car;
    }),
    define_libfunc("cdr", 1, 1, function (e) {
      if (!(e[0] instanceof Pair))
        throw new BiwaError("Attempt to apply cdr on " + e[0]);
      return e[0].cdr;
    }),
    define_libfunc("set-car!", 2, 2, function (e) {
      if (!(e[0] instanceof Pair))
        throw new BiwaError("Attempt to apply set-car! on " + e[0]);
      return (e[0].car = e[1]), undef;
    }),
    define_libfunc("set-cdr!", 2, 2, function (e) {
      if (!(e[0] instanceof Pair))
        throw new BiwaError("Attempt to apply set-cdr! on " + e[0]);
      return (e[0].cdr = e[1]), undef;
    }),
    (get = function (e, t, n) {
      var r = n;
      return (
        t.forEach(function (t) {
          if (!(r instanceof Pair))
            throw new BiwaError(
              e + ": attempt to get " + (t ? "cdr" : "car") + " of " + r
            );
          r = t ? r.cdr : r.car;
        }),
        r
      );
    }),
    define_libfunc("caar", 1, 1, function (e) {
      return get("caar", [0, 0], e[0]);
    }),
    define_libfunc("cadr", 1, 1, function (e) {
      return get("cadr", [1, 0], e[0]);
    }),
    define_libfunc("cdar", 1, 1, function (e) {
      return get("cadr", [0, 1], e[0]);
    }),
    define_libfunc("cddr", 1, 1, function (e) {
      return get("cadr", [1, 1], e[0]);
    }),
    define_libfunc("caaar", 1, 1, function (e) {
      return get("caaar", [0, 0, 0], e[0]);
    }),
    define_libfunc("caadr", 1, 1, function (e) {
      return get("caadr", [1, 0, 0], e[0]);
    }),
    define_libfunc("cadar", 1, 1, function (e) {
      return get("cadar", [0, 1, 0], e[0]);
    }),
    define_libfunc("caddr", 1, 1, function (e) {
      return get("caddr", [1, 1, 0], e[0]);
    }),
    define_libfunc("cdaar", 1, 1, function (e) {
      return get("cdaar", [0, 0, 1], e[0]);
    }),
    define_libfunc("cdadr", 1, 1, function (e) {
      return get("cdadr", [1, 0, 1], e[0]);
    }),
    define_libfunc("cddar", 1, 1, function (e) {
      return get("cddar", [0, 1, 1], e[0]);
    }),
    define_libfunc("cdddr", 1, 1, function (e) {
      return get("cdddr", [1, 1, 1], e[0]);
    }),
    define_libfunc("caaaar", 1, 1, function (e) {
      return get("caaaar", [0, 0, 0, 0], e[0]);
    }),
    define_libfunc("caaadr", 1, 1, function (e) {
      return get("caaadr", [1, 0, 0, 0], e[0]);
    }),
    define_libfunc("caadar", 1, 1, function (e) {
      return get("caadar", [0, 1, 0, 0], e[0]);
    }),
    define_libfunc("caaddr", 1, 1, function (e) {
      return get("caaddr", [1, 1, 0, 0], e[0]);
    }),
    define_libfunc("cadaar", 1, 1, function (e) {
      return get("cadaar", [0, 0, 1, 0], e[0]);
    }),
    define_libfunc("cadadr", 1, 1, function (e) {
      return get("cadadr", [1, 0, 1, 0], e[0]);
    }),
    define_libfunc("caddar", 1, 1, function (e) {
      return get("caddar", [0, 1, 1, 0], e[0]);
    }),
    define_libfunc("cadddr", 1, 1, function (e) {
      return get("cadddr", [1, 1, 1, 0], e[0]);
    }),
    define_libfunc("cdaaar", 1, 1, function (e) {
      return get("cdaaar", [0, 0, 0, 1], e[0]);
    }),
    define_libfunc("cdaadr", 1, 1, function (e) {
      return get("cdaadr", [1, 0, 0, 1], e[0]);
    }),
    define_libfunc("cdadar", 1, 1, function (e) {
      return get("cdadar", [0, 1, 0, 1], e[0]);
    }),
    define_libfunc("cdaddr", 1, 1, function (e) {
      return get("cdaddr", [1, 1, 0, 1], e[0]);
    }),
    define_libfunc("cddaar", 1, 1, function (e) {
      return get("cddaar", [0, 0, 1, 1], e[0]);
    }),
    define_libfunc("cddadr", 1, 1, function (e) {
      return get("cddadr", [1, 0, 1, 1], e[0]);
    }),
    define_libfunc("cdddar", 1, 1, function (e) {
      return get("cdddar", [0, 1, 1, 1], e[0]);
    }),
    define_libfunc("cddddr", 1, 1, function (e) {
      return get("cddddr", [1, 1, 1, 1], e[0]);
    }),
    define_libfunc("null?", 1, 1, function (e) {
      return e[0] === nil;
    }),
    define_libfunc("list?", 1, 1, function (e) {
      return isList(e[0]);
    }),
    define_libfunc("list", 0, null, function (e) {
      for (var t = nil, n = e.length - 1; n >= 0; n--) t = new Pair(e[n], t);
      return t;
    }),
    define_libfunc("length", 1, 1, function (e) {
      assert_list(e[0]);
      for (var t = 0, n = e[0]; n != nil; n = n.cdr) t++;
      return t;
    }),
    define_libfunc("append", 1, null, function (e) {
      for (var t = e.length, n = e[--t]; t--; )
        e[t]
          .to_array()
          .reverse()
          .forEach(function (e) {
            n = new Pair(e, n);
          });
      return n;
    }),
    define_libfunc("reverse", 1, 1, function (e) {
      if (e[0] == nil) return nil;
      assert_pair(e[0]);
      for (var t = nil, n = e[0]; n != nil; n = n.cdr) t = new Pair(n.car, t);
      return t;
    }),
    define_libfunc("list-tail", 2, 2, function (e) {
      if ((assert_pair(e[0]), assert_integer(e[1]), e[1] < 0))
        throw new BiwaError("list-tail: index out of range (" + e[1] + ")");
      for (var t = e[0], n = 0; n < e[1]; n++) {
        if (!(t instanceof Pair))
          throw new BiwaError("list-tail: the list is shorter than " + e[1]);
        t = t.cdr;
      }
      return t;
    }),
    define_libfunc("list-ref", 2, 2, function (e) {
      if ((assert_pair(e[0]), assert_integer(e[1]), e[1] < 0))
        throw new BiwaError("list-tail: index out of range (" + e[1] + ")");
      for (var t = e[0], n = 0; n < e[1]; n++) {
        if (!(t instanceof Pair))
          throw new BiwaError("list-ref: the list is shorter than " + e[1]);
        t = t.cdr;
      }
      return t.car;
    }),
    define_libfunc("map", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      n.forEach(assert_list);
      var r = [];
      return Call.multi_foreach(n, {
        call: function (e) {
          return new Call(
            t,
            e.map(function (e) {
              return e.car;
            })
          );
        },
        result: function (e) {
          r.push(e);
        },
        finish: function () {
          return array_to_list(r);
        },
      });
    }),
    define_libfunc("for-each", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      return (
        n.forEach(assert_list),
        Call.multi_foreach(n, {
          call: function (e) {
            return new Call(
              t,
              e.map(function (e) {
                return e.car;
              })
            );
          },
          finish: function () {
            return undef;
          },
        })
      );
    }),
    define_libfunc("symbol?", 1, 1, function (e) {
      return e[0] instanceof BiwaSymbol;
    }),
    define_libfunc("symbol->string", 1, 1, function (e) {
      return assert_symbol(e[0]), e[0].name;
    }),
    define_libfunc("symbol=?", 2, null, function (e) {
      assert_symbol(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((assert_symbol(e[t]), e[t] != e[0])) return !1;
      return !0;
    }),
    define_libfunc("string->symbol", 1, 1, function (e) {
      return assert_string(e[0]), Sym(e[0]);
    }),
    define_libfunc("char?", 1, 1, function (e) {
      return e[0] instanceof Char;
    }),
    define_libfunc("char->integer", 1, 1, function (e) {
      return assert_char(e[0]), e[0].value.charCodeAt(0);
    }),
    define_libfunc("integer->char", 1, 1, function (e) {
      return assert_integer(e[0]), Char.get(String.fromCharCode(e[0]));
    });
  var make_char_compare_func = function (e) {
    return function (t) {
      assert_char(t[0]);
      for (var n = 1; n < t.length; n++)
        if ((assert_char(t[n]), !e(t[n - 1].value, t[n].value))) return !1;
      return !0;
    };
  };
  define_libfunc(
    "char=?",
    2,
    null,
    make_char_compare_func(function (e, t) {
      return e == t;
    })
  ),
    define_libfunc(
      "char<?",
      2,
      null,
      make_char_compare_func(function (e, t) {
        return e < t;
      })
    ),
    define_libfunc(
      "char>?",
      2,
      null,
      make_char_compare_func(function (e, t) {
        return e > t;
      })
    ),
    define_libfunc(
      "char<=?",
      2,
      null,
      make_char_compare_func(function (e, t) {
        return e <= t;
      })
    ),
    define_libfunc(
      "char>=?",
      2,
      null,
      make_char_compare_func(function (e, t) {
        return e >= t;
      })
    ),
    define_libfunc("string?", 1, 1, function (e) {
      return "string" == typeof e[0];
    }),
    define_libfunc("make-string", 1, 2, function (e) {
      assert_integer(e[0]);
      var t = " ";
      e[1] && (assert_char(e[1]), (t = e[1].value));
      var n = "";
      return (
        Array(e[0])
          .fill()
          .map(() => {
            n += t;
          }),
        n
      );
    }),
    define_libfunc("string", 0, null, function (e) {
      if (0 == e.length) return "";
      for (var t = 0; t < e.length; t++) assert_char(e[t]);
      return e
        .map(function (e) {
          return e.value;
        })
        .join("");
    }),
    define_libfunc("string-length", 1, 1, function (e) {
      return assert_string(e[0]), e[0].length;
    }),
    define_libfunc("string-ref", 2, 2, function (e) {
      return (
        assert_string(e[0]),
        assert_between(e[1], 0, e[0].length - 1),
        Char.get(e[0].charAt([e[1]]))
      );
    }),
    define_libfunc("string=?", 2, null, function (e) {
      assert_string(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((assert_string(e[t]), e[0] != e[t])) return !1;
      return !0;
    }),
    define_libfunc("string<?", 2, null, function (e) {
      assert_string(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((assert_string(e[t]), !(e[t - 1] < e[t]))) return !1;
      return !0;
    }),
    define_libfunc("string>?", 2, null, function (e) {
      assert_string(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((assert_string(e[t]), !(e[t - 1] > e[t]))) return !1;
      return !0;
    }),
    define_libfunc("string<=?", 2, null, function (e) {
      assert_string(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((assert_string(e[t]), !(e[t - 1] <= e[t]))) return !1;
      return !0;
    }),
    define_libfunc("string>=?", 2, null, function (e) {
      assert_string(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((assert_string(e[t]), !(e[t - 1] >= e[t]))) return !1;
      return !0;
    }),
    define_libfunc("substring", 3, 3, function (e) {
      if (
        (assert_string(e[0]),
        assert_integer(e[1]),
        assert_integer(e[2]),
        e[1] < 0)
      )
        throw new BiwaError("substring: start too small: " + e[1]);
      if (e[2] < 0) throw new BiwaError("substring: end too small: " + e[2]);
      if (e[0].length + 1 <= e[1])
        throw new BiwaError("substring: start too big: " + e[1]);
      if (e[0].length + 1 <= e[2])
        throw new BiwaError("substring: end too big: " + e[2]);
      if (!(e[1] <= e[2]))
        throw new BiwaError(
          "substring: not start <= end: " + e[1] + ", " + e[2]
        );
      return e[0].substring(e[1], e[2]);
    }),
    define_libfunc("string-append", 0, null, function (e) {
      for (var t = 0; t < e.length; t++) assert_string(e[t]);
      return e.join("");
    }),
    define_libfunc("string->list", 1, 1, function (e) {
      return (
        assert_string(e[0]),
        array_to_list(
          e[0].split("").map(function (e) {
            return Char.get(e[0]);
          })
        )
      );
    }),
    define_libfunc("list->string", 1, 1, function (e) {
      return (
        assert_list(e[0]),
        e[0]
          .to_array()
          .map(function (e) {
            return e.value;
          })
          .join("")
      );
    }),
    define_libfunc("string-for-each", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      return (
        n.forEach(assert_string),
        Call.multi_foreach(n, {
          call: function (e) {
            return new Call(t, e);
          },
          finish: function () {
            return undef;
          },
        })
      );
    }),
    define_libfunc("string-copy", 1, 1, function (e) {
      return assert_string(e[0]), e[0];
    }),
    define_libfunc("vector?", 1, 1, function (e) {
      return isVector(e[0]);
    }),
    define_libfunc("make-vector", 1, 2, function (e) {
      assert_integer(e[0]);
      var t = new Array(e[0]);
      if (2 == e.length) for (var n = 0; n < e[0]; n++) t[n] = e[1];
      return t;
    }),
    define_libfunc("vector", 0, null, function (e) {
      return e;
    }),
    define_libfunc("vector-length", 1, 1, function (e) {
      return assert_vector(e[0]), e[0].length;
    }),
    define_libfunc("vector-ref", 2, 2, function (e) {
      return (
        assert_vector(e[0]),
        assert_integer(e[1]),
        assert_between(e[1], 0, e[0].length - 1),
        e[0][e[1]]
      );
    }),
    define_libfunc("vector-set!", 3, 3, function (e) {
      return (
        assert_vector(e[0]), assert_integer(e[1]), (e[0][e[1]] = e[2]), undef
      );
    }),
    define_libfunc("vector->list", 1, 1, function (e) {
      return assert_vector(e[0]), array_to_list(e[0]);
    }),
    define_libfunc("list->vector", 1, 1, function (e) {
      return assert_list(e[0]), e[0].to_array();
    }),
    define_libfunc("vector-fill!", 2, 2, function (e) {
      assert_vector(e[0]);
      for (var t = e[0], n = e[1], r = 0; r < t.length; r++) t[r] = n;
      return t;
    }),
    define_libfunc("vector-map", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      n.forEach(assert_vector);
      var r = [];
      return Call.multi_foreach(n, {
        call: function (e) {
          return new Call(t, e);
        },
        result: function (e) {
          r.push(e);
        },
        finish: function () {
          return r;
        },
      });
    }),
    define_libfunc("vector-for-each", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      return (
        n.forEach(assert_vector),
        Call.multi_foreach(n, {
          call: function (e) {
            return new Call(t, e);
          },
          finish: function () {
            return undef;
          },
        })
      );
    }),
    define_libfunc("apply", 2, null, function (e) {
      var t = e.shift(),
        n = e.pop(),
        r = e;
      return (r = r.concat(n.to_array())), new Call(t, r);
    }),
    define_syntax("call-with-current-continuation", function (e) {
      return new Pair(Sym("call/cc"), e.cdr);
    }),
    define_libfunc("values", 0, null, function (e) {
      return 1 == e.length ? e[0] : new Values$1(e);
    }),
    define_libfunc("call-with-values", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        assert_procedure(t),
        assert_procedure(n),
        new Call(t, [], function (e) {
          var t = e[0];
          return new Call(n, t instanceof Values$1 ? t.content : [t]);
        })
      );
    }),
    define_libfunc("dynamic-wind", 3, 3, function (e, t) {
      var n = e[0],
        r = e[1],
        i = e[2];
      return new Call(n, [], function () {
        return (
          t.push_dynamic_winder(n, i),
          new Call(r, [], function (e) {
            var n = e[0];
            return (
              t.pop_dynamic_winder(),
              new Call(i, [], function () {
                return n;
              })
            );
          })
        );
      });
    });
  var expand_qq = function (e, t) {
    if (e instanceof BiwaSymbol || e === nil) return List(Sym("quote"), e);
    if (e instanceof Pair) {
      var n = e.car;
      return n instanceof Pair && n.car === Sym("unquote-splicing")
        ? 1 == t
          ? e.cdr === nil
            ? e.car.cdr.car
            : List(Sym("append"), e.car.cdr.car, expand_qq(e.cdr, t))
          : List(
              Sym("cons"),
              List(
                Sym("list"),
                List(Sym("quote"), Sym("unquote-splicing")),
                expand_qq(e.car.cdr.car, t - 1)
              ),
              expand_qq(e.cdr, t)
            )
        : n === Sym("unquote")
        ? 1 == t
          ? e.cdr.car
          : List(
              Sym("list"),
              List(Sym("quote"), Sym("unquote")),
              expand_qq(e.cdr.car, t - 1)
            )
        : n === Sym("quasiquote")
        ? List(
            Sym("list"),
            List(Sym("quote"), Sym("quasiquote")),
            expand_qq(e.cdr.car, t + 1)
          )
        : List(Sym("cons"), expand_qq(e.car, t), expand_qq(e.cdr, t));
    }
    if (e instanceof Array) {
      for (var r = [[]], i = 0; i < e.length; i++)
        if (e[i] instanceof Pair && e[i].car === Sym("unquote-splicing"))
          if (1 == t) {
            ((a = List(Sym("list->vector"), e[i].cdr.car)).splicing = !0),
              r.push(a),
              r.push([]);
          } else {
            var a = List(
              Sym("cons"),
              List(
                Sym("list"),
                List(Sym("quote"), Sym("unquote-splicing")),
                expand_qq(e[i].car.cdr.car, t - 1)
              ),
              expand_qq(e[i].cdr, t)
            );
            r[r.length - 1].push(a);
          }
        else r[r.length - 1].push(expand_qq(e[i], t));
      var s = r.map(function (e) {
        return e.splicing ? e : Cons(Sym("vector"), array_to_list(e));
      });
      return 1 == s.length
        ? Cons(Sym("vector"), array_to_list(r[0]))
        : Cons(Sym("vector-append"), array_to_list(s));
    }
    return e;
  };
  define_syntax("quasiquote", function (e) {
    return expand_qq(e.cdr.car, 1);
  }),
    define_syntax("unquote", function (e) {
      throw new BiwaError("unquote(,) must be inside quasiquote(`)");
    }),
    define_syntax("unquote-splicing", function (e) {
      throw new BiwaError("unquote-splicing(,@) must be inside quasiquote(`)");
    }),
    define_libfunc("string-upcase", 1, 1, function (e) {
      return assert_string(e[0]), e[0].toUpperCase();
    }),
    define_libfunc("string-downcase", 1, 1, function (e) {
      return assert_string(e[0]), e[0].toLowerCase();
    });
  const make_string_ci_function = function (e) {
    return function (t) {
      assert_string(t[0]);
      for (var n = t[0].toUpperCase(), r = 1; r < t.length; r++)
        if ((assert_string(t[r]), !e(n, t[r].toUpperCase()))) return !1;
      return !0;
    };
  };
  define_libfunc(
    "string-ci=?",
    2,
    null,
    make_string_ci_function(function (e, t) {
      return e == t;
    })
  ),
    define_libfunc(
      "string-ci<?",
      2,
      null,
      make_string_ci_function(function (e, t) {
        return e < t;
      })
    ),
    define_libfunc(
      "string-ci>?",
      2,
      null,
      make_string_ci_function(function (e, t) {
        return e > t;
      })
    ),
    define_libfunc(
      "string-ci<=?",
      2,
      null,
      make_string_ci_function(function (e, t) {
        return e <= t;
      })
    ),
    define_libfunc(
      "string-ci>=?",
      2,
      null,
      make_string_ci_function(function (e, t) {
        return e >= t;
      })
    ),
    define_libfunc("find", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        assert_list(n),
        Call.foreach(n, {
          call: function (e) {
            return new Call(t, [e.car]);
          },
          result: function (e, t) {
            if (e) return t.car;
          },
          finish: function () {
            return !1;
          },
        })
      );
    }),
    define_libfunc("for-all", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      n.forEach(assert_list);
      var r = !0;
      return Call.multi_foreach(n, {
        call: function (e) {
          return new Call(
            t,
            e.map(function (e) {
              return e.car;
            })
          );
        },
        result: function (e, t) {
          if (!1 === e) return !1;
          r = e;
        },
        finish: function () {
          return r;
        },
      });
    }),
    define_libfunc("exists", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      return (
        n.forEach(assert_list),
        Call.multi_foreach(n, {
          call: function (e) {
            return new Call(
              t,
              e.map(function (e) {
                return e.car;
              })
            );
          },
          result: function (e, t) {
            if (!1 !== e) return e;
          },
          finish: function () {
            return !1;
          },
        })
      );
    }),
    define_libfunc("filter", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      assert_list(n);
      var r = [];
      return Call.foreach(n, {
        call: function (e) {
          return new Call(t, [e.car]);
        },
        result: function (e, t) {
          e && r.push(t.car);
        },
        finish: function () {
          return array_to_list(r);
        },
      });
    }),
    define_libfunc("partition", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      assert_list(n);
      var r = [],
        i = [];
      return Call.foreach(n, {
        call: function (e) {
          return new Call(t, [e.car]);
        },
        result: function (e, t) {
          e ? r.push(t.car) : i.push(t.car);
        },
        finish: function () {
          return new Values$1([array_to_list(r), array_to_list(i)]);
        },
      });
    }),
    define_libfunc("fold-left", 3, null, function (e) {
      var t = e.shift(),
        n = e.shift(),
        r = e;
      return (
        r.forEach(assert_list),
        Call.multi_foreach(r, {
          call: function (e) {
            var r = e.map(function (e) {
              return e.car;
            });
            return r.unshift(n), new Call(t, r);
          },
          result: function (e, t) {
            n = e;
          },
          finish: function () {
            return n;
          },
        })
      );
    }),
    define_libfunc("fold-right", 3, null, function (e) {
      var t = e.shift(),
        n = e.shift(),
        r = e.map(function (e) {
          return assert_list(e), array_to_list(e.to_array().reverse());
        });
      return Call.multi_foreach(r, {
        call: function (e) {
          var r = e.map(function (e) {
            return e.car;
          });
          return r.push(n), new Call(t, r);
        },
        result: function (e, t) {
          n = e;
        },
        finish: function () {
          return n;
        },
      });
    }),
    define_libfunc("remp", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      assert_list(n);
      var r = [];
      return Call.foreach(n, {
        call: function (e) {
          return new Call(t, [e.car]);
        },
        result: function (e, t) {
          e || r.push(t.car);
        },
        finish: function () {
          return array_to_list(r);
        },
      });
    });
  var make_remover = function (e) {
    return function (t) {
      var n = t[0],
        r = t[1];
      assert_list(r);
      var i = [];
      return Call.foreach(r, {
        call: function (t) {
          return new Call(TopEnv[e] || CoreEnv[e], [n, t.car]);
        },
        result: function (e, t) {
          e || i.push(t.car);
        },
        finish: function () {
          return array_to_list(i);
        },
      });
    };
  };
  define_libfunc("remove", 2, 2, make_remover("equal?")),
    define_libfunc("remv", 2, 2, make_remover("eqv?")),
    define_libfunc("remq", 2, 2, make_remover("eq?")),
    define_libfunc("memp", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        assert_list(n),
        Call.foreach(n, {
          call: function (e) {
            return new Call(t, [e.car]);
          },
          result: function (e, t) {
            if (e) return t;
          },
          finish: function () {
            return !1;
          },
        })
      );
    });
  var make_finder = function (e) {
    return function (t) {
      var n = t[0],
        r = t[1];
      return (
        assert_list(r),
        Call.foreach(r, {
          call: function (t) {
            return new Call(TopEnv[e] || CoreEnv[e], [n, t.car]);
          },
          result: function (e, t) {
            if (e) return t;
          },
          finish: function () {
            return !1;
          },
        })
      );
    };
  };
  define_libfunc("member", 2, 2, make_finder("equal?")),
    define_libfunc("memv", 2, 2, make_finder("eqv?")),
    define_libfunc("memq", 2, 2, make_finder("eq?")),
    define_libfunc("assp", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        assert_list(n),
        Call.foreach(n, {
          call: function (e) {
            if (e.car.car) return new Call(t, [e.car.car]);
            throw new BiwaError(
              "ass*: pair required but got " + to_write$1(e.car)
            );
          },
          result: function (e, t) {
            if (e) return t.car;
          },
          finish: function () {
            return !1;
          },
        })
      );
    });
  var make_assoc = function (e, t) {
    return function (n) {
      var r = n[0],
        i = n[1];
      return (
        assert_list(i),
        Call.foreach(i, {
          call: function (n) {
            if (!isPair(n.car))
              throw new BiwaError(
                e + ": pair required but got " + to_write$1(n.car)
              );
            var i = TopEnv[t] || CoreEnv[t];
            return new Call(i, [r, n.car.car]);
          },
          result: function (e, t) {
            if (e) return t.car;
          },
          finish: function () {
            return !1;
          },
        })
      );
    };
  };
  define_libfunc("assoc", 2, 2, make_assoc("assoc", "equal?")),
    define_libfunc("assv", 2, 2, make_assoc("assv", "eqv?")),
    define_libfunc("assq", 2, 2, make_assoc("assq", "eq?")),
    define_libfunc("cons*", 1, null, function (e) {
      if (1 == e.length) return e[0];
      var t = null;
      return (
        e.reverse().forEach(function (e) {
          t = t ? new Pair(e, t) : e;
        }),
        t
      );
    }),
    (function () {
      var e = function (e, n, r) {
          return e.length <= 1 ? r(e) : t(e, n, r, [[0, e.length, !1]], !1);
        },
        t = function (e, r, i, a, s) {
          for (;;) {
            var o = a[a.length - 1][0],
              u = a[a.length - 1][1],
              c = a[a.length - 1][2],
              l = u - o;
            if (l >= 2 && !s) a.push([o, o + (l >> 1), !0]);
            else {
              if (!c) {
                a.pop();
                var f = a[a.length - 1][0],
                  d = e.slice(f, o),
                  h = e.slice(o, u);
                return n(d, h, r, [], 0, 0, function (n) {
                  for (var s = 0; s < n.length; s++) e[f + s] = n[s];
                  return 1 == a.length ? i(e) : t(e, r, i, a, !0);
                });
              }
              a.pop();
              var p = a[a.length - 1][1];
              a.push([u, p, !1]), (s = !1);
            }
          }
        },
        n = function (e, t, r, i, a, s, o) {
          var u = e.length,
            c = t.length;
          if (a < u && s < c)
            return new Call(r, [t[s], e[a]], function (u) {
              return (
                u[0] ? (i.push(t[s]), (s += 1)) : (i.push(e[a]), (a += 1)),
                n(e, t, r, i, a, s, o)
              );
            });
          for (; a < u; ) i.push(e[a]), (a += 1);
          for (; s < c; ) i.push(t[s]), (s += 1);
          return o(i);
        },
        r = function (e, t) {
          return lt(e, t) ? -1 : lt(t, e) ? 1 : 0;
        };
      define_libfunc("list-sort", 1, 2, function (t) {
        return t[1]
          ? (assert_procedure(t[0]),
            assert_list(t[1]),
            e(t[1].to_array(), t[0], function (e) {
              return array_to_list(e);
            }))
          : (assert_list(t[0]), array_to_list(t[0].to_array().sort(r)));
      }),
        define_libfunc("vector-sort", 1, 2, function (t) {
          return t[1]
            ? (assert_procedure(t[0]),
              assert_vector(t[1]),
              e([...t[1]], t[0], function (e) {
                return e;
              }))
            : (assert_vector(t[0]), [...t[0]].sort(r));
        }),
        define_libfunc("vector-sort!", 1, 2, function (t) {
          return t[1]
            ? (assert_procedure(t[0]),
              assert_vector(t[1]),
              e(t[1], t[0], function (e) {
                return undef;
              }))
            : (assert_vector(t[0]), t[0].sort(r), undef);
        });
    })(),
    define_syntax("when", function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr;
      return new Pair(
        Sym("if"),
        new Pair(t, new Pair(new Pair(Sym("begin"), n), new Pair(undef, nil)))
      );
    }),
    define_syntax("unless", function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr;
      return new Pair(
        Sym("if"),
        new Pair(
          new Pair(Sym("not"), new Pair(t, nil)),
          new Pair(new Pair(Sym("begin"), n), new Pair(undef, nil))
        )
      );
    }),
    define_syntax("do", function (e) {
      if (!isPair(e.cdr)) throw new BiwaError("do: no variables of do");
      var t = e.cdr.car;
      if (!isPair(t))
        throw new BiwaError("do: variables must be given as a list");
      if (!isPair(e.cdr.cdr))
        throw new BiwaError("do: no resulting form of do");
      var n = e.cdr.cdr.car,
        r = e.cdr.cdr.cdr,
        i = gensym(),
        a = array_to_list(
          t.map(function (e) {
            var t = e.to_array();
            return List(t[0], t[1]);
          })
        ),
        s = n.car,
        o = new Pair(Sym("begin"), n.cdr),
        u = new Pair(
          i,
          array_to_list(
            t.map(function (e) {
              var t = e.to_array();
              return t[2] || t[0];
            })
          )
        ),
        c = new Pair(Sym("begin"), r).concat(List(u));
      return List(Sym("let"), i, a, List(Sym("if"), s, o, c));
    }),
    define_syntax("case-lambda", function (e) {
      if (!isPair(e.cdr))
        throw new BiwaError("case-lambda: at least 1 clause required");
      var t = e.cdr.to_array(),
        n = gensym(),
        r = List(Sym("raise"), "case-lambda: no matching clause found");
      return (
        t.reverse().forEach(function (e) {
          if (!isPair(e))
            throw new BiwaError(
              "case-lambda: clause must be a pair: " + to_write$1(e)
            );
          var t = e.car,
            i = e.cdr;
          if (t === nil)
            r = List(
              Sym("if"),
              List(Sym("null?"), n),
              new Pair(Sym("begin"), i),
              r
            );
          else if (isPair(t)) {
            var a = t.length(),
              s = t.last_cdr(),
              o = Sym(s === nil ? "=" : ">="),
              u = new Pair(Sym("lambda"), new Pair(t, i));
            r = List(
              Sym("if"),
              List(o, List(Sym("length"), n), a),
              List(Sym("apply"), u, n),
              r
            );
          } else {
            if (!isSymbol(t))
              throw new BiwaError(
                "case-lambda: invalid formals: " + to_write$1(t)
              );
            r = new Pair(Sym("let1"), new Pair(t, new Pair(n, i)));
          }
        }),
        List(Sym("lambda"), n, r)
      );
    }),
    define_syntax("define-record-type", function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr;
      if (isSymbol(t))
        var r = t,
          i = Sym("make-" + t.name),
          a = Sym(t.name + "?");
      else {
        assert_list(t);
        (r = t.car), (i = t.cdr.car), (a = t.cdr.cdr.car);
        assert_symbol(r), assert_symbol(i), assert_symbol(a);
      }
      var s,
        o = !1,
        u = !1,
        c = !1,
        l = !1,
        f = !1,
        d = !1,
        h = [];
      n.to_array().forEach(function (e) {
        switch (e.car) {
          case Sym("fields"):
            h = e.cdr.to_array().map(function (e, t) {
              if (isSymbol(e))
                return {
                  name: e,
                  idx: t,
                  mutable: !1,
                  accessor_name: null,
                  mutator_name: null,
                };
              switch ((assert_list(e), assert_symbol(e.car), e.car)) {
                case Sym("immutable"):
                  var n = e.cdr.car;
                  return (
                    assert_symbol(n),
                    isNil(e.cdr.cdr)
                      ? { name: n, idx: t, mutable: !1 }
                      : {
                          name: n,
                          idx: t,
                          mutable: !1,
                          accessor_name: e.cdr.cdr.car,
                        }
                  );
                case Sym("mutable"):
                  n = e.cdr.car;
                  return (
                    assert_symbol(n),
                    isNil(e.cdr.cdr)
                      ? { name: n, idx: t, mutable: !0 }
                      : {
                          name: n,
                          idx: t,
                          mutable: !0,
                          accessor_name: e.cdr.cdr.car,
                          mutator_name: e.cdr.cdr.cdr.car,
                        }
                  );
                default:
                  throw new BiwaError(
                    "define-record-type: field definition must start with `immutable' or `mutable' but got " +
                      inspect(e.car)
                  );
              }
            });
            break;
          case Sym("parent"):
            (s = e.cdr.car), assert_symbol(s);
            break;
          case Sym("protocol"):
            d = e.cdr.car;
            break;
          case Sym("sealed"):
            o = !!e.cdr.car;
            break;
          case Sym("opaque"):
            u = !!e.cdr.car;
            break;
          case Sym("nongenerative"):
            c = e.cdr.car;
            break;
          case Sym("parent-rtd"):
            (l = e.cdr.car), (f = e.cdr.cdr.car);
            break;
          default:
            throw new BiwaError(
              "define-record-type: unknown clause `" + to_write$1(e.car) + "'"
            );
        }
      }),
        s &&
          ((l = [Sym("record-type-descriptor"), s]),
          (f = [Sym("record-constructor-descriptor"), s]));
      var p = [Sym("record-type-descriptor"), r],
        m = [Sym("record-constructor-descriptor"), r],
        _ = h.map(function (e) {
          return List(Sym(e.mutable ? "mutable" : "immutable"), e.name);
        });
      _.is_vector = !0;
      var b = [
          Sym("make-record-type-descriptor"),
          [Sym("quote"), r],
          l,
          c,
          o,
          u,
          _,
        ],
        g = [Sym("make-record-constructor-descriptor"), Sym("__rtd"), f, d],
        y = [
          Sym("let*"),
          [
            [Sym("__rtd"), b],
            [Sym("__cd"), g],
          ],
          [
            Sym("_define-record-type"),
            [Sym("quote"), r],
            Sym("__rtd"),
            Sym("__cd"),
          ],
        ],
        v = h.map(function (e) {
          var t = e.accessor_name || Sym(r.name + "-" + e.name.name);
          return [Sym("define"), t, [Sym("record-accessor"), p, e.idx]];
        }),
        w = h.filter(function (e) {
          return e.mutable;
        });
      return (
        (w = w.map(function (e) {
          var t = e.mutator_name || Sym(r.name + "-" + e.name.name + "-set!");
          return [Sym("define"), t, [Sym("record-mutator"), p, e.idx]];
        })),
        deep_array_to_list(
          [
            Sym("begin"),
            y,
            [Sym("define"), i, [Sym("record-constructor"), m]],
            [Sym("define"), a, [Sym("record-predicate"), p]],
          ]
            .concat(v)
            .concat(w)
        )
      );
    }),
    define_libfunc("_define-record-type", 3, 3, function (e) {
      return (
        assert_symbol(e[0]),
        assert_record_td(e[1]),
        assert_record_cd(e[2]),
        Record.define_type(e[0].name, e[1], e[2]),
        undef
      );
    }),
    define_syntax("record-type-descriptor", function (e) {
      return deep_array_to_list([
        Sym("_record-type-descriptor"),
        [Sym("quote"), e.cdr.car],
      ]);
    }),
    define_libfunc("_record-type-descriptor", 1, 1, function (e) {
      assert_symbol(e[0]);
      var t = Record.get_type(e[0].name);
      if (t) return t.rtd;
      throw new BiwaError(
        "record-type-descriptor: unknown record type " + e[0].name
      );
    }),
    define_syntax("record-constructor-descriptor", function (e) {
      return deep_array_to_list([
        Sym("_record-constructor-descriptor"),
        [Sym("quote"), e.cdr.car],
      ]);
    }),
    define_libfunc("_record-constructor-descriptor", 1, 1, function (e) {
      assert_symbol(e[0]);
      var t = Record.get_type(e[0].name);
      if (t) return t.cd;
      throw new BiwaError(
        "record-constructor-descriptor: unknown record type " + e[0].name
      );
    }),
    define_libfunc("make-record-type-descriptor", 6, 6, function (e) {
      var t = e[0],
        n = e[1],
        r = e[2],
        i = e[3],
        a = e[4],
        s = e[5];
      if ((assert_symbol(t), n && assert_record_td(n), r)) {
        assert_symbol(r);
        var o = Record.RTD.NongenerativeRecords[r.name];
        if (o) return o;
      }
      (i = !!i), (a = !!a), assert_vector(s);
      for (var u = 0; u < s.length; u++) {
        var c = s[u];
        assert_symbol(c.car, "mutability"),
          assert_symbol(c.cdr.car, "field name"),
          (s[u] = [c.cdr.car.name, c.car == Sym("mutable")]);
      }
      var l = new Record.RTD(t, n, r, i, a, s);
      return r && (Record.RTD.NongenerativeRecords[r.name] = l), l;
    }),
    define_libfunc("record-type-descriptor?", 1, 1, function (e) {
      return e[0] instanceof Record.RTD;
    }),
    define_libfunc("make-record-constructor-descriptor", 3, 3, function (e) {
      var t = e[0],
        n = e[1],
        r = e[2];
      return (
        assert_record_td(t),
        n && assert_record_cd(n),
        r && assert_procedure(r),
        new Record.CD(t, n, r)
      );
    }),
    define_libfunc("record-constructor", 1, 1, function (e) {
      var t = e[0];
      return assert_record_cd(t), t.record_constructor();
    }),
    define_libfunc("record-predicate", 1, 1, function (e) {
      var t = e[0];
      return (
        assert_record_td(t),
        function (e) {
          var n = e[0];
          if (n instanceof Record) {
            if (n.rtd === t) return !0;
            for (let e = n.rtd; e; e = e.parent_rtd) if (e == t) return !0;
            return !1;
          }
          return !1;
        }
      );
    }),
    define_libfunc("record-accessor", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      assert_record_td(t), assert_integer(n);
      for (var r = t.parent_rtd; r; r = r.parent_rtd) n += r.fields.length;
      return function (e) {
        var r = e[0],
          i =
            t.name.name +
            "-" +
            t.field_name(n) +
            ": " +
            to_write$1(r) +
            " is not a " +
            t.name.name;
        assert(isRecord(r), i);
        for (var a = !1, s = r.rtd; s; s = s.parent_rtd) s == t && (a = !0);
        return assert(a, i), r.get(n);
      };
    }),
    define_libfunc("record-mutator", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      assert_record_td(t), assert_integer(n);
      for (var r = t.parent_rtd; r; r = r.parent_rtd) n += r.fields.length;
      return function (e) {
        var r = e[0],
          i = e[1],
          a = t.field_name(n);
        assert_record(r),
          assert(
            r.rtd === t,
            a + ": " + to_write$1(r) + " is not a " + t.name.name
          ),
          assert(
            !r.rtd.sealed,
            a + ": " + t.name.name + " is sealed (can't mutate)"
          ),
          r.set(n, i);
      };
    }),
    define_libfunc("record?", 1, 1, function (e) {
      var t = e[0];
      return !!isRecord(t) && !t.rtd.opaque;
    }),
    define_libfunc("record-rtd", 1, 1, function (e) {
      return assert_record(e[0]), e[0].rtd;
    }),
    define_libfunc("record-type-name", 1, 1, function (e) {
      return assert_record_td(e[0]), e[0].name;
    }),
    define_libfunc("record-type-parent", 1, 1, function (e) {
      return assert_record_td(e[0]), e[0].parent_rtd;
    }),
    define_libfunc("record-type-uid", 1, 1, function (e) {
      return assert_record_td(e[0]), e[0].uid;
    }),
    define_libfunc("record-type-generative?", 1, 1, function (e) {
      return assert_record_td(e[0]), e[0].generative;
    }),
    define_libfunc("record-type-sealed?", 1, 1, function (e) {
      return assert_record_td(e[0]), e[0].sealed;
    }),
    define_libfunc("record-type-opaque?", 1, 1, function (e) {
      return assert_record_td(e[0]), e[0].opaque;
    }),
    define_libfunc("record-type-field-names", 1, 1, function (e) {
      return (
        assert_record_td(e[0]),
        e[0].fields.map(function (e) {
          return e.name;
        })
      );
    }),
    define_libfunc("record-field-mutable?", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      assert_record_td(e[0]), assert_integer(n);
      for (var r = t.parent_rtd; r; r = r.parent_rtd) n += r.fields.length;
      return e[0].fields[n].mutable;
    }),
    define_libfunc("raise", 1, 1, function (e) {
      throw new UserError(to_write$1(e[0]));
    }),
    define_libfunc("port?", 1, 1, function (e) {
      return e[0] instanceof Port;
    }),
    define_libfunc("textual-port?", 1, 1, function (e) {
      return assert_port(e[0]), !e[0].is_binary;
    }),
    define_libfunc("binary-port?", 1, 1, function (e) {
      return assert_port(e[0]), e[0].is_binary;
    }),
    define_libfunc("close-port", 1, 1, function (e) {
      return assert_port(e[0]), e[0].close(), undef;
    }),
    define_libfunc("call-with-port", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        assert_port(t),
        assert_closure(n),
        new Call(n, [t], function (e) {
          return t.close(), e[0];
        })
      );
    }),
    define_libfunc("call-with-string-output-port", 1, 1, function (e) {
      var t = e[0];
      assert_procedure(t);
      var n = new Port.StringOutput();
      return new Call(t, [n], function (e) {
        return n.close(), n.output_string();
      });
    }),
    define_libfunc("put-char", 2, 2, function (e) {
      return (
        assert_port(e[0]), assert_char(e[1]), e[0].put_string(e[1].value), undef
      );
    }),
    define_libfunc("put-string", 2, 2, function (e) {
      return (
        assert_port(e[0]), assert_string(e[1]), e[0].put_string(e[1]), undef
      );
    }),
    define_libfunc("put-datum", 2, 2, function (e) {
      return assert_port(e[0]), e[0].put_string(to_write$1(e[1])), undef;
    }),
    define_libfunc("eof-object", 0, 0, function (e) {
      return eof;
    }),
    define_libfunc("eof-object?", 1, 1, function (e) {
      return e[0] === eof;
    }),
    define_libfunc("input-port?", 1, 1, function (e) {
      return assert_port(e[0]), e[0].is_input;
    }),
    define_libfunc("output-port?", 1, 1, function (e) {
      return assert_port(e[0]), e[0].is_output;
    }),
    define_libfunc("current-input-port", 0, 0, function (e) {
      return Port.current_input;
    }),
    define_libfunc("current-output-port", 0, 0, function (e) {
      return Port.current_output;
    }),
    define_libfunc("current-error-port", 0, 0, function (e) {
      return Port.current_error;
    }),
    define_libfunc("close-input-port", 1, 1, function (e) {
      if ((assert_port(e[0]), !e[0].is_input))
        throw new BiwaError("close-input-port: port is not input port");
      return e[0].close(), undef;
    }),
    define_libfunc("close-output-port", 1, 1, function (e) {
      if ((assert_port(e[0]), !e[0].is_output))
        throw new BiwaError("close-output-port: port is not output port");
      return e[0].close(), undef;
    }),
    define_libfunc("read", 0, 1, function (e) {
      var t = e[0] || Port.current_input;
      return (
        assert_port(t),
        t.get_string(function (e) {
          return Interpreter.read(e);
        })
      );
    }),
    define_libfunc("write-char", 1, 2, function (e) {
      var t = e[1] || Port.current_output;
      return assert_char(e[0]), t.put_string(e[0].value), undef;
    }),
    define_libfunc("newline", 0, 1, function (e) {
      return (e[0] || Port.current_output).put_string("\n"), undef;
    }),
    define_libfunc("display", 1, 2, function (e) {
      return (e[1] || Port.current_output).put_string(to_display(e[0])), undef;
    }),
    define_libfunc("write", 1, 2, function (e) {
      var t = e[1] || Port.current_output;
      return assert_port(t), t.put_string(to_write$1(e[0])), undef;
    }),
    define_libfunc("write-shared", 1, 2, function (e) {
      var t = e[1] || Port.current_output;
      return assert_port(t), t.put_string(write_shared(e[0])), undef;
    }),
    define_libfunc("write-simple", 1, 2, function (e) {
      var t = e[1] || Port.current_output;
      return assert_port(t), t.put_string(write_simple(e[0])), undef;
    }),
    define_libfunc("bitwise-not", 1, 1, function (e) {
      return ~e[0];
    }),
    define_libfunc("bitwise-and", 1, null, function (e) {
      return e.reduce(function (e, t) {
        return e & t;
      });
    }),
    define_libfunc("bitwise-ior", 1, null, function (e) {
      return e.reduce(function (e, t) {
        return e | t;
      });
    }),
    define_libfunc("bitwise-xor", 1, null, function (e) {
      return e.reduce(function (e, t) {
        return e ^ t;
      });
    }),
    define_libfunc("bitwise-if", 3, 3, function (e) {
      return (e[0] & e[1]) | (~e[0] & e[2]);
    }),
    define_libfunc("bitwise-bit-count", 1, 1, function (e) {
      for (var t = Math.abs(e[0]), n = 0; 0 != t; t >>= 1) 1 & t && n++;
      return n;
    }),
    define_libfunc("bitwise-length", 1, 1, function (e) {
      for (var t = Math.abs(e[0]), n = 0; 0 != t; t >>= 1) n++;
      return n;
    }),
    define_libfunc("bitwise-first-bit-set", 1, 1, function (e) {
      var t = Math.abs(e[0]),
        n = 0;
      if (0 == t) return -1;
      for (; 0 != t; t >>= 1) {
        if (1 & t) return n;
        n++;
      }
    }),
    define_libfunc("bitwise-bit-set?", 2, 2, function (e) {
      return !!(e[0] & (1 << e[1]));
    }),
    define_libfunc("bitwise-copy-bit", 3, 3, function (e) {
      var t = 1 << e[1];
      return (t & (e[2] << e[1])) | (~t & e[0]);
    }),
    define_libfunc("bitwise-bit-field", 3, 3, function (e) {
      return (~(-1 << e[2]) & e[0]) >> e[1];
    }),
    define_libfunc("bitwise-copy-bit-field", 4, 4, function (e) {
      var t = e[0],
        n = e[1],
        r = ~(-1 << e[2]) & (-1 << n);
      return (r & (e[3] << n)) | (~r & t);
    }),
    define_libfunc("bitwise-arithmetic-shift", 2, 2, function (e) {
      return e[1] >= 0 ? e[0] << e[1] : e[0] >> -e[1];
    }),
    define_libfunc("bitwise-arithmetic-shift-left", 2, 2, function (e) {
      return e[0] << e[1];
    }),
    define_libfunc("bitwise-arithmetic-shift-right", 2, 2, function (e) {
      return e[0] >> e[1];
    }),
    define_libfunc("bitwise-rotate-bit-field", 4, 4, function (e) {
      var t = e[0],
        n = e[1],
        r = e[2],
        i = e[3],
        a = r - n;
      if (a <= 0) return t;
      var s = (~(-1 << r) & t) >> n,
        o = ~(-1 << r) & (-1 << n);
      return (o & (((s << (i %= a)) | (s >> (a - i))) << n)) | (~o & t);
    }),
    define_libfunc("bitwise-reverse-bit-field", 3, 3, function (e) {
      for (
        var t = e[0],
          n = e[0],
          r = e[1],
          i = e[2],
          a = (~(-1 << i) & n) >> r,
          s = 0;
        s < i - r;
        s++, a >>= 1
      ) {
        var o = i - 1 - s,
          u = 1 << o;
        t = (u & ((1 & a) << o)) | (~u & t);
      }
      return t;
    }),
    define_libfunc("make-eq-hashtable", 0, 1, function (e) {
      return new Hashtable(Hashtable.eq_hash, Hashtable.eq_equiv);
    }),
    define_libfunc("make-eqv-hashtable", 0, 1, function (e) {
      return new Hashtable(Hashtable.eqv_hash, Hashtable.eqv_equiv);
    }),
    define_libfunc("make-hashtable", 2, 3, function (e) {
      return (
        assert_procedure(e[0]),
        assert_procedure(e[1]),
        new Hashtable(e[0], e[1])
      );
    }),
    define_libfunc("hashtable?", 1, 1, function (e) {
      return e[0] instanceof Hashtable;
    }),
    define_libfunc("hashtable-size", 1, 1, function (e) {
      return assert_hashtable(e[0]), e[0].keys().length;
    });
  const find_hash_pair = function (e, t, n) {
    return new Call(e.hash_proc, [t], function (r) {
      var i = r[0],
        a = e.candidate_pairs(i);
      return a
        ? Call.foreach(a, {
            call: function (n) {
              return new Call(e.equiv_proc, [t, n[0]]);
            },
            result: function (e, t) {
              if (e) return n.on_found(t, i);
            },
            finish: function () {
              return n.on_not_found(i);
            },
          })
        : n.on_not_found(i);
    });
  };
  define_libfunc("hashtable-ref", 3, 3, function (e) {
    var t = e[0],
      n = e[1],
      r = e[2];
    return (
      assert_hashtable(t),
      find_hash_pair(t, n, {
        on_found: function (e) {
          return e[1];
        },
        on_not_found: function (e) {
          return r;
        },
      })
    );
  }),
    define_libfunc("hashtable-set!", 3, 3, function (e) {
      var t = e[0],
        n = e[1],
        r = e[2];
      return (
        assert_hashtable(t),
        assert(t.mutable, "hashtable is not mutable"),
        find_hash_pair(t, n, {
          on_found: function (e) {
            return (e[1] = r), undef;
          },
          on_not_found: function (e) {
            return t.add_pair(e, n, r), undef;
          },
        })
      );
    }),
    define_libfunc("hashtable-delete!", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        assert_hashtable(t),
        assert(t.mutable, "hashtable is not mutable"),
        find_hash_pair(t, n, {
          on_found: function (e, n) {
            return t.remove_pair(n, e), undef;
          },
          on_not_found: function (e) {
            return undef;
          },
        })
      );
    }),
    define_libfunc("hashtable-contains?", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        assert_hashtable(t),
        find_hash_pair(t, n, {
          on_found: function (e) {
            return !0;
          },
          on_not_found: function (e) {
            return !1;
          },
        })
      );
    }),
    define_libfunc("hashtable-update!", 4, 4, function (e) {
      var t = e[0],
        n = e[1],
        r = e[2],
        i = e[3];
      return (
        assert_hashtable(t),
        assert(t.mutable, "hashtable is not mutable"),
        assert_procedure(r),
        find_hash_pair(t, n, {
          on_found: function (e, t) {
            return new Call(r, [e[1]], function (t) {
              return (e[1] = t[0]), undef;
            });
          },
          on_not_found: function (e) {
            return new Call(r, [i], function (r) {
              return t.add_pair(e, n, r[0]), undef;
            });
          },
        })
      );
    }),
    define_libfunc("hashtable-copy", 1, 2, function (e) {
      var t = void 0 !== e[1] && !!e[1];
      return assert_hashtable(e[0]), e[0].create_copy(t);
    }),
    define_libfunc("hashtable-clear!", 0, 1, function (e) {
      return (
        assert_hashtable(e[0]),
        assert(e[0].mutable, "hashtable is not mutable"),
        e[0].clear(),
        undef
      );
    }),
    define_libfunc("hashtable-keys", 1, 1, function (e) {
      return assert_hashtable(e[0]), e[0].keys();
    }),
    define_libfunc("hashtable-entries", 1, 1, function (e) {
      return assert_hashtable(e[0]), new Values$1([e[0].keys(), e[0].values()]);
    }),
    define_libfunc("hashtable-equivalence-function", 1, 1, function (e) {
      return assert_hashtable(e[0]), e[0].equiv_proc;
    }),
    define_libfunc("hashtable-hash-function", 1, 1, function (e) {
      return assert_hashtable(e[0]), e[0].hash_proc;
    }),
    define_libfunc("hashtable-mutable?", 1, 1, function (e) {
      return assert_hashtable(e[0]), e[0].mutable;
    }),
    define_libfunc("equal-hash", 0, 0, function (e) {
      return Hashtable.equal_hash;
    }),
    define_libfunc("string-hash", 0, 0, function (e) {
      return Hashtable.string_hash;
    }),
    define_libfunc("string-ci-hash", 0, 0, function (e) {
      return Hashtable.string_ci_hash;
    }),
    define_libfunc("symbol-hash", 0, 0, function (e) {
      return Hashtable.symbol_hash;
    }),
    define_libfunc("make-enumeration", 1, 1, function (e) {
      assert_list(e[0]);
      var t = e[0].to_array();
      return new Enumeration.EnumType(t).universe();
    }),
    define_libfunc("enum-set-universe", 1, 1, function (e) {
      return assert_enum_set(e[0]), e[0].enum_type.universe();
    }),
    define_libfunc("enum-set-indexer", 1, 1, function (e) {
      return assert_enum_set(e[0]), e[0].enum_type.indexer();
    }),
    define_libfunc("enum-set-constructor", 1, 1, function (e) {
      return assert_enum_set(e[0]), e[0].enum_type.constructor_();
    }),
    define_libfunc("enum-set->list", 1, 1, function (e) {
      return assert_enum_set(e[0]), e[0].symbol_list();
    }),
    define_libfunc("enum-set-member?", 2, 2, function (e) {
      return assert_symbol(e[0]), assert_enum_set(e[1]), e[1].is_member(e[0]);
    }),
    define_libfunc("enum-set-subset?", 2, 2, function (e) {
      return assert_enum_set(e[0]), assert_enum_set(e[1]), e[0].is_subset(e[1]);
    }),
    define_libfunc("enum-set=?", 2, 2, function (e) {
      return assert_enum_set(e[0]), assert_enum_set(e[1]), e[0].equal_to(e[1]);
    }),
    define_libfunc("enum-set-union", 2, 2, function (e) {
      return (
        assert_enum_set(e[0]),
        assert_enum_set(e[1]),
        assert(
          e[0].enum_type === e[1].enum_type,
          "two enum-sets must be the same enum-type",
          "enum-set-union"
        ),
        e[0].union(e[1])
      );
    }),
    define_libfunc("enum-set-intersection", 2, 2, function (e) {
      return (
        assert_enum_set(e[0]), assert_enum_set(e[1]), e[0].intersection(e[1])
      );
    }),
    define_libfunc("enum-set-difference", 2, 2, function (e) {
      return (
        assert_enum_set(e[0]), assert_enum_set(e[1]), e[0].difference(e[1])
      );
    }),
    define_libfunc("enum-set-complement", 1, 1, function (e) {
      return assert_enum_set(e[0]), e[0].complement();
    }),
    define_libfunc("enum-set-projection", 2, 2, function (e) {
      return (
        assert_enum_set(e[0]), assert_enum_set(e[1]), e[0].projection(e[1])
      );
    }),
    define_syntax("define-enumeration", function (e) {
      var t = e.cdr.car;
      assert(
        isSymbol(t),
        "expected symbol for type_name",
        "define-enumeration"
      ),
        (t = t.name);
      var n = e.cdr.cdr.car;
      assert(
        isList(n),
        "expected list of symbol for members",
        "define-enumeration"
      ),
        (n = n.to_array());
      var r = e.cdr.cdr.cdr.car;
      assert(
        isSymbol(r),
        "expected symbol for constructor_name",
        "define-enumeration"
      ),
        (r = r.name);
      var i = new Enumeration.EnumType(n);
      define_syntax(t, function (e) {
        assert(!isNil(e.cdr), "an argument is needed", t);
        var n = e.cdr.car;
        return (
          assert_symbol(n, t),
          assert(
            i.members.includes(n),
            n.name +
              " is not included in the universe: " +
              to_write$1(i.members),
            t
          ),
          List(Sym("quote"), n)
        );
      }),
        define_syntax(r, function (e) {
          assert_list(e.cdr, r);
          var t = e.cdr.to_array();
          return (
            t.forEach(function (e) {
              assert_symbol(e, r),
                assert(
                  i.members.includes(e),
                  e.name +
                    " is not included in the universe: " +
                    to_write$1(i.members),
                  r
                );
            }),
            new Enumeration.EnumSet(i, t)
          );
        });
    }),
    define_libfunc("eval", 1, 1, function (e, t) {
      var n = e[0];
      return new Interpreter(t).evaluate(to_write$1(n));
    }),
    define_syntax("delay", function (e) {
      if (e.cdr === nil) throw new BiwaError("malformed delay: no argument");
      if (e.cdr.cdr !== nil)
        throw new BiwaError(
          "malformed delay: too many arguments: " + write_ss(e)
        );
      var t = e.cdr.car;
      return new Pair(
        Sym(" procedure->promise"),
        new Pair(
          new Pair(
            Sym("lambda"),
            new Pair(
              nil,
              new Pair(new Pair(Sym("make-promise"), new Pair(t, nil)), nil)
            )
          )
        )
      );
    }),
    define_syntax("delay-force", function (e) {
      if (e.cdr === nil)
        throw new BiwaError("malformed delay-force: no argument");
      if (e.cdr.cdr !== nil)
        throw new BiwaError(
          "malformed delay-force: too many arguments: " + write_ss(e)
        );
      var t = e.cdr.car;
      return new Pair(
        Sym(" procedure->promise"),
        new Pair(new Pair(Sym("lambda"), new Pair(nil, new Pair(t, nil))), nil)
      );
    });
  var force = function (e) {
    return e.is_done()
      ? e.value()
      : new Call(e.thunk(), [], function (t) {
          assert_promise(t[0]);
          var n = t[0];
          return e.is_done() ? e.value() : (e.update_with(n), force(n));
        });
  };
  define_libfunc("force", 1, 1, function (e, t) {
    return assert_promise(e[0]), force(e[0]);
  }),
    define_libfunc("promise?", 1, 1, function (e, t) {
      return e[0] instanceof BiwaPromise;
    }),
    define_libfunc("make-promise", 1, 1, function (e, t) {
      var n = e[0];
      return n instanceof BiwaPromise ? n : BiwaPromise.done(n);
    }),
    define_libfunc(" procedure->promise", 1, 1, function (e, t) {
      return assert_procedure(e[0]), BiwaPromise.fresh(e[0]);
    }),
    define_libfunc("make-parameter", 1, 2, function (e, t) {
      let n;
      const r = e[1],
        i = function (e) {
          if (0 == e.length) return n;
          {
            const t = n;
            return r
              ? new Call(r, [e[0]], (e) => ((n = e[0]), t))
              : ((n = e[0]), t);
          }
        };
      if (r) return new Call(r, [e[0]], (e) => ((n = e), i));
      {
        const t = e[0];
        return (n = t), i;
      }
    }),
    define_syntax("parameterize", function (e) {
      const t = e.cdr.car.to_array(),
        n = e.cdr.cdr,
        r = t.map(() => gensym()),
        i = List(...t.map((e, t) => List(r[t], e.cdr.car))),
        a = Cons(
          Sym("begin"),
          List(...t.map((e, t) => List(Sym("set!"), r[t], List(e.car, r[t]))))
        ),
        s = List(Sym("lambda"), nil, a),
        o = Cons(Sym("lambda"), Cons(nil, n)),
        u = List(Sym("lambda"), nil, a);
      return List(Sym("let"), i, List(Sym("dynamic-wind"), s, o, u));
    }),
    define_libfunc("iota", 1, 3, function (e) {
      var t = e[0],
        n = e[1] || 0,
        r = void 0 === e[2] ? 1 : e[2];
      assert_integer(t), assert_number(n), assert_number(r);
      for (var i = [], a = n, s = 0; s < t; s++) i.push(a), (a += r);
      return array_to_list(i);
    });
  var copy_pair = function (e) {
    var t = isPair(e.car) ? copy_pair(e.car) : e.car,
      n = isPair(e.cdr) ? copy_pair(e.cdr) : e.cdr;
    return new Pair(t, n);
  };
  define_libfunc("list-copy", 1, 1, function (e) {
    return isPair(e[0]) ? copy_pair(e[0]) : nil;
  }),
    define_libfunc("open-input-string", 1, 1, function (e) {
      return assert_string(e[0]), new Port.StringInput(e[0]);
    }),
    define_libfunc("open-output-string", 0, 0, function (e) {
      return new Port.StringOutput();
    }),
    define_libfunc("get-output-string", 1, 1, function (e) {
      if ((assert_port(e[0]), !(e[0] instanceof Port.StringOutput)))
        throw new Error(
          "get-output-string: port must be made by 'open-output-string'"
        );
      return e[0].output_string();
    }),
    define_syntax("receive", function (e) {
      assert(isPair(e.cdr), "missing formals", "receive");
      var t = e.cdr.car;
      assert(isPair(e.cdr.cdr), "missing expression", "receive");
      var n = e.cdr.cdr.car,
        r = e.cdr.cdr.cdr;
      return deep_array_to_list([
        Sym("call-with-values"),
        [Sym("lambda"), nil, n],
        new Pair(Sym("lambda"), new Pair(t, r)),
      ]);
    }),
    define_libfunc("current-date", 0, 1, function (e) {
      return new Date();
    }),
    define_libfunc("date?", 1, 1, function (e) {
      return e[0] instanceof Date;
    }),
    define_libfunc("date-nanosecond", 1, 1, function (e) {
      return assert_date(e[0]), 1e6 * e[0].getMilliseconds();
    }),
    define_libfunc("date-millisecond", 1, 1, function (e) {
      return assert_date(e[0]), e[0].getMilliseconds();
    }),
    define_libfunc("date-second", 1, 1, function (e) {
      return assert_date(e[0]), e[0].getSeconds();
    }),
    define_libfunc("date-minute", 1, 1, function (e) {
      return assert_date(e[0]), e[0].getMinutes();
    }),
    define_libfunc("date-hour", 1, 1, function (e) {
      return assert_date(e[0]), e[0].getHours();
    }),
    define_libfunc("date-day", 1, 1, function (e) {
      return assert_date(e[0]), e[0].getDate();
    }),
    define_libfunc("date-month", 1, 1, function (e) {
      return assert_date(e[0]), e[0].getMonth() + 1;
    }),
    define_libfunc("date-year", 1, 1, function (e) {
      return assert_date(e[0]), e[0].getFullYear();
    }),
    define_libfunc("date-week-day", 1, 1, function (e) {
      return assert_date(e[0]), e[0].getDay();
    });
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
    },
    date2string = function (e, t) {
      var n = function (e) {
          return e < 10 ? "0" + e : "" + e;
        },
        r = function (e) {
          return e < 10 ? " " + e : "" + e;
        },
        i = {
          a: function (e) {
            return date_names.weekday[e.getDay()];
          },
          A: function (e) {
            return date_names.full_weekday[e.getDay()];
          },
          b: function (e) {
            return date_names.month[e.getMonth()];
          },
          B: function (e) {
            return date_names.full_month[e.getMonth()];
          },
          c: function (e) {
            return e.toString();
          },
          d: function (e) {
            return n(e.getDate());
          },
          D: function (e) {
            return i.d(e) + i.m(e) + i.y(e);
          },
          e: function (e) {
            return r(e.getDate());
          },
          f: function (e) {
            return e.getSeconds() + e.getMilliseconds() / 1e3;
          },
          h: function (e) {
            return date_names.month[e.getMonth()];
          },
          H: function (e) {
            return n(e.getHours());
          },
          I: function (e) {
            var t = e.getHours();
            return n(t < 13 ? t : t - 12);
          },
          j: function (e) {
            throw new Bug("not implemented: day of year");
          },
          k: function (e) {
            return r(e.getHours());
          },
          l: function (e) {
            var t = e.getHours();
            return r(t < 13 ? t : t - 12);
          },
          m: function (e) {
            return n(e.getMonth() + 1);
          },
          M: function (e) {
            return n(e.getMinutes());
          },
          n: function (e) {
            return "\n";
          },
          N: function (e) {
            throw new Bug("not implemented: nanoseconds");
          },
          p: function (e) {
            return e.getHours() < 13 ? "AM" : "PM";
          },
          r: function (e) {
            return i.I(e) + ":" + i.M(e) + ":" + i.S(e) + " " + i.p(e);
          },
          s: function (e) {
            return Math.floor(e.getTime() / 1e3);
          },
          S: function (e) {
            return n(e.getSeconds());
          },
          t: function (e) {
            return "\t";
          },
          T: function (e) {
            return i.H(e) + ":" + i.M(e) + ":" + i.S(e);
          },
          U: function (e) {
            throw new Bug("not implemented: weeknum(0~, Sun)");
          },
          V: function (e) {
            return (function (e) {
              var t = new Date(e.getFullYear(), 0, 4),
                n = new Date(e.getFullYear(), 0, 4);
              return (
                t.getDay() >= date_names.weekday.indexOf("Thu")
                  ? n.setDate(t.getDate() - (t.getDay() + 1))
                  : n.setDate(t.getDate() + (7 - t.getDay() + 1)),
                Math.ceil((e - n) / 864e5 / 7)
              );
            })(e);
          },
          w: function (e) {
            return e.getDay();
          },
          W: function (e) {
            throw new Bug("not implemented: weeknum(0~, Mon)");
          },
          x: function (e) {
            throw new Bug("not implemented: weeknum(1~, Mon)");
          },
          X: function (e) {
            return i.Y(e) + "/" + i.m(e) + "/" + i.d(e);
          },
          y: function (e) {
            return e.getFullYear() % 100;
          },
          Y: function (e) {
            return e.getFullYear();
          },
          z: function (e) {
            throw new Bug("not implemented: time-zone");
          },
          Z: function (e) {
            throw new Bug("not implemented: symbol time zone");
          },
          1: function (e) {
            throw new Bug("not implemented: ISO-8601 year-month-day format");
          },
          2: function (e) {
            throw new Bug(
              "not implemented: ISO-8601 hour-minute-second-timezone format"
            );
          },
          3: function (e) {
            throw new Bug(
              "not implemented: ISO-8601 hour-minute-second format"
            );
          },
          4: function (e) {
            throw new Bug(
              "not implemented: ISO-8601 year-month-day-hour-minute-second-timezone format"
            );
          },
          5: function (e) {
            throw new Bug(
              "not implemented: ISO-8601 year-month-day-hour-minute-second format"
            );
          },
        };
      return t.replace(/~([\w1-5~])/g, function (t, n) {
        var r = i[n];
        return r ? r(e) : "~" == n ? "~" : n;
      });
    };
  define_libfunc("date->string", 1, 2, function (e) {
    return (
      assert_date(e[0]),
      e[1] ? (assert_string(e[1]), date2string(e[0], e[1])) : e[0].toString()
    );
  }),
    define_libfunc("parse-date", 1, 1, function (e) {
      return assert_string(e[0]), new Date(Date.parse(e[0]));
    }),
    define_libfunc("random-integer", 1, 1, function (e) {
      var t = e[0];
      if ((assert_integer(t), t < 0))
        throw new Error("random-integer: the argument must be >= 0");
      return Math.floor(Math.random() * e[0]);
    }),
    define_libfunc("random-real", 0, 0, function (e) {
      return Math.random();
    }),
    define_libfunc("format", 1, null, function (e) {
      if ("string" == typeof e[0])
        var t = null,
          n = e.shift();
      else if (!1 === e[0]) {
        e.shift();
        (t = null), (n = e.shift());
      } else if (!0 === e[0]) {
        e.shift();
        (t = Port.current_output), (n = e.shift());
      } else {
        (t = e.shift()), (n = e.shift());
        assert_port(t);
      }
      var r = n
        .replace(/~[as]/g, function (t) {
          return (
            assert(e.length > 0, "insufficient number of arguments", "format"),
            "~a" == t ? to_display(e.shift()) : to_write$1(e.shift())
          );
        })
        .replace(/~%/, "\n")
        .replace(/~~/, "~");
      return t ? (t.put_string(r), undef) : r;
    });
  const user_write_ss = function (e) {
    return Console.puts(write_shared(e[0]), !0), undef;
  };
  define_libfunc("write/ss", 1, 2, user_write_ss),
    define_libfunc("write-with-shared-structure", 1, 2, user_write_ss),
    define_libfunc("write*", 1, 2, user_write_ss),
    define_libfunc("vector-append", 2, null, function (e) {
      var t = [];
      return t.concat.apply(t, e);
    }),
    define_libfunc("vector-copy", 1, 1, function (e) {
      return assert_vector(e[0]), [...e[0]];
    });
  var BiwaScheme$1 = {
    TopEnv: TopEnv,
    CoreEnv: CoreEnv,
    nil: nil,
    undef: undef,
    max_trace_size: max_trace_size,
    suppress_deprecation_warning: suppress_deprecation_warning,
    Version: VERSION,
    VERSION: VERSION,
    GitCommit: GitCommit,
    eq: eq,
    eqv: eqv,
    equal: equal,
    isNil: isNil,
    isUndef: isUndef,
    isBoolean: isBoolean,
    isString: isString,
    isSymbol: isSymbol,
    isPort: isPort,
    isPair: isPair,
    isList: isList,
    isVector: isVector,
    isHashtable: isHashtable,
    isMutableHashtable: isMutableHashtable,
    isProcedure: isProcedure,
    lt: lt,
    to_write: to_write$1,
    to_display: to_display,
    inspect: inspect,
    write_ss: write_shared,
    to_write_ss: write_shared,
    Call: Call,
    Char: Char,
    isChar: isChar,
    Closure: Closure,
    isClosure: isClosure,
    Compiler: Compiler,
    Enumeration: Enumeration,
    isEnumSet: isEnumSet,
    Error: BiwaError,
    Bug: Bug,
    UserError: UserError,
    Hashtable: Hashtable,
    Interpreter: Interpreter,
    Complex: Complex,
    Rational: Rational$1,
    isNumber: isNumber,
    isComplex: isComplex,
    isReal: isReal,
    isRational: isRational,
    isInteger: isInteger,
    Pair: Pair,
    List: List,
    array_to_list: array_to_list,
    deep_array_to_list: deep_array_to_list,
    Cons: Cons,
    Parser: Parser,
    Pause: Pause,
    Port: Port,
    eof: eof,
    Promise: BiwaPromise,
    isPromise: isPromise,
    Record: Record,
    isRecord: isRecord,
    isRecordTD: isRecordTD,
    isRecordCD: isRecordCD,
    Set: BiwaSet,
    Symbol: BiwaSymbol,
    Sym: Sym,
    gensym: gensym,
    Syntax: Syntax,
    Values: Values$1,
    VMCode: VMCode,
    define_libfunc: define_libfunc,
    define_scmfunc: define_scmfunc,
    parse_fraction: parse_fraction,
    is_valid_integer_notation: is_valid_integer_notation,
    parse_integer: parse_integer,
    is_valid_float_notation: is_valid_float_notation,
    parse_float: parse_float,
    assert_number: assert_number,
    assert_integer: assert_integer,
    assert_real: assert_real,
    assert_between: assert_between,
    assert_string: assert_string,
    assert_char: assert_char,
    assert_symbol: assert_symbol,
    assert_port: assert_port,
    assert_pair: assert_pair,
    assert_list: assert_list,
    assert_vector: assert_vector,
    assert_hashtable: assert_hashtable,
    assert_mutable_hashtable: assert_mutable_hashtable,
    assert_promise: assert_promise,
    assert_function: assert_function,
    assert_closure: assert_closure,
    assert_procedure: assert_procedure,
    assert_date: assert_date,
    assert: assert,
  };
  (Console.puts = function (e, t) {
    Port.current_output.put_string(e + (t ? "" : "\n"));
  }),
    (Console.p = function () {
      Port.current_output.put_string(
        "p> " + Array.arrayFrom(arguments).map(inspect).join(" ")
      );
    });
  const current_input = new Port.CustomInput(function (e) {
      const t = document.querySelector("#bs-console"),
        n = document.createElement("form");
      (n.innerHTML =
        "<input id='webscheme-read-line' type='text'><input type='submit' value='ok'>"),
        t.appendChild(n),
        n.addEventListener("submit", function () {
          const t = document.querySelector("#webscheme-read-line").value;
          return n.remove(), e(t), !1;
        });
    }),
    current_output = new Port.CustomOutput(function (e) {
      const t = document.querySelector("#bs-console");
      if (!t) return;
      const n = document.createElement("span");
      (n.innerHTML = escape(e).replace(/\n/g, "<br>").replace(/ /g, "&nbsp;")),
        t.appendChild(n);
    }),
    current_error = current_output,
    $$1 = window.jQuery;
  define_libfunc("read-line", 0, 1, function (e) {
    var t = e[0] || Port.current_input;
    return assert_port(t), t.get_string((e) => e);
  }),
    define_libfunc("element-empty!", 1, 1, function (e) {
      return $$1(e[0]).prop("value") ? $$1(e[0]).val("") : $$1(e[0]).empty();
    }),
    alias_libfunc("element-empty!", "element-clear!"),
    define_libfunc("element-visible?", 1, 1, function (e) {
      return $$1(e[0]).is(":visible");
    }),
    define_libfunc("element-toggle!", 1, 1, function (e) {
      return $$1(e[0]).toggle();
    }),
    define_libfunc("element-hide!", 1, 1, function (e) {
      return $$1(e[0]).hide();
    }),
    define_libfunc("element-show!", 1, 1, function (e) {
      return $$1(e[0]).show();
    }),
    define_libfunc("element-remove!", 1, 1, function (e) {
      return $$1(e[0]).remove();
    }),
    define_libfunc("element-update!", 2, 2, function (e) {
      return $$1(e[0]).html(e[1]);
    }),
    define_libfunc("element-replace!", 2, 2, function (e) {
      return $$1(e[0]).replaceWith(e[1]);
    }),
    define_libfunc("element-insert!", 2, 2, function (e) {
      return $$1(e[0]).append(e[1]);
    }),
    define_libfunc("element-wrap!", 3, 3, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-ancestors", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-descendants", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-first-descendant", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-immediate-descendants", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-previous-sibling", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-next-sibling", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-siblings", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-match?", 2, 2, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-up", 3, 3, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-down", 3, 3, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-previous", 3, 3, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-next", 3, 3, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-select", 1, 1, function (e) {
      $$1(e[0]).select();
    }),
    define_libfunc("element-adjacent", 0, 0, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-identify", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-read-attribute", 2, 2, function (e) {
      return (
        assert_string(e[1]),
        e[1].startsWith("data-") ? $$1(e[0]).attr(e[1]) : $$1(e[0]).prop(e[1])
      );
    });
  var element_write_attribute = function (e) {
    return (
      assert_string(e[1]),
      e[1].startsWith("data-")
        ? $$1(e[0]).attr(e[1], e[2])
        : $$1(e[0]).prop(e[1], e[2])
    );
  };
  define_libfunc("element-write-attribute", 3, 3, function (e) {
    return (
      deprecate("element-write-attribute", "1.0", "element-write-attribute!"),
      element_write_attribute(e)
    );
  }),
    define_libfunc("element-write-attribute!", 3, 3, element_write_attribute),
    define_libfunc("element-height", 1, 1, function (e) {
      return $$1(e[0]).height();
    }),
    define_libfunc("element-width", 1, 1, function (e) {
      return $$1(e[0]).width();
    }),
    define_libfunc("element-class-names", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-has-class-name?", 2, 2, function (e) {
      return assert_string(e[1]), $$1(e[0]).hasClass(e[1]);
    });
  var element_add_class_name = function (e) {
    return assert_string(e[1]), $$1(e[0]).addClass(e[1]);
  };
  define_libfunc("element-add-class-name", 2, 2, function (e) {
    return (
      deprecate("element-add-class-name", "1.0", "element-add-class-name!"),
      element_add_class_name(e)
    );
  }),
    define_libfunc("element-add-class-name!", 2, 2, element_add_class_name);
  var element_remove_class_name = function (e) {
    return assert_string(e[1]), $$1(e[0]).removeClass(e[1]);
  };
  define_libfunc("element-remove-class-name", 2, 2, function (e) {
    return (
      deprecate(
        "element-remove-class-name",
        "1.0",
        "element-remove-class-name!"
      ),
      element_remove_class_name(e)
    );
  }),
    define_libfunc(
      "element-remove-class-name!",
      2,
      2,
      element_remove_class_name
    );
  var element_toggle_class_name = function (e) {
    return assert_string(e[1]), $$1(e[0]).toggleClass(e[1]);
  };
  define_libfunc("element-toggle-class-name", 2, 2, function (e) {
    return (
      deprecate(
        "element-toggle-class-name",
        "1.0",
        "element-toggle-class-name!"
      ),
      element_toggle_class_name(e)
    );
  }),
    define_libfunc(
      "element-toggle-class-name!",
      2,
      2,
      element_toggle_class_name
    ),
    define_libfunc("element-clean-whitespace!", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-empty?", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-descendant-of!", 2, 2, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("scroll-to-element!", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-style", 2, 2, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-opacity", 2, 2, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-style-set!", 2, 2, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-opacity-set!", 2, 2, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-dimensions", 1, 1, function (e) {
      return new Values($$1(e[0]).width(), $$1(e[0]).height());
    }),
    define_libfunc("element-make-positioned!", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-undo-positioned!", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-make-clipping!", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-undo-clipping!", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-cumulative-offset", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-positioned-offset", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-absolutize!", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-relativize!", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-cumulative-scroll-offset", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-offset-parent", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-viewport-offset", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-clone-position!", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-absolutize!", 1, 1, function (e) {
      throw new Bug("not yet implemented");
    }),
    define_libfunc("element-focus!", 1, 1, function (e) {
      return $$1(e[0]).focus();
    });
  const create_elements_by_string = function (e) {
      var t = (e = e.to_array()).shift();
      t instanceof BiwaSymbol && (t = t.name);
      var n = t.match(/(.*)\.(.*)/);
      n && ((t = n[1]), e.unshift(Sym("class"), n[2])),
        (n = t.match(/(.*)\#(.*)/)) && ((t = n[1]), e.unshift(Sym("id"), n[2]));
      for (var r = [], i = ["<" + t], a = 0; a < e.length; a++)
        e[a] instanceof BiwaSymbol
          ? (i.push(" " + e[a].name + '="' + e[a + 1] + '"'), a++)
          : e[a] instanceof Pair
          ? r.push(create_elements_by_string(e[a]))
          : r.push(e[a]);
      return (
        i.push(">"), i.push(r.join("")), i.push("</" + t + ">"), i.join("")
      );
    },
    tree_all = function (e, t) {
      return e === nil || (!1 !== t(e.car) && tree_all(e.cdr, t));
    };
  define_libfunc("element-new", 1, 1, function (e) {
    return tree_all(e[0], function (e) {
      return (
        "string" == typeof e || e instanceof BiwaSymbol || e instanceof Pair
      );
    })
      ? $$1(create_elements_by_string(e[0]))[0]
      : nil;
  });
  const element_content = function (e) {
    return $$1(e).prop("value") ? $$1(e).val() : escape($$1(e).html());
  };
  define_libfunc("element-content", 1, 1, function (e) {
    return element_content(e[0]);
  }),
    define_libfunc("load", 1, 1, function (e, t) {
      var n = e[0];
      assert_string(n);
      var r = new Interpreter(t);
      return new Pause(function (e) {
        $$1.ajax(n, {
          dataType: "text",
          mimeType: "text/plain; charset=UTF-8",
          success: function (t) {
            r.evaluate(t, function () {
              return e.resume(undef);
            });
          },
          error: function () {
            throw new Error("load: network error: failed to load " + n);
          },
        });
      });
    });
  const _require = function (e, t, n) {
    var r = $$1("<script/>", { src: e });
    $$1("body").append(r);
    var i = new Function("return !!(" + t + ")");
    i()
      ? n()
      : setTimeout(function () {
          i() ? n() : setTimeout(arguments.callee, 10);
        }, 10);
  };
  define_libfunc("js-load", 2, 2, function (e) {
    var t = e[0],
      n = e[1];
    return (
      assert_string(t),
      assert_string(n),
      new Pause(function (e) {
        _require(t, "window." + n, function () {
          e.resume(undef);
        });
      })
    );
  });
  const getelem = function (e) {
    e.length > 1 && !1 === e[1] && (e[1] = []);
    var t = $$1.apply(this, e);
    return t.length > 0 && t;
  };
  define_libfunc("$", 1, 2, getelem),
    define_libfunc("getelem", 1, 2, getelem),
    define_libfunc("dom-element", 1, 1, function (e) {
      return $$1(e[0])[0];
    }),
    define_libfunc("set-style!", 3, 3, function (e) {
      return assert_string(e[1]), $$1(e[0]).css(e[1], e[2]), undef;
    }),
    define_libfunc("get-style", 2, 2, function (e) {
      return assert_string(e[1]), $$1(e[0]).css(e[1]);
    }),
    define_libfunc("set-content!", 2, 2, function (e) {
      assert_string(e[1]);
      var t = e[1].replace(/\n/g, "<br>").replace(/\t/g, "&nbsp;&nbsp;&nbsp;");
      return $$1(e[0]).html(t), undef;
    }),
    define_libfunc("get-content", 1, 1, function (e) {
      return element_content(e[0]);
    }),
    define_libfunc("set-handler!", 3, 3, function (e, t) {
      throw new Error(
        "set-handler! is obsolete, please use add-handler! instead"
      );
    }),
    define_libfunc("add-handler!", 3, 3, function (e, t) {
      var n = e[0],
        r = e[1],
        i = e[2],
        a = function (e) {
          return new Interpreter(t).invoke_closure(i, [e]);
        };
      return $$1(n).on(r, a), a;
    }),
    define_libfunc("remove-handler!", 3, 3, function (e, t) {
      var n = e[0],
        r = e[1],
        i = e[2];
      return $$1(n).off(r, i), undef;
    }),
    define_libfunc("wait-for", 2, 2, function (e) {
      var t = e[0],
        n = e[1],
        r = $$1(t);
      r.biwascheme_wait_for = r.biwascheme_wait_for || {};
      var i = r.biwascheme_wait_for[n];
      return (
        i && r.off(n, i),
        new Pause(function (e) {
          var t = function (i) {
            return (
              (r.biwascheme_wait_for[n] = void 0), r.off(n, t), e.resume(i)
            );
          };
          (r.biwascheme_wait_for[n] = t), r.on(n, t);
        })
      );
    }),
    define_libfunc("domelem", 1, null, function (e) {
      throw new Error("obsolete");
    }),
    define_libfunc("dom-remove-children!", 1, 1, function (e) {
      return (
        Console.puts(
          "warning: dom-remove-children! is obsolete. use element-empty! instead"
        ),
        $$1(e[0]).empty(),
        undef
      );
    }),
    define_libfunc("dom-create-element", 1, 1, function (e) {
      throw new Error("obsolete");
    }),
    define_libfunc("element-append-child!", 2, 2, function (e) {
      return $$1(e[0]).append(e[1]);
    }),
    define_libfunc("dom-remove-child!", 2, 2, function (e) {
      throw new Error("obsolete");
    }),
    define_libfunc("http-request", 1, 1, function (e) {
      var t = e[0];
      return (
        assert_string(t),
        new Pause(function (e) {
          $$1.get(
            t,
            function (t) {
              e.resume(t);
            },
            "text"
          );
        })
      );
    }),
    define_libfunc("http-post", 2, 2, function (e) {
      var t = e[0];
      assert_string(t);
      var n = e[1];
      assert_list(n);
      var r = alist_to_js_obj(n);
      return new Pause(function (e) {
        $$1.post(
          t,
          r,
          function (t) {
            e.resume(t);
          },
          "text"
        );
      });
    });
  const jsonp_receiver = [];
  define_libfunc("receive-jsonp", 1, 1, function (e) {
    var t = e[0];
    assert_string(t);
    for (var n = jsonp_receiver, r = 0; r < n.length && null !== n[r]; r++);
    var i = r;
    return (
      (t += "?callback=BiwaScheme.jsonp_receiver[" + i + "]"),
      new Pause(function (e) {
        n[i] = function (t) {
          e.resume(t), (n[i] = null);
        };
        var r = $$1("<script/>", { src: t });
        $$1("body").append(r);
      })
    );
  }),
    define_libfunc("alert", 1, 1, function (e) {
      return alert(e[0]), undef;
    }),
    define_libfunc("confirm", 1, 1, function (e) {
      return confirm(e[0]);
    });
  const DUMP_PAD = "&nbsp;&nbsp;&nbsp;",
    FOLD_LIMIT = 20,
    STACK_MAX_LEN = 80;
  class Dumper {
    constructor(e) {
      (this.dumparea = e || $("#dumparea")[0] || null), this.reset();
    }
    reset() {
      this.dumparea && $(this.dumparea).empty(),
        (this.n_folds = 0),
        (this.closures = []),
        (this.n_dumps = 0),
        (this.cur = -1),
        (this.is_folded = !0);
    }
    is_opc(e) {
      return e instanceof Array && "string" == typeof e[0];
    }
    dump_opc(e, t, n) {
      var r = "",
        i = "",
        a = "";
      (t = t || 0), (n = n || !1);
      Array(t)
        .fill()
        .map(() => {
          i += DUMP_PAD;
        }),
        Array(t + 1)
          .fill()
          .map(() => {
            a += DUMP_PAD;
          }),
        (r += i + '[<span class="dump_opecode">' + e[0] + "</span>");
      for (var s = 1; !(e[s] instanceof Array) && s < e.length; )
        "constant" == e[0]
          ? (r +=
              "&nbsp;<span class='dump_constant'>" +
              this.dump_obj(e[s]) +
              "</span>")
          : (r += "&nbsp;" + this.dump_obj(e[s])),
          s++;
      for (s < e.length && (r += "<br>\n"); s < e.length; s++)
        this.is_opc(e[s])
          ? (r += this.dump_opc(e[s], s == e.length - 1 ? t : t + 1, !0))
          : ((r += s == e.length - 1 ? i : a), (r += this.dump_obj(e[s]))),
          s != e.length - 1 && (r += "<br>\n");
      return (r += "]"), n ? r : this.add_fold(r);
    }
    add_fold(e) {
      var t = e.split(/<br>/gim);
      if (t.length > FOLD_LIMIT) {
        var n =
            " <span style='text-decoration:underline; color:blue; cursor:pointer;'onclick='BiwaScheme.Dumper.toggle_fold(" +
            this.n_folds +
            ")'>more</span>",
          r = "<div style='display:none' class='fold" + this.n_folds + "'>";
        return (
          this.n_folds++,
          [
            t.slice(0, FOLD_LIMIT).join("<br>"),
            n,
            r,
            t.slice(FOLD_LIMIT).join("<br>"),
            "</div>",
          ].join("")
        );
      }
      return e;
    }
    dump_stack(e, t) {
      if (null == e) return inspect(e);
      var n = "<table>";
      if (0 == e.length)
        n += "<tr><td class='dump_dead'>(stack is empty)</td></tr>";
      else if (t < e.length) {
        var r = e.length - 1;
        n +=
          "<tr><td class='dump_dead'>[" +
          r +
          "]</td><td class='dump_dead'>" +
          truncate(this.dump_obj(e[r]), STACK_MAX_LEN) +
          "</td></tr>";
      }
      for (var i = t - 1; i >= 0; i--)
        n +=
          "<tr><td class='dump_stknum'>[" +
          i +
          "]</td><td>" +
          truncate(this.dump_obj(e[i]), STACK_MAX_LEN) +
          "</td></tr>";
      return n + "</table>";
    }
    dump_object(e) {
      var t = [];
      for (var n in e) t.push(n.toString());
      return "#<Object{" + t.join(",") + "}>";
    }
    dump_closure(e) {
      if (!e) return "**BROKEN**";
      if (0 == e.length) return "[]";
      for (var t = null, n = 0; n < this.closures.length; n++)
        this.closures[n] == e && (t = n);
      null == t && ((t = this.closures.length), this.closures.push(e));
      var r = [...e],
        i = r.shift && r.shift();
      return [
        "c",
        t,
        " <span class='dump_closure'>free vars :</span> ",
        this.dump_obj(r),
        " <span class='dump_closure'>body :</span> ",
        truncate(this.dump_obj(i), 100),
      ].join("");
    }
    dump_obj(e) {
      if (e && "function" == typeof e.to_html) return e.to_html();
      var t = to_write$1(e);
      return "[object Object]" == t && (t = this.dump_object(e)), escape(t);
    }
    dump(e) {
      var t = "";
      e instanceof Object
        ? ((t += "<table>"),
          (t +=
            "<tr><td colspan='4'><a href='#' class='header'>#" +
            this.n_dumps +
            "</a></td></tr>"),
          Object.keys(e).forEach(
            function (n) {
              var r = e[n];
              "x" != n &&
                "stack" != n &&
                ((r = "c" == n ? this.dump_closure(r) : this.dump_obj(r)),
                (t +=
                  "<tr><td>" +
                  n +
                  ": </td><td colspan='3'>" +
                  r +
                  "</td></tr>"));
            }.bind(this)
          ),
          (t +=
            "<tr><td>x:</td><td>" +
            (this.is_opc(e.x) ? this.dump_opc(e.x) : this.dump_obj(e.x)) +
            "</td>"),
          (t +=
            "<td style='border-left: 1px solid black'>stack:</td><td>" +
            this.dump_stack(e.stack, e.s) +
            "</td></tr>"),
          (t += "</table>"))
        : (t = escape(inspect(e)) + "<br>\n");
      var n = $("<div/>", { class: "dump" + this.n_dumps });
      n.html(t),
        $(this.dumparea).append(n),
        function (e) {
          $(".header", this.dump_el(this.n_dumps)).click(
            function () {
              this.dump_move_to(e), this.dump_fold();
            }.bind(this)
          );
        }.bind(this)(this.n_dumps),
        n.hide(),
        this.n_dumps++;
    }
    dump_el(e) {
      return $(".dump" + e, this.dumparea);
    }
    dump_move_to(e) {
      e < 0 && (e = this.n_dumps + e),
        0 <= e &&
          e <= this.n_dumps &&
          (this.dump_el(this.cur).hide(),
          (this.cur = e),
          this.dump_el(this.cur).show());
    }
    dump_move(e) {
      0 <= this.cur && this.cur < this.n_dumps && this.dump_el(this.cur).hide(),
        0 <= this.cur + e && this.cur + e < this.n_dumps && (this.cur += e),
        this.dump_el(this.cur).show();
    }
    dump_fold() {
      for (var e = 0; e < this.n_dumps; e++)
        e != this.cur && this.dump_el(e).hide();
      this.is_folded = !0;
    }
    dump_unfold() {
      for (var e = 0; e < this.n_dumps; e++) this.dump_el(e).show();
      this.is_folded = !1;
    }
    dump_toggle_fold() {
      this.is_folded ? this.dump_unfold() : this.dump_fold();
    }
  }
  Dumper.toggle_fold = function (e) {
    $(".fold" + e, this.dumparea).toggle();
  };
  const execute_user_program = function () {
    let e = null;
    const t = document.querySelector("#biwascheme-debugger");
    t && (e = new Dumper(t));
    const n = function (t, n) {
        if ((BiwaScheme.Port.current_error.put_string(t.message + "\n"), e))
          e.dump(n), e.dump_move(1);
        else {
          if ("undefined" == typeof console || !console.error) throw t;
          console.error(t.message);
        }
      },
      r = function (e) {
        const t = new BiwaScheme.Interpreter(n);
        try {
          t.evaluate(e, function () {});
        } catch (e) {
          n(e);
        }
      };
    let i = "";
    for (const e of document.querySelectorAll("script[src$='biwascheme.js']"))
      i += e.innerHTML;
    for (const e of document.querySelectorAll(
      "script[src$='biwascheme-min.js']"
    ))
      i += e.innerHTML;
    i.length > 0 && r(i),
      window.addEventListener("DOMContentLoaded", function () {
        for (const e of document.querySelectorAll(
          "script[type='text/biwascheme']"
        ))
          r(e.innerHTML);
      });
  };
  return (
    (BiwaScheme$1.on_node = !1),
    (BiwaScheme$1.Console = Console),
    (BiwaScheme$1.Port.current_input = current_input),
    (BiwaScheme$1.Port.current_output = current_output),
    (BiwaScheme$1.Port.current_error = current_error),
    (BiwaScheme$1.jsonp_receiver = jsonp_receiver),
    (BiwaScheme$1.Dumper = Dumper),
    (window.BiwaScheme = window.BiwaScheme || {}),
    Object.assign(window.BiwaScheme, BiwaScheme$1),
    execute_user_program(),
    BiwaScheme$1
  );
})();
