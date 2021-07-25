/*
 * BiwaScheme 0.7.4 - R6RS/R7RS Scheme in JavaScript
 *
 * Copyright (c) 2007-2021 Yutaka HARA (http://www.biwascheme.org/)
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
    o = n.flat
      ? function (e) {
          return n.flat.call(e);
        }
      : function (e) {
          return n.concat.apply([], e);
        },
    u = n.push,
    a = n.indexOf,
    c = {},
    s = c.toString,
    f = c.hasOwnProperty,
    l = f.toString,
    d = l.call(Object),
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
    v = e.document,
    g = { type: !0, src: !0, nonce: !0, noModule: !0 };
  function y(e, t, n) {
    var r,
      i,
      o = (n = n || v).createElement("script");
    if (((o.text = e), t))
      for (r in g)
        (i = t[r] || (t.getAttribute && t.getAttribute(r))) &&
          o.setAttribute(r, i);
    n.head.appendChild(o).parentNode.removeChild(o);
  }
  function w(e) {
    return null == e
      ? e + ""
      : "object" == typeof e || "function" == typeof e
      ? c[s.call(e)] || "object"
      : typeof e;
  }
  var b = function (e, t) {
    return new b.fn.init(e, t);
  };
  function _(e) {
    var t = !!e && "length" in e && e.length,
      n = w(e);
    return (
      !p(e) &&
      !m(e) &&
      ("array" === n ||
        0 === t ||
        ("number" == typeof t && t > 0 && t - 1 in e))
    );
  }
  (b.fn = b.prototype = {
    jquery: "3.6.0",
    constructor: b,
    length: 0,
    toArray: function () {
      return i.call(this);
    },
    get: function (e) {
      return null == e ? i.call(this) : e < 0 ? this[e + this.length] : this[e];
    },
    pushStack: function (e) {
      var t = b.merge(this.constructor(), e);
      return (t.prevObject = this), t;
    },
    each: function (e) {
      return b.each(this, e);
    },
    map: function (e) {
      return this.pushStack(
        b.map(this, function (t, n) {
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
        b.grep(this, function (e, t) {
          return (t + 1) % 2;
        })
      );
    },
    odd: function () {
      return this.pushStack(
        b.grep(this, function (e, t) {
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
    push: u,
    sort: n.sort,
    splice: n.splice,
  }),
    (b.extend = b.fn.extend = function () {
      var e,
        t,
        n,
        r,
        i,
        o,
        u = arguments[0] || {},
        a = 1,
        c = arguments.length,
        s = !1;
      for (
        "boolean" == typeof u && ((s = u), (u = arguments[a] || {}), a++),
          "object" == typeof u || p(u) || (u = {}),
          a === c && ((u = this), a--);
        a < c;
        a++
      )
        if (null != (e = arguments[a]))
          for (t in e)
            (r = e[t]),
              "__proto__" !== t &&
                u !== r &&
                (s && r && (b.isPlainObject(r) || (i = Array.isArray(r)))
                  ? ((n = u[t]),
                    (o =
                      i && !Array.isArray(n)
                        ? []
                        : i || b.isPlainObject(n)
                        ? n
                        : {}),
                    (i = !1),
                    (u[t] = b.extend(s, o, r)))
                  : void 0 !== r && (u[t] = r));
      return u;
    }),
    b.extend({
      expando: "jQuery" + ("3.6.0" + Math.random()).replace(/\D/g, ""),
      isReady: !0,
      error: function (e) {
        throw new Error(e);
      },
      noop: function () {},
      isPlainObject: function (e) {
        var t, n;
        return (
          !(!e || "[object Object]" !== s.call(e)) &&
          (!(t = r(e)) ||
            ("function" ==
              typeof (n = f.call(t, "constructor") && t.constructor) &&
              l.call(n) === d))
        );
      },
      isEmptyObject: function (e) {
        var t;
        for (t in e) return !1;
        return !0;
      },
      globalEval: function (e, t, n) {
        y(e, { nonce: t && t.nonce }, n);
      },
      each: function (e, t) {
        var n,
          r = 0;
        if (_(e))
          for (n = e.length; r < n && !1 !== t.call(e[r], r, e[r]); r++);
        else for (r in e) if (!1 === t.call(e[r], r, e[r])) break;
        return e;
      },
      makeArray: function (e, t) {
        var n = t || [];
        return (
          null != e &&
            (_(Object(e))
              ? b.merge(n, "string" == typeof e ? [e] : e)
              : u.call(n, e)),
          n
        );
      },
      inArray: function (e, t, n) {
        return null == t ? -1 : a.call(t, e, n);
      },
      merge: function (e, t) {
        for (var n = +t.length, r = 0, i = e.length; r < n; r++) e[i++] = t[r];
        return (e.length = i), e;
      },
      grep: function (e, t, n) {
        for (var r = [], i = 0, o = e.length, u = !n; i < o; i++)
          !t(e[i], i) !== u && r.push(e[i]);
        return r;
      },
      map: function (e, t, n) {
        var r,
          i,
          u = 0,
          a = [];
        if (_(e))
          for (r = e.length; u < r; u++)
            null != (i = t(e[u], u, n)) && a.push(i);
        else for (u in e) null != (i = t(e[u], u, n)) && a.push(i);
        return o(a);
      },
      guid: 1,
      support: h,
    }),
    "function" == typeof Symbol && (b.fn[Symbol.iterator] = n[Symbol.iterator]),
    b.each(
      "Boolean Number String Function Array Date RegExp Object Error Symbol".split(
        " "
      ),
      function (e, t) {
        c["[object " + t + "]"] = t.toLowerCase();
      }
    );
  var x =
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
        o,
        u,
        a,
        c,
        s,
        f,
        l,
        d,
        h,
        p,
        m,
        v,
        g,
        y,
        w,
        b = "sizzle" + 1 * new Date(),
        _ = e.document,
        x = 0,
        k = 0,
        j = ce(),
        S = ce(),
        T = ce(),
        C = ce(),
        E = function (e, t) {
          return e === t && (l = !0), 0;
        },
        q = {}.hasOwnProperty,
        A = [],
        D = A.pop,
        N = A.push,
        O = A.push,
        M = A.slice,
        L = function (e, t) {
          for (var n = 0, r = e.length; n < r; n++) if (e[n] === t) return n;
          return -1;
        },
        R =
          "checked|selected|async|autofocus|autoplay|controls|defer|disabled|hidden|ismap|loop|multiple|open|readonly|required|scoped",
        I = "[\\x20\\t\\r\\n\\f]",
        H =
          "(?:\\\\[\\da-fA-F]{1,6}" +
          I +
          "?|\\\\[^\\r\\n\\f]|[\\w-]|[^\0-\\x7f])+",
        P =
          "\\[" +
          I +
          "*(" +
          H +
          ")(?:" +
          I +
          "*([*^$|!~]?=)" +
          I +
          "*(?:'((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\"|(" +
          H +
          "))|)" +
          I +
          "*\\]",
        z =
          ":(" +
          H +
          ")(?:\\((('((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\")|((?:\\\\.|[^\\\\()[\\]]|" +
          P +
          ")*)|.*)\\)|)",
        F = new RegExp(I + "+", "g"),
        B = new RegExp("^" + I + "+|((?:^|[^\\\\])(?:\\\\.)*)" + I + "+$", "g"),
        $ = new RegExp("^" + I + "*," + I + "*"),
        W = new RegExp("^" + I + "*([>+~]|" + I + ")" + I + "*"),
        U = new RegExp(I + "|>"),
        V = new RegExp(z),
        X = new RegExp("^" + H + "$"),
        Y = {
          ID: new RegExp("^#(" + H + ")"),
          CLASS: new RegExp("^\\.(" + H + ")"),
          TAG: new RegExp("^(" + H + "|[*])"),
          ATTR: new RegExp("^" + P),
          PSEUDO: new RegExp("^" + z),
          CHILD: new RegExp(
            "^:(only|first|last|nth|nth-last)-(child|of-type)(?:\\(" +
              I +
              "*(even|odd|(([+-]|)(\\d*)n|)" +
              I +
              "*(?:([+-]|)" +
              I +
              "*(\\d+)|))" +
              I +
              "*\\)|)",
            "i"
          ),
          bool: new RegExp("^(?:" + R + ")$", "i"),
          needsContext: new RegExp(
            "^" +
              I +
              "*[>+~]|:(even|odd|eq|gt|lt|nth|first|last)(?:\\(" +
              I +
              "*((?:-\\d)?\\d*)" +
              I +
              "*\\)|)(?=[^-]|$)",
            "i"
          ),
        },
        G = /HTML$/i,
        J = /^(?:input|select|textarea|button)$/i,
        Q = /^h\d$/i,
        K = /^[^{]+\{\s*\[native \w/,
        Z = /^(?:#([\w-]+)|(\w+)|\.([\w-]+))$/,
        ee = /[+~]/,
        te = new RegExp(
          "\\\\[\\da-fA-F]{1,6}" + I + "?|\\\\([^\\r\\n\\f])",
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
        oe = function () {
          d();
        },
        ue = be(
          function (e) {
            return !0 === e.disabled && "fieldset" === e.nodeName.toLowerCase();
          },
          { dir: "parentNode", next: "legend" }
        );
      try {
        O.apply((A = M.call(_.childNodes)), _.childNodes),
          A[_.childNodes.length].nodeType;
      } catch (e) {
        O = {
          apply: A.length
            ? function (e, t) {
                N.apply(e, M.call(t));
              }
            : function (e, t) {
                for (var n = e.length, r = 0; (e[n++] = t[r++]); );
                e.length = n - 1;
              },
        };
      }
      function ae(e, t, r, i) {
        var o,
          a,
          s,
          f,
          l,
          p,
          g,
          y = t && t.ownerDocument,
          _ = t ? t.nodeType : 9;
        if (
          ((r = r || []),
          "string" != typeof e || !e || (1 !== _ && 9 !== _ && 11 !== _))
        )
          return r;
        if (!i && (d(t), (t = t || h), m)) {
          if (11 !== _ && (l = Z.exec(e)))
            if ((o = l[1])) {
              if (9 === _) {
                if (!(s = t.getElementById(o))) return r;
                if (s.id === o) return r.push(s), r;
              } else if (
                y &&
                (s = y.getElementById(o)) &&
                w(t, s) &&
                s.id === o
              )
                return r.push(s), r;
            } else {
              if (l[2]) return O.apply(r, t.getElementsByTagName(e)), r;
              if (
                (o = l[3]) &&
                n.getElementsByClassName &&
                t.getElementsByClassName
              )
                return O.apply(r, t.getElementsByClassName(o)), r;
            }
          if (
            n.qsa &&
            !C[e + " "] &&
            (!v || !v.test(e)) &&
            (1 !== _ || "object" !== t.nodeName.toLowerCase())
          ) {
            if (((g = e), (y = t), 1 === _ && (U.test(e) || W.test(e)))) {
              for (
                ((y = (ee.test(e) && ge(t.parentNode)) || t) === t &&
                  n.scope) ||
                  ((f = t.getAttribute("id"))
                    ? (f = f.replace(re, ie))
                    : t.setAttribute("id", (f = b))),
                  a = (p = u(e)).length;
                a--;

              )
                p[a] = (f ? "#" + f : ":scope") + " " + we(p[a]);
              g = p.join(",");
            }
            try {
              return O.apply(r, y.querySelectorAll(g)), r;
            } catch (t) {
              C(e, !0);
            } finally {
              f === b && t.removeAttribute("id");
            }
          }
        }
        return c(e.replace(B, "$1"), t, r, i);
      }
      function ce() {
        var e = [];
        return function t(n, i) {
          return (
            e.push(n + " ") > r.cacheLength && delete t[e.shift()],
            (t[n + " "] = i)
          );
        };
      }
      function se(e) {
        return (e[b] = !0), e;
      }
      function fe(e) {
        var t = h.createElement("fieldset");
        try {
          return !!e(t);
        } catch (e) {
          return !1;
        } finally {
          t.parentNode && t.parentNode.removeChild(t), (t = null);
        }
      }
      function le(e, t) {
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
                : t.isDisabled === e || (t.isDisabled !== !e && ue(t) === e)
              : t.disabled === e
            : "label" in t && t.disabled === e;
        };
      }
      function ve(e) {
        return se(function (t) {
          return (
            (t = +t),
            se(function (n, r) {
              for (var i, o = e([], n.length, t), u = o.length; u--; )
                n[(i = o[u])] && (n[i] = !(r[i] = n[i]));
            })
          );
        });
      }
      function ge(e) {
        return e && void 0 !== e.getElementsByTagName && e;
      }
      for (t in ((n = ae.support = {}),
      (o = ae.isXML = function (e) {
        var t = e && e.namespaceURI,
          n = e && (e.ownerDocument || e).documentElement;
        return !G.test(t || (n && n.nodeName) || "HTML");
      }),
      (d = ae.setDocument = function (e) {
        var t,
          i,
          u = e ? e.ownerDocument || e : _;
        return u != h && 9 === u.nodeType && u.documentElement
          ? ((p = (h = u).documentElement),
            (m = !o(h)),
            _ != h &&
              (i = h.defaultView) &&
              i.top !== i &&
              (i.addEventListener
                ? i.addEventListener("unload", oe, !1)
                : i.attachEvent && i.attachEvent("onunload", oe)),
            (n.scope = fe(function (e) {
              return (
                p.appendChild(e).appendChild(h.createElement("div")),
                void 0 !== e.querySelectorAll &&
                  !e.querySelectorAll(":scope fieldset div").length
              );
            })),
            (n.attributes = fe(function (e) {
              return (e.className = "i"), !e.getAttribute("className");
            })),
            (n.getElementsByTagName = fe(function (e) {
              return (
                e.appendChild(h.createComment("")),
                !e.getElementsByTagName("*").length
              );
            })),
            (n.getElementsByClassName = K.test(h.getElementsByClassName)),
            (n.getById = fe(function (e) {
              return (
                (p.appendChild(e).id = b),
                !h.getElementsByName || !h.getElementsByName(b).length
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
                      o = t.getElementById(e);
                    if (o) {
                      if ((n = o.getAttributeNode("id")) && n.value === e)
                        return [o];
                      for (i = t.getElementsByName(e), r = 0; (o = i[r++]); )
                        if ((n = o.getAttributeNode("id")) && n.value === e)
                          return [o];
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
                    o = t.getElementsByTagName(e);
                  if ("*" === e) {
                    for (; (n = o[i++]); ) 1 === n.nodeType && r.push(n);
                    return r;
                  }
                  return o;
                }),
            (r.find.CLASS =
              n.getElementsByClassName &&
              function (e, t) {
                if (void 0 !== t.getElementsByClassName && m)
                  return t.getElementsByClassName(e);
              }),
            (g = []),
            (v = []),
            (n.qsa = K.test(h.querySelectorAll)) &&
              (fe(function (e) {
                var t;
                (p.appendChild(e).innerHTML =
                  "<a id='" +
                  b +
                  "'></a><select id='" +
                  b +
                  "-\r\\' msallowcapture=''><option selected=''></option></select>"),
                  e.querySelectorAll("[msallowcapture^='']").length &&
                    v.push("[*^$]=" + I + "*(?:''|\"\")"),
                  e.querySelectorAll("[selected]").length ||
                    v.push("\\[" + I + "*(?:value|" + R + ")"),
                  e.querySelectorAll("[id~=" + b + "-]").length || v.push("~="),
                  (t = h.createElement("input")).setAttribute("name", ""),
                  e.appendChild(t),
                  e.querySelectorAll("[name='']").length ||
                    v.push("\\[" + I + "*name" + I + "*=" + I + "*(?:''|\"\")"),
                  e.querySelectorAll(":checked").length || v.push(":checked"),
                  e.querySelectorAll("a#" + b + "+*").length ||
                    v.push(".#.+[+~]"),
                  e.querySelectorAll("\\\f"),
                  v.push("[\\r\\n\\f]");
              }),
              fe(function (e) {
                e.innerHTML =
                  "<a href='' disabled='disabled'></a><select disabled='disabled'><option/></select>";
                var t = h.createElement("input");
                t.setAttribute("type", "hidden"),
                  e.appendChild(t).setAttribute("name", "D"),
                  e.querySelectorAll("[name=d]").length &&
                    v.push("name" + I + "*[*^$|!~]?="),
                  2 !== e.querySelectorAll(":enabled").length &&
                    v.push(":enabled", ":disabled"),
                  (p.appendChild(e).disabled = !0),
                  2 !== e.querySelectorAll(":disabled").length &&
                    v.push(":enabled", ":disabled"),
                  e.querySelectorAll("*,:x"),
                  v.push(",.*:");
              })),
            (n.matchesSelector = K.test(
              (y =
                p.matches ||
                p.webkitMatchesSelector ||
                p.mozMatchesSelector ||
                p.oMatchesSelector ||
                p.msMatchesSelector)
            )) &&
              fe(function (e) {
                (n.disconnectedMatch = y.call(e, "*")),
                  y.call(e, "[s!='']:x"),
                  g.push("!=", z);
              }),
            (v = v.length && new RegExp(v.join("|"))),
            (g = g.length && new RegExp(g.join("|"))),
            (t = K.test(p.compareDocumentPosition)),
            (w =
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
            (E = t
              ? function (e, t) {
                  if (e === t) return (l = !0), 0;
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
                      ? e == h || (e.ownerDocument == _ && w(_, e))
                        ? -1
                        : t == h || (t.ownerDocument == _ && w(_, t))
                        ? 1
                        : f
                        ? L(f, e) - L(f, t)
                        : 0
                      : 4 & r
                      ? -1
                      : 1)
                  );
                }
              : function (e, t) {
                  if (e === t) return (l = !0), 0;
                  var n,
                    r = 0,
                    i = e.parentNode,
                    o = t.parentNode,
                    u = [e],
                    a = [t];
                  if (!i || !o)
                    return e == h
                      ? -1
                      : t == h
                      ? 1
                      : i
                      ? -1
                      : o
                      ? 1
                      : f
                      ? L(f, e) - L(f, t)
                      : 0;
                  if (i === o) return de(e, t);
                  for (n = e; (n = n.parentNode); ) u.unshift(n);
                  for (n = t; (n = n.parentNode); ) a.unshift(n);
                  for (; u[r] === a[r]; ) r++;
                  return r
                    ? de(u[r], a[r])
                    : u[r] == _
                    ? -1
                    : a[r] == _
                    ? 1
                    : 0;
                }),
            h)
          : h;
      }),
      (ae.matches = function (e, t) {
        return ae(e, null, null, t);
      }),
      (ae.matchesSelector = function (e, t) {
        if (
          (d(e),
          n.matchesSelector &&
            m &&
            !C[t + " "] &&
            (!g || !g.test(t)) &&
            (!v || !v.test(t)))
        )
          try {
            var r = y.call(e, t);
            if (
              r ||
              n.disconnectedMatch ||
              (e.document && 11 !== e.document.nodeType)
            )
              return r;
          } catch (e) {
            C(t, !0);
          }
        return ae(t, h, null, [e]).length > 0;
      }),
      (ae.contains = function (e, t) {
        return (e.ownerDocument || e) != h && d(e), w(e, t);
      }),
      (ae.attr = function (e, t) {
        (e.ownerDocument || e) != h && d(e);
        var i = r.attrHandle[t.toLowerCase()],
          o = i && q.call(r.attrHandle, t.toLowerCase()) ? i(e, t, !m) : void 0;
        return void 0 !== o
          ? o
          : n.attributes || !m
          ? e.getAttribute(t)
          : (o = e.getAttributeNode(t)) && o.specified
          ? o.value
          : null;
      }),
      (ae.escape = function (e) {
        return (e + "").replace(re, ie);
      }),
      (ae.error = function (e) {
        throw new Error("Syntax error, unrecognized expression: " + e);
      }),
      (ae.uniqueSort = function (e) {
        var t,
          r = [],
          i = 0,
          o = 0;
        if (
          ((l = !n.detectDuplicates),
          (f = !n.sortStable && e.slice(0)),
          e.sort(E),
          l)
        ) {
          for (; (t = e[o++]); ) t === e[o] && (i = r.push(o));
          for (; i--; ) e.splice(r[i], 1);
        }
        return (f = null), e;
      }),
      (i = ae.getText = function (e) {
        var t,
          n = "",
          r = 0,
          o = e.nodeType;
        if (o) {
          if (1 === o || 9 === o || 11 === o) {
            if ("string" == typeof e.textContent) return e.textContent;
            for (e = e.firstChild; e; e = e.nextSibling) n += i(e);
          } else if (3 === o || 4 === o) return e.nodeValue;
        } else for (; (t = e[r++]); ) n += i(t);
        return n;
      }),
      ((r = ae.selectors = {
        cacheLength: 50,
        createPseudo: se,
        match: Y,
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
                ? (e[3] || ae.error(e[0]),
                  (e[4] = +(e[4]
                    ? e[5] + (e[6] || 1)
                    : 2 * ("even" === e[3] || "odd" === e[3]))),
                  (e[5] = +(e[7] + e[8] || "odd" === e[3])))
                : e[3] && ae.error(e[0]),
              e
            );
          },
          PSEUDO: function (e) {
            var t,
              n = !e[6] && e[2];
            return Y.CHILD.test(e[0])
              ? null
              : (e[3]
                  ? (e[2] = e[4] || e[5] || "")
                  : n &&
                    V.test(n) &&
                    (t = u(n, !0)) &&
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
            var t = j[e + " "];
            return (
              t ||
              ((t = new RegExp("(^|" + I + ")" + e + "(" + I + "|$)")) &&
                j(e, function (e) {
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
              var i = ae.attr(r, e);
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
                      ? (" " + i.replace(F, " ") + " ").indexOf(n) > -1
                      : "|=" === t &&
                        (i === n || i.slice(0, n.length + 1) === n + "-"));
            };
          },
          CHILD: function (e, t, n, r, i) {
            var o = "nth" !== e.slice(0, 3),
              u = "last" !== e.slice(-4),
              a = "of-type" === t;
            return 1 === r && 0 === i
              ? function (e) {
                  return !!e.parentNode;
                }
              : function (t, n, c) {
                  var s,
                    f,
                    l,
                    d,
                    h,
                    p,
                    m = o !== u ? "nextSibling" : "previousSibling",
                    v = t.parentNode,
                    g = a && t.nodeName.toLowerCase(),
                    y = !c && !a,
                    w = !1;
                  if (v) {
                    if (o) {
                      for (; m; ) {
                        for (d = t; (d = d[m]); )
                          if (
                            a
                              ? d.nodeName.toLowerCase() === g
                              : 1 === d.nodeType
                          )
                            return !1;
                        p = m = "only" === e && !p && "nextSibling";
                      }
                      return !0;
                    }
                    if (((p = [u ? v.firstChild : v.lastChild]), u && y)) {
                      for (
                        w =
                          (h =
                            (s =
                              (f =
                                (l = (d = v)[b] || (d[b] = {}))[d.uniqueID] ||
                                (l[d.uniqueID] = {}))[e] || [])[0] === x &&
                            s[1]) && s[2],
                          d = h && v.childNodes[h];
                        (d = (++h && d && d[m]) || (w = h = 0) || p.pop());

                      )
                        if (1 === d.nodeType && ++w && d === t) {
                          f[e] = [x, h, w];
                          break;
                        }
                    } else if (
                      (y &&
                        (w = h =
                          (s =
                            (f =
                              (l = (d = t)[b] || (d[b] = {}))[d.uniqueID] ||
                              (l[d.uniqueID] = {}))[e] || [])[0] === x && s[1]),
                      !1 === w)
                    )
                      for (
                        ;
                        (d = (++h && d && d[m]) || (w = h = 0) || p.pop()) &&
                        ((a
                          ? d.nodeName.toLowerCase() !== g
                          : 1 !== d.nodeType) ||
                          !++w ||
                          (y &&
                            ((f =
                              (l = d[b] || (d[b] = {}))[d.uniqueID] ||
                              (l[d.uniqueID] = {}))[e] = [x, w]),
                          d !== t));

                      );
                    return (w -= i) === r || (w % r == 0 && w / r >= 0);
                  }
                };
          },
          PSEUDO: function (e, t) {
            var n,
              i =
                r.pseudos[e] ||
                r.setFilters[e.toLowerCase()] ||
                ae.error("unsupported pseudo: " + e);
            return i[b]
              ? i(t)
              : i.length > 1
              ? ((n = [e, e, "", t]),
                r.setFilters.hasOwnProperty(e.toLowerCase())
                  ? se(function (e, n) {
                      for (var r, o = i(e, t), u = o.length; u--; )
                        e[(r = L(e, o[u]))] = !(n[r] = o[u]);
                    })
                  : function (e) {
                      return i(e, 0, n);
                    })
              : i;
          },
        },
        pseudos: {
          not: se(function (e) {
            var t = [],
              n = [],
              r = a(e.replace(B, "$1"));
            return r[b]
              ? se(function (e, t, n, i) {
                  for (var o, u = r(e, null, i, []), a = e.length; a--; )
                    (o = u[a]) && (e[a] = !(t[a] = o));
                })
              : function (e, i, o) {
                  return (t[0] = e), r(t, null, o, n), (t[0] = null), !n.pop();
                };
          }),
          has: se(function (e) {
            return function (t) {
              return ae(e, t).length > 0;
            };
          }),
          contains: se(function (e) {
            return (
              (e = e.replace(te, ne)),
              function (t) {
                return (t.textContent || i(t)).indexOf(e) > -1;
              }
            );
          }),
          lang: se(function (e) {
            return (
              X.test(e || "") || ae.error("unsupported lang: " + e),
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
            return Q.test(e.nodeName);
          },
          input: function (e) {
            return J.test(e.nodeName);
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
          first: ve(function () {
            return [0];
          }),
          last: ve(function (e, t) {
            return [t - 1];
          }),
          eq: ve(function (e, t, n) {
            return [n < 0 ? n + t : n];
          }),
          even: ve(function (e, t) {
            for (var n = 0; n < t; n += 2) e.push(n);
            return e;
          }),
          odd: ve(function (e, t) {
            for (var n = 1; n < t; n += 2) e.push(n);
            return e;
          }),
          lt: ve(function (e, t, n) {
            for (var r = n < 0 ? n + t : n > t ? t : n; --r >= 0; ) e.push(r);
            return e;
          }),
          gt: ve(function (e, t, n) {
            for (var r = n < 0 ? n + t : n; ++r < t; ) e.push(r);
            return e;
          }),
        },
      }).pseudos.nth = r.pseudos.eq),
      { radio: !0, checkbox: !0, file: !0, password: !0, image: !0 }))
        r.pseudos[t] = he(t);
      for (t in { submit: !0, reset: !0 }) r.pseudos[t] = pe(t);
      function ye() {}
      function we(e) {
        for (var t = 0, n = e.length, r = ""; t < n; t++) r += e[t].value;
        return r;
      }
      function be(e, t, n) {
        var r = t.dir,
          i = t.next,
          o = i || r,
          u = n && "parentNode" === o,
          a = k++;
        return t.first
          ? function (t, n, i) {
              for (; (t = t[r]); ) if (1 === t.nodeType || u) return e(t, n, i);
              return !1;
            }
          : function (t, n, c) {
              var s,
                f,
                l,
                d = [x, a];
              if (c) {
                for (; (t = t[r]); )
                  if ((1 === t.nodeType || u) && e(t, n, c)) return !0;
              } else
                for (; (t = t[r]); )
                  if (1 === t.nodeType || u)
                    if (
                      ((f =
                        (l = t[b] || (t[b] = {}))[t.uniqueID] ||
                        (l[t.uniqueID] = {})),
                      i && i === t.nodeName.toLowerCase())
                    )
                      t = t[r] || t;
                    else {
                      if ((s = f[o]) && s[0] === x && s[1] === a)
                        return (d[2] = s[2]);
                      if (((f[o] = d), (d[2] = e(t, n, c)))) return !0;
                    }
              return !1;
            };
      }
      function _e(e) {
        return e.length > 1
          ? function (t, n, r) {
              for (var i = e.length; i--; ) if (!e[i](t, n, r)) return !1;
              return !0;
            }
          : e[0];
      }
      function xe(e, t, n, r, i) {
        for (var o, u = [], a = 0, c = e.length, s = null != t; a < c; a++)
          (o = e[a]) && ((n && !n(o, r, i)) || (u.push(o), s && t.push(a)));
        return u;
      }
      function ke(e, t, n, r, i, o) {
        return (
          r && !r[b] && (r = ke(r)),
          i && !i[b] && (i = ke(i, o)),
          se(function (o, u, a, c) {
            var s,
              f,
              l,
              d = [],
              h = [],
              p = u.length,
              m =
                o ||
                (function (e, t, n) {
                  for (var r = 0, i = t.length; r < i; r++) ae(e, t[r], n);
                  return n;
                })(t || "*", a.nodeType ? [a] : a, []),
              v = !e || (!o && t) ? m : xe(m, d, e, a, c),
              g = n ? (i || (o ? e : p || r) ? [] : u) : v;
            if ((n && n(v, g, a, c), r))
              for (s = xe(g, h), r(s, [], a, c), f = s.length; f--; )
                (l = s[f]) && (g[h[f]] = !(v[h[f]] = l));
            if (o) {
              if (i || e) {
                if (i) {
                  for (s = [], f = g.length; f--; )
                    (l = g[f]) && s.push((v[f] = l));
                  i(null, (g = []), s, c);
                }
                for (f = g.length; f--; )
                  (l = g[f]) &&
                    (s = i ? L(o, l) : d[f]) > -1 &&
                    (o[s] = !(u[s] = l));
              }
            } else (g = xe(g === u ? g.splice(p, g.length) : g)), i ? i(null, u, g, c) : O.apply(u, g);
          })
        );
      }
      function je(e) {
        for (
          var t,
            n,
            i,
            o = e.length,
            u = r.relative[e[0].type],
            a = u || r.relative[" "],
            c = u ? 1 : 0,
            f = be(
              function (e) {
                return e === t;
              },
              a,
              !0
            ),
            l = be(
              function (e) {
                return L(t, e) > -1;
              },
              a,
              !0
            ),
            d = [
              function (e, n, r) {
                var i =
                  (!u && (r || n !== s)) ||
                  ((t = n).nodeType ? f(e, n, r) : l(e, n, r));
                return (t = null), i;
              },
            ];
          c < o;
          c++
        )
          if ((n = r.relative[e[c].type])) d = [be(_e(d), n)];
          else {
            if ((n = r.filter[e[c].type].apply(null, e[c].matches))[b]) {
              for (i = ++c; i < o && !r.relative[e[i].type]; i++);
              return ke(
                c > 1 && _e(d),
                c > 1 &&
                  we(
                    e
                      .slice(0, c - 1)
                      .concat({ value: " " === e[c - 2].type ? "*" : "" })
                  ).replace(B, "$1"),
                n,
                c < i && je(e.slice(c, i)),
                i < o && je((e = e.slice(i))),
                i < o && we(e)
              );
            }
            d.push(n);
          }
        return _e(d);
      }
      return (
        (ye.prototype = r.filters = r.pseudos),
        (r.setFilters = new ye()),
        (u = ae.tokenize = function (e, t) {
          var n,
            i,
            o,
            u,
            a,
            c,
            s,
            f = S[e + " "];
          if (f) return t ? 0 : f.slice(0);
          for (a = e, c = [], s = r.preFilter; a; ) {
            for (u in ((n && !(i = $.exec(a))) ||
              (i && (a = a.slice(i[0].length) || a), c.push((o = []))),
            (n = !1),
            (i = W.exec(a)) &&
              ((n = i.shift()),
              o.push({ value: n, type: i[0].replace(B, " ") }),
              (a = a.slice(n.length))),
            r.filter))
              !(i = Y[u].exec(a)) ||
                (s[u] && !(i = s[u](i))) ||
                ((n = i.shift()),
                o.push({ value: n, type: u, matches: i }),
                (a = a.slice(n.length)));
            if (!n) break;
          }
          return t ? a.length : a ? ae.error(e) : S(e, c).slice(0);
        }),
        (a = ae.compile = function (e, t) {
          var n,
            i = [],
            o = [],
            a = T[e + " "];
          if (!a) {
            for (t || (t = u(e)), n = t.length; n--; )
              (a = je(t[n]))[b] ? i.push(a) : o.push(a);
            (a = T(
              e,
              (function (e, t) {
                var n = t.length > 0,
                  i = e.length > 0,
                  o = function (o, u, a, c, f) {
                    var l,
                      p,
                      v,
                      g = 0,
                      y = "0",
                      w = o && [],
                      b = [],
                      _ = s,
                      k = o || (i && r.find.TAG("*", f)),
                      j = (x += null == _ ? 1 : Math.random() || 0.1),
                      S = k.length;
                    for (
                      f && (s = u == h || u || f);
                      y !== S && null != (l = k[y]);
                      y++
                    ) {
                      if (i && l) {
                        for (
                          p = 0, u || l.ownerDocument == h || (d(l), (a = !m));
                          (v = e[p++]);

                        )
                          if (v(l, u || h, a)) {
                            c.push(l);
                            break;
                          }
                        f && (x = j);
                      }
                      n && ((l = !v && l) && g--, o && w.push(l));
                    }
                    if (((g += y), n && y !== g)) {
                      for (p = 0; (v = t[p++]); ) v(w, b, u, a);
                      if (o) {
                        if (g > 0)
                          for (; y--; ) w[y] || b[y] || (b[y] = D.call(c));
                        b = xe(b);
                      }
                      O.apply(c, b),
                        f &&
                          !o &&
                          b.length > 0 &&
                          g + t.length > 1 &&
                          ae.uniqueSort(c);
                    }
                    return f && ((x = j), (s = _)), w;
                  };
                return n ? se(o) : o;
              })(o, i)
            )).selector = e;
          }
          return a;
        }),
        (c = ae.select = function (e, t, n, i) {
          var o,
            c,
            s,
            f,
            l,
            d = "function" == typeof e && e,
            h = !i && u((e = d.selector || e));
          if (((n = n || []), 1 === h.length)) {
            if (
              (c = h[0] = h[0].slice(0)).length > 2 &&
              "ID" === (s = c[0]).type &&
              9 === t.nodeType &&
              m &&
              r.relative[c[1].type]
            ) {
              if (!(t = (r.find.ID(s.matches[0].replace(te, ne), t) || [])[0]))
                return n;
              d && (t = t.parentNode), (e = e.slice(c.shift().value.length));
            }
            for (
              o = Y.needsContext.test(e) ? 0 : c.length;
              o-- && ((s = c[o]), !r.relative[(f = s.type)]);

            )
              if (
                (l = r.find[f]) &&
                (i = l(
                  s.matches[0].replace(te, ne),
                  (ee.test(c[0].type) && ge(t.parentNode)) || t
                ))
              ) {
                if ((c.splice(o, 1), !(e = i.length && we(c))))
                  return O.apply(n, i), n;
                break;
              }
          }
          return (
            (d || a(e, h))(
              i,
              t,
              !m,
              n,
              !t || (ee.test(e) && ge(t.parentNode)) || t
            ),
            n
          );
        }),
        (n.sortStable = b.split("").sort(E).join("") === b),
        (n.detectDuplicates = !!l),
        d(),
        (n.sortDetached = fe(function (e) {
          return 1 & e.compareDocumentPosition(h.createElement("fieldset"));
        })),
        fe(function (e) {
          return (
            (e.innerHTML = "<a href='#'></a>"),
            "#" === e.firstChild.getAttribute("href")
          );
        }) ||
          le("type|href|height|width", function (e, t, n) {
            if (!n)
              return e.getAttribute(t, "type" === t.toLowerCase() ? 1 : 2);
          }),
        (n.attributes &&
          fe(function (e) {
            return (
              (e.innerHTML = "<input/>"),
              e.firstChild.setAttribute("value", ""),
              "" === e.firstChild.getAttribute("value")
            );
          })) ||
          le("value", function (e, t, n) {
            if (!n && "input" === e.nodeName.toLowerCase())
              return e.defaultValue;
          }),
        fe(function (e) {
          return null == e.getAttribute("disabled");
        }) ||
          le(R, function (e, t, n) {
            var r;
            if (!n)
              return !0 === e[t]
                ? t.toLowerCase()
                : (r = e.getAttributeNode(t)) && r.specified
                ? r.value
                : null;
          }),
        ae
      );
    })(e);
  (b.find = x),
    (b.expr = x.selectors),
    (b.expr[":"] = b.expr.pseudos),
    (b.uniqueSort = b.unique = x.uniqueSort),
    (b.text = x.getText),
    (b.isXMLDoc = x.isXML),
    (b.contains = x.contains),
    (b.escapeSelector = x.escape);
  var k = function (e, t, n) {
      for (var r = [], i = void 0 !== n; (e = e[t]) && 9 !== e.nodeType; )
        if (1 === e.nodeType) {
          if (i && b(e).is(n)) break;
          r.push(e);
        }
      return r;
    },
    j = function (e, t) {
      for (var n = []; e; e = e.nextSibling)
        1 === e.nodeType && e !== t && n.push(e);
      return n;
    },
    S = b.expr.match.needsContext;
  function T(e, t) {
    return e.nodeName && e.nodeName.toLowerCase() === t.toLowerCase();
  }
  var C = /^<([a-z][^\/\0>:\x20\t\r\n\f]*)[\x20\t\r\n\f]*\/?>(?:<\/\1>|)$/i;
  function E(e, t, n) {
    return p(t)
      ? b.grep(e, function (e, r) {
          return !!t.call(e, r, e) !== n;
        })
      : t.nodeType
      ? b.grep(e, function (e) {
          return (e === t) !== n;
        })
      : "string" != typeof t
      ? b.grep(e, function (e) {
          return a.call(t, e) > -1 !== n;
        })
      : b.filter(t, e, n);
  }
  (b.filter = function (e, t, n) {
    var r = t[0];
    return (
      n && (e = ":not(" + e + ")"),
      1 === t.length && 1 === r.nodeType
        ? b.find.matchesSelector(r, e)
          ? [r]
          : []
        : b.find.matches(
            e,
            b.grep(t, function (e) {
              return 1 === e.nodeType;
            })
          )
    );
  }),
    b.fn.extend({
      find: function (e) {
        var t,
          n,
          r = this.length,
          i = this;
        if ("string" != typeof e)
          return this.pushStack(
            b(e).filter(function () {
              for (t = 0; t < r; t++) if (b.contains(i[t], this)) return !0;
            })
          );
        for (n = this.pushStack([]), t = 0; t < r; t++) b.find(e, i[t], n);
        return r > 1 ? b.uniqueSort(n) : n;
      },
      filter: function (e) {
        return this.pushStack(E(this, e || [], !1));
      },
      not: function (e) {
        return this.pushStack(E(this, e || [], !0));
      },
      is: function (e) {
        return !!E(this, "string" == typeof e && S.test(e) ? b(e) : e || [], !1)
          .length;
      },
    });
  var q,
    A = /^(?:\s*(<[\w\W]+>)[^>]*|#([\w-]+))$/;
  ((b.fn.init = function (e, t, n) {
    var r, i;
    if (!e) return this;
    if (((n = n || q), "string" == typeof e)) {
      if (
        !(r =
          "<" === e[0] && ">" === e[e.length - 1] && e.length >= 3
            ? [null, e, null]
            : A.exec(e)) ||
        (!r[1] && t)
      )
        return !t || t.jquery ? (t || n).find(e) : this.constructor(t).find(e);
      if (r[1]) {
        if (
          ((t = t instanceof b ? t[0] : t),
          b.merge(
            this,
            b.parseHTML(r[1], t && t.nodeType ? t.ownerDocument || t : v, !0)
          ),
          C.test(r[1]) && b.isPlainObject(t))
        )
          for (r in t) p(this[r]) ? this[r](t[r]) : this.attr(r, t[r]);
        return this;
      }
      return (
        (i = v.getElementById(r[2])) && ((this[0] = i), (this.length = 1)), this
      );
    }
    return e.nodeType
      ? ((this[0] = e), (this.length = 1), this)
      : p(e)
      ? void 0 !== n.ready
        ? n.ready(e)
        : e(b)
      : b.makeArray(e, this);
  }).prototype = b.fn),
    (q = b(v));
  var D = /^(?:parents|prev(?:Until|All))/,
    N = { children: !0, contents: !0, next: !0, prev: !0 };
  function O(e, t) {
    for (; (e = e[t]) && 1 !== e.nodeType; );
    return e;
  }
  b.fn.extend({
    has: function (e) {
      var t = b(e, this),
        n = t.length;
      return this.filter(function () {
        for (var e = 0; e < n; e++) if (b.contains(this, t[e])) return !0;
      });
    },
    closest: function (e, t) {
      var n,
        r = 0,
        i = this.length,
        o = [],
        u = "string" != typeof e && b(e);
      if (!S.test(e))
        for (; r < i; r++)
          for (n = this[r]; n && n !== t; n = n.parentNode)
            if (
              n.nodeType < 11 &&
              (u
                ? u.index(n) > -1
                : 1 === n.nodeType && b.find.matchesSelector(n, e))
            ) {
              o.push(n);
              break;
            }
      return this.pushStack(o.length > 1 ? b.uniqueSort(o) : o);
    },
    index: function (e) {
      return e
        ? "string" == typeof e
          ? a.call(b(e), this[0])
          : a.call(this, e.jquery ? e[0] : e)
        : this[0] && this[0].parentNode
        ? this.first().prevAll().length
        : -1;
    },
    add: function (e, t) {
      return this.pushStack(b.uniqueSort(b.merge(this.get(), b(e, t))));
    },
    addBack: function (e) {
      return this.add(null == e ? this.prevObject : this.prevObject.filter(e));
    },
  }),
    b.each(
      {
        parent: function (e) {
          var t = e.parentNode;
          return t && 11 !== t.nodeType ? t : null;
        },
        parents: function (e) {
          return k(e, "parentNode");
        },
        parentsUntil: function (e, t, n) {
          return k(e, "parentNode", n);
        },
        next: function (e) {
          return O(e, "nextSibling");
        },
        prev: function (e) {
          return O(e, "previousSibling");
        },
        nextAll: function (e) {
          return k(e, "nextSibling");
        },
        prevAll: function (e) {
          return k(e, "previousSibling");
        },
        nextUntil: function (e, t, n) {
          return k(e, "nextSibling", n);
        },
        prevUntil: function (e, t, n) {
          return k(e, "previousSibling", n);
        },
        siblings: function (e) {
          return j((e.parentNode || {}).firstChild, e);
        },
        children: function (e) {
          return j(e.firstChild);
        },
        contents: function (e) {
          return null != e.contentDocument && r(e.contentDocument)
            ? e.contentDocument
            : (T(e, "template") && (e = e.content || e),
              b.merge([], e.childNodes));
        },
      },
      function (e, t) {
        b.fn[e] = function (n, r) {
          var i = b.map(this, t, n);
          return (
            "Until" !== e.slice(-5) && (r = n),
            r && "string" == typeof r && (i = b.filter(r, i)),
            this.length > 1 &&
              (N[e] || b.uniqueSort(i), D.test(e) && i.reverse()),
            this.pushStack(i)
          );
        };
      }
    );
  var M = /[^\x20\t\r\n\f]+/g;
  function L(e) {
    return e;
  }
  function R(e) {
    throw e;
  }
  function I(e, t, n, r) {
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
  (b.Callbacks = function (e) {
    e =
      "string" == typeof e
        ? (function (e) {
            var t = {};
            return (
              b.each(e.match(M) || [], function (e, n) {
                t[n] = !0;
              }),
              t
            );
          })(e)
        : b.extend({}, e);
    var t,
      n,
      r,
      i,
      o = [],
      u = [],
      a = -1,
      c = function () {
        for (i = i || e.once, r = t = !0; u.length; a = -1)
          for (n = u.shift(); ++a < o.length; )
            !1 === o[a].apply(n[0], n[1]) &&
              e.stopOnFalse &&
              ((a = o.length), (n = !1));
        e.memory || (n = !1), (t = !1), i && (o = n ? [] : "");
      },
      s = {
        add: function () {
          return (
            o &&
              (n && !t && ((a = o.length - 1), u.push(n)),
              (function t(n) {
                b.each(n, function (n, r) {
                  p(r)
                    ? (e.unique && s.has(r)) || o.push(r)
                    : r && r.length && "string" !== w(r) && t(r);
                });
              })(arguments),
              n && !t && c()),
            this
          );
        },
        remove: function () {
          return (
            b.each(arguments, function (e, t) {
              for (var n; (n = b.inArray(t, o, n)) > -1; )
                o.splice(n, 1), n <= a && a--;
            }),
            this
          );
        },
        has: function (e) {
          return e ? b.inArray(e, o) > -1 : o.length > 0;
        },
        empty: function () {
          return o && (o = []), this;
        },
        disable: function () {
          return (i = u = []), (o = n = ""), this;
        },
        disabled: function () {
          return !o;
        },
        lock: function () {
          return (i = u = []), n || t || (o = n = ""), this;
        },
        locked: function () {
          return !!i;
        },
        fireWith: function (e, n) {
          return (
            i ||
              ((n = [e, (n = n || []).slice ? n.slice() : n]),
              u.push(n),
              t || c()),
            this
          );
        },
        fire: function () {
          return s.fireWith(this, arguments), this;
        },
        fired: function () {
          return !!r;
        },
      };
    return s;
  }),
    b.extend({
      Deferred: function (t) {
        var n = [
            [
              "notify",
              "progress",
              b.Callbacks("memory"),
              b.Callbacks("memory"),
              2,
            ],
            [
              "resolve",
              "done",
              b.Callbacks("once memory"),
              b.Callbacks("once memory"),
              0,
              "resolved",
            ],
            [
              "reject",
              "fail",
              b.Callbacks("once memory"),
              b.Callbacks("once memory"),
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
              return o.done(arguments).fail(arguments), this;
            },
            catch: function (e) {
              return i.then(null, e);
            },
            pipe: function () {
              var e = arguments;
              return b
                .Deferred(function (t) {
                  b.each(n, function (n, r) {
                    var i = p(e[r[4]]) && e[r[4]];
                    o[r[1]](function () {
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
              var o = 0;
              function u(t, n, r, i) {
                return function () {
                  var a = this,
                    c = arguments,
                    s = function () {
                      var e, s;
                      if (!(t < o)) {
                        if ((e = r.apply(a, c)) === n.promise())
                          throw new TypeError("Thenable self-resolution");
                        (s =
                          e &&
                          ("object" == typeof e || "function" == typeof e) &&
                          e.then),
                          p(s)
                            ? i
                              ? s.call(e, u(o, n, L, i), u(o, n, R, i))
                              : (o++,
                                s.call(
                                  e,
                                  u(o, n, L, i),
                                  u(o, n, R, i),
                                  u(o, n, L, n.notifyWith)
                                ))
                            : (r !== L && ((a = void 0), (c = [e])),
                              (i || n.resolveWith)(a, c));
                      }
                    },
                    f = i
                      ? s
                      : function () {
                          try {
                            s();
                          } catch (e) {
                            b.Deferred.exceptionHook &&
                              b.Deferred.exceptionHook(e, f.stackTrace),
                              t + 1 >= o &&
                                (r !== R && ((a = void 0), (c = [e])),
                                n.rejectWith(a, c));
                          }
                        };
                  t
                    ? f()
                    : (b.Deferred.getStackHook &&
                        (f.stackTrace = b.Deferred.getStackHook()),
                      e.setTimeout(f));
                };
              }
              return b
                .Deferred(function (e) {
                  n[0][3].add(u(0, e, p(i) ? i : L, e.notifyWith)),
                    n[1][3].add(u(0, e, p(t) ? t : L)),
                    n[2][3].add(u(0, e, p(r) ? r : R));
                })
                .promise();
            },
            promise: function (e) {
              return null != e ? b.extend(e, i) : i;
            },
          },
          o = {};
        return (
          b.each(n, function (e, t) {
            var u = t[2],
              a = t[5];
            (i[t[1]] = u.add),
              a &&
                u.add(
                  function () {
                    r = a;
                  },
                  n[3 - e][2].disable,
                  n[3 - e][3].disable,
                  n[0][2].lock,
                  n[0][3].lock
                ),
              u.add(t[3].fire),
              (o[t[0]] = function () {
                return (
                  o[t[0] + "With"](this === o ? void 0 : this, arguments), this
                );
              }),
              (o[t[0] + "With"] = u.fireWith);
          }),
          i.promise(o),
          t && t.call(o, o),
          o
        );
      },
      when: function (e) {
        var t = arguments.length,
          n = t,
          r = Array(n),
          o = i.call(arguments),
          u = b.Deferred(),
          a = function (e) {
            return function (n) {
              (r[e] = this),
                (o[e] = arguments.length > 1 ? i.call(arguments) : n),
                --t || u.resolveWith(r, o);
            };
          };
        if (
          t <= 1 &&
          (I(e, u.done(a(n)).resolve, u.reject, !t),
          "pending" === u.state() || p(o[n] && o[n].then))
        )
          return u.then();
        for (; n--; ) I(o[n], a(n), u.reject);
        return u.promise();
      },
    });
  var H = /^(Eval|Internal|Range|Reference|Syntax|Type|URI)Error$/;
  (b.Deferred.exceptionHook = function (t, n) {
    e.console &&
      e.console.warn &&
      t &&
      H.test(t.name) &&
      e.console.warn("jQuery.Deferred exception: " + t.message, t.stack, n);
  }),
    (b.readyException = function (t) {
      e.setTimeout(function () {
        throw t;
      });
    });
  var P = b.Deferred();
  function z() {
    v.removeEventListener("DOMContentLoaded", z),
      e.removeEventListener("load", z),
      b.ready();
  }
  (b.fn.ready = function (e) {
    return (
      P.then(e).catch(function (e) {
        b.readyException(e);
      }),
      this
    );
  }),
    b.extend({
      isReady: !1,
      readyWait: 1,
      ready: function (e) {
        (!0 === e ? --b.readyWait : b.isReady) ||
          ((b.isReady = !0),
          (!0 !== e && --b.readyWait > 0) || P.resolveWith(v, [b]));
      },
    }),
    (b.ready.then = P.then),
    "complete" === v.readyState ||
    ("loading" !== v.readyState && !v.documentElement.doScroll)
      ? e.setTimeout(b.ready)
      : (v.addEventListener("DOMContentLoaded", z),
        e.addEventListener("load", z));
  var F = function (e, t, n, r, i, o, u) {
      var a = 0,
        c = e.length,
        s = null == n;
      if ("object" === w(n))
        for (a in ((i = !0), n)) F(e, t, a, n[a], !0, o, u);
      else if (
        void 0 !== r &&
        ((i = !0),
        p(r) || (u = !0),
        s &&
          (u
            ? (t.call(e, r), (t = null))
            : ((s = t),
              (t = function (e, t, n) {
                return s.call(b(e), n);
              }))),
        t)
      )
        for (; a < c; a++) t(e[a], n, u ? r : r.call(e[a], a, t(e[a], n)));
      return i ? e : s ? t.call(e) : c ? t(e[0], n) : o;
    },
    B = /^-ms-/,
    $ = /-([a-z])/g;
  function W(e, t) {
    return t.toUpperCase();
  }
  function U(e) {
    return e.replace(B, "ms-").replace($, W);
  }
  var V = function (e) {
    return 1 === e.nodeType || 9 === e.nodeType || !+e.nodeType;
  };
  function X() {
    this.expando = b.expando + X.uid++;
  }
  (X.uid = 1),
    (X.prototype = {
      cache: function (e) {
        var t = e[this.expando];
        return (
          t ||
            ((t = {}),
            V(e) &&
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
        if ("string" == typeof t) i[U(t)] = n;
        else for (r in t) i[U(r)] = t[r];
        return i;
      },
      get: function (e, t) {
        return void 0 === t
          ? this.cache(e)
          : e[this.expando] && e[this.expando][U(t)];
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
              ? t.map(U)
              : (t = U(t)) in r
              ? [t]
              : t.match(M) || []).length;
            for (; n--; ) delete r[t[n]];
          }
          (void 0 === t || b.isEmptyObject(r)) &&
            (e.nodeType ? (e[this.expando] = void 0) : delete e[this.expando]);
        }
      },
      hasData: function (e) {
        var t = e[this.expando];
        return void 0 !== t && !b.isEmptyObject(t);
      },
    });
  var Y = new X(),
    G = new X(),
    J = /^(?:\{[\w\W]*\}|\[[\w\W]*\])$/,
    Q = /[A-Z]/g;
  function K(e, t, n) {
    var r;
    if (void 0 === n && 1 === e.nodeType)
      if (
        ((r = "data-" + t.replace(Q, "-$&").toLowerCase()),
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
        G.set(e, t, n);
      } else n = void 0;
    return n;
  }
  b.extend({
    hasData: function (e) {
      return G.hasData(e) || Y.hasData(e);
    },
    data: function (e, t, n) {
      return G.access(e, t, n);
    },
    removeData: function (e, t) {
      G.remove(e, t);
    },
    _data: function (e, t, n) {
      return Y.access(e, t, n);
    },
    _removeData: function (e, t) {
      Y.remove(e, t);
    },
  }),
    b.fn.extend({
      data: function (e, t) {
        var n,
          r,
          i,
          o = this[0],
          u = o && o.attributes;
        if (void 0 === e) {
          if (
            this.length &&
            ((i = G.get(o)), 1 === o.nodeType && !Y.get(o, "hasDataAttrs"))
          ) {
            for (n = u.length; n--; )
              u[n] &&
                0 === (r = u[n].name).indexOf("data-") &&
                ((r = U(r.slice(5))), K(o, r, i[r]));
            Y.set(o, "hasDataAttrs", !0);
          }
          return i;
        }
        return "object" == typeof e
          ? this.each(function () {
              G.set(this, e);
            })
          : F(
              this,
              function (t) {
                var n;
                if (o && void 0 === t)
                  return void 0 !== (n = G.get(o, e)) ||
                    void 0 !== (n = K(o, e))
                    ? n
                    : void 0;
                this.each(function () {
                  G.set(this, e, t);
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
          G.remove(this, e);
        });
      },
    }),
    b.extend({
      queue: function (e, t, n) {
        var r;
        if (e)
          return (
            (t = (t || "fx") + "queue"),
            (r = Y.get(e, t)),
            n &&
              (!r || Array.isArray(n)
                ? (r = Y.access(e, t, b.makeArray(n)))
                : r.push(n)),
            r || []
          );
      },
      dequeue: function (e, t) {
        t = t || "fx";
        var n = b.queue(e, t),
          r = n.length,
          i = n.shift(),
          o = b._queueHooks(e, t);
        "inprogress" === i && ((i = n.shift()), r--),
          i &&
            ("fx" === t && n.unshift("inprogress"),
            delete o.stop,
            i.call(
              e,
              function () {
                b.dequeue(e, t);
              },
              o
            )),
          !r && o && o.empty.fire();
      },
      _queueHooks: function (e, t) {
        var n = t + "queueHooks";
        return (
          Y.get(e, n) ||
          Y.access(e, n, {
            empty: b.Callbacks("once memory").add(function () {
              Y.remove(e, [t + "queue", n]);
            }),
          })
        );
      },
    }),
    b.fn.extend({
      queue: function (e, t) {
        var n = 2;
        return (
          "string" != typeof e && ((t = e), (e = "fx"), n--),
          arguments.length < n
            ? b.queue(this[0], e)
            : void 0 === t
            ? this
            : this.each(function () {
                var n = b.queue(this, e, t);
                b._queueHooks(this, e),
                  "fx" === e && "inprogress" !== n[0] && b.dequeue(this, e);
              })
        );
      },
      dequeue: function (e) {
        return this.each(function () {
          b.dequeue(this, e);
        });
      },
      clearQueue: function (e) {
        return this.queue(e || "fx", []);
      },
      promise: function (e, t) {
        var n,
          r = 1,
          i = b.Deferred(),
          o = this,
          u = this.length,
          a = function () {
            --r || i.resolveWith(o, [o]);
          };
        for (
          "string" != typeof e && ((t = e), (e = void 0)), e = e || "fx";
          u--;

        )
          (n = Y.get(o[u], e + "queueHooks")) &&
            n.empty &&
            (r++, n.empty.add(a));
        return a(), i.promise(t);
      },
    });
  var Z = /[+-]?(?:\d*\.|)\d+(?:[eE][+-]?\d+|)/.source,
    ee = new RegExp("^(?:([+-])=|)(" + Z + ")([a-z%]*)$", "i"),
    te = ["Top", "Right", "Bottom", "Left"],
    ne = v.documentElement,
    re = function (e) {
      return b.contains(e.ownerDocument, e);
    },
    ie = { composed: !0 };
  ne.getRootNode &&
    (re = function (e) {
      return (
        b.contains(e.ownerDocument, e) || e.getRootNode(ie) === e.ownerDocument
      );
    });
  var oe = function (e, t) {
    return (
      "none" === (e = t || e).style.display ||
      ("" === e.style.display && re(e) && "none" === b.css(e, "display"))
    );
  };
  function ue(e, t, n, r) {
    var i,
      o,
      u = 20,
      a = r
        ? function () {
            return r.cur();
          }
        : function () {
            return b.css(e, t, "");
          },
      c = a(),
      s = (n && n[3]) || (b.cssNumber[t] ? "" : "px"),
      f =
        e.nodeType &&
        (b.cssNumber[t] || ("px" !== s && +c)) &&
        ee.exec(b.css(e, t));
    if (f && f[3] !== s) {
      for (c /= 2, s = s || f[3], f = +c || 1; u--; )
        b.style(e, t, f + s),
          (1 - o) * (1 - (o = a() / c || 0.5)) <= 0 && (u = 0),
          (f /= o);
      (f *= 2), b.style(e, t, f + s), (n = n || []);
    }
    return (
      n &&
        ((f = +f || +c || 0),
        (i = n[1] ? f + (n[1] + 1) * n[2] : +n[2]),
        r && ((r.unit = s), (r.start = f), (r.end = i))),
      i
    );
  }
  var ae = {};
  function ce(e) {
    var t,
      n = e.ownerDocument,
      r = e.nodeName,
      i = ae[r];
    return (
      i ||
      ((t = n.body.appendChild(n.createElement(r))),
      (i = b.css(t, "display")),
      t.parentNode.removeChild(t),
      "none" === i && (i = "block"),
      (ae[r] = i),
      i)
    );
  }
  function se(e, t) {
    for (var n, r, i = [], o = 0, u = e.length; o < u; o++)
      (r = e[o]).style &&
        ((n = r.style.display),
        t
          ? ("none" === n &&
              ((i[o] = Y.get(r, "display") || null),
              i[o] || (r.style.display = "")),
            "" === r.style.display && oe(r) && (i[o] = ce(r)))
          : "none" !== n && ((i[o] = "none"), Y.set(r, "display", n)));
    for (o = 0; o < u; o++) null != i[o] && (e[o].style.display = i[o]);
    return e;
  }
  b.fn.extend({
    show: function () {
      return se(this, !0);
    },
    hide: function () {
      return se(this);
    },
    toggle: function (e) {
      return "boolean" == typeof e
        ? e
          ? this.show()
          : this.hide()
        : this.each(function () {
            oe(this) ? b(this).show() : b(this).hide();
          });
    },
  });
  var fe,
    le,
    de = /^(?:checkbox|radio)$/i,
    he = /<([a-z][^\/\0>\x20\t\r\n\f]*)/i,
    pe = /^$|^module$|\/(?:java|ecma)script/i;
  (fe = v.createDocumentFragment().appendChild(v.createElement("div"))),
    (le = v.createElement("input")).setAttribute("type", "radio"),
    le.setAttribute("checked", "checked"),
    le.setAttribute("name", "t"),
    fe.appendChild(le),
    (h.checkClone = fe.cloneNode(!0).cloneNode(!0).lastChild.checked),
    (fe.innerHTML = "<textarea>x</textarea>"),
    (h.noCloneChecked = !!fe.cloneNode(!0).lastChild.defaultValue),
    (fe.innerHTML = "<option></option>"),
    (h.option = !!fe.lastChild);
  var me = {
    thead: [1, "<table>", "</table>"],
    col: [2, "<table><colgroup>", "</colgroup></table>"],
    tr: [2, "<table><tbody>", "</tbody></table>"],
    td: [3, "<table><tbody><tr>", "</tr></tbody></table>"],
    _default: [0, "", ""],
  };
  function ve(e, t) {
    var n;
    return (
      (n =
        void 0 !== e.getElementsByTagName
          ? e.getElementsByTagName(t || "*")
          : void 0 !== e.querySelectorAll
          ? e.querySelectorAll(t || "*")
          : []),
      void 0 === t || (t && T(e, t)) ? b.merge([e], n) : n
    );
  }
  function ge(e, t) {
    for (var n = 0, r = e.length; n < r; n++)
      Y.set(e[n], "globalEval", !t || Y.get(t[n], "globalEval"));
  }
  (me.tbody = me.tfoot = me.colgroup = me.caption = me.thead),
    (me.th = me.td),
    h.option ||
      (me.optgroup = me.option = [
        1,
        "<select multiple='multiple'>",
        "</select>",
      ]);
  var ye = /<|&#?\w+;/;
  function we(e, t, n, r, i) {
    for (
      var o,
        u,
        a,
        c,
        s,
        f,
        l = t.createDocumentFragment(),
        d = [],
        h = 0,
        p = e.length;
      h < p;
      h++
    )
      if ((o = e[h]) || 0 === o)
        if ("object" === w(o)) b.merge(d, o.nodeType ? [o] : o);
        else if (ye.test(o)) {
          for (
            u = u || l.appendChild(t.createElement("div")),
              a = (he.exec(o) || ["", ""])[1].toLowerCase(),
              c = me[a] || me._default,
              u.innerHTML = c[1] + b.htmlPrefilter(o) + c[2],
              f = c[0];
            f--;

          )
            u = u.lastChild;
          b.merge(d, u.childNodes), ((u = l.firstChild).textContent = "");
        } else d.push(t.createTextNode(o));
    for (l.textContent = "", h = 0; (o = d[h++]); )
      if (r && b.inArray(o, r) > -1) i && i.push(o);
      else if (
        ((s = re(o)), (u = ve(l.appendChild(o), "script")), s && ge(u), n)
      )
        for (f = 0; (o = u[f++]); ) pe.test(o.type || "") && n.push(o);
    return l;
  }
  var be = /^([^.]*)(?:\.(.+)|)/;
  function _e() {
    return !0;
  }
  function xe() {
    return !1;
  }
  function ke(e, t) {
    return (
      (e ===
        (function () {
          try {
            return v.activeElement;
          } catch (e) {}
        })()) ==
      ("focus" === t)
    );
  }
  function je(e, t, n, r, i, o) {
    var u, a;
    if ("object" == typeof t) {
      for (a in ("string" != typeof n && ((r = r || n), (n = void 0)), t))
        je(e, a, n, r, t[a], o);
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
      i = xe;
    else if (!i) return e;
    return (
      1 === o &&
        ((u = i),
        ((i = function (e) {
          return b().off(e), u.apply(this, arguments);
        }).guid = u.guid || (u.guid = b.guid++))),
      e.each(function () {
        b.event.add(this, t, i, r, n);
      })
    );
  }
  function Se(e, t, n) {
    n
      ? (Y.set(e, t, !1),
        b.event.add(e, t, {
          namespace: !1,
          handler: function (e) {
            var r,
              o,
              u = Y.get(this, t);
            if (1 & e.isTrigger && this[t]) {
              if (u.length)
                (b.event.special[t] || {}).delegateType && e.stopPropagation();
              else if (
                ((u = i.call(arguments)),
                Y.set(this, t, u),
                (r = n(this, t)),
                this[t](),
                u !== (o = Y.get(this, t)) || r ? Y.set(this, t, !1) : (o = {}),
                u !== o)
              )
                return (
                  e.stopImmediatePropagation(), e.preventDefault(), o && o.value
                );
            } else
              u.length &&
                (Y.set(this, t, {
                  value: b.event.trigger(
                    b.extend(u[0], b.Event.prototype),
                    u.slice(1),
                    this
                  ),
                }),
                e.stopImmediatePropagation());
          },
        }))
      : void 0 === Y.get(e, t) && b.event.add(e, t, _e);
  }
  (b.event = {
    global: {},
    add: function (e, t, n, r, i) {
      var o,
        u,
        a,
        c,
        s,
        f,
        l,
        d,
        h,
        p,
        m,
        v = Y.get(e);
      if (V(e))
        for (
          n.handler && ((n = (o = n).handler), (i = o.selector)),
            i && b.find.matchesSelector(ne, i),
            n.guid || (n.guid = b.guid++),
            (c = v.events) || (c = v.events = Object.create(null)),
            (u = v.handle) ||
              (u = v.handle = function (t) {
                return void 0 !== b && b.event.triggered !== t.type
                  ? b.event.dispatch.apply(e, arguments)
                  : void 0;
              }),
            s = (t = (t || "").match(M) || [""]).length;
          s--;

        )
          (h = m = (a = be.exec(t[s]) || [])[1]),
            (p = (a[2] || "").split(".").sort()),
            h &&
              ((l = b.event.special[h] || {}),
              (h = (i ? l.delegateType : l.bindType) || h),
              (l = b.event.special[h] || {}),
              (f = b.extend(
                {
                  type: h,
                  origType: m,
                  data: r,
                  handler: n,
                  guid: n.guid,
                  selector: i,
                  needsContext: i && b.expr.match.needsContext.test(i),
                  namespace: p.join("."),
                },
                o
              )),
              (d = c[h]) ||
                (((d = c[h] = []).delegateCount = 0),
                (l.setup && !1 !== l.setup.call(e, r, p, u)) ||
                  (e.addEventListener && e.addEventListener(h, u))),
              l.add &&
                (l.add.call(e, f), f.handler.guid || (f.handler.guid = n.guid)),
              i ? d.splice(d.delegateCount++, 0, f) : d.push(f),
              (b.event.global[h] = !0));
    },
    remove: function (e, t, n, r, i) {
      var o,
        u,
        a,
        c,
        s,
        f,
        l,
        d,
        h,
        p,
        m,
        v = Y.hasData(e) && Y.get(e);
      if (v && (c = v.events)) {
        for (s = (t = (t || "").match(M) || [""]).length; s--; )
          if (
            ((h = m = (a = be.exec(t[s]) || [])[1]),
            (p = (a[2] || "").split(".").sort()),
            h)
          ) {
            for (
              l = b.event.special[h] || {},
                d = c[(h = (r ? l.delegateType : l.bindType) || h)] || [],
                a =
                  a[2] &&
                  new RegExp("(^|\\.)" + p.join("\\.(?:.*\\.|)") + "(\\.|$)"),
                u = o = d.length;
              o--;

            )
              (f = d[o]),
                (!i && m !== f.origType) ||
                  (n && n.guid !== f.guid) ||
                  (a && !a.test(f.namespace)) ||
                  (r && r !== f.selector && ("**" !== r || !f.selector)) ||
                  (d.splice(o, 1),
                  f.selector && d.delegateCount--,
                  l.remove && l.remove.call(e, f));
            u &&
              !d.length &&
              ((l.teardown && !1 !== l.teardown.call(e, p, v.handle)) ||
                b.removeEvent(e, h, v.handle),
              delete c[h]);
          } else for (h in c) b.event.remove(e, h + t[s], n, r, !0);
        b.isEmptyObject(c) && Y.remove(e, "handle events");
      }
    },
    dispatch: function (e) {
      var t,
        n,
        r,
        i,
        o,
        u,
        a = new Array(arguments.length),
        c = b.event.fix(e),
        s = (Y.get(this, "events") || Object.create(null))[c.type] || [],
        f = b.event.special[c.type] || {};
      for (a[0] = c, t = 1; t < arguments.length; t++) a[t] = arguments[t];
      if (
        ((c.delegateTarget = this),
        !f.preDispatch || !1 !== f.preDispatch.call(this, c))
      ) {
        for (
          u = b.event.handlers.call(this, c, s), t = 0;
          (i = u[t++]) && !c.isPropagationStopped();

        )
          for (
            c.currentTarget = i.elem, n = 0;
            (o = i.handlers[n++]) && !c.isImmediatePropagationStopped();

          )
            (c.rnamespace &&
              !1 !== o.namespace &&
              !c.rnamespace.test(o.namespace)) ||
              ((c.handleObj = o),
              (c.data = o.data),
              void 0 !==
                (r = (
                  (b.event.special[o.origType] || {}).handle || o.handler
                ).apply(i.elem, a)) &&
                !1 === (c.result = r) &&
                (c.preventDefault(), c.stopPropagation()));
        return f.postDispatch && f.postDispatch.call(this, c), c.result;
      }
    },
    handlers: function (e, t) {
      var n,
        r,
        i,
        o,
        u,
        a = [],
        c = t.delegateCount,
        s = e.target;
      if (c && s.nodeType && !("click" === e.type && e.button >= 1))
        for (; s !== this; s = s.parentNode || this)
          if (1 === s.nodeType && ("click" !== e.type || !0 !== s.disabled)) {
            for (o = [], u = {}, n = 0; n < c; n++)
              void 0 === u[(i = (r = t[n]).selector + " ")] &&
                (u[i] = r.needsContext
                  ? b(i, this).index(s) > -1
                  : b.find(i, this, null, [s]).length),
                u[i] && o.push(r);
            o.length && a.push({ elem: s, handlers: o });
          }
      return (
        (s = this), c < t.length && a.push({ elem: s, handlers: t.slice(c) }), a
      );
    },
    addProp: function (e, t) {
      Object.defineProperty(b.Event.prototype, e, {
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
      return e[b.expando] ? e : new b.Event(e);
    },
    special: {
      load: { noBubble: !0 },
      click: {
        setup: function (e) {
          var t = this || e;
          return (
            de.test(t.type) && t.click && T(t, "input") && Se(t, "click", _e),
            !1
          );
        },
        trigger: function (e) {
          var t = this || e;
          return (
            de.test(t.type) && t.click && T(t, "input") && Se(t, "click"), !0
          );
        },
        _default: function (e) {
          var t = e.target;
          return (
            (de.test(t.type) &&
              t.click &&
              T(t, "input") &&
              Y.get(t, "click")) ||
            T(t, "a")
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
    (b.removeEvent = function (e, t, n) {
      e.removeEventListener && e.removeEventListener(t, n);
    }),
    (b.Event = function (e, t) {
      if (!(this instanceof b.Event)) return new b.Event(e, t);
      e && e.type
        ? ((this.originalEvent = e),
          (this.type = e.type),
          (this.isDefaultPrevented =
            e.defaultPrevented ||
            (void 0 === e.defaultPrevented && !1 === e.returnValue)
              ? _e
              : xe),
          (this.target =
            e.target && 3 === e.target.nodeType
              ? e.target.parentNode
              : e.target),
          (this.currentTarget = e.currentTarget),
          (this.relatedTarget = e.relatedTarget))
        : (this.type = e),
        t && b.extend(this, t),
        (this.timeStamp = (e && e.timeStamp) || Date.now()),
        (this[b.expando] = !0);
    }),
    (b.Event.prototype = {
      constructor: b.Event,
      isDefaultPrevented: xe,
      isPropagationStopped: xe,
      isImmediatePropagationStopped: xe,
      isSimulated: !1,
      preventDefault: function () {
        var e = this.originalEvent;
        (this.isDefaultPrevented = _e),
          e && !this.isSimulated && e.preventDefault();
      },
      stopPropagation: function () {
        var e = this.originalEvent;
        (this.isPropagationStopped = _e),
          e && !this.isSimulated && e.stopPropagation();
      },
      stopImmediatePropagation: function () {
        var e = this.originalEvent;
        (this.isImmediatePropagationStopped = _e),
          e && !this.isSimulated && e.stopImmediatePropagation(),
          this.stopPropagation();
      },
    }),
    b.each(
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
      b.event.addProp
    ),
    b.each({ focus: "focusin", blur: "focusout" }, function (e, t) {
      b.event.special[e] = {
        setup: function () {
          return Se(this, e, ke), !1;
        },
        trigger: function () {
          return Se(this, e), !0;
        },
        _default: function () {
          return !0;
        },
        delegateType: t,
      };
    }),
    b.each(
      {
        mouseenter: "mouseover",
        mouseleave: "mouseout",
        pointerenter: "pointerover",
        pointerleave: "pointerout",
      },
      function (e, t) {
        b.event.special[e] = {
          delegateType: t,
          bindType: t,
          handle: function (e) {
            var n,
              r = this,
              i = e.relatedTarget,
              o = e.handleObj;
            return (
              (i && (i === r || b.contains(r, i))) ||
                ((e.type = o.origType),
                (n = o.handler.apply(this, arguments)),
                (e.type = t)),
              n
            );
          },
        };
      }
    ),
    b.fn.extend({
      on: function (e, t, n, r) {
        return je(this, e, t, n, r);
      },
      one: function (e, t, n, r) {
        return je(this, e, t, n, r, 1);
      },
      off: function (e, t, n) {
        var r, i;
        if (e && e.preventDefault && e.handleObj)
          return (
            (r = e.handleObj),
            b(e.delegateTarget).off(
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
          !1 === n && (n = xe),
          this.each(function () {
            b.event.remove(this, e, n, t);
          })
        );
      },
    });
  var Te = /<script|<style|<link/i,
    Ce = /checked\s*(?:[^=]|=\s*.checked.)/i,
    Ee = /^\s*<!(?:\[CDATA\[|--)|(?:\]\]|--)>\s*$/g;
  function qe(e, t) {
    return (
      (T(e, "table") &&
        T(11 !== t.nodeType ? t : t.firstChild, "tr") &&
        b(e).children("tbody")[0]) ||
      e
    );
  }
  function Ae(e) {
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
  function Ne(e, t) {
    var n, r, i, o, u, a;
    if (1 === t.nodeType) {
      if (Y.hasData(e) && (a = Y.get(e).events))
        for (i in (Y.remove(t, "handle events"), a))
          for (n = 0, r = a[i].length; n < r; n++) b.event.add(t, i, a[i][n]);
      G.hasData(e) && ((o = G.access(e)), (u = b.extend({}, o)), G.set(t, u));
    }
  }
  function Oe(e, t) {
    var n = t.nodeName.toLowerCase();
    "input" === n && de.test(e.type)
      ? (t.checked = e.checked)
      : ("input" !== n && "textarea" !== n) ||
        (t.defaultValue = e.defaultValue);
  }
  function Me(e, t, n, r) {
    t = o(t);
    var i,
      u,
      a,
      c,
      s,
      f,
      l = 0,
      d = e.length,
      m = d - 1,
      v = t[0],
      g = p(v);
    if (g || (d > 1 && "string" == typeof v && !h.checkClone && Ce.test(v)))
      return e.each(function (i) {
        var o = e.eq(i);
        g && (t[0] = v.call(this, i, o.html())), Me(o, t, n, r);
      });
    if (
      d &&
      ((u = (i = we(t, e[0].ownerDocument, !1, e, r)).firstChild),
      1 === i.childNodes.length && (i = u),
      u || r)
    ) {
      for (c = (a = b.map(ve(i, "script"), Ae)).length; l < d; l++)
        (s = i),
          l !== m &&
            ((s = b.clone(s, !0, !0)), c && b.merge(a, ve(s, "script"))),
          n.call(e[l], s, l);
      if (c)
        for (f = a[a.length - 1].ownerDocument, b.map(a, De), l = 0; l < c; l++)
          (s = a[l]),
            pe.test(s.type || "") &&
              !Y.access(s, "globalEval") &&
              b.contains(f, s) &&
              (s.src && "module" !== (s.type || "").toLowerCase()
                ? b._evalUrl &&
                  !s.noModule &&
                  b._evalUrl(
                    s.src,
                    { nonce: s.nonce || s.getAttribute("nonce") },
                    f
                  )
                : y(s.textContent.replace(Ee, ""), s, f));
    }
    return e;
  }
  function Le(e, t, n) {
    for (var r, i = t ? b.filter(t, e) : e, o = 0; null != (r = i[o]); o++)
      n || 1 !== r.nodeType || b.cleanData(ve(r)),
        r.parentNode &&
          (n && re(r) && ge(ve(r, "script")), r.parentNode.removeChild(r));
    return e;
  }
  b.extend({
    htmlPrefilter: function (e) {
      return e;
    },
    clone: function (e, t, n) {
      var r,
        i,
        o,
        u,
        a = e.cloneNode(!0),
        c = re(e);
      if (
        !(
          h.noCloneChecked ||
          (1 !== e.nodeType && 11 !== e.nodeType) ||
          b.isXMLDoc(e)
        )
      )
        for (u = ve(a), r = 0, i = (o = ve(e)).length; r < i; r++)
          Oe(o[r], u[r]);
      if (t)
        if (n)
          for (o = o || ve(e), u = u || ve(a), r = 0, i = o.length; r < i; r++)
            Ne(o[r], u[r]);
        else Ne(e, a);
      return (
        (u = ve(a, "script")).length > 0 && ge(u, !c && ve(e, "script")), a
      );
    },
    cleanData: function (e) {
      for (var t, n, r, i = b.event.special, o = 0; void 0 !== (n = e[o]); o++)
        if (V(n)) {
          if ((t = n[Y.expando])) {
            if (t.events)
              for (r in t.events)
                i[r] ? b.event.remove(n, r) : b.removeEvent(n, r, t.handle);
            n[Y.expando] = void 0;
          }
          n[G.expando] && (n[G.expando] = void 0);
        }
    },
  }),
    b.fn.extend({
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
              ? b.text(this)
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
        return Me(this, arguments, function (e) {
          (1 !== this.nodeType &&
            11 !== this.nodeType &&
            9 !== this.nodeType) ||
            qe(this, e).appendChild(e);
        });
      },
      prepend: function () {
        return Me(this, arguments, function (e) {
          if (
            1 === this.nodeType ||
            11 === this.nodeType ||
            9 === this.nodeType
          ) {
            var t = qe(this, e);
            t.insertBefore(e, t.firstChild);
          }
        });
      },
      before: function () {
        return Me(this, arguments, function (e) {
          this.parentNode && this.parentNode.insertBefore(e, this);
        });
      },
      after: function () {
        return Me(this, arguments, function (e) {
          this.parentNode && this.parentNode.insertBefore(e, this.nextSibling);
        });
      },
      empty: function () {
        for (var e, t = 0; null != (e = this[t]); t++)
          1 === e.nodeType && (b.cleanData(ve(e, !1)), (e.textContent = ""));
        return this;
      },
      clone: function (e, t) {
        return (
          (e = null != e && e),
          (t = null == t ? e : t),
          this.map(function () {
            return b.clone(this, e, t);
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
              !Te.test(e) &&
              !me[(he.exec(e) || ["", ""])[1].toLowerCase()]
            ) {
              e = b.htmlPrefilter(e);
              try {
                for (; n < r; n++)
                  1 === (t = this[n] || {}).nodeType &&
                    (b.cleanData(ve(t, !1)), (t.innerHTML = e));
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
        return Me(
          this,
          arguments,
          function (t) {
            var n = this.parentNode;
            b.inArray(this, e) < 0 &&
              (b.cleanData(ve(this)), n && n.replaceChild(t, this));
          },
          e
        );
      },
    }),
    b.each(
      {
        appendTo: "append",
        prependTo: "prepend",
        insertBefore: "before",
        insertAfter: "after",
        replaceAll: "replaceWith",
      },
      function (e, t) {
        b.fn[e] = function (e) {
          for (var n, r = [], i = b(e), o = i.length - 1, a = 0; a <= o; a++)
            (n = a === o ? this : this.clone(!0)),
              b(i[a])[t](n),
              u.apply(r, n.get());
          return this.pushStack(r);
        };
      }
    );
  var Re = new RegExp("^(" + Z + ")(?!px)[a-z%]+$", "i"),
    Ie = function (t) {
      var n = t.ownerDocument.defaultView;
      return (n && n.opener) || (n = e), n.getComputedStyle(t);
    },
    He = function (e, t, n) {
      var r,
        i,
        o = {};
      for (i in t) (o[i] = e.style[i]), (e.style[i] = t[i]);
      for (i in ((r = n.call(e)), t)) e.style[i] = o[i];
      return r;
    },
    Pe = new RegExp(te.join("|"), "i");
  function ze(e, t, n) {
    var r,
      i,
      o,
      u,
      a = e.style;
    return (
      (n = n || Ie(e)) &&
        ("" !== (u = n.getPropertyValue(t) || n[t]) ||
          re(e) ||
          (u = b.style(e, t)),
        !h.pixelBoxStyles() &&
          Re.test(u) &&
          Pe.test(t) &&
          ((r = a.width),
          (i = a.minWidth),
          (o = a.maxWidth),
          (a.minWidth = a.maxWidth = a.width = u),
          (u = n.width),
          (a.width = r),
          (a.minWidth = i),
          (a.maxWidth = o))),
      void 0 !== u ? u + "" : u
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
      if (f) {
        (s.style.cssText =
          "position:absolute;left:-11111px;width:60px;margin-top:1px;padding:0;border:0"),
          (f.style.cssText =
            "position:relative;display:block;box-sizing:border-box;overflow:scroll;margin:auto;border:1px;padding:1px;width:60%;top:1%"),
          ne.appendChild(s).appendChild(f);
        var t = e.getComputedStyle(f);
        (r = "1%" !== t.top),
          (c = 12 === n(t.marginLeft)),
          (f.style.right = "60%"),
          (u = 36 === n(t.right)),
          (i = 36 === n(t.width)),
          (f.style.position = "absolute"),
          (o = 12 === n(f.offsetWidth / 3)),
          ne.removeChild(s),
          (f = null);
      }
    }
    function n(e) {
      return Math.round(parseFloat(e));
    }
    var r,
      i,
      o,
      u,
      a,
      c,
      s = v.createElement("div"),
      f = v.createElement("div");
    f.style &&
      ((f.style.backgroundClip = "content-box"),
      (f.cloneNode(!0).style.backgroundClip = ""),
      (h.clearCloneStyle = "content-box" === f.style.backgroundClip),
      b.extend(h, {
        boxSizingReliable: function () {
          return t(), i;
        },
        pixelBoxStyles: function () {
          return t(), u;
        },
        pixelPosition: function () {
          return t(), r;
        },
        reliableMarginLeft: function () {
          return t(), c;
        },
        scrollboxSize: function () {
          return t(), o;
        },
        reliableTrDimensions: function () {
          var t, n, r, i;
          return (
            null == a &&
              ((t = v.createElement("table")),
              (n = v.createElement("tr")),
              (r = v.createElement("div")),
              (t.style.cssText =
                "position:absolute;left:-11111px;border-collapse:separate"),
              (n.style.cssText = "border:1px solid"),
              (n.style.height = "1px"),
              (r.style.height = "9px"),
              (r.style.display = "block"),
              ne.appendChild(t).appendChild(n).appendChild(r),
              (i = e.getComputedStyle(n)),
              (a =
                parseInt(i.height, 10) +
                  parseInt(i.borderTopWidth, 10) +
                  parseInt(i.borderBottomWidth, 10) ===
                n.offsetHeight),
              ne.removeChild(t)),
            a
          );
        },
      }));
  })();
  var Be = ["Webkit", "Moz", "ms"],
    $e = v.createElement("div").style,
    We = {};
  function Ue(e) {
    var t = b.cssProps[e] || We[e];
    return (
      t ||
      (e in $e
        ? e
        : (We[e] =
            (function (e) {
              for (
                var t = e[0].toUpperCase() + e.slice(1), n = Be.length;
                n--;

              )
                if ((e = Be[n] + t) in $e) return e;
            })(e) || e))
    );
  }
  var Ve = /^(none|table(?!-c[ea]).+)/,
    Xe = /^--/,
    Ye = { position: "absolute", visibility: "hidden", display: "block" },
    Ge = { letterSpacing: "0", fontWeight: "400" };
  function Je(e, t, n) {
    var r = ee.exec(t);
    return r ? Math.max(0, r[2] - (n || 0)) + (r[3] || "px") : t;
  }
  function Qe(e, t, n, r, i, o) {
    var u = "width" === t ? 1 : 0,
      a = 0,
      c = 0;
    if (n === (r ? "border" : "content")) return 0;
    for (; u < 4; u += 2)
      "margin" === n && (c += b.css(e, n + te[u], !0, i)),
        r
          ? ("content" === n && (c -= b.css(e, "padding" + te[u], !0, i)),
            "margin" !== n &&
              (c -= b.css(e, "border" + te[u] + "Width", !0, i)))
          : ((c += b.css(e, "padding" + te[u], !0, i)),
            "padding" !== n
              ? (c += b.css(e, "border" + te[u] + "Width", !0, i))
              : (a += b.css(e, "border" + te[u] + "Width", !0, i)));
    return (
      !r &&
        o >= 0 &&
        (c +=
          Math.max(
            0,
            Math.ceil(
              e["offset" + t[0].toUpperCase() + t.slice(1)] - o - c - a - 0.5
            )
          ) || 0),
      c
    );
  }
  function Ke(e, t, n) {
    var r = Ie(e),
      i =
        (!h.boxSizingReliable() || n) &&
        "border-box" === b.css(e, "boxSizing", !1, r),
      o = i,
      u = ze(e, t, r),
      a = "offset" + t[0].toUpperCase() + t.slice(1);
    if (Re.test(u)) {
      if (!n) return u;
      u = "auto";
    }
    return (
      ((!h.boxSizingReliable() && i) ||
        (!h.reliableTrDimensions() && T(e, "tr")) ||
        "auto" === u ||
        (!parseFloat(u) && "inline" === b.css(e, "display", !1, r))) &&
        e.getClientRects().length &&
        ((i = "border-box" === b.css(e, "boxSizing", !1, r)),
        (o = a in e) && (u = e[a])),
      (u = parseFloat(u) || 0) +
        Qe(e, t, n || (i ? "border" : "content"), o, r, u) +
        "px"
    );
  }
  function Ze(e, t, n, r, i) {
    return new Ze.prototype.init(e, t, n, r, i);
  }
  b.extend({
    cssHooks: {
      opacity: {
        get: function (e, t) {
          if (t) {
            var n = ze(e, "opacity");
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
          o,
          u,
          a = U(t),
          c = Xe.test(t),
          s = e.style;
        if (
          (c || (t = Ue(a)), (u = b.cssHooks[t] || b.cssHooks[a]), void 0 === n)
        )
          return u && "get" in u && void 0 !== (i = u.get(e, !1, r)) ? i : s[t];
        "string" === (o = typeof n) &&
          (i = ee.exec(n)) &&
          i[1] &&
          ((n = ue(e, t, i)), (o = "number")),
          null != n &&
            n == n &&
            ("number" !== o ||
              c ||
              (n += (i && i[3]) || (b.cssNumber[a] ? "" : "px")),
            h.clearCloneStyle ||
              "" !== n ||
              0 !== t.indexOf("background") ||
              (s[t] = "inherit"),
            (u && "set" in u && void 0 === (n = u.set(e, n, r))) ||
              (c ? s.setProperty(t, n) : (s[t] = n)));
      }
    },
    css: function (e, t, n, r) {
      var i,
        o,
        u,
        a = U(t);
      return (
        Xe.test(t) || (t = Ue(a)),
        (u = b.cssHooks[t] || b.cssHooks[a]) &&
          "get" in u &&
          (i = u.get(e, !0, n)),
        void 0 === i && (i = ze(e, t, r)),
        "normal" === i && t in Ge && (i = Ge[t]),
        "" === n || n
          ? ((o = parseFloat(i)), !0 === n || isFinite(o) ? o || 0 : i)
          : i
      );
    },
  }),
    b.each(["height", "width"], function (e, t) {
      b.cssHooks[t] = {
        get: function (e, n, r) {
          if (n)
            return !Ve.test(b.css(e, "display")) ||
              (e.getClientRects().length && e.getBoundingClientRect().width)
              ? Ke(e, t, r)
              : He(e, Ye, function () {
                  return Ke(e, t, r);
                });
        },
        set: function (e, n, r) {
          var i,
            o = Ie(e),
            u = !h.scrollboxSize() && "absolute" === o.position,
            a = (u || r) && "border-box" === b.css(e, "boxSizing", !1, o),
            c = r ? Qe(e, t, r, a, o) : 0;
          return (
            a &&
              u &&
              (c -= Math.ceil(
                e["offset" + t[0].toUpperCase() + t.slice(1)] -
                  parseFloat(o[t]) -
                  Qe(e, t, "border", !1, o) -
                  0.5
              )),
            c &&
              (i = ee.exec(n)) &&
              "px" !== (i[3] || "px") &&
              ((e.style[t] = n), (n = b.css(e, t))),
            Je(0, n, c)
          );
        },
      };
    }),
    (b.cssHooks.marginLeft = Fe(h.reliableMarginLeft, function (e, t) {
      if (t)
        return (
          (parseFloat(ze(e, "marginLeft")) ||
            e.getBoundingClientRect().left -
              He(e, { marginLeft: 0 }, function () {
                return e.getBoundingClientRect().left;
              })) + "px"
        );
    })),
    b.each({ margin: "", padding: "", border: "Width" }, function (e, t) {
      (b.cssHooks[e + t] = {
        expand: function (n) {
          for (
            var r = 0, i = {}, o = "string" == typeof n ? n.split(" ") : [n];
            r < 4;
            r++
          )
            i[e + te[r] + t] = o[r] || o[r - 2] || o[0];
          return i;
        },
      }),
        "margin" !== e && (b.cssHooks[e + t].set = Je);
    }),
    b.fn.extend({
      css: function (e, t) {
        return F(
          this,
          function (e, t, n) {
            var r,
              i,
              o = {},
              u = 0;
            if (Array.isArray(t)) {
              for (r = Ie(e), i = t.length; u < i; u++)
                o[t[u]] = b.css(e, t[u], !1, r);
              return o;
            }
            return void 0 !== n ? b.style(e, t, n) : b.css(e, t);
          },
          e,
          t,
          arguments.length > 1
        );
      },
    }),
    (b.Tween = Ze),
    (Ze.prototype = {
      constructor: Ze,
      init: function (e, t, n, r, i, o) {
        (this.elem = e),
          (this.prop = n),
          (this.easing = i || b.easing._default),
          (this.options = t),
          (this.start = this.now = this.cur()),
          (this.end = r),
          (this.unit = o || (b.cssNumber[n] ? "" : "px"));
      },
      cur: function () {
        var e = Ze.propHooks[this.prop];
        return e && e.get ? e.get(this) : Ze.propHooks._default.get(this);
      },
      run: function (e) {
        var t,
          n = Ze.propHooks[this.prop];
        return (
          this.options.duration
            ? (this.pos = t = b.easing[this.easing](
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
          n && n.set ? n.set(this) : Ze.propHooks._default.set(this),
          this
        );
      },
    }),
    (Ze.prototype.init.prototype = Ze.prototype),
    (Ze.propHooks = {
      _default: {
        get: function (e) {
          var t;
          return 1 !== e.elem.nodeType ||
            (null != e.elem[e.prop] && null == e.elem.style[e.prop])
            ? e.elem[e.prop]
            : (t = b.css(e.elem, e.prop, "")) && "auto" !== t
            ? t
            : 0;
        },
        set: function (e) {
          b.fx.step[e.prop]
            ? b.fx.step[e.prop](e)
            : 1 !== e.elem.nodeType ||
              (!b.cssHooks[e.prop] && null == e.elem.style[Ue(e.prop)])
            ? (e.elem[e.prop] = e.now)
            : b.style(e.elem, e.prop, e.now + e.unit);
        },
      },
    }),
    (Ze.propHooks.scrollTop = Ze.propHooks.scrollLeft = {
      set: function (e) {
        e.elem.nodeType && e.elem.parentNode && (e.elem[e.prop] = e.now);
      },
    }),
    (b.easing = {
      linear: function (e) {
        return e;
      },
      swing: function (e) {
        return 0.5 - Math.cos(e * Math.PI) / 2;
      },
      _default: "swing",
    }),
    (b.fx = Ze.prototype.init),
    (b.fx.step = {});
  var et,
    tt,
    nt = /^(?:toggle|show|hide)$/,
    rt = /queueHooks$/;
  function it() {
    tt &&
      (!1 === v.hidden && e.requestAnimationFrame
        ? e.requestAnimationFrame(it)
        : e.setTimeout(it, b.fx.interval),
      b.fx.tick());
  }
  function ot() {
    return (
      e.setTimeout(function () {
        et = void 0;
      }),
      (et = Date.now())
    );
  }
  function ut(e, t) {
    var n,
      r = 0,
      i = { height: e };
    for (t = t ? 1 : 0; r < 4; r += 2 - t)
      i["margin" + (n = te[r])] = i["padding" + n] = e;
    return t && (i.opacity = i.width = e), i;
  }
  function at(e, t, n) {
    for (
      var r,
        i = (ct.tweeners[t] || []).concat(ct.tweeners["*"]),
        o = 0,
        u = i.length;
      o < u;
      o++
    )
      if ((r = i[o].call(n, t, e))) return r;
  }
  function ct(e, t, n) {
    var r,
      i,
      o = 0,
      u = ct.prefilters.length,
      a = b.Deferred().always(function () {
        delete c.elem;
      }),
      c = function () {
        if (i) return !1;
        for (
          var t = et || ot(),
            n = Math.max(0, s.startTime + s.duration - t),
            r = 1 - (n / s.duration || 0),
            o = 0,
            u = s.tweens.length;
          o < u;
          o++
        )
          s.tweens[o].run(r);
        return (
          a.notifyWith(e, [s, r, n]),
          r < 1 && u
            ? n
            : (u || a.notifyWith(e, [s, 1, 0]), a.resolveWith(e, [s]), !1)
        );
      },
      s = a.promise({
        elem: e,
        props: b.extend({}, t),
        opts: b.extend(!0, { specialEasing: {}, easing: b.easing._default }, n),
        originalProperties: t,
        originalOptions: n,
        startTime: et || ot(),
        duration: n.duration,
        tweens: [],
        createTween: function (t, n) {
          var r = b.Tween(
            e,
            s.opts,
            t,
            n,
            s.opts.specialEasing[t] || s.opts.easing
          );
          return s.tweens.push(r), r;
        },
        stop: function (t) {
          var n = 0,
            r = t ? s.tweens.length : 0;
          if (i) return this;
          for (i = !0; n < r; n++) s.tweens[n].run(1);
          return (
            t
              ? (a.notifyWith(e, [s, 1, 0]), a.resolveWith(e, [s, t]))
              : a.rejectWith(e, [s, t]),
            this
          );
        },
      }),
      f = s.props;
    for (
      !(function (e, t) {
        var n, r, i, o, u;
        for (n in e)
          if (
            ((i = t[(r = U(n))]),
            (o = e[n]),
            Array.isArray(o) && ((i = o[1]), (o = e[n] = o[0])),
            n !== r && ((e[r] = o), delete e[n]),
            (u = b.cssHooks[r]) && ("expand" in u))
          )
            for (n in ((o = u.expand(o)), delete e[r], o))
              (n in e) || ((e[n] = o[n]), (t[n] = i));
          else t[r] = i;
      })(f, s.opts.specialEasing);
      o < u;
      o++
    )
      if ((r = ct.prefilters[o].call(s, e, f, s.opts)))
        return (
          p(r.stop) &&
            (b._queueHooks(s.elem, s.opts.queue).stop = r.stop.bind(r)),
          r
        );
    return (
      b.map(f, at, s),
      p(s.opts.start) && s.opts.start.call(e, s),
      s
        .progress(s.opts.progress)
        .done(s.opts.done, s.opts.complete)
        .fail(s.opts.fail)
        .always(s.opts.always),
      b.fx.timer(b.extend(c, { elem: e, anim: s, queue: s.opts.queue })),
      s
    );
  }
  (b.Animation = b.extend(ct, {
    tweeners: {
      "*": [
        function (e, t) {
          var n = this.createTween(e, t);
          return ue(n.elem, e, ee.exec(t), n), n;
        },
      ],
    },
    tweener: function (e, t) {
      p(e) ? ((t = e), (e = ["*"])) : (e = e.match(M));
      for (var n, r = 0, i = e.length; r < i; r++)
        (n = e[r]),
          (ct.tweeners[n] = ct.tweeners[n] || []),
          ct.tweeners[n].unshift(t);
    },
    prefilters: [
      function (e, t, n) {
        var r,
          i,
          o,
          u,
          a,
          c,
          s,
          f,
          l = "width" in t || "height" in t,
          d = this,
          h = {},
          p = e.style,
          m = e.nodeType && oe(e),
          v = Y.get(e, "fxshow");
        for (r in (n.queue ||
          (null == (u = b._queueHooks(e, "fx")).unqueued &&
            ((u.unqueued = 0),
            (a = u.empty.fire),
            (u.empty.fire = function () {
              u.unqueued || a();
            })),
          u.unqueued++,
          d.always(function () {
            d.always(function () {
              u.unqueued--, b.queue(e, "fx").length || u.empty.fire();
            });
          })),
        t))
          if (((i = t[r]), nt.test(i))) {
            if (
              (delete t[r],
              (o = o || "toggle" === i),
              i === (m ? "hide" : "show"))
            ) {
              if ("show" !== i || !v || void 0 === v[r]) continue;
              m = !0;
            }
            h[r] = (v && v[r]) || b.style(e, r);
          }
        if ((c = !b.isEmptyObject(t)) || !b.isEmptyObject(h))
          for (r in (l &&
            1 === e.nodeType &&
            ((n.overflow = [p.overflow, p.overflowX, p.overflowY]),
            null == (s = v && v.display) && (s = Y.get(e, "display")),
            "none" === (f = b.css(e, "display")) &&
              (s
                ? (f = s)
                : (se([e], !0),
                  (s = e.style.display || s),
                  (f = b.css(e, "display")),
                  se([e]))),
            ("inline" === f || ("inline-block" === f && null != s)) &&
              "none" === b.css(e, "float") &&
              (c ||
                (d.done(function () {
                  p.display = s;
                }),
                null == s && ((f = p.display), (s = "none" === f ? "" : f))),
              (p.display = "inline-block"))),
          n.overflow &&
            ((p.overflow = "hidden"),
            d.always(function () {
              (p.overflow = n.overflow[0]),
                (p.overflowX = n.overflow[1]),
                (p.overflowY = n.overflow[2]);
            })),
          (c = !1),
          h))
            c ||
              (v
                ? "hidden" in v && (m = v.hidden)
                : (v = Y.access(e, "fxshow", { display: s })),
              o && (v.hidden = !m),
              m && se([e], !0),
              d.done(function () {
                for (r in (m || se([e]), Y.remove(e, "fxshow"), h))
                  b.style(e, r, h[r]);
              })),
              (c = at(m ? v[r] : 0, r, d)),
              r in v ||
                ((v[r] = c.start), m && ((c.end = c.start), (c.start = 0)));
      },
    ],
    prefilter: function (e, t) {
      t ? ct.prefilters.unshift(e) : ct.prefilters.push(e);
    },
  })),
    (b.speed = function (e, t, n) {
      var r =
        e && "object" == typeof e
          ? b.extend({}, e)
          : {
              complete: n || (!n && t) || (p(e) && e),
              duration: e,
              easing: (n && t) || (t && !p(t) && t),
            };
      return (
        b.fx.off
          ? (r.duration = 0)
          : "number" != typeof r.duration &&
            (r.duration in b.fx.speeds
              ? (r.duration = b.fx.speeds[r.duration])
              : (r.duration = b.fx.speeds._default)),
        (null != r.queue && !0 !== r.queue) || (r.queue = "fx"),
        (r.old = r.complete),
        (r.complete = function () {
          p(r.old) && r.old.call(this), r.queue && b.dequeue(this, r.queue);
        }),
        r
      );
    }),
    b.fn.extend({
      fadeTo: function (e, t, n, r) {
        return this.filter(oe)
          .css("opacity", 0)
          .show()
          .end()
          .animate({ opacity: t }, e, n, r);
      },
      animate: function (e, t, n, r) {
        var i = b.isEmptyObject(e),
          o = b.speed(t, n, r),
          u = function () {
            var t = ct(this, b.extend({}, e), o);
            (i || Y.get(this, "finish")) && t.stop(!0);
          };
        return (
          (u.finish = u),
          i || !1 === o.queue ? this.each(u) : this.queue(o.queue, u)
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
              o = b.timers,
              u = Y.get(this);
            if (i) u[i] && u[i].stop && r(u[i]);
            else for (i in u) u[i] && u[i].stop && rt.test(i) && r(u[i]);
            for (i = o.length; i--; )
              o[i].elem !== this ||
                (null != e && o[i].queue !== e) ||
                (o[i].anim.stop(n), (t = !1), o.splice(i, 1));
            (!t && n) || b.dequeue(this, e);
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
              o = b.timers,
              u = r ? r.length : 0;
            for (
              n.finish = !0,
                b.queue(this, e, []),
                i && i.stop && i.stop.call(this, !0),
                t = o.length;
              t--;

            )
              o[t].elem === this &&
                o[t].queue === e &&
                (o[t].anim.stop(!0), o.splice(t, 1));
            for (t = 0; t < u; t++)
              r[t] && r[t].finish && r[t].finish.call(this);
            delete n.finish;
          })
        );
      },
    }),
    b.each(["toggle", "show", "hide"], function (e, t) {
      var n = b.fn[t];
      b.fn[t] = function (e, r, i) {
        return null == e || "boolean" == typeof e
          ? n.apply(this, arguments)
          : this.animate(ut(t, !0), e, r, i);
      };
    }),
    b.each(
      {
        slideDown: ut("show"),
        slideUp: ut("hide"),
        slideToggle: ut("toggle"),
        fadeIn: { opacity: "show" },
        fadeOut: { opacity: "hide" },
        fadeToggle: { opacity: "toggle" },
      },
      function (e, t) {
        b.fn[e] = function (e, n, r) {
          return this.animate(t, e, n, r);
        };
      }
    ),
    (b.timers = []),
    (b.fx.tick = function () {
      var e,
        t = 0,
        n = b.timers;
      for (et = Date.now(); t < n.length; t++)
        (e = n[t])() || n[t] !== e || n.splice(t--, 1);
      n.length || b.fx.stop(), (et = void 0);
    }),
    (b.fx.timer = function (e) {
      b.timers.push(e), b.fx.start();
    }),
    (b.fx.interval = 13),
    (b.fx.start = function () {
      tt || ((tt = !0), it());
    }),
    (b.fx.stop = function () {
      tt = null;
    }),
    (b.fx.speeds = { slow: 600, fast: 200, _default: 400 }),
    (b.fn.delay = function (t, n) {
      return (
        (t = (b.fx && b.fx.speeds[t]) || t),
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
      var e = v.createElement("input"),
        t = v.createElement("select").appendChild(v.createElement("option"));
      (e.type = "checkbox"),
        (h.checkOn = "" !== e.value),
        (h.optSelected = t.selected),
        ((e = v.createElement("input")).value = "t"),
        (e.type = "radio"),
        (h.radioValue = "t" === e.value);
    })();
  var st,
    ft = b.expr.attrHandle;
  b.fn.extend({
    attr: function (e, t) {
      return F(this, b.attr, e, t, arguments.length > 1);
    },
    removeAttr: function (e) {
      return this.each(function () {
        b.removeAttr(this, e);
      });
    },
  }),
    b.extend({
      attr: function (e, t, n) {
        var r,
          i,
          o = e.nodeType;
        if (3 !== o && 8 !== o && 2 !== o)
          return void 0 === e.getAttribute
            ? b.prop(e, t, n)
            : ((1 === o && b.isXMLDoc(e)) ||
                (i =
                  b.attrHooks[t.toLowerCase()] ||
                  (b.expr.match.bool.test(t) ? st : void 0)),
              void 0 !== n
                ? null === n
                  ? void b.removeAttr(e, t)
                  : i && "set" in i && void 0 !== (r = i.set(e, n, t))
                  ? r
                  : (e.setAttribute(t, n + ""), n)
                : i && "get" in i && null !== (r = i.get(e, t))
                ? r
                : null == (r = b.find.attr(e, t))
                ? void 0
                : r);
      },
      attrHooks: {
        type: {
          set: function (e, t) {
            if (!h.radioValue && "radio" === t && T(e, "input")) {
              var n = e.value;
              return e.setAttribute("type", t), n && (e.value = n), t;
            }
          },
        },
      },
      removeAttr: function (e, t) {
        var n,
          r = 0,
          i = t && t.match(M);
        if (i && 1 === e.nodeType) for (; (n = i[r++]); ) e.removeAttribute(n);
      },
    }),
    (st = {
      set: function (e, t, n) {
        return !1 === t ? b.removeAttr(e, n) : e.setAttribute(n, n), n;
      },
    }),
    b.each(b.expr.match.bool.source.match(/\w+/g), function (e, t) {
      var n = ft[t] || b.find.attr;
      ft[t] = function (e, t, r) {
        var i,
          o,
          u = t.toLowerCase();
        return (
          r ||
            ((o = ft[u]),
            (ft[u] = i),
            (i = null != n(e, t, r) ? u : null),
            (ft[u] = o)),
          i
        );
      };
    });
  var lt = /^(?:input|select|textarea|button)$/i,
    dt = /^(?:a|area)$/i;
  function ht(e) {
    return (e.match(M) || []).join(" ");
  }
  function pt(e) {
    return (e.getAttribute && e.getAttribute("class")) || "";
  }
  function mt(e) {
    return Array.isArray(e) ? e : ("string" == typeof e && e.match(M)) || [];
  }
  b.fn.extend({
    prop: function (e, t) {
      return F(this, b.prop, e, t, arguments.length > 1);
    },
    removeProp: function (e) {
      return this.each(function () {
        delete this[b.propFix[e] || e];
      });
    },
  }),
    b.extend({
      prop: function (e, t, n) {
        var r,
          i,
          o = e.nodeType;
        if (3 !== o && 8 !== o && 2 !== o)
          return (
            (1 === o && b.isXMLDoc(e)) ||
              ((t = b.propFix[t] || t), (i = b.propHooks[t])),
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
            var t = b.find.attr(e, "tabindex");
            return t
              ? parseInt(t, 10)
              : lt.test(e.nodeName) || (dt.test(e.nodeName) && e.href)
              ? 0
              : -1;
          },
        },
      },
      propFix: { for: "htmlFor", class: "className" },
    }),
    h.optSelected ||
      (b.propHooks.selected = {
        get: function (e) {
          var t = e.parentNode;
          return t && t.parentNode && t.parentNode.selectedIndex, null;
        },
        set: function (e) {
          var t = e.parentNode;
          t && (t.selectedIndex, t.parentNode && t.parentNode.selectedIndex);
        },
      }),
    b.each(
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
        b.propFix[this.toLowerCase()] = this;
      }
    ),
    b.fn.extend({
      addClass: function (e) {
        var t,
          n,
          r,
          i,
          o,
          u,
          a,
          c = 0;
        if (p(e))
          return this.each(function (t) {
            b(this).addClass(e.call(this, t, pt(this)));
          });
        if ((t = mt(e)).length)
          for (; (n = this[c++]); )
            if (((i = pt(n)), (r = 1 === n.nodeType && " " + ht(i) + " "))) {
              for (u = 0; (o = t[u++]); )
                r.indexOf(" " + o + " ") < 0 && (r += o + " ");
              i !== (a = ht(r)) && n.setAttribute("class", a);
            }
        return this;
      },
      removeClass: function (e) {
        var t,
          n,
          r,
          i,
          o,
          u,
          a,
          c = 0;
        if (p(e))
          return this.each(function (t) {
            b(this).removeClass(e.call(this, t, pt(this)));
          });
        if (!arguments.length) return this.attr("class", "");
        if ((t = mt(e)).length)
          for (; (n = this[c++]); )
            if (((i = pt(n)), (r = 1 === n.nodeType && " " + ht(i) + " "))) {
              for (u = 0; (o = t[u++]); )
                for (; r.indexOf(" " + o + " ") > -1; )
                  r = r.replace(" " + o + " ", " ");
              i !== (a = ht(r)) && n.setAttribute("class", a);
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
              b(this).toggleClass(e.call(this, n, pt(this), t), t);
            })
          : this.each(function () {
              var t, i, o, u;
              if (r)
                for (i = 0, o = b(this), u = mt(e); (t = u[i++]); )
                  o.hasClass(t) ? o.removeClass(t) : o.addClass(t);
              else
                (void 0 !== e && "boolean" !== n) ||
                  ((t = pt(this)) && Y.set(this, "__className__", t),
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
          if (1 === n.nodeType && (" " + ht(pt(n)) + " ").indexOf(t) > -1)
            return !0;
        return !1;
      },
    });
  var vt = /\r/g;
  b.fn.extend({
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
              (null == (i = r ? e.call(this, n, b(this).val()) : e)
                ? (i = "")
                : "number" == typeof i
                ? (i += "")
                : Array.isArray(i) &&
                  (i = b.map(i, function (e) {
                    return null == e ? "" : e + "";
                  })),
              ((t =
                b.valHooks[this.type] ||
                b.valHooks[this.nodeName.toLowerCase()]) &&
                "set" in t &&
                void 0 !== t.set(this, i, "value")) ||
                (this.value = i));
          }))
        : i
        ? (t = b.valHooks[i.type] || b.valHooks[i.nodeName.toLowerCase()]) &&
          "get" in t &&
          void 0 !== (n = t.get(i, "value"))
          ? n
          : "string" == typeof (n = i.value)
          ? n.replace(vt, "")
          : null == n
          ? ""
          : n
        : void 0;
    },
  }),
    b.extend({
      valHooks: {
        option: {
          get: function (e) {
            var t = b.find.attr(e, "value");
            return null != t ? t : ht(b.text(e));
          },
        },
        select: {
          get: function (e) {
            var t,
              n,
              r,
              i = e.options,
              o = e.selectedIndex,
              u = "select-one" === e.type,
              a = u ? null : [],
              c = u ? o + 1 : i.length;
            for (r = o < 0 ? c : u ? o : 0; r < c; r++)
              if (
                ((n = i[r]).selected || r === o) &&
                !n.disabled &&
                (!n.parentNode.disabled || !T(n.parentNode, "optgroup"))
              ) {
                if (((t = b(n).val()), u)) return t;
                a.push(t);
              }
            return a;
          },
          set: function (e, t) {
            for (
              var n, r, i = e.options, o = b.makeArray(t), u = i.length;
              u--;

            )
              ((r = i[u]).selected =
                b.inArray(b.valHooks.option.get(r), o) > -1) && (n = !0);
            return n || (e.selectedIndex = -1), o;
          },
        },
      },
    }),
    b.each(["radio", "checkbox"], function () {
      (b.valHooks[this] = {
        set: function (e, t) {
          if (Array.isArray(t))
            return (e.checked = b.inArray(b(e).val(), t) > -1);
        },
      }),
        h.checkOn ||
          (b.valHooks[this].get = function (e) {
            return null === e.getAttribute("value") ? "on" : e.value;
          });
    }),
    (h.focusin = "onfocusin" in e);
  var gt = /^(?:focusinfocus|focusoutblur)$/,
    yt = function (e) {
      e.stopPropagation();
    };
  b.extend(b.event, {
    trigger: function (t, n, r, i) {
      var o,
        u,
        a,
        c,
        s,
        l,
        d,
        h,
        g = [r || v],
        y = f.call(t, "type") ? t.type : t,
        w = f.call(t, "namespace") ? t.namespace.split(".") : [];
      if (
        ((u = h = a = r = r || v),
        3 !== r.nodeType &&
          8 !== r.nodeType &&
          !gt.test(y + b.event.triggered) &&
          (y.indexOf(".") > -1 &&
            ((w = y.split(".")), (y = w.shift()), w.sort()),
          (s = y.indexOf(":") < 0 && "on" + y),
          ((t = t[b.expando]
            ? t
            : new b.Event(y, "object" == typeof t && t)).isTrigger = i ? 2 : 3),
          (t.namespace = w.join(".")),
          (t.rnamespace = t.namespace
            ? new RegExp("(^|\\.)" + w.join("\\.(?:.*\\.|)") + "(\\.|$)")
            : null),
          (t.result = void 0),
          t.target || (t.target = r),
          (n = null == n ? [t] : b.makeArray(n, [t])),
          (d = b.event.special[y] || {}),
          i || !d.trigger || !1 !== d.trigger.apply(r, n)))
      ) {
        if (!i && !d.noBubble && !m(r)) {
          for (
            c = d.delegateType || y, gt.test(c + y) || (u = u.parentNode);
            u;
            u = u.parentNode
          )
            g.push(u), (a = u);
          a === (r.ownerDocument || v) &&
            g.push(a.defaultView || a.parentWindow || e);
        }
        for (o = 0; (u = g[o++]) && !t.isPropagationStopped(); )
          (h = u),
            (t.type = o > 1 ? c : d.bindType || y),
            (l =
              (Y.get(u, "events") || Object.create(null))[t.type] &&
              Y.get(u, "handle")) && l.apply(u, n),
            (l = s && u[s]) &&
              l.apply &&
              V(u) &&
              ((t.result = l.apply(u, n)),
              !1 === t.result && t.preventDefault());
        return (
          (t.type = y),
          i ||
            t.isDefaultPrevented() ||
            (d._default && !1 !== d._default.apply(g.pop(), n)) ||
            !V(r) ||
            (s &&
              p(r[y]) &&
              !m(r) &&
              ((a = r[s]) && (r[s] = null),
              (b.event.triggered = y),
              t.isPropagationStopped() && h.addEventListener(y, yt),
              r[y](),
              t.isPropagationStopped() && h.removeEventListener(y, yt),
              (b.event.triggered = void 0),
              a && (r[s] = a))),
          t.result
        );
      }
    },
    simulate: function (e, t, n) {
      var r = b.extend(new b.Event(), n, { type: e, isSimulated: !0 });
      b.event.trigger(r, null, t);
    },
  }),
    b.fn.extend({
      trigger: function (e, t) {
        return this.each(function () {
          b.event.trigger(e, t, this);
        });
      },
      triggerHandler: function (e, t) {
        var n = this[0];
        if (n) return b.event.trigger(e, t, n, !0);
      },
    }),
    h.focusin ||
      b.each({ focus: "focusin", blur: "focusout" }, function (e, t) {
        var n = function (e) {
          b.event.simulate(t, e.target, b.event.fix(e));
        };
        b.event.special[t] = {
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
  var wt = e.location,
    bt = { guid: Date.now() },
    _t = /\?/;
  b.parseXML = function (t) {
    var n, r;
    if (!t || "string" != typeof t) return null;
    try {
      n = new e.DOMParser().parseFromString(t, "text/xml");
    } catch (e) {}
    return (
      (r = n && n.getElementsByTagName("parsererror")[0]),
      (n && !r) ||
        b.error(
          "Invalid XML: " +
            (r
              ? b
                  .map(r.childNodes, function (e) {
                    return e.textContent;
                  })
                  .join("\n")
              : t)
        ),
      n
    );
  };
  var xt = /\[\]$/,
    kt = /\r?\n/g,
    jt = /^(?:submit|button|image|reset|file)$/i,
    St = /^(?:input|select|textarea|keygen)/i;
  function Tt(e, t, n, r) {
    var i;
    if (Array.isArray(t))
      b.each(t, function (t, i) {
        n || xt.test(e)
          ? r(e, i)
          : Tt(
              e + "[" + ("object" == typeof i && null != i ? t : "") + "]",
              i,
              n,
              r
            );
      });
    else if (n || "object" !== w(t)) r(e, t);
    else for (i in t) Tt(e + "[" + i + "]", t[i], n, r);
  }
  (b.param = function (e, t) {
    var n,
      r = [],
      i = function (e, t) {
        var n = p(t) ? t() : t;
        r[r.length] =
          encodeURIComponent(e) + "=" + encodeURIComponent(null == n ? "" : n);
      };
    if (null == e) return "";
    if (Array.isArray(e) || (e.jquery && !b.isPlainObject(e)))
      b.each(e, function () {
        i(this.name, this.value);
      });
    else for (n in e) Tt(n, e[n], t, i);
    return r.join("&");
  }),
    b.fn.extend({
      serialize: function () {
        return b.param(this.serializeArray());
      },
      serializeArray: function () {
        return this.map(function () {
          var e = b.prop(this, "elements");
          return e ? b.makeArray(e) : this;
        })
          .filter(function () {
            var e = this.type;
            return (
              this.name &&
              !b(this).is(":disabled") &&
              St.test(this.nodeName) &&
              !jt.test(e) &&
              (this.checked || !de.test(e))
            );
          })
          .map(function (e, t) {
            var n = b(this).val();
            return null == n
              ? null
              : Array.isArray(n)
              ? b.map(n, function (e) {
                  return { name: t.name, value: e.replace(kt, "\r\n") };
                })
              : { name: t.name, value: n.replace(kt, "\r\n") };
          })
          .get();
      },
    });
  var Ct = /%20/g,
    Et = /#.*$/,
    qt = /([?&])_=[^&]*/,
    At = /^(.*?):[ \t]*([^\r\n]*)$/gm,
    Dt = /^(?:GET|HEAD)$/,
    Nt = /^\/\//,
    Ot = {},
    Mt = {},
    Lt = "*/".concat("*"),
    Rt = v.createElement("a");
  function It(e) {
    return function (t, n) {
      "string" != typeof t && ((n = t), (t = "*"));
      var r,
        i = 0,
        o = t.toLowerCase().match(M) || [];
      if (p(n))
        for (; (r = o[i++]); )
          "+" === r[0]
            ? ((r = r.slice(1) || "*"), (e[r] = e[r] || []).unshift(n))
            : (e[r] = e[r] || []).push(n);
    };
  }
  function Ht(e, t, n, r) {
    var i = {},
      o = e === Mt;
    function u(a) {
      var c;
      return (
        (i[a] = !0),
        b.each(e[a] || [], function (e, a) {
          var s = a(t, n, r);
          return "string" != typeof s || o || i[s]
            ? o
              ? !(c = s)
              : void 0
            : (t.dataTypes.unshift(s), u(s), !1);
        }),
        c
      );
    }
    return u(t.dataTypes[0]) || (!i["*"] && u("*"));
  }
  function Pt(e, t) {
    var n,
      r,
      i = b.ajaxSettings.flatOptions || {};
    for (n in t) void 0 !== t[n] && ((i[n] ? e : r || (r = {}))[n] = t[n]);
    return r && b.extend(!0, e, r), e;
  }
  (Rt.href = wt.href),
    b.extend({
      active: 0,
      lastModified: {},
      etag: {},
      ajaxSettings: {
        url: wt.href,
        type: "GET",
        isLocal: /^(?:about|app|app-storage|.+-extension|file|res|widget):$/.test(
          wt.protocol
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
          "text xml": b.parseXML,
        },
        flatOptions: { url: !0, context: !0 },
      },
      ajaxSetup: function (e, t) {
        return t ? Pt(Pt(e, b.ajaxSettings), t) : Pt(b.ajaxSettings, e);
      },
      ajaxPrefilter: It(Ot),
      ajaxTransport: It(Mt),
      ajax: function (t, n) {
        "object" == typeof t && ((n = t), (t = void 0)), (n = n || {});
        var r,
          i,
          o,
          u,
          a,
          c,
          s,
          f,
          l,
          d,
          h = b.ajaxSetup({}, n),
          p = h.context || h,
          m = h.context && (p.nodeType || p.jquery) ? b(p) : b.event,
          g = b.Deferred(),
          y = b.Callbacks("once memory"),
          w = h.statusCode || {},
          _ = {},
          x = {},
          k = "canceled",
          j = {
            readyState: 0,
            getResponseHeader: function (e) {
              var t;
              if (s) {
                if (!u)
                  for (u = {}; (t = At.exec(o)); )
                    u[t[1].toLowerCase() + " "] = (
                      u[t[1].toLowerCase() + " "] || []
                    ).concat(t[2]);
                t = u[e.toLowerCase() + " "];
              }
              return null == t ? null : t.join(", ");
            },
            getAllResponseHeaders: function () {
              return s ? o : null;
            },
            setRequestHeader: function (e, t) {
              return (
                null == s &&
                  ((e = x[e.toLowerCase()] = x[e.toLowerCase()] || e),
                  (_[e] = t)),
                this
              );
            },
            overrideMimeType: function (e) {
              return null == s && (h.mimeType = e), this;
            },
            statusCode: function (e) {
              var t;
              if (e)
                if (s) j.always(e[j.status]);
                else for (t in e) w[t] = [w[t], e[t]];
              return this;
            },
            abort: function (e) {
              var t = e || k;
              return r && r.abort(t), S(0, t), this;
            },
          };
        if (
          (g.promise(j),
          (h.url = ((t || h.url || wt.href) + "").replace(
            Nt,
            wt.protocol + "//"
          )),
          (h.type = n.method || n.type || h.method || h.type),
          (h.dataTypes = (h.dataType || "*").toLowerCase().match(M) || [""]),
          null == h.crossDomain)
        ) {
          c = v.createElement("a");
          try {
            (c.href = h.url),
              (c.href = c.href),
              (h.crossDomain =
                Rt.protocol + "//" + Rt.host != c.protocol + "//" + c.host);
          } catch (e) {
            h.crossDomain = !0;
          }
        }
        if (
          (h.data &&
            h.processData &&
            "string" != typeof h.data &&
            (h.data = b.param(h.data, h.traditional)),
          Ht(Ot, h, n, j),
          s)
        )
          return j;
        for (l in ((f = b.event && h.global) &&
          0 == b.active++ &&
          b.event.trigger("ajaxStart"),
        (h.type = h.type.toUpperCase()),
        (h.hasContent = !Dt.test(h.type)),
        (i = h.url.replace(Et, "")),
        h.hasContent
          ? h.data &&
            h.processData &&
            0 ===
              (h.contentType || "").indexOf(
                "application/x-www-form-urlencoded"
              ) &&
            (h.data = h.data.replace(Ct, "+"))
          : ((d = h.url.slice(i.length)),
            h.data &&
              (h.processData || "string" == typeof h.data) &&
              ((i += (_t.test(i) ? "&" : "?") + h.data), delete h.data),
            !1 === h.cache &&
              ((i = i.replace(qt, "$1")),
              (d = (_t.test(i) ? "&" : "?") + "_=" + bt.guid++ + d)),
            (h.url = i + d)),
        h.ifModified &&
          (b.lastModified[i] &&
            j.setRequestHeader("If-Modified-Since", b.lastModified[i]),
          b.etag[i] && j.setRequestHeader("If-None-Match", b.etag[i])),
        ((h.data && h.hasContent && !1 !== h.contentType) || n.contentType) &&
          j.setRequestHeader("Content-Type", h.contentType),
        j.setRequestHeader(
          "Accept",
          h.dataTypes[0] && h.accepts[h.dataTypes[0]]
            ? h.accepts[h.dataTypes[0]] +
                ("*" !== h.dataTypes[0] ? ", " + Lt + "; q=0.01" : "")
            : h.accepts["*"]
        ),
        h.headers))
          j.setRequestHeader(l, h.headers[l]);
        if (h.beforeSend && (!1 === h.beforeSend.call(p, j, h) || s))
          return j.abort();
        if (
          ((k = "abort"),
          y.add(h.complete),
          j.done(h.success),
          j.fail(h.error),
          (r = Ht(Mt, h, n, j)))
        ) {
          if (((j.readyState = 1), f && m.trigger("ajaxSend", [j, h]), s))
            return j;
          h.async &&
            h.timeout > 0 &&
            (a = e.setTimeout(function () {
              j.abort("timeout");
            }, h.timeout));
          try {
            (s = !1), r.send(_, S);
          } catch (e) {
            if (s) throw e;
            S(-1, e);
          }
        } else S(-1, "No Transport");
        function S(t, n, u, c) {
          var l,
            d,
            v,
            _,
            x,
            k = n;
          s ||
            ((s = !0),
            a && e.clearTimeout(a),
            (r = void 0),
            (o = c || ""),
            (j.readyState = t > 0 ? 4 : 0),
            (l = (t >= 200 && t < 300) || 304 === t),
            u &&
              (_ = (function (e, t, n) {
                for (
                  var r, i, o, u, a = e.contents, c = e.dataTypes;
                  "*" === c[0];

                )
                  c.shift(),
                    void 0 === r &&
                      (r = e.mimeType || t.getResponseHeader("Content-Type"));
                if (r)
                  for (i in a)
                    if (a[i] && a[i].test(r)) {
                      c.unshift(i);
                      break;
                    }
                if (c[0] in n) o = c[0];
                else {
                  for (i in n) {
                    if (!c[0] || e.converters[i + " " + c[0]]) {
                      o = i;
                      break;
                    }
                    u || (u = i);
                  }
                  o = o || u;
                }
                if (o) return o !== c[0] && c.unshift(o), n[o];
              })(h, j, u)),
            !l &&
              b.inArray("script", h.dataTypes) > -1 &&
              b.inArray("json", h.dataTypes) < 0 &&
              (h.converters["text script"] = function () {}),
            (_ = (function (e, t, n, r) {
              var i,
                o,
                u,
                a,
                c,
                s = {},
                f = e.dataTypes.slice();
              if (f[1])
                for (u in e.converters) s[u.toLowerCase()] = e.converters[u];
              for (o = f.shift(); o; )
                if (
                  (e.responseFields[o] && (n[e.responseFields[o]] = t),
                  !c && r && e.dataFilter && (t = e.dataFilter(t, e.dataType)),
                  (c = o),
                  (o = f.shift()))
                )
                  if ("*" === o) o = c;
                  else if ("*" !== c && c !== o) {
                    if (!(u = s[c + " " + o] || s["* " + o]))
                      for (i in s)
                        if (
                          (a = i.split(" "))[1] === o &&
                          (u = s[c + " " + a[0]] || s["* " + a[0]])
                        ) {
                          !0 === u
                            ? (u = s[i])
                            : !0 !== s[i] && ((o = a[0]), f.unshift(a[1]));
                          break;
                        }
                    if (!0 !== u)
                      if (u && e.throws) t = u(t);
                      else
                        try {
                          t = u(t);
                        } catch (e) {
                          return {
                            state: "parsererror",
                            error: u
                              ? e
                              : "No conversion from " + c + " to " + o,
                          };
                        }
                  }
              return { state: "success", data: t };
            })(h, _, j, l)),
            l
              ? (h.ifModified &&
                  ((x = j.getResponseHeader("Last-Modified")) &&
                    (b.lastModified[i] = x),
                  (x = j.getResponseHeader("etag")) && (b.etag[i] = x)),
                204 === t || "HEAD" === h.type
                  ? (k = "nocontent")
                  : 304 === t
                  ? (k = "notmodified")
                  : ((k = _.state), (d = _.data), (l = !(v = _.error))))
              : ((v = k), (!t && k) || ((k = "error"), t < 0 && (t = 0))),
            (j.status = t),
            (j.statusText = (n || k) + ""),
            l ? g.resolveWith(p, [d, k, j]) : g.rejectWith(p, [j, k, v]),
            j.statusCode(w),
            (w = void 0),
            f && m.trigger(l ? "ajaxSuccess" : "ajaxError", [j, h, l ? d : v]),
            y.fireWith(p, [j, k]),
            f &&
              (m.trigger("ajaxComplete", [j, h]),
              --b.active || b.event.trigger("ajaxStop")));
        }
        return j;
      },
      getJSON: function (e, t, n) {
        return b.get(e, t, n, "json");
      },
      getScript: function (e, t) {
        return b.get(e, void 0, t, "script");
      },
    }),
    b.each(["get", "post"], function (e, t) {
      b[t] = function (e, n, r, i) {
        return (
          p(n) && ((i = i || r), (r = n), (n = void 0)),
          b.ajax(
            b.extend(
              { url: e, type: t, dataType: i, data: n, success: r },
              b.isPlainObject(e) && e
            )
          )
        );
      };
    }),
    b.ajaxPrefilter(function (e) {
      var t;
      for (t in e.headers)
        "content-type" === t.toLowerCase() &&
          (e.contentType = e.headers[t] || "");
    }),
    (b._evalUrl = function (e, t, n) {
      return b.ajax({
        url: e,
        type: "GET",
        dataType: "script",
        cache: !0,
        async: !1,
        global: !1,
        converters: { "text script": function () {} },
        dataFilter: function (e) {
          b.globalEval(e, t, n);
        },
      });
    }),
    b.fn.extend({
      wrapAll: function (e) {
        var t;
        return (
          this[0] &&
            (p(e) && (e = e.call(this[0])),
            (t = b(e, this[0].ownerDocument).eq(0).clone(!0)),
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
              b(this).wrapInner(e.call(this, t));
            })
          : this.each(function () {
              var t = b(this),
                n = t.contents();
              n.length ? n.wrapAll(e) : t.append(e);
            });
      },
      wrap: function (e) {
        var t = p(e);
        return this.each(function (n) {
          b(this).wrapAll(t ? e.call(this, n) : e);
        });
      },
      unwrap: function (e) {
        return (
          this.parent(e)
            .not("body")
            .each(function () {
              b(this).replaceWith(this.childNodes);
            }),
          this
        );
      },
    }),
    (b.expr.pseudos.hidden = function (e) {
      return !b.expr.pseudos.visible(e);
    }),
    (b.expr.pseudos.visible = function (e) {
      return !!(e.offsetWidth || e.offsetHeight || e.getClientRects().length);
    }),
    (b.ajaxSettings.xhr = function () {
      try {
        return new e.XMLHttpRequest();
      } catch (e) {}
    });
  var zt = { 0: 200, 1223: 204 },
    Ft = b.ajaxSettings.xhr();
  (h.cors = !!Ft && "withCredentials" in Ft),
    (h.ajax = Ft = !!Ft),
    b.ajaxTransport(function (t) {
      var n, r;
      if (h.cors || (Ft && !t.crossDomain))
        return {
          send: function (i, o) {
            var u,
              a = t.xhr();
            if (
              (a.open(t.type, t.url, t.async, t.username, t.password),
              t.xhrFields)
            )
              for (u in t.xhrFields) a[u] = t.xhrFields[u];
            for (u in (t.mimeType &&
              a.overrideMimeType &&
              a.overrideMimeType(t.mimeType),
            t.crossDomain ||
              i["X-Requested-With"] ||
              (i["X-Requested-With"] = "XMLHttpRequest"),
            i))
              a.setRequestHeader(u, i[u]);
            (n = function (e) {
              return function () {
                n &&
                  ((n = r = a.onload = a.onerror = a.onabort = a.ontimeout = a.onreadystatechange = null),
                  "abort" === e
                    ? a.abort()
                    : "error" === e
                    ? "number" != typeof a.status
                      ? o(0, "error")
                      : o(a.status, a.statusText)
                    : o(
                        zt[a.status] || a.status,
                        a.statusText,
                        "text" !== (a.responseType || "text") ||
                          "string" != typeof a.responseText
                          ? { binary: a.response }
                          : { text: a.responseText },
                        a.getAllResponseHeaders()
                      ));
              };
            }),
              (a.onload = n()),
              (r = a.onerror = a.ontimeout = n("error")),
              void 0 !== a.onabort
                ? (a.onabort = r)
                : (a.onreadystatechange = function () {
                    4 === a.readyState &&
                      e.setTimeout(function () {
                        n && r();
                      });
                  }),
              (n = n("abort"));
            try {
              a.send((t.hasContent && t.data) || null);
            } catch (e) {
              if (n) throw e;
            }
          },
          abort: function () {
            n && n();
          },
        };
    }),
    b.ajaxPrefilter(function (e) {
      e.crossDomain && (e.contents.script = !1);
    }),
    b.ajaxSetup({
      accepts: {
        script:
          "text/javascript, application/javascript, application/ecmascript, application/x-ecmascript",
      },
      contents: { script: /\b(?:java|ecma)script\b/ },
      converters: {
        "text script": function (e) {
          return b.globalEval(e), e;
        },
      },
    }),
    b.ajaxPrefilter("script", function (e) {
      void 0 === e.cache && (e.cache = !1), e.crossDomain && (e.type = "GET");
    }),
    b.ajaxTransport("script", function (e) {
      var t, n;
      if (e.crossDomain || e.scriptAttrs)
        return {
          send: function (r, i) {
            (t = b("<script>")
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
              v.head.appendChild(t[0]);
          },
          abort: function () {
            n && n();
          },
        };
    });
  var Bt,
    $t = [],
    Wt = /(=)\?(?=&|$)|\?\?/;
  b.ajaxSetup({
    jsonp: "callback",
    jsonpCallback: function () {
      var e = $t.pop() || b.expando + "_" + bt.guid++;
      return (this[e] = !0), e;
    },
  }),
    b.ajaxPrefilter("json jsonp", function (t, n, r) {
      var i,
        o,
        u,
        a =
          !1 !== t.jsonp &&
          (Wt.test(t.url)
            ? "url"
            : "string" == typeof t.data &&
              0 ===
                (t.contentType || "").indexOf(
                  "application/x-www-form-urlencoded"
                ) &&
              Wt.test(t.data) &&
              "data");
      if (a || "jsonp" === t.dataTypes[0])
        return (
          (i = t.jsonpCallback = p(t.jsonpCallback)
            ? t.jsonpCallback()
            : t.jsonpCallback),
          a
            ? (t[a] = t[a].replace(Wt, "$1" + i))
            : !1 !== t.jsonp &&
              (t.url += (_t.test(t.url) ? "&" : "?") + t.jsonp + "=" + i),
          (t.converters["script json"] = function () {
            return u || b.error(i + " was not called"), u[0];
          }),
          (t.dataTypes[0] = "json"),
          (o = e[i]),
          (e[i] = function () {
            u = arguments;
          }),
          r.always(function () {
            void 0 === o ? b(e).removeProp(i) : (e[i] = o),
              t[i] && ((t.jsonpCallback = n.jsonpCallback), $t.push(i)),
              u && p(o) && o(u[0]),
              (u = o = void 0);
          }),
          "script"
        );
    }),
    (h.createHTMLDocument =
      (((Bt = v.implementation.createHTMLDocument("").body).innerHTML =
        "<form></form><form></form>"),
      2 === Bt.childNodes.length)),
    (b.parseHTML = function (e, t, n) {
      return "string" != typeof e
        ? []
        : ("boolean" == typeof t && ((n = t), (t = !1)),
          t ||
            (h.createHTMLDocument
              ? (((r = (t = v.implementation.createHTMLDocument(
                  ""
                )).createElement("base")).href = v.location.href),
                t.head.appendChild(r))
              : (t = v)),
          (o = !n && []),
          (i = C.exec(e))
            ? [t.createElement(i[1])]
            : ((i = we([e], t, o)),
              o && o.length && b(o).remove(),
              b.merge([], i.childNodes)));
      var r, i, o;
    }),
    (b.fn.load = function (e, t, n) {
      var r,
        i,
        o,
        u = this,
        a = e.indexOf(" ");
      return (
        a > -1 && ((r = ht(e.slice(a))), (e = e.slice(0, a))),
        p(t)
          ? ((n = t), (t = void 0))
          : t && "object" == typeof t && (i = "POST"),
        u.length > 0 &&
          b
            .ajax({ url: e, type: i || "GET", dataType: "html", data: t })
            .done(function (e) {
              (o = arguments),
                u.html(r ? b("<div>").append(b.parseHTML(e)).find(r) : e);
            })
            .always(
              n &&
                function (e, t) {
                  u.each(function () {
                    n.apply(this, o || [e.responseText, t, e]);
                  });
                }
            ),
        this
      );
    }),
    (b.expr.pseudos.animated = function (e) {
      return b.grep(b.timers, function (t) {
        return e === t.elem;
      }).length;
    }),
    (b.offset = {
      setOffset: function (e, t, n) {
        var r,
          i,
          o,
          u,
          a,
          c,
          s = b.css(e, "position"),
          f = b(e),
          l = {};
        "static" === s && (e.style.position = "relative"),
          (a = f.offset()),
          (o = b.css(e, "top")),
          (c = b.css(e, "left")),
          ("absolute" === s || "fixed" === s) && (o + c).indexOf("auto") > -1
            ? ((u = (r = f.position()).top), (i = r.left))
            : ((u = parseFloat(o) || 0), (i = parseFloat(c) || 0)),
          p(t) && (t = t.call(e, n, b.extend({}, a))),
          null != t.top && (l.top = t.top - a.top + u),
          null != t.left && (l.left = t.left - a.left + i),
          "using" in t ? t.using.call(e, l) : f.css(l);
      },
    }),
    b.fn.extend({
      offset: function (e) {
        if (arguments.length)
          return void 0 === e
            ? this
            : this.each(function (t) {
                b.offset.setOffset(this, e, t);
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
          if ("fixed" === b.css(r, "position")) t = r.getBoundingClientRect();
          else {
            for (
              t = this.offset(),
                n = r.ownerDocument,
                e = r.offsetParent || n.documentElement;
              e &&
              (e === n.body || e === n.documentElement) &&
              "static" === b.css(e, "position");

            )
              e = e.parentNode;
            e &&
              e !== r &&
              1 === e.nodeType &&
              (((i = b(e).offset()).top += b.css(e, "borderTopWidth", !0)),
              (i.left += b.css(e, "borderLeftWidth", !0)));
          }
          return {
            top: t.top - i.top - b.css(r, "marginTop", !0),
            left: t.left - i.left - b.css(r, "marginLeft", !0),
          };
        }
      },
      offsetParent: function () {
        return this.map(function () {
          for (
            var e = this.offsetParent;
            e && "static" === b.css(e, "position");

          )
            e = e.offsetParent;
          return e || ne;
        });
      },
    }),
    b.each({ scrollLeft: "pageXOffset", scrollTop: "pageYOffset" }, function (
      e,
      t
    ) {
      var n = "pageYOffset" === t;
      b.fn[e] = function (r) {
        return F(
          this,
          function (e, r, i) {
            var o;
            if (
              (m(e) ? (o = e) : 9 === e.nodeType && (o = e.defaultView),
              void 0 === i)
            )
              return o ? o[t] : e[r];
            o
              ? o.scrollTo(n ? o.pageXOffset : i, n ? i : o.pageYOffset)
              : (e[r] = i);
          },
          e,
          r,
          arguments.length
        );
      };
    }),
    b.each(["top", "left"], function (e, t) {
      b.cssHooks[t] = Fe(h.pixelPosition, function (e, n) {
        if (n)
          return (n = ze(e, t)), Re.test(n) ? b(e).position()[t] + "px" : n;
      });
    }),
    b.each({ Height: "height", Width: "width" }, function (e, t) {
      b.each({ padding: "inner" + e, content: t, "": "outer" + e }, function (
        n,
        r
      ) {
        b.fn[r] = function (i, o) {
          var u = arguments.length && (n || "boolean" != typeof i),
            a = n || (!0 === i || !0 === o ? "margin" : "border");
          return F(
            this,
            function (t, n, i) {
              var o;
              return m(t)
                ? 0 === r.indexOf("outer")
                  ? t["inner" + e]
                  : t.document.documentElement["client" + e]
                : 9 === t.nodeType
                ? ((o = t.documentElement),
                  Math.max(
                    t.body["scroll" + e],
                    o["scroll" + e],
                    t.body["offset" + e],
                    o["offset" + e],
                    o["client" + e]
                  ))
                : void 0 === i
                ? b.css(t, n, a)
                : b.style(t, n, i, a);
            },
            t,
            u ? i : void 0,
            u
          );
        };
      });
    }),
    b.each(
      [
        "ajaxStart",
        "ajaxStop",
        "ajaxComplete",
        "ajaxError",
        "ajaxSuccess",
        "ajaxSend",
      ],
      function (e, t) {
        b.fn[t] = function (e) {
          return this.on(t, e);
        };
      }
    ),
    b.fn.extend({
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
    b.each(
      "blur focus focusin focusout resize scroll click dblclick mousedown mouseup mousemove mouseover mouseout mouseenter mouseleave change select submit keydown keypress keyup contextmenu".split(
        " "
      ),
      function (e, t) {
        b.fn[t] = function (e, n) {
          return arguments.length > 0
            ? this.on(t, null, e, n)
            : this.trigger(t);
        };
      }
    );
  var Ut = /^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g;
  (b.proxy = function (e, t) {
    var n, r, o;
    if (("string" == typeof t && ((n = e[t]), (t = e), (e = n)), p(e)))
      return (
        (r = i.call(arguments, 2)),
        ((o = function () {
          return e.apply(t || this, r.concat(i.call(arguments)));
        }).guid = e.guid = e.guid || b.guid++),
        o
      );
  }),
    (b.holdReady = function (e) {
      e ? b.readyWait++ : b.ready(!0);
    }),
    (b.isArray = Array.isArray),
    (b.parseJSON = JSON.parse),
    (b.nodeName = T),
    (b.isFunction = p),
    (b.isWindow = m),
    (b.camelCase = U),
    (b.type = w),
    (b.now = Date.now),
    (b.isNumeric = function (e) {
      var t = b.type(e);
      return ("number" === t || "string" === t) && !isNaN(e - parseFloat(e));
    }),
    (b.trim = function (e) {
      return null == e ? "" : (e + "").replace(Ut, "");
    }),
    "function" == typeof define &&
      define.amd &&
      define("jquery", [], function () {
        return b;
      });
  var Vt = e.jQuery,
    Xt = e.$;
  return (
    (b.noConflict = function (t) {
      return e.$ === b && (e.$ = Xt), t && e.jQuery === b && (e.jQuery = Vt), b;
    }),
    void 0 === t && (e.jQuery = e.$ = b),
    b
  );
});
var BiwaScheme = (function () {
  const e = {},
    t = {},
    n = {
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
    r = new Object();
  r.toString = function () {
    return "#<undef>";
  };
  var o =
      ("object" == typeof self && self.self === self && self) ||
      ("object" == typeof global && global.global === global && global) ||
      Function("return this")() ||
      {},
    u = Array.prototype,
    a = Object.prototype,
    c = "undefined" != typeof Symbol ? Symbol.prototype : null,
    s = u.push,
    f = u.slice,
    l = a.toString,
    d = a.hasOwnProperty,
    h = "undefined" != typeof ArrayBuffer,
    p = "undefined" != typeof DataView,
    m = Array.isArray,
    v = Object.keys,
    g = Object.create,
    y = h && ArrayBuffer.isView,
    w = isNaN,
    b = isFinite,
    _ = !{ toString: null }.propertyIsEnumerable("toString"),
    x = [
      "valueOf",
      "isPrototypeOf",
      "toString",
      "propertyIsEnumerable",
      "hasOwnProperty",
      "toLocaleString",
    ],
    k = Math.pow(2, 53) - 1;
  function j(e, t) {
    return (
      (t = null == t ? e.length - 1 : +t),
      function () {
        for (
          var n = Math.max(arguments.length - t, 0), r = Array(n), i = 0;
          i < n;
          i++
        )
          r[i] = arguments[i + t];
        switch (t) {
          case 0:
            return e.call(this, r);
          case 1:
            return e.call(this, arguments[0], r);
          case 2:
            return e.call(this, arguments[0], arguments[1], r);
        }
        var o = Array(t + 1);
        for (i = 0; i < t; i++) o[i] = arguments[i];
        return (o[t] = r), e.apply(this, o);
      }
    );
  }
  function S(e) {
    var t = typeof e;
    return "function" === t || ("object" === t && !!e);
  }
  function T(e) {
    return null === e;
  }
  function C(e) {
    return void 0 === e;
  }
  function E(e) {
    return !0 === e || !1 === e || "[object Boolean]" === l.call(e);
  }
  function q(e) {
    var t = "[object " + e + "]";
    return function (e) {
      return l.call(e) === t;
    };
  }
  var A = q("String"),
    D = q("Number"),
    N = q("Date"),
    O = q("RegExp"),
    M = q("Error"),
    L = q("Symbol"),
    R = q("ArrayBuffer"),
    I = q("Function"),
    H = o.document && o.document.childNodes;
  "function" != typeof /./ &&
    "object" != typeof Int8Array &&
    "function" != typeof H &&
    (I = function (e) {
      return "function" == typeof e || !1;
    });
  var P = I,
    z = q("Object"),
    F = p && z(new DataView(new ArrayBuffer(8))),
    B = "undefined" != typeof Map && z(new Map()),
    W = q("DataView");
  var U = F
      ? function (e) {
          return null != e && P(e.getInt8) && R(e.buffer);
        }
      : W,
    V = m || q("Array");
  function X(e, t) {
    return null != e && d.call(e, t);
  }
  var Y = q("Arguments");
  !(function () {
    Y(arguments) ||
      (Y = function (e) {
        return X(e, "callee");
      });
  })();
  var G = Y;
  function J(e) {
    return D(e) && w(e);
  }
  function Q(e) {
    return function () {
      return e;
    };
  }
  function K(e) {
    return function (t) {
      var n = e(t);
      return "number" == typeof n && n >= 0 && n <= k;
    };
  }
  function Z(e) {
    return function (t) {
      return null == t ? void 0 : t[e];
    };
  }
  var ee = Z("byteLength"),
    te = K(ee),
    ne = /\[object ((I|Ui)nt(8|16|32)|Float(32|64)|Uint8Clamped|Big(I|Ui)nt64)Array\]/;
  var re = h
      ? function (e) {
          return y ? y(e) && !U(e) : te(e) && ne.test(l.call(e));
        }
      : Q(!1),
    ie = Z("length");
  function oe(e, t) {
    t = (function (e) {
      for (var t = {}, n = e.length, r = 0; r < n; ++r) t[e[r]] = !0;
      return {
        contains: function (e) {
          return t[e];
        },
        push: function (n) {
          return (t[n] = !0), e.push(n);
        },
      };
    })(t);
    var n = x.length,
      r = e.constructor,
      i = (P(r) && r.prototype) || a,
      o = "constructor";
    for (X(e, o) && !t.contains(o) && t.push(o); n--; )
      (o = x[n]) in e && e[o] !== i[o] && !t.contains(o) && t.push(o);
  }
  function ue(e) {
    if (!S(e)) return [];
    if (v) return v(e);
    var t = [];
    for (var n in e) X(e, n) && t.push(n);
    return _ && oe(e, t), t;
  }
  function ae(e, t) {
    var n = ue(t),
      r = n.length;
    if (null == e) return !r;
    for (var i = Object(e), o = 0; o < r; o++) {
      var u = n[o];
      if (t[u] !== i[u] || !(u in i)) return !1;
    }
    return !0;
  }
  function ce(e) {
    return e instanceof ce
      ? e
      : this instanceof ce
      ? void (this._wrapped = e)
      : new ce(e);
  }
  function se(e) {
    return new Uint8Array(e.buffer || e, e.byteOffset || 0, ee(e));
  }
  (ce.VERSION = "1.13.1"),
    (ce.prototype.value = function () {
      return this._wrapped;
    }),
    (ce.prototype.valueOf = ce.prototype.toJSON = ce.prototype.value),
    (ce.prototype.toString = function () {
      return String(this._wrapped);
    });
  function fe(e, t, n, r) {
    if (e === t) return 0 !== e || 1 / e == 1 / t;
    if (null == e || null == t) return !1;
    if (e != e) return t != t;
    var i = typeof e;
    return (
      ("function" === i || "object" === i || "object" == typeof t) &&
      (function e(t, n, r, i) {
        t instanceof ce && (t = t._wrapped);
        n instanceof ce && (n = n._wrapped);
        var o = l.call(t);
        if (o !== l.call(n)) return !1;
        if (F && "[object Object]" == o && U(t)) {
          if (!U(n)) return !1;
          o = "[object DataView]";
        }
        switch (o) {
          case "[object RegExp]":
          case "[object String]":
            return "" + t == "" + n;
          case "[object Number]":
            return +t != +t ? +n != +n : 0 == +t ? 1 / +t == 1 / n : +t == +n;
          case "[object Date]":
          case "[object Boolean]":
            return +t == +n;
          case "[object Symbol]":
            return c.valueOf.call(t) === c.valueOf.call(n);
          case "[object ArrayBuffer]":
          case "[object DataView]":
            return e(se(t), se(n), r, i);
        }
        var u = "[object Array]" === o;
        if (!u && re(t)) {
          if (ee(t) !== ee(n)) return !1;
          if (t.buffer === n.buffer && t.byteOffset === n.byteOffset) return !0;
          u = !0;
        }
        if (!u) {
          if ("object" != typeof t || "object" != typeof n) return !1;
          var a = t.constructor,
            s = n.constructor;
          if (
            a !== s &&
            !(P(a) && a instanceof a && P(s) && s instanceof s) &&
            "constructor" in t &&
            "constructor" in n
          )
            return !1;
        }
        i = i || [];
        var f = (r = r || []).length;
        for (; f--; ) if (r[f] === t) return i[f] === n;
        if ((r.push(t), i.push(n), u)) {
          if ((f = t.length) !== n.length) return !1;
          for (; f--; ) if (!fe(t[f], n[f], r, i)) return !1;
        } else {
          var d,
            h = ue(t);
          if (((f = h.length), ue(n).length !== f)) return !1;
          for (; f--; )
            if (!X(n, (d = h[f])) || !fe(t[d], n[d], r, i)) return !1;
        }
        return r.pop(), i.pop(), !0;
      })(e, t, n, r)
    );
  }
  function le(e) {
    if (!S(e)) return [];
    var t = [];
    for (var n in e) t.push(n);
    return _ && oe(e, t), t;
  }
  function de(e) {
    var t = ie(e);
    return function (n) {
      if (null == n) return !1;
      var r = le(n);
      if (ie(r)) return !1;
      for (var i = 0; i < t; i++) if (!P(n[e[i]])) return !1;
      return e !== ge || !P(n[he]);
    };
  }
  var he = "forEach",
    pe = ["clear", "delete"],
    me = ["get", "has", "set"],
    ve = pe.concat(he, me),
    ge = pe.concat(me),
    ye = ["add"].concat(pe, he, "has"),
    we = B ? de(ve) : q("Map"),
    be = B ? de(ge) : q("WeakMap"),
    _e = B ? de(ye) : q("Set"),
    xe = q("WeakSet");
  function ke(e) {
    for (var t = ue(e), n = t.length, r = Array(n), i = 0; i < n; i++)
      r[i] = e[t[i]];
    return r;
  }
  function je(e) {
    for (var t = {}, n = ue(e), r = 0, i = n.length; r < i; r++)
      t[e[n[r]]] = n[r];
    return t;
  }
  function Se(e) {
    var t = [];
    for (var n in e) P(e[n]) && t.push(n);
    return t.sort();
  }
  function Te(e, t) {
    return function (n) {
      var r = arguments.length;
      if ((t && (n = Object(n)), r < 2 || null == n)) return n;
      for (var i = 1; i < r; i++)
        for (var o = arguments[i], u = e(o), a = u.length, c = 0; c < a; c++) {
          var s = u[c];
          (t && void 0 !== n[s]) || (n[s] = o[s]);
        }
      return n;
    };
  }
  var Ce = Te(le),
    Ee = Te(ue),
    qe = Te(le, !0);
  function Ae(e) {
    if (!S(e)) return {};
    if (g) return g(e);
    var t = function () {};
    t.prototype = e;
    var n = new t();
    return (t.prototype = null), n;
  }
  function De(e) {
    return S(e) ? (V(e) ? e.slice() : Ce({}, e)) : e;
  }
  function Ne(e) {
    return V(e) ? e : [e];
  }
  function Oe(e) {
    return ce.toPath(e);
  }
  function Me(e, t) {
    for (var n = t.length, r = 0; r < n; r++) {
      if (null == e) return;
      e = e[t[r]];
    }
    return n ? e : void 0;
  }
  function Le(e, t, n) {
    var r = Me(e, Oe(t));
    return C(r) ? n : r;
  }
  function Re(e) {
    return e;
  }
  function Ie(e) {
    return (
      (e = Ee({}, e)),
      function (t) {
        return ae(t, e);
      }
    );
  }
  function He(e) {
    return (
      (e = Oe(e)),
      function (t) {
        return Me(t, e);
      }
    );
  }
  function Pe(e, t, n) {
    if (void 0 === t) return e;
    switch (null == n ? 3 : n) {
      case 1:
        return function (n) {
          return e.call(t, n);
        };
      case 3:
        return function (n, r, i) {
          return e.call(t, n, r, i);
        };
      case 4:
        return function (n, r, i, o) {
          return e.call(t, n, r, i, o);
        };
    }
    return function () {
      return e.apply(t, arguments);
    };
  }
  function ze(e, t, n) {
    return null == e ? Re : P(e) ? Pe(e, t, n) : S(e) && !V(e) ? Ie(e) : He(e);
  }
  function Fe(e, t) {
    return ze(e, t, 1 / 0);
  }
  function Be(e, t, n) {
    return ce.iteratee !== Fe ? ce.iteratee(e, t) : ze(e, t, n);
  }
  function $e() {}
  function We(e, t, n) {
    var r = Array(Math.max(0, e));
    t = Pe(t, n, 1);
    for (var i = 0; i < e; i++) r[i] = t(i);
    return r;
  }
  function Ue(e, t) {
    return (
      null == t && ((t = e), (e = 0)),
      e + Math.floor(Math.random() * (t - e + 1))
    );
  }
  (ce.toPath = Ne), (ce.iteratee = Fe);
  var Ve =
    Date.now ||
    function () {
      return new Date().getTime();
    };
  function Xe(e) {
    var t = function (t) {
        return e[t];
      },
      n = "(?:" + ue(e).join("|") + ")",
      r = RegExp(n),
      i = RegExp(n, "g");
    return function (e) {
      return (e = null == e ? "" : "" + e), r.test(e) ? e.replace(i, t) : e;
    };
  }
  var Ye = {
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#x27;",
      "`": "&#x60;",
    },
    Ge = Xe(Ye),
    Je = Xe(je(Ye)),
    Qe = (ce.templateSettings = {
      evaluate: /<%([\s\S]+?)%>/g,
      interpolate: /<%=([\s\S]+?)%>/g,
      escape: /<%-([\s\S]+?)%>/g,
    }),
    Ke = /(.)^/,
    Ze = {
      "'": "'",
      "\\": "\\",
      "\r": "r",
      "\n": "n",
      "\u2028": "u2028",
      "\u2029": "u2029",
    },
    et = /\\|'|\r|\n|\u2028|\u2029/g;
  function tt(e) {
    return "\\" + Ze[e];
  }
  var nt = /^\s*(\w|\$)+\s*$/;
  var rt = 0;
  function it(e) {
    var t = ++rt + "";
    return e ? e + t : t;
  }
  function ot(e, t, n, r, i) {
    if (!(r instanceof t)) return e.apply(n, i);
    var o = Ae(e.prototype),
      u = e.apply(o, i);
    return S(u) ? u : o;
  }
  var ut = j(function (e, t) {
    var n = ut.placeholder,
      r = function () {
        for (var i = 0, o = t.length, u = Array(o), a = 0; a < o; a++)
          u[a] = t[a] === n ? arguments[i++] : t[a];
        for (; i < arguments.length; ) u.push(arguments[i++]);
        return ot(e, r, this, this, u);
      };
    return r;
  });
  ut.placeholder = ce;
  var at = j(function (e, t, n) {
      if (!P(e)) throw new TypeError("Bind must be called on a function");
      var r = j(function (i) {
        return ot(e, r, t, this, n.concat(i));
      });
      return r;
    }),
    ct = K(ie);
  function st(e, t, n, r) {
    if (((r = r || []), t || 0 === t)) {
      if (t <= 0) return r.concat(e);
    } else t = 1 / 0;
    for (var i = r.length, o = 0, u = ie(e); o < u; o++) {
      var a = e[o];
      if (ct(a) && (V(a) || G(a)))
        if (t > 1) st(a, t - 1, n, r), (i = r.length);
        else for (var c = 0, s = a.length; c < s; ) r[i++] = a[c++];
      else n || (r[i++] = a);
    }
    return r;
  }
  var ft = j(function (e, t) {
    var n = (t = st(t, !1, !1)).length;
    if (n < 1) throw new Error("bindAll must be passed function names");
    for (; n--; ) {
      var r = t[n];
      e[r] = at(e[r], e);
    }
    return e;
  });
  var lt = j(function (e, t, n) {
      return setTimeout(function () {
        return e.apply(null, n);
      }, t);
    }),
    dt = ut(lt, ce, 1);
  function ht(e) {
    return function () {
      return !e.apply(this, arguments);
    };
  }
  function pt(e, t) {
    var n;
    return function () {
      return --e > 0 && (n = t.apply(this, arguments)), e <= 1 && (t = null), n;
    };
  }
  var mt = ut(pt, 2);
  function vt(e, t, n) {
    t = Be(t, n);
    for (var r, i = ue(e), o = 0, u = i.length; o < u; o++)
      if (t(e[(r = i[o])], r, e)) return r;
  }
  function gt(e) {
    return function (t, n, r) {
      n = Be(n, r);
      for (var i = ie(t), o = e > 0 ? 0 : i - 1; o >= 0 && o < i; o += e)
        if (n(t[o], o, t)) return o;
      return -1;
    };
  }
  var yt = gt(1),
    wt = gt(-1);
  function bt(e, t, n, r) {
    for (var i = (n = Be(n, r, 1))(t), o = 0, u = ie(e); o < u; ) {
      var a = Math.floor((o + u) / 2);
      n(e[a]) < i ? (o = a + 1) : (u = a);
    }
    return o;
  }
  function _t(e, t, n) {
    return function (r, i, o) {
      var u = 0,
        a = ie(r);
      if ("number" == typeof o)
        e > 0
          ? (u = o >= 0 ? o : Math.max(o + a, u))
          : (a = o >= 0 ? Math.min(o + 1, a) : o + a + 1);
      else if (n && o && a) return r[(o = n(r, i))] === i ? o : -1;
      if (i != i) return (o = t(f.call(r, u, a), J)) >= 0 ? o + u : -1;
      for (o = e > 0 ? u : a - 1; o >= 0 && o < a; o += e)
        if (r[o] === i) return o;
      return -1;
    };
  }
  var xt = _t(1, yt, bt),
    kt = _t(-1, wt);
  function jt(e, t, n) {
    var r = (ct(e) ? yt : vt)(e, t, n);
    if (void 0 !== r && -1 !== r) return e[r];
  }
  function St(e, t, n) {
    var r, i;
    if (((t = Pe(t, n)), ct(e)))
      for (r = 0, i = e.length; r < i; r++) t(e[r], r, e);
    else {
      var o = ue(e);
      for (r = 0, i = o.length; r < i; r++) t(e[o[r]], o[r], e);
    }
    return e;
  }
  function Tt(e, t, n) {
    t = Be(t, n);
    for (
      var r = !ct(e) && ue(e), i = (r || e).length, o = Array(i), u = 0;
      u < i;
      u++
    ) {
      var a = r ? r[u] : u;
      o[u] = t(e[a], a, e);
    }
    return o;
  }
  function Ct(e) {
    var t = function (t, n, r, i) {
      var o = !ct(t) && ue(t),
        u = (o || t).length,
        a = e > 0 ? 0 : u - 1;
      for (i || ((r = t[o ? o[a] : a]), (a += e)); a >= 0 && a < u; a += e) {
        var c = o ? o[a] : a;
        r = n(r, t[c], c, t);
      }
      return r;
    };
    return function (e, n, r, i) {
      var o = arguments.length >= 3;
      return t(e, Pe(n, i, 4), r, o);
    };
  }
  var Et = Ct(1),
    qt = Ct(-1);
  function At(e, t, n) {
    var r = [];
    return (
      (t = Be(t, n)),
      St(e, function (e, n, i) {
        t(e, n, i) && r.push(e);
      }),
      r
    );
  }
  function Dt(e, t, n) {
    t = Be(t, n);
    for (var r = !ct(e) && ue(e), i = (r || e).length, o = 0; o < i; o++) {
      var u = r ? r[o] : o;
      if (!t(e[u], u, e)) return !1;
    }
    return !0;
  }
  function Nt(e, t, n) {
    t = Be(t, n);
    for (var r = !ct(e) && ue(e), i = (r || e).length, o = 0; o < i; o++) {
      var u = r ? r[o] : o;
      if (t(e[u], u, e)) return !0;
    }
    return !1;
  }
  function Ot(e, t, n, r) {
    return (
      ct(e) || (e = ke(e)),
      ("number" != typeof n || r) && (n = 0),
      xt(e, t, n) >= 0
    );
  }
  var Mt = j(function (e, t, n) {
    var r, i;
    return (
      P(t)
        ? (i = t)
        : ((t = Oe(t)), (r = t.slice(0, -1)), (t = t[t.length - 1])),
      Tt(e, function (e) {
        var o = i;
        if (!o) {
          if ((r && r.length && (e = Me(e, r)), null == e)) return;
          o = e[t];
        }
        return null == o ? o : o.apply(e, n);
      })
    );
  });
  function Lt(e, t) {
    return Tt(e, He(t));
  }
  function Rt(e, t, n) {
    var r,
      i,
      o = -1 / 0,
      u = -1 / 0;
    if (
      null == t ||
      ("number" == typeof t && "object" != typeof e[0] && null != e)
    )
      for (var a = 0, c = (e = ct(e) ? e : ke(e)).length; a < c; a++)
        null != (r = e[a]) && r > o && (o = r);
    else
      (t = Be(t, n)),
        St(e, function (e, n, r) {
          ((i = t(e, n, r)) > u || (i === -1 / 0 && o === -1 / 0)) &&
            ((o = e), (u = i));
        });
    return o;
  }
  function It(e, t, n) {
    if (null == t || n) return ct(e) || (e = ke(e)), e[Ue(e.length - 1)];
    var r = ct(e) ? De(e) : ke(e),
      i = ie(r);
    t = Math.max(Math.min(t, i), 0);
    for (var o = i - 1, u = 0; u < t; u++) {
      var a = Ue(u, o),
        c = r[u];
      (r[u] = r[a]), (r[a] = c);
    }
    return r.slice(0, t);
  }
  function Ht(e, t) {
    return function (n, r, i) {
      var o = t ? [[], []] : {};
      return (
        (r = Be(r, i)),
        St(n, function (t, i) {
          var u = r(t, i, n);
          e(o, t, u);
        }),
        o
      );
    };
  }
  var Pt = Ht(function (e, t, n) {
      X(e, n) ? e[n].push(t) : (e[n] = [t]);
    }),
    zt = Ht(function (e, t, n) {
      e[n] = t;
    }),
    Ft = Ht(function (e, t, n) {
      X(e, n) ? e[n]++ : (e[n] = 1);
    }),
    Bt = Ht(function (e, t, n) {
      e[n ? 0 : 1].push(t);
    }, !0),
    $t = /[^\ud800-\udfff]|[\ud800-\udbff][\udc00-\udfff]|[\ud800-\udfff]/g;
  function Wt(e) {
    return e
      ? V(e)
        ? f.call(e)
        : A(e)
        ? e.match($t)
        : ct(e)
        ? Tt(e, Re)
        : ke(e)
      : [];
  }
  function Ut(e, t, n) {
    return t in n;
  }
  var Vt = j(function (e, t) {
      var n = {},
        r = t[0];
      if (null == e) return n;
      P(r)
        ? (t.length > 1 && (r = Pe(r, t[1])), (t = le(e)))
        : ((r = Ut), (t = st(t, !1, !1)), (e = Object(e)));
      for (var i = 0, o = t.length; i < o; i++) {
        var u = t[i],
          a = e[u];
        r(a, u, e) && (n[u] = a);
      }
      return n;
    }),
    Xt = j(function (e, t) {
      var n,
        r = t[0];
      return (
        P(r)
          ? ((r = ht(r)), t.length > 1 && (n = t[1]))
          : ((t = Tt(st(t, !1, !1), String)),
            (r = function (e, n) {
              return !Ot(t, n);
            })),
        Vt(e, r, n)
      );
    });
  function Yt(e, t, n) {
    return f.call(e, 0, Math.max(0, e.length - (null == t || n ? 1 : t)));
  }
  function Gt(e, t, n) {
    return null == e || e.length < 1
      ? null == t || n
        ? void 0
        : []
      : null == t || n
      ? e[0]
      : Yt(e, e.length - t);
  }
  function Jt(e, t, n) {
    return f.call(e, null == t || n ? 1 : t);
  }
  function Qt(e, t, n) {
    return null == e || e.length < 1
      ? null == t || n
        ? void 0
        : []
      : null == t || n
      ? e[e.length - 1]
      : Jt(e, Math.max(0, e.length - t));
  }
  var Kt = j(function (e, t) {
      return (
        (t = st(t, !0, !0)),
        At(e, function (e) {
          return !Ot(t, e);
        })
      );
    }),
    Zt = j(function (e, t) {
      return Kt(e, t);
    });
  function en(e, t, n, r) {
    E(t) || ((r = n), (n = t), (t = !1)), null != n && (n = Be(n, r));
    for (var i = [], o = [], u = 0, a = ie(e); u < a; u++) {
      var c = e[u],
        s = n ? n(c, u, e) : c;
      t && !n
        ? ((u && o === s) || i.push(c), (o = s))
        : n
        ? Ot(o, s) || (o.push(s), i.push(c))
        : Ot(i, c) || i.push(c);
    }
    return i;
  }
  var tn = j(function (e) {
    return en(st(e, !0, !0));
  });
  function nn(e) {
    for (var t = (e && Rt(e, ie).length) || 0, n = Array(t), r = 0; r < t; r++)
      n[r] = Lt(e, r);
    return n;
  }
  var rn = j(nn);
  function on(e, t) {
    return e._chain ? ce(t).chain() : t;
  }
  function un(e) {
    return (
      St(Se(e), function (t) {
        var n = (ce[t] = e[t]);
        ce.prototype[t] = function () {
          var e = [this._wrapped];
          return s.apply(e, arguments), on(this, n.apply(ce, e));
        };
      }),
      ce
    );
  }
  St(
    ["pop", "push", "reverse", "shift", "sort", "splice", "unshift"],
    function (e) {
      var t = u[e];
      ce.prototype[e] = function () {
        var n = this._wrapped;
        return (
          null != n &&
            (t.apply(n, arguments),
            ("shift" !== e && "splice" !== e) || 0 !== n.length || delete n[0]),
          on(this, n)
        );
      };
    }
  ),
    St(["concat", "join", "slice"], function (e) {
      var t = u[e];
      ce.prototype[e] = function () {
        var e = this._wrapped;
        return null != e && (e = t.apply(e, arguments)), on(this, e);
      };
    });
  var an = un({
    __proto__: null,
    VERSION: "1.13.1",
    restArguments: j,
    isObject: S,
    isNull: T,
    isUndefined: C,
    isBoolean: E,
    isElement: function (e) {
      return !(!e || 1 !== e.nodeType);
    },
    isString: A,
    isNumber: D,
    isDate: N,
    isRegExp: O,
    isError: M,
    isSymbol: L,
    isArrayBuffer: R,
    isDataView: U,
    isArray: V,
    isFunction: P,
    isArguments: G,
    isFinite: function (e) {
      return !L(e) && b(e) && !isNaN(parseFloat(e));
    },
    isNaN: J,
    isTypedArray: re,
    isEmpty: function (e) {
      if (null == e) return !0;
      var t = ie(e);
      return "number" == typeof t && (V(e) || A(e) || G(e))
        ? 0 === t
        : 0 === ie(ue(e));
    },
    isMatch: ae,
    isEqual: function (e, t) {
      return fe(e, t);
    },
    isMap: we,
    isWeakMap: be,
    isSet: _e,
    isWeakSet: xe,
    keys: ue,
    allKeys: le,
    values: ke,
    pairs: function (e) {
      for (var t = ue(e), n = t.length, r = Array(n), i = 0; i < n; i++)
        r[i] = [t[i], e[t[i]]];
      return r;
    },
    invert: je,
    functions: Se,
    methods: Se,
    extend: Ce,
    extendOwn: Ee,
    assign: Ee,
    defaults: qe,
    create: function (e, t) {
      var n = Ae(e);
      return t && Ee(n, t), n;
    },
    clone: De,
    tap: function (e, t) {
      return t(e), e;
    },
    get: Le,
    has: function (e, t) {
      for (var n = (t = Oe(t)).length, r = 0; r < n; r++) {
        var i = t[r];
        if (!X(e, i)) return !1;
        e = e[i];
      }
      return !!n;
    },
    mapObject: function (e, t, n) {
      t = Be(t, n);
      for (var r = ue(e), i = r.length, o = {}, u = 0; u < i; u++) {
        var a = r[u];
        o[a] = t(e[a], a, e);
      }
      return o;
    },
    identity: Re,
    constant: Q,
    noop: $e,
    toPath: Ne,
    property: He,
    propertyOf: function (e) {
      return null == e
        ? $e
        : function (t) {
            return Le(e, t);
          };
    },
    matcher: Ie,
    matches: Ie,
    times: We,
    random: Ue,
    now: Ve,
    escape: Ge,
    unescape: Je,
    templateSettings: Qe,
    template: function (e, t, n) {
      !t && n && (t = n), (t = qe({}, t, ce.templateSettings));
      var r = RegExp(
          [
            (t.escape || Ke).source,
            (t.interpolate || Ke).source,
            (t.evaluate || Ke).source,
          ].join("|") + "|$",
          "g"
        ),
        i = 0,
        o = "__p+='";
      e.replace(r, function (t, n, r, u, a) {
        return (
          (o += e.slice(i, a).replace(et, tt)),
          (i = a + t.length),
          n
            ? (o += "'+\n((__t=(" + n + "))==null?'':_.escape(__t))+\n'")
            : r
            ? (o += "'+\n((__t=(" + r + "))==null?'':__t)+\n'")
            : u && (o += "';\n" + u + "\n__p+='"),
          t
        );
      }),
        (o += "';\n");
      var u,
        a = t.variable;
      if (a) {
        if (!nt.test(a))
          throw new Error("variable is not a bare identifier: " + a);
      } else (o = "with(obj||{}){\n" + o + "}\n"), (a = "obj");
      o =
        "var __t,__p='',__j=Array.prototype.join,print=function(){__p+=__j.call(arguments,'');};\n" +
        o +
        "return __p;\n";
      try {
        u = new Function(a, "_", o);
      } catch (e) {
        throw ((e.source = o), e);
      }
      var c = function (e) {
        return u.call(this, e, ce);
      };
      return (c.source = "function(" + a + "){\n" + o + "}"), c;
    },
    result: function (e, t, n) {
      var r = (t = Oe(t)).length;
      if (!r) return P(n) ? n.call(e) : n;
      for (var i = 0; i < r; i++) {
        var o = null == e ? void 0 : e[t[i]];
        void 0 === o && ((o = n), (i = r)), (e = P(o) ? o.call(e) : o);
      }
      return e;
    },
    uniqueId: it,
    chain: function (e) {
      var t = ce(e);
      return (t._chain = !0), t;
    },
    iteratee: Fe,
    partial: ut,
    bind: at,
    bindAll: ft,
    memoize: function (e, t) {
      var n = function (r) {
        var i = n.cache,
          o = "" + (t ? t.apply(this, arguments) : r);
        return X(i, o) || (i[o] = e.apply(this, arguments)), i[o];
      };
      return (n.cache = {}), n;
    },
    delay: lt,
    defer: dt,
    throttle: function (e, t, n) {
      var r,
        i,
        o,
        u,
        a = 0;
      n || (n = {});
      var c = function () {
          (a = !1 === n.leading ? 0 : Ve()),
            (r = null),
            (u = e.apply(i, o)),
            r || (i = o = null);
        },
        s = function () {
          var s = Ve();
          a || !1 !== n.leading || (a = s);
          var f = t - (s - a);
          return (
            (i = this),
            (o = arguments),
            f <= 0 || f > t
              ? (r && (clearTimeout(r), (r = null)),
                (a = s),
                (u = e.apply(i, o)),
                r || (i = o = null))
              : r || !1 === n.trailing || (r = setTimeout(c, f)),
            u
          );
        };
      return (
        (s.cancel = function () {
          clearTimeout(r), (a = 0), (r = i = o = null);
        }),
        s
      );
    },
    debounce: function (e, t, n) {
      var r,
        i,
        o,
        u,
        a,
        c = function () {
          var s = Ve() - i;
          t > s
            ? (r = setTimeout(c, t - s))
            : ((r = null), n || (u = e.apply(a, o)), r || (o = a = null));
        },
        s = j(function (s) {
          return (
            (a = this),
            (o = s),
            (i = Ve()),
            r || ((r = setTimeout(c, t)), n && (u = e.apply(a, o))),
            u
          );
        });
      return (
        (s.cancel = function () {
          clearTimeout(r), (r = o = a = null);
        }),
        s
      );
    },
    wrap: function (e, t) {
      return ut(t, e);
    },
    negate: ht,
    compose: function () {
      var e = arguments,
        t = e.length - 1;
      return function () {
        for (var n = t, r = e[t].apply(this, arguments); n--; )
          r = e[n].call(this, r);
        return r;
      };
    },
    after: function (e, t) {
      return function () {
        if (--e < 1) return t.apply(this, arguments);
      };
    },
    before: pt,
    once: mt,
    findKey: vt,
    findIndex: yt,
    findLastIndex: wt,
    sortedIndex: bt,
    indexOf: xt,
    lastIndexOf: kt,
    find: jt,
    detect: jt,
    findWhere: function (e, t) {
      return jt(e, Ie(t));
    },
    each: St,
    forEach: St,
    map: Tt,
    collect: Tt,
    reduce: Et,
    foldl: Et,
    inject: Et,
    reduceRight: qt,
    foldr: qt,
    filter: At,
    select: At,
    reject: function (e, t, n) {
      return At(e, ht(Be(t)), n);
    },
    every: Dt,
    all: Dt,
    some: Nt,
    any: Nt,
    contains: Ot,
    includes: Ot,
    include: Ot,
    invoke: Mt,
    pluck: Lt,
    where: function (e, t) {
      return At(e, Ie(t));
    },
    max: Rt,
    min: function (e, t, n) {
      var r,
        i,
        o = 1 / 0,
        u = 1 / 0;
      if (
        null == t ||
        ("number" == typeof t && "object" != typeof e[0] && null != e)
      )
        for (var a = 0, c = (e = ct(e) ? e : ke(e)).length; a < c; a++)
          null != (r = e[a]) && r < o && (o = r);
      else
        (t = Be(t, n)),
          St(e, function (e, n, r) {
            ((i = t(e, n, r)) < u || (i === 1 / 0 && o === 1 / 0)) &&
              ((o = e), (u = i));
          });
      return o;
    },
    shuffle: function (e) {
      return It(e, 1 / 0);
    },
    sample: It,
    sortBy: function (e, t, n) {
      var r = 0;
      return (
        (t = Be(t, n)),
        Lt(
          Tt(e, function (e, n, i) {
            return { value: e, index: r++, criteria: t(e, n, i) };
          }).sort(function (e, t) {
            var n = e.criteria,
              r = t.criteria;
            if (n !== r) {
              if (n > r || void 0 === n) return 1;
              if (n < r || void 0 === r) return -1;
            }
            return e.index - t.index;
          }),
          "value"
        )
      );
    },
    groupBy: Pt,
    indexBy: zt,
    countBy: Ft,
    partition: Bt,
    toArray: Wt,
    size: function (e) {
      return null == e ? 0 : ct(e) ? e.length : ue(e).length;
    },
    pick: Vt,
    omit: Xt,
    first: Gt,
    head: Gt,
    take: Gt,
    initial: Yt,
    last: Qt,
    rest: Jt,
    tail: Jt,
    drop: Jt,
    compact: function (e) {
      return At(e, Boolean);
    },
    flatten: function (e, t) {
      return st(e, t, !1);
    },
    without: Zt,
    uniq: en,
    unique: en,
    union: tn,
    intersection: function (e) {
      for (var t = [], n = arguments.length, r = 0, i = ie(e); r < i; r++) {
        var o = e[r];
        if (!Ot(t, o)) {
          var u;
          for (u = 1; u < n && Ot(arguments[u], o); u++);
          u === n && t.push(o);
        }
      }
      return t;
    },
    difference: Kt,
    unzip: nn,
    transpose: nn,
    zip: rn,
    object: function (e, t) {
      for (var n = {}, r = 0, i = ie(e); r < i; r++)
        t ? (n[e[r]] = t[r]) : (n[e[r][0]] = e[r][1]);
      return n;
    },
    range: function (e, t, n) {
      null == t && ((t = e || 0), (e = 0)), n || (n = t < e ? -1 : 1);
      for (
        var r = Math.max(Math.ceil((t - e) / n), 0), i = Array(r), o = 0;
        o < r;
        o++, e += n
      )
        i[o] = e;
      return i;
    },
    chunk: function (e, t) {
      if (null == t || t < 1) return [];
      for (var n = [], r = 0, i = e.length; r < i; )
        n.push(f.call(e, r, (r += t)));
      return n;
    },
    mixin: un,
    default: ce,
  });
  an._ = an;
  const cn = {
      create: function (e) {
        var t = function () {
          this.initialize.apply(this, arguments);
        };
        return Ce(t.prototype, e), t;
      },
      extend: function (e, t) {
        var n = function () {
          this.initialize.apply(this, arguments);
        };
        return Ce((n.prototype = e), t), n;
      },
      memoize: function (e, t) {
        var n = e.prototype;
        St(V(t) ? t : [t], function (e) {
          (n["compute_" + e] = n[e]),
            (n[e] = function () {
              return (
                this.hasOwnProperty("cached_" + e) ||
                  (this["cached_" + e] = this["compute_" + e].apply(
                    this,
                    Wt(arguments)
                  )),
                this["cached_" + e]
              );
            });
        });
      },
    },
    sn = {},
    fn = cn.create({
      initialize: function (e) {
        (this.name = e), (sn[e] = this);
      },
      inspect: function () {
        return "'" + this.name;
      },
      toString: function () {
        return "'" + this.name;
      },
      to_write: function () {
        return this.name;
      },
    }),
    ln = function (e, t) {
      return void 0 === sn[e]
        ? new fn(e)
        : sn[e] instanceof fn
        ? sn[e]
        : new fn(e);
    },
    dn = function () {
      return ln(it("__gensym"));
    },
    hn = cn.create({
      initialize: function (e, t = null) {
        const n = null === t ? "" : ": " + Mn(t);
        (this.message = `Error: ${e}${n}`), (this.form = t);
      },
      toString: function () {
        return this.message;
      },
    }),
    pn = cn.extend(new hn(), {
      initialize: function (e) {
        this.message = "[BUG] " + e;
      },
    }),
    mn = cn.extend(new hn(), {
      initialize: function (e) {
        this.message = e;
      },
    }),
    vn = cn.create({
      initialize: function () {
        var e;
        for (this.arr = [], e = 0; e < arguments.length; e++)
          this.arr[e] = arguments[e];
      },
      equals: function (e) {
        if (this.arr.length != e.arr.length) return !1;
        var t = De(this.arr),
          n = De(e.arr);
        t.sort(), n.sort();
        for (var r = 0; r < this.arr.length; r++) if (t[r] != n[r]) return !1;
        return !0;
      },
      set_cons: function (e) {
        var t = new vn(e);
        return (t.arr = De(this.arr)), t.arr.push(e), t;
      },
      set_union: function () {
        var e = new vn();
        e.arr = De(this.arr);
        for (var t = 0; t < arguments.length; t++) {
          var n = arguments[t];
          if (!(n instanceof vn))
            throw new hn("set_union: arguments must be a set");
          for (var r = 0; r < n.arr.length; r++) e.add(n.arr[r]);
        }
        return e;
      },
      set_intersect: function (e) {
        if (!(e instanceof vn))
          throw new hn("set_intersect: arguments must be a set");
        for (var t = new vn(), n = 0; n < this.arr.length; n++)
          e.member(this.arr[n]) && t.add(this.arr[n]);
        return t;
      },
      set_minus: function (e) {
        if (!(e instanceof vn))
          throw new hn("set_minus: arguments must be a set");
        for (var t = new vn(), n = 0; n < this.arr.length; n++)
          e.member(this.arr[n]) || t.add(this.arr[n]);
        return t;
      },
      add: function (e) {
        this.member(e) || this.arr.push(e);
      },
      member: function (e) {
        for (var t = 0; t < this.arr.length; t++)
          if (this.arr[t] == e) return !0;
        return !1;
      },
      rindex: function (e) {
        for (var t = this.arr.length - 1; t >= 0; t--)
          if (this.arr[t] == e) return this.arr.length - 1 - t;
        return null;
      },
      index: function (e) {
        for (var t = 0; t < this.arr.length; t++)
          if (this.arr[t] == e) return t;
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
    }),
    gn = cn.create({
      initialize: function (e, t) {
        (this.car = e), (this.cdr = t);
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
      to_array: function () {
        for (var e = [], t = this; t instanceof gn; t = t.cdr) e.push(t.car);
        return e;
      },
      to_set: function () {
        for (var e = new vn(), t = this; t instanceof gn; t = t.cdr)
          e.add(t.car);
        return e;
      },
      length: function () {
        for (var e = 0, t = this; t instanceof gn; t = t.cdr) e++;
        return e;
      },
      last_cdr: function () {
        var e;
        for (e = this; e instanceof gn; e = e.cdr);
        return e;
      },
      forEach: function (e) {
        for (var t = this; t instanceof gn; t = t.cdr) e(t.car);
        return t;
      },
      foreach: function (e) {
        for (var t = this; t instanceof gn; t = t.cdr) e(t.car);
        return t;
      },
      map: function (e) {
        for (var t = [], n = this; yn(n); n = n.cdr) t.push(e(n.car));
        return t;
      },
      mapList: function (e) {
        return xn(this.map(e));
      },
      concat: function (e) {
        for (var t = this; t instanceof gn && t.cdr != n; ) t = t.cdr;
        return (t.cdr = e), this;
      },
      inspect: function (e) {
        e || (e = An);
        var t = [],
          r = this.foreach(function (n) {
            t.push(e(n));
          });
        return r != n && (t.push("."), t.push(e(r))), "(" + t.join(" ") + ")";
      },
      toString: function () {
        return this.inspect();
      },
      to_display: function (e) {
        return this.inspect(e);
      },
      to_write: function () {
        return this.inspect(Mn);
      },
    }),
    yn = function (e) {
      return e instanceof gn;
    },
    wn = function (e) {
      if (e === n) return !0;
      if (!(e instanceof gn)) return !1;
      for (var t = e, r = e.cdr; ; ) {
        if (r === n) return !0;
        if (r === t) return !1;
        if (!(r instanceof gn)) return !1;
        if (r.cdr === n) return !0;
        if (!(r.cdr instanceof gn)) return !1;
        (r = r.cdr.cdr), (t = t.cdr);
      }
    },
    bn = function (e, t) {
      for (var r = n, i = e.length - 1; i >= 0; i--) {
        var o = e[i];
        t && V(o) && !o.is_vector && (o = bn(o, t)), (r = new gn(o, r));
      }
      return r;
    },
    _n = function () {
      var e = Wt(arguments);
      return bn(e, !1);
    },
    xn = function (e) {
      return bn(e, !1);
    },
    kn = function (e) {
      return bn(e, !0);
    },
    jn = function (e, t) {
      return new gn(e, t);
    },
    Sn = function (e) {
      if (void 0 === e) return n;
      var t = [];
      return (
        St(e, function (e, n) {
          t.push(new gn(n, e));
        }),
        xn(t)
      );
    },
    Tn = function (e) {
      if (e === n) return {};
      var t = {};
      return (
        e.foreach(function (e) {
          t[e.car] = e.cdr;
        }),
        t
      );
    },
    Cn = function (e, t) {
      return e.length > t ? e.slice(0, t) + "..." : e;
    },
    En = function (e) {
      if (void 0 === e) return "undefined";
      if (null === e) return "null";
      if (P(e))
        return (
          "#<Function " +
          (e.fname ? e.fname : e.toSource ? Cn(e.toSource(), 40) : "") +
          ">"
        );
      if (A(e))
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
      if (V(e))
        return (
          "#(" +
          Tt(e, function (e) {
            return En(e);
          }).join(" ") +
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
      return An(e);
    },
    qn = function (e) {
      return C(e)
        ? "undefined"
        : T(e)
        ? "null"
        : e.to_display
        ? e.to_display(qn)
        : "string" == typeof e.valueOf()
        ? e
        : e instanceof fn
        ? e.name
        : e instanceof Array
        ? "#(" + Tt(e, qn).join(" ") + ")"
        : En(e);
    },
    An = function (e, t) {
      try {
        return C(e)
          ? "undefined"
          : null === e
          ? "null"
          : !0 === e
          ? "#t"
          : !1 === e
          ? "#f"
          : e.inspect
          ? e.inspect()
          : A(e)
          ? '"' + e.replace(/"/g, '\\"') + '"'
          : V(e)
          ? "[" + Tt(e, An).join(", ") + "]"
          : t && t.fallback
          ? t.fallback
          : e.toString();
      } catch (e) {
        if (e instanceof RangeError) return "...";
        throw e;
      }
    };
  function Dn(e) {
    const t = {
      objs: new Set(),
      shared_objs: new Set(),
      parents: new Set(),
      cyclic: !1,
    };
    !(function e(t, n) {
      n.parents.has(t) && (n.cyclic = !0);
      if (n.shared_objs.has(t)) return;
      if (n.objs.has(t)) return void n.shared_objs.add(t);
      n.objs.add(t),
        yn(t)
          ? (n.parents.add(t), e(t.car, n), e(t.cdr, n), n.parents.delete(t))
          : Gn(t) &&
            (n.parents.add(t),
            t.forEach((t) => {
              e(t, n);
            }),
            n.parents.delete(t));
    })(e, t);
    const n = new Map();
    for (const e of t.shared_objs) n.set(e, null);
    return { ids: n, last_id: -1, cyclic: t.cyclic };
  }
  function Nn(e) {
    return On(e, Dn(e));
  }
  function On(e, t) {
    let r = "";
    if (t.ids.has(e)) {
      const n = t.ids.get(e);
      if (null !== n) return `#${n}#`;
      {
        const n = t.last_id + 1;
        t.ids.set(e, n), (t.last_id = n), (r += `#${n}=`);
      }
    }
    if (yn(e)) {
      const i = [];
      i.push(On(e.car, t));
      for (let r = e.cdr; r !== n; r = r.cdr) {
        if (!yn(r) || t.ids.has(r)) {
          i.push("."), i.push(On(r, t));
          break;
        }
        i.push(On(r.car, t));
      }
      r += "(" + i.join(" ") + ")";
    } else if (Gn(e)) {
      r += "#(" + e.map((e) => On(e, t)).join(" ") + ")";
    } else r += En(e);
    return r;
  }
  const Mn = function (e) {
      const t = Dn(e);
      return t.cyclic ? On(e, t) : En(e);
    },
    Ln = {},
    Rn = cn.create({
      initialize: function (e) {
        Ln[(this.value = e)] = this;
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
  Rn.get = function (e) {
    if ("string" != typeof e)
      throw new pn("Char.get: " + An(e) + " is not a string");
    return void 0 === Ln[e] ? new Rn(e) : Ln[e];
  };
  var In = cn.create({
    initialize: function (e) {
      this.on_pause = e;
    },
    set_state: function (e, t, n, r, i) {
      (this.interpreter = e),
        (this.x = t),
        (this.f = n),
        (this.c = r),
        (this.s = i);
    },
    ready: function () {
      this.on_pause(this);
    },
    resume: function (e) {
      return this.interpreter.resume(!0, e, this.x, this.f, this.c, this.s);
    },
  });
  const Hn = new Object(),
    Pn = cn.create({
      initialize: function (e, t) {
        (this.is_open = !0),
          (this.is_binary = !1),
          (this.is_input = e),
          (this.is_output = t);
      },
      close: function () {
        this.is_open = !1;
      },
      inspect: function () {
        return "#<Port>";
      },
      to_write: function () {
        return "#<Port>";
      },
    });
  (Pn.StringOutput = cn.extend(new Pn(!1, !0), {
    initialize: function () {
      this.buffer = [];
    },
    put_string: function (e) {
      this.buffer.push(e);
    },
    output_string: function (e) {
      return this.buffer.join("");
    },
  })),
    (Pn.StringInput = cn.extend(new Pn(!0, !1), {
      initialize: function (e) {
        this.str = e;
      },
      get_string: function (e) {
        return e(this.str);
      },
    })),
    (Pn.NullInput = cn.extend(new Pn(!0, !1), {
      initialize: function () {},
      get_string: function (e) {
        return e("");
      },
    })),
    (Pn.NullOutput = cn.extend(new Pn(!1, !0), {
      initialize: function (e) {
        this.output_function = e;
      },
      put_string: function (e) {},
    })),
    (Pn.CustomOutput = cn.extend(new Pn(!1, !0), {
      initialize: function (e) {
        this.output_function = e;
      },
      put_string: function (e) {
        this.output_function(e);
      },
    })),
    (Pn.CustomInput = cn.extend(new Pn(!0, !1), {
      initialize: function (e) {
        this.input_function = e;
      },
      get_string: function (e) {
        var t = this.input_function;
        return new In(function (n) {
          t(function (t) {
            n.resume(e(t));
          });
        });
      },
    })),
    (Pn.current_input = new Pn.NullInput()),
    (Pn.current_output = new Pn.NullOutput()),
    (Pn.current_error = new Pn.NullOutput());
  const zn = cn.create({
      initialize: function (e, t, n, r) {
        (this.body = e),
          (this.freevars = t),
          (this.dotpos = n),
          (this.expected_args = r);
      },
      to_write: function () {
        return "#<Closure>";
      },
    }),
    Fn = function (e) {
      return e instanceof zn;
    },
    Bn = function (e) {
      return e === n;
    },
    $n = E,
    Wn = A,
    Un = P,
    Vn = function (e) {
      return e instanceof Rn;
    },
    Xn = function (e) {
      return e instanceof fn;
    },
    Yn = function (e) {
      return e instanceof Pn;
    },
    Gn = function (e) {
      return e instanceof Array;
    },
    Jn = function (e) {
      return Fn(e) || P(e);
    },
    Qn = function (e, t) {
      return e === t;
    },
    Kn = function (e, t) {
      return e == t && typeof e == typeof t;
    },
    Zn = function (e, t) {
      return Mn(e) == Mn(t);
    },
    er = function (e, t) {
      return typeof e != typeof t ? compareFn(typeof e, typeof t) : e < t;
    },
    tr = cn.create({
      initialize: function (e, t, n) {
        (this.proc = e),
          (this.args = t),
          (this.after =
            n ||
            function (e) {
              return e[0];
            });
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
    }),
    nr = {
      ForArray: cn.create({
        initialize: function (e) {
          (this.arr = e), (this.i = 0);
        },
        has_next: function () {
          return this.i < this.arr.length;
        },
        next: function () {
          return this.arr[this.i++];
        },
      }),
      ForString: cn.create({
        initialize: function (e) {
          (this.str = e), (this.i = 0);
        },
        has_next: function () {
          return this.i < this.str.length;
        },
        next: function () {
          return Rn.get(this.str.charAt(this.i++));
        },
      }),
      ForList: cn.create({
        initialize: function (e) {
          this.ls = e;
        },
        has_next: function () {
          return this.ls instanceof gn && this.ls != n;
        },
        next: function () {
          var e = this.ls;
          return (this.ls = this.ls.cdr), e;
        },
      }),
      ForMulti: cn.create({
        initialize: function (e) {
          (this.objs = e),
            (this.size = e.length),
            (this.iterators = Tt(e, function (e) {
              return nr.of(e);
            }));
        },
        has_next: function () {
          for (var e = 0; e < this.size; e++)
            if (!this.iterators[e].has_next()) return !1;
          return !0;
        },
        next: function () {
          return Tt(this.iterators, function (e) {
            return e.next();
          });
        },
      }),
      of: function (e) {
        switch (!0) {
          case e instanceof Array:
            return new this.ForArray(e);
          case "string" == typeof e:
            return new this.ForString(e);
          case e instanceof gn:
          case e === n:
            return new this.ForList(e);
          default:
            throw new pn("Iterator.of: unknown class: " + An(e));
        }
      },
    };
  (tr.default_callbacks = {
    call: function (e) {
      return new tr(this.proc, [e]);
    },
    result: function () {},
    finish: function () {},
  }),
    (tr.foreach = function (e, t, n) {
      n || (n = !1),
        St(["call", "result", "finish"], function (e) {
          t[e] || (t[e] = tr.default_callbacks[e]);
        });
      var r = null,
        i = null,
        o = function (u) {
          if (r) {
            var a = t.result(u[0], i);
            if (void 0 !== a) return a;
          } else r = n ? new nr.ForMulti(e) : nr.of(e);
          if (r.has_next()) {
            i = r.next();
            var c = t.call(i);
            return (c.after = o), c;
          }
          return t.finish();
        };
      return o(null);
    }),
    (tr.multi_foreach = function (e, t) {
      return tr.foreach(e, t, !0);
    });
  const rr = cn.create({
    initialize: function (e, t) {
      (this.sname = e), (this.func = t);
    },
    transform: function (e) {
      if (!this.func)
        throw new pn("sorry, syntax " + this.sname + " is a pseudo syntax now");
      return this.func(e);
    },
    inspect: function () {
      return "#<Syntax " + this.sname + ">";
    },
  });
  (t.define = new rr("define")),
    (t.begin = new rr("begin")),
    (t.quote = new rr("quote")),
    (t.lambda = new rr("lambda")),
    (t.if = new rr("if")),
    (t["set!"] = new rr("set!"));
  const ir = cn.create({
      initialize: function (e) {
        if (!Gn(e)) throw (console.error(e), "not array");
        this.il = e;
      },
      to_write: function () {
        return "#<VMCode>";
      },
    }),
    or = cn.create({
      initialize: function () {},
      is_tail: function (e) {
        return "return" == e[0];
      },
      collect_free: function (e, t, n) {
        for (var r = n, i = e.arr, o = 0; o < i.length; o++)
          r = this.compile_refer(i[o], t, ["argument", r]);
        return r;
      },
      compile_refer: function (e, t, n) {
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
      },
      compile_lookup: function (e, t, n, r, i) {
        var o,
          u = t[0],
          a = t[1];
        return null != (o = u.index(e))
          ? n(o)
          : null != (o = a.index(e))
          ? r(o)
          : i(e.name);
      },
      make_boxes: function (e, t, n) {
        t = t;
        for (var r = 0, i = []; t instanceof gn; )
          e.member(t.car) && i.push(r), r++, (t = t.cdr);
        for (var o = n, u = i.length - 1; u >= 0; u--) o = ["box", i[u], o];
        return o;
      },
      find_sets: function (e, t) {
        var n = null;
        if (e instanceof fn) n = new vn();
        else if (e instanceof gn)
          switch (e.first()) {
            case ln("define"):
              var r = e.third();
              n = this.find_sets(r, t);
            case ln("begin"):
              n = this.find_sets(e.cdr, t);
              break;
            case ln("quote"):
              n = new vn();
              break;
            case ln("lambda"):
              var i = e.second(),
                o = e.cdr.cdr;
              n =
                i instanceof gn
                  ? this.find_sets(o, t.set_minus(i.to_set()))
                  : this.find_sets(o, t.set_minus(new vn(i)));
              break;
            case ln("if"):
              var u = e.second(),
                a = e.third(),
                c = e.fourth();
              n = this.find_sets(u, t).set_union(
                this.find_sets(a, t),
                this.find_sets(c, t)
              );
              break;
            case ln("set!"):
              var s = e.second(),
                f = e.third();
              n = t.member(s)
                ? this.find_sets(f, t).set_cons(s)
                : this.find_sets(f, t);
              break;
            case ln("call/cc"):
              r = e.second();
              n = this.find_sets(r, t);
              break;
            default:
              for (var l = new vn(), d = e; d instanceof gn; d = d.cdr)
                l = l.set_union(this.find_sets(d.car, t));
              n = l;
          }
        else n = new vn();
        if (null == n) throw new pn("find_sets() exited in unusual way");
        return n;
      },
      find_free: function (e, t, n) {
        var r = null;
        if (e instanceof fn) r = n.member(e) ? new vn(e) : new vn();
        else if (e instanceof gn)
          switch (e.first()) {
            case ln("define"):
              var i = e.third();
              r = this.find_free(i, t, n);
              break;
            case ln("begin"):
              r = this.find_free(e.cdr, t, n);
              break;
            case ln("quote"):
              r = new vn();
              break;
            case ln("lambda"):
              var o = e.second(),
                u = e.cdr.cdr;
              r =
                o instanceof gn
                  ? this.find_free(u, t.set_union(o.to_set()), n)
                  : this.find_free(u, t.set_cons(o), n);
              break;
            case ln("if"):
              var a = e.second(),
                c = e.third(),
                s = e.fourth();
              r = this.find_free(a, t, n).set_union(
                this.find_free(c, t, n),
                this.find_free(s, t, n)
              );
              break;
            case ln("set!"):
              var f = e.second();
              i = e.third();
              r = n.member(f)
                ? this.find_free(i, t, n).set_cons(f)
                : this.find_free(i, t, n);
              break;
            case ln("call/cc"):
              i = e.second();
              r = this.find_free(i, t, n);
              break;
            default:
              for (var l = new vn(), d = e; d instanceof gn; d = d.cdr)
                l = l.set_union(this.find_free(d.car, t, n));
              r = l;
          }
        else r = new vn();
        if (null == r) throw new pn("find_free() exited in unusual way");
        return r;
      },
      find_dot_pos: function (e) {
        for (var t = 0; e instanceof gn; e = e.cdr, ++t);
        return e != n ? t : -1;
      },
      last_pair: function (e) {
        if (e instanceof gn) for (; e.cdr instanceof gn; e = e.cdr);
        return e;
      },
      dotted2proper: function (e) {
        if (e === n) return n;
        if (e instanceof gn) {
          var t = this.last_pair(e);
          if (t instanceof gn && t.cdr === n) return e;
          var r = (function (e) {
            for (var t = n; e instanceof gn; e = e.cdr) t = new gn(e.car, t);
            return (function (e) {
              for (var t = n; e instanceof gn; ) {
                var r = e.cdr;
                (e.cdr = t), (t = e), (e = r);
              }
              return t;
            })(t);
          })(e);
          return (this.last_pair(r).cdr = new gn(t.cdr, n)), r;
        }
        return new gn(e, n);
      },
      compile: function (e, t, n, r, i) {
        for (var o = null; ; ) {
          if (e instanceof fn)
            return this.compile_refer(e, t, n.member(e) ? ["indirect", i] : i);
          if (!(e instanceof gn)) return ["constant", e, i];
          switch (e.first()) {
            case ln("define"):
              (e = (o = this._compile_define(e, i))[0]), (i = o[1]);
              break;
            case ln("begin"):
              for (var u = [], a = e.cdr; a instanceof gn; a = a.cdr)
                u.push(a.car);
              for (var c = i, s = u.length - 1; s >= 0; s--)
                c = this.compile(u[s], t, n, r, c);
              return c;
            case ln("quote"):
              if (e.length() < 2)
                throw new hn("Invalid quote: " + e.to_write());
              return ["constant", e.second(), i];
            case ln("lambda"):
              return this._compile_lambda(e, t, n, r, i);
            case ln("if"):
              if (e.length() < 3 || e.length() > 4)
                throw new hn("Invalid if: " + e.to_write());
              var f = e.second(),
                l = e.third(),
                d = e.fourth();
              (l = this.compile(l, t, n, r, i)),
                (d = this.compile(d, t, n, r, i));
              (e = f), (i = ["test", l, d]);
              break;
            case ln("set!"):
              if (3 != e.length())
                throw new hn("Invalid set!: " + e.to_write());
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
            case ln("call/cc"):
              (e = e.second()),
                (c = [
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
              return this.is_tail(i) ? c : ["frame", c, i];
            default:
              var m = e.car,
                v = e.cdr;
              c = this.compile(
                m,
                t,
                n,
                r,
                this.is_tail(i)
                  ? ["shift", v.length(), ["tco_hinted_apply"]]
                  : ["apply"]
              );
              c = this.compile(v.length(), t, n, r, ["argument", c]);
              for (a = v; a instanceof gn; a = a.cdr)
                c = this.compile(a.car, t, n, r, ["argument", c]);
              return this.is_tail(i) ? c : ["frame", c, i];
          }
        }
      },
      _compile_define: function (t, i) {
        if (1 == t.length()) throw new hn("Invalid `define': " + t.to_write());
        var o = t.cdr.car,
          u = t.cdr.cdr;
        if (o instanceof fn) {
          if (u === n) t = r;
          else {
            if (u.cdr !== n) throw new hn("Invalid `define': " + t.to_write());
            t = u.car;
          }
          e.hasOwnProperty(o.name) || (e[o.name] = r),
            (i = ["assign-global", o.name, i]);
        } else {
          if (!(o instanceof gn))
            throw new hn("define: symbol or pair expected but got " + o);
          var a = o.car,
            c = o.cdr;
          (t = new gn(ln("lambda"), new gn(c, u))),
            e.hasOwnProperty(o.name) || (e[a.name] = r),
            (i = ["assign-global", a.name, i]);
        }
        return [t, i];
      },
      _compile_lambda: function (e, t, n, r, i) {
        if (e.length() < 3) throw new hn("Invalid lambda: " + e.to_write());
        var o = e.cdr.car,
          u = e.cdr.cdr,
          a = or.transform_internal_define(u);
        if (yn(a) && Xn(a.car) && "letrec*" == a.car.name) var c = or.expand(a);
        else c = new gn(ln("begin"), e.cdr.cdr);
        var s = this.find_dot_pos(o),
          f = this.dotted2proper(o),
          l = this.find_free(c, f.to_set(), r),
          d = this.find_sets(c, f.to_set()),
          h = this.compile(
            c,
            [f.to_set(), l],
            d.set_union(n.set_intersect(l)),
            r.set_union(f.to_set()),
            ["return"]
          ),
          p = [
            "close",
            o instanceof gn ? o.length() : 0,
            l.size(),
            this.make_boxes(d, f, h),
            i,
            s,
          ];
        return this.collect_free(l, t, p);
      },
      run: function (e) {
        const t = this.compile(e, [new vn(), new vn()], new vn(), new vn(), [
          "halt",
        ]);
        return new ir(t);
      },
    });
  (or.compile = function (e, t) {
    return (e = or.expand(e)), new or().run(e, t);
  }),
    (or.expand = function (r, i) {
      var o = or.expand;
      i || (i = {});
      var u = null;
      if (r instanceof gn)
        switch (r.car) {
          case ln("define"):
            var a = r.cdr.car,
              c = r.cdr.cdr;
            u = new gn(ln("define"), new gn(a, o(c, i)));
            break;
          case ln("begin"):
            u = new gn(ln("begin"), o(r.cdr, i));
            break;
          case ln("quote"):
            u = r;
            break;
          case ln("lambda"):
            var s = r.cdr.car,
              f = r.cdr.cdr;
            u = new gn(ln("lambda"), new gn(s, o(f, i)));
            break;
          case ln("if"):
            var l = r.second(),
              d = r.third(),
              h = r.fourth();
            u = _n(ln("if"), o(l, i), o(d, i), o(h, i));
            break;
          case ln("set!"):
            var p = r.second();
            r = r.third();
            u = _n(ln("set!"), p, o(r, i));
            break;
          case ln("call-with-current-continuation"):
          case ln("call/cc"):
            r = r.second();
            u = _n(ln("call/cc"), o(r, i));
            break;
          default:
            var m = null;
            if (
              (Xn(r.car) &&
                (e[r.car.name] instanceof rr
                  ? (m = e[r.car.name])
                  : t[r.car.name] instanceof rr && (m = t[r.car.name])),
              m)
            ) {
              var v;
              for (
                i.modified = !0, u = m.transform(r);
                (u = o(u, (v = {}))), v.modified;

              );
            } else {
              var g,
                y = o(r.car, i);
              if (!(r.cdr instanceof gn) && r.cdr !== n)
                throw new hn(
                  "proper list required for function application or macro use: " +
                    to_write(r)
                );
              (g = xn(
                r.cdr.to_array().map(function (e) {
                  return o(e, i);
                })
              )),
                (u = new gn(y, g));
            }
        }
      else u = r;
      return u;
    });
  var ur = function (e) {
      return yn(e) && ln("define") === e.car;
    },
    ar = function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr;
      if (Xn(t)) return new gn((r = t), n);
      var r = t.car,
        i = new gn(ln("lambda"), new gn(t.cdr, n));
      return _n(r, i);
    };
  or.transform_internal_define = function (e) {
    for (var t = [], n = e; ur(n.car); ) t.push(n.car), (n = n.cdr);
    var r = n;
    if (0 == t.length) return e;
    var i = _n.apply(null, Tt(t, ar));
    return new gn(ln("letrec*"), new gn(i, r));
  };
  const cr = function (e) {
      return function () {
        const t = "";
        e.apply(this, [t].concat(Wt(arguments)));
      };
    },
    sr = function (e, t, n) {
      return cr(function (n, r, i) {
        const o = i ? "(" + i + "): " : "";
        if (!t(r)) throw new hn(o + e + " required, but got " + Mn(r));
      });
    },
    fr = cn.create({
      initialize: function (e, t, n) {
        (this.mutable = void 0 === n || !!n),
          (this.hash_proc = e),
          (this.equiv_proc = t),
          (this.pairs_of = {});
      },
      clear: function () {
        this.pairs_of = {};
      },
      candidate_pairs: function (e) {
        return this.pairs_of[e];
      },
      add_pair: function (e, t, n) {
        var r = this.pairs_of[e];
        r ? r.push([t, n]) : (this.pairs_of[e] = [[t, n]]);
      },
      remove_pair: function (e, t) {
        var n = this.pairs_of[e],
          r = n.indexOf(t);
        if (-1 == r) throw new pn("Hashtable#remove_pair: pair not found!");
        n.splice(r, 1);
      },
      create_copy: function (e) {
        var t = new fr(this.hash_proc, this.equiv_proc, e);
        return (
          St(
            ue(this.pairs_of),
            at(function (e) {
              var n = Tt(this.pairs_of[e], function (e) {
                return De(e);
              });
              t.pairs_of[e] = n;
            }, this)
          ),
          t
        );
      },
      size: function () {
        var e = 0;
        return (
          this._apply_pair(function (t) {
            e++;
          }),
          e
        );
      },
      keys: function () {
        return this._apply_pair(function (e) {
          return e[0];
        });
      },
      values: function () {
        return this._apply_pair(function (e) {
          return e[1];
        });
      },
      _apply_pair: function (e) {
        var t = [];
        return (
          St(ke(this.pairs_of), function (n) {
            St(n, function (n) {
              t.push(e(n));
            });
          }),
          t
        );
      },
      to_write: function () {
        return "#<Hashtable size=" + this.size() + ">";
      },
    }),
    lr = function (e) {
      return e instanceof fr;
    };
  (fr.equal_hash = function (e) {
    return Mn(e[0]);
  }),
    (fr.eq_hash = fr.equal_hash),
    (fr.eqv_hash = fr.equal_hash),
    (fr.string_hash = function (e) {
      return e[0];
    }),
    (fr.string_ci_hash = function (e) {
      return A(e[0]) ? e[0].toLowerCase() : e[0];
    }),
    (fr.symbol_hash = function (e) {
      return e[0] instanceof fn ? e[0].name : e[0];
    }),
    (fr.eq_equiv = function (e) {
      return Qn(e[0], e[1]);
    }),
    (fr.eqv_equiv = function (e) {
      return Kn(e[0], e[1]);
    });
  const dr = cn.create({
    initialize: function (e) {
      (this.tokens = this.tokenize(e)), (this.i = 0);
    },
    insert: function (e) {
      this.tokens.splice(this.i, 0, ...this.tokenize(e));
    },
    inspect: function () {
      return [
        "#<Parser:",
        this.i,
        "/",
        this.tokens.length,
        " ",
        An(this.tokens),
        ">",
      ].join("");
    },
    tokenize: function (e) {
      for (var t = new Array(), n = null, r = 0; "" != e && n != e; )
        (n = e),
          (e = e.replace(
            /^\s*(;[^\r\n]*(\r|\n|$)|#;|#\||#\\[^\w]|#?(\(|\[|{)|\)|\]|}|\'|`|,@|,|\+inf\.0|-inf\.0|\+nan\.0|\"(\\(.|$)|[^\"\\])*(\"|$)|[^\s()\[\]{}]+)/,
            function (e, n) {
              var i = n;
              if ("#|" == i) return r++, "";
              if (r > 0) {
                if (/(.*\|#)/.test(i)) {
                  if (--r < 0)
                    throw new hn("Found an extra comment terminator: `|#'");
                  return i.substring(RegExp.$1.length, i.length);
                }
                return "";
              }
              return ";" != i.charAt(0) && (t[t.length] = i), "";
            }
          ));
      return t;
    },
    sexpCommentMarker: new Object(),
    getObject: function () {
      var e = this.getObject0();
      if (e != this.sexpCommentMarker) return e;
      if ((e = this.getObject()) == dr.EOS)
        throw new hn("Readable object not found after S exression comment");
      return (e = this.getObject());
    },
    getList: function (e) {
      for (var t = n, r = t; this.i < this.tokens.length; ) {
        if (
          (this.eatObjectsInSexpComment(
            "Input stream terminated unexpectedly(in list)"
          ),
          ")" == this.tokens[this.i] ||
            "]" == this.tokens[this.i] ||
            "}" == this.tokens[this.i])
        ) {
          this.i++;
          break;
        }
        if ("." == this.tokens[this.i]) {
          this.i++;
          var i = this.getObject();
          i != dr.EOS && t != n && (r.cdr = i);
        } else {
          var o = new gn(this.getObject(), n);
          t == n ? (t = o) : (r.cdr = o), (r = o);
        }
      }
      return t;
    },
    getVector: function (e) {
      for (var t = new Array(); this.i < this.tokens.length; ) {
        if (
          (this.eatObjectsInSexpComment(
            "Input stream terminated unexpectedly(in vector)"
          ),
          ")" == this.tokens[this.i] ||
            "]" == this.tokens[this.i] ||
            "}" == this.tokens[this.i])
        ) {
          this.i++;
          break;
        }
        t[t.length] = this.getObject();
      }
      return t;
    },
    eatObjectsInSexpComment: function (e) {
      for (; "#;" == this.tokens[this.i]; )
        if (
          (this.i++, this.getObject() == dr.EOS || this.i >= this.tokens.length)
        )
          throw new hn(e);
    },
    getObject0: function () {
      if (this.i >= this.tokens.length) return dr.EOS;
      var e = this.tokens[this.i++];
      if ("#;" == e) return this.sexpCommentMarker;
      var t,
        r =
          "'" == e
            ? "quote"
            : "`" == e
            ? "quasiquote"
            : "," == e
            ? "unquote"
            : ",@" == e && "unquote-splicing";
      if (
        r ||
        "(" == e ||
        "#(" == e ||
        "[" == e ||
        "#[" == e ||
        "{" == e ||
        "#{" == e
      )
        return r
          ? new gn(ln(r), new gn(this.getObject(), n))
          : "(" == e || "[" == e || "{" == e
          ? this.getList(e)
          : this.getVector(e);
      switch (e) {
        case "+inf.0":
          return 1 / 0;
        case "-inf.0":
          return -1 / 0;
        case "+nan.0":
          return NaN;
      }
      if (
        ((t = /^#x[0-9a-z]+$/i.test(e)
          ? new Number("0x" + e.substring(2, e.length))
          : /^#d[0-9\.]+$/i.test(e)
          ? new Number(e.substring(2, e.length))
          : new Number(e)),
        isNaN(t))
      ) {
        if ("#f" == e || "#F" == e) return !1;
        if ("#t" == e || "#T" == e) return !0;
        if ("#\\newline" == e.toLowerCase()) return Rn.get("\n");
        if ("#\\space" == e.toLowerCase()) return Rn.get(" ");
        if ("#\\tab" == e.toLowerCase()) return Rn.get("\t");
        if (/^#\\.$/.test(e)) return Rn.get(e.charAt(2));
        if (/^#\\x[a-zA-Z0-9]+$/.test(e)) {
          var i = parseInt(e.slice(3), 16);
          if (i >= 55296 && i <= 57343)
            throw new hn("Character in Unicode excluded range.");
          if (i > 65535) throw new hn("Character literal out of range.");
          return Rn.get(String.fromCharCode(i));
        }
        return /^\"(\\(.|$)|[^\"\\])*\"?$/.test(e)
          ? e
              .replace(/(\r?\n|\\n)/g, "\n")
              .replace(/^\"|\\(.|$)|\"$/g, function (e, t) {
                return t || "";
              })
          : ln(e);
      }
      return t.valueOf();
    },
  });
  (dr.EOS = new Object()),
    (dr.parse = (e) => {
      const t = new dr(e),
        n = [];
      for (;;) {
        var r = t.getObject();
        if (r === dr.EOS) break;
        n.push(r);
      }
      return n;
    });
  const hr = cn.create({
    initialize: function () {
      var e = null,
        t = null;
      2 == arguments.length
        ? ((e = arguments[0]), (t = arguments[1]))
        : 1 == arguments.length && arguments[0] instanceof hr
        ? (e = arguments[0])
        : 1 == arguments.length &&
          "function" == typeof arguments[0] &&
          (t = arguments[0]),
        (this.stack = []),
        (this.on_error = t || (e ? e.on_error : function (e) {})),
        (this.after_evaluate = function () {}),
        (this.last_refer = e ? e.last_refer : null),
        (this.call_stack = e ? De(e.call_stack) : []),
        (this.tco_counter = []),
        (this.max_trace_size = e ? e.max_trace_size : 40),
        (this.current_dynamic_winder = hr.DynamicWind.ROOT);
    },
    inspect: function () {
      return [
        "#<Interpreter: stack size=>",
        this.stack.length,
        " ",
        "after_evaluate=",
        An(this.after_evaluate),
        ">",
      ].join("");
    },
    push: function (e, t) {
      return (this.stack[t] = e), t + 1;
    },
    save_stack: function (e) {
      for (var t = [], n = 0; n < e; n++) t[n] = this.stack[n];
      return {
        stack: t,
        last_refer: this.last_refer,
        call_stack: De(this.call_stack),
        tco_counter: De(this.tco_counter),
      };
    },
    restore_stack: function (e) {
      const t = e.stack,
        n = t.length;
      for (var r = 0; r < n; r++) this.stack[r] = t[r];
      return (
        (this.last_refer = e.last_refer),
        (this.call_stack = De(e.call_stack)),
        (this.tco_counter = De(e.tco_counter)),
        n
      );
    },
    capture_continuation: function (e, t) {
      var n = this.push(t, e);
      return this.closure(
        ["nuate1", this.save_stack(n), this.current_dynamic_winder],
        1,
        0,
        null,
        -1
      );
    },
    shift_args: function (e, t, n) {
      for (var r = e; r >= 0; r--)
        this.index_set(n, r + t + 1, this.index(n, r));
      return n - t - 1;
    },
    index: function (e, t) {
      return this.stack[e - 1 - t];
    },
    index_set: function (e, t, n) {
      this.stack[e - 1 - t] = n;
    },
    closure: function (e, t, n, r, i) {
      const o = [];
      for (var u = 0; u < n; u++) o[u] = this.index(r, u);
      return new zn(e, o, i, -1 == i ? t : void 0);
    },
    run_dump_hook: function (e, t, n, r, i) {
      var o, u;
      if (this.dumper) o = this.dumper;
      else {
        if (!hr.dumper) return;
        o = hr.dumper;
      }
      o &&
        ((u = { a: e, f: n, c: r, s: i, x: t, stack: this.stack }), o.dump(u));
    },
    _execute: function (i, o, u, a, c) {
      for (var s = null; ; )
        switch ((this.run_dump_hook(i, o, u, a, c), o[0])) {
          case "halt":
            return i;
          case "refer-local":
            var f = o[1];
            o = o[2];
            (i = this.index(u, f + 1)), (this.last_refer = "(anon)");
            break;
          case "refer-free":
            (f = o[1]), (o = o[2]);
            (i = a.freevars[f]), (this.last_refer = "(anon)");
            break;
          case "refer-global":
            var l = o[1];
            o = o[2];
            if (e.hasOwnProperty(l)) var d = e[l];
            else {
              if (!t.hasOwnProperty(l))
                throw new hn("execute: unbound symbol: " + An(l));
              d = t[l];
            }
            (i = d), (this.last_refer = l || "(anon)");
            break;
          case "indirect":
            o = o[1];
            i = i[0];
            break;
          case "constant":
            var h = o[1];
            o = o[2];
            (i = h), (this.last_refer = "(anon)");
            break;
          case "close":
            var p = o,
              m = p[1],
              v = ((f = p[2]), p[3]),
              g = ((o = p[4]), p[5]);
            (i = this.closure(v, m, f, c, g)), (c -= f);
            break;
          case "box":
            (f = o[1]), (o = o[2]);
            this.index_set(c, f + 1, [this.index(c, f + 1)]);
            break;
          case "test":
            var y = o[1],
              w = o[2];
            o = !1 !== i ? y : w;
            break;
          case "assign-global":
            var b = o[1];
            o = o[2];
            if (!e.hasOwnProperty(b) && !t.hasOwnProperty(b))
              throw new hn("global variable '" + b + "' is not defined");
            (e[b] = i), (i = r);
            break;
          case "assign-local":
            (f = o[1]), (o = o[2]);
            (this.index(u, f + 1)[0] = i), (i = r);
            break;
          case "assign-free":
            (f = o[1]), (o = o[2]);
            (a.freevars[f][0] = i), (i = r);
            break;
          case "conti":
            (f = o[1]), (o = o[2]);
            i = this.capture_continuation(c, f);
            break;
          case "nuate1":
            var _ = o[1],
              x = o[2],
              k = this.current_dynamic_winder,
              j = hr.DynamicWind.listWinders(k, x);
            o = hr.DynamicWind.joinWinders(j, [
              "refer-local",
              0,
              ["nuate2", _],
            ]);
            break;
          case "nuate2":
            (_ = o[1]), (o = ["return"]);
            c = this.restore_stack(_);
            break;
          case "frame":
            s = o[2];
            (o = o[1]),
              (c = this.push(s, this.push(u, this.push(a, c)))),
              (this.tco_counter[this.tco_counter.length] = 0);
            break;
          case "argument":
            o = o[1];
            c = this.push(i, c);
            break;
          case "shift":
            (f = o[1]), (o = o[2]);
            var S = this.index(c, f + 1);
            c = this.shift_args(f, S, c);
            break;
          case "tco_hinted_apply":
            this.tco_counter[this.tco_counter.length - 1]++,
              (o = ["apply"].concat(Jt(o)));
            break;
          case "apply":
            var T = i;
            this.call_stack.push(this.last_refer),
              this.call_stack.length > this.max_trace_size &&
                this.call_stack.shift();
            S = this.index(c, 0);
            if (Fn(T)) {
              (i = T), (o = T.body);
              const e = T.dotpos;
              if (e >= 0) {
                for (var C = n, E = S; --E >= e; )
                  C = new gn(this.index(c, E + 1), C);
                if (e >= S) {
                  for (E = 0; E < S + 1; E++)
                    this.index_set(c, E - 1, this.index(c, E));
                  c++, this.index_set(c, 0, this.index(c, 0) + 1);
                }
                this.index_set(c, e + 1, C);
              } else if (void 0 !== T.expected_args && S != T.expected_args) {
                var q =
                  "Function call error: got " +
                  S +
                  " but wanted " +
                  T.expected_args;
                throw new hn(q);
              }
              (u = c), (a = T);
            } else {
              if (!(T instanceof Function))
                throw new hn(An(T) + " is not a function");
              var A = [];
              for (E = 0; E < S; E++) A.push(this.index(c, E + 1));
              var D = T(A, this);
              if (D instanceof In) {
                var N = D;
                return N.set_state(this, ["return"], u, a, c), N.ready(), N;
              }
              if (D instanceof tr) {
                var O = [
                    "frame",
                    [
                      "argument",
                      [
                        "constant",
                        1,
                        ["argument", ["constant", D.after, ["apply"]]],
                      ],
                    ],
                    ["return"],
                  ],
                  M = [
                    "constant",
                    D.args.length,
                    [
                      "argument",
                      ["constant", D.proc, ["apply", D.args.length]],
                    ],
                  ];
                o = [
                  "frame",
                  Et(
                    D.args,
                    function (e, t) {
                      return ["constant", t, ["argument", e]];
                    },
                    M
                  ),
                  O,
                ];
              } else (i = D), (o = ["return"]);
            }
            break;
          case "return":
            var L = c - (f = this.index(c, 0));
            (o = this.index(L, 1)),
              (u = this.index(L, 2)),
              (a = this.index(L, 3)),
              (c = L - 3 - 1);
            var R = 1 + this.tco_counter[this.tco_counter.length - 1];
            this.call_stack.splice(-R), this.tco_counter.pop();
            break;
          default:
            throw new pn("unknown opecode type: " + o[0]);
        }
      return i;
    },
    evaluate: function (e, t) {
      (this.call_stack = []),
        (this.parser = new dr(e)),
        (this.compiler = new or()),
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
    },
    resume: function (e, t, n, i, o, u) {
      for (var a = r; ; ) {
        if (e) (a = this._execute(t, n, i, o, u)), (e = !1);
        else {
          if (!this.parser) break;
          var c = this.parser.getObject();
          if (c === dr.EOS) break;
          c = or.expand(c);
          const e = this.compiler.run(c);
          a = this._execute(c, e.il, 0, [], 0);
        }
        if (a instanceof In) return a;
      }
      return this.after_evaluate(a), a;
    },
    invoke_closure: function (e, t) {
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
    },
    compile: function (e) {
      var t = hr.read(e);
      return or.compile(t);
    },
    push_dynamic_winder: function (e, t) {
      this.current_dynamic_winder = new hr.DynamicWind(
        this.current_dynamic_winder,
        e,
        t
      );
    },
    pop_dynamic_winder: function (e, t) {
      this.current_dynamic_winder = this.current_dynamic_winder.parent;
    },
  });
  (hr.read = function (e) {
    var t = new dr(e).getObject();
    return t == dr.EOS ? Hn : t;
  }),
    (hr.expand = function () {
      throw "Interpreter.expand is moved to Compiler.expand";
    }),
    (hr.DynamicWind = cn.create({
      initialize: function (e, t, n) {
        (this.parent = e), (this.before = t), (this.after = n);
      },
    })),
    (hr.DynamicWind.ROOT = { _: "this is ROOT." }),
    (hr.DynamicWind.listWinders = function (e, t) {
      for (var n = [e]; e !== hr.DynamicWind.ROOT; ) (e = e.parent), n.push(e);
      for (var r, i = []; ; ) {
        var o = n.find(function (e) {
          return e === t;
        });
        if (o) {
          r = o;
          break;
        }
        i.push(t), (t = t.parent);
      }
      for (var u = [], a = 0; a < n.length && n[a] !== r; a++)
        u.push(n[a].after);
      return (
        i.reverse(),
        i.forEach(function (e) {
          u.push(e.before);
        }),
        u
      );
    }),
    (hr.DynamicWind.joinWinders = function (e, t) {
      return e.reduceRight(function (e, t) {
        return [
          "frame",
          ["constant", 0, ["argument", ["constant", t, ["apply"]]]],
          e,
        ];
      }, t);
    });
  const pr = cn.create({
    initialize: function (e, t) {
      (this.real = e), (this.imag = t);
    },
    magnitude: function () {
      return Math.sqrt(this.real * this.real + this.imag * this.imag);
    },
    angle: function () {
      return Math.atan2(this.imag, this.real);
    },
    isReal: function () {
      return 0 == this.imag;
    },
    isRational: function () {
      return 0 == this.imag && wr(this.real);
    },
    isInteger: function () {
      return 0 == this.imag && br(this.real);
    },
    toString: function (e) {
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
    },
  });
  (pr.from_polar = function (e, t) {
    var n = e * Math.cos(t),
      r = e * Math.sin(t);
    return new pr(n, r);
  }),
    (pr.assure = function (e) {
      return e instanceof pr ? e : new pr(e, 0);
    });
  const mr = cn.create({
      initialize: function (e, t) {
        (this.numerator = e), (this.denominator = t);
      },
      isInteger: function () {},
    }),
    vr = function (e) {
      return e instanceof pr || e instanceof mr || "number" == typeof e;
    },
    gr = vr,
    yr = function (e) {
      return e instanceof pr || e instanceof mr
        ? e.isReal()
        : "number" == typeof e;
    },
    wr = function (e) {
      return e instanceof pr
        ? e.isRational()
        : e instanceof mr || "number" == typeof e;
    },
    br = function (e) {
      return e instanceof pr || e instanceof mr
        ? e.isInteger()
        : "number" == typeof e && e % 1 == 0;
    },
    _r = cn.create({
      initialize: function (e, t) {
        this.box = [e, t];
      },
      is_done: function () {
        return this.box[0];
      },
      value: function () {
        if (!this.is_done()) throw new pn("this promise is not calculated yet");
        return this.box[1];
      },
      thunk: function () {
        if (this.is_done())
          throw new pn("this promise does not know the thunk");
        return this.box[1];
      },
      update_with: function (e) {
        (this.box[0] = e.box[0]), (this.box[1] = e.box[1]), (e.box = this.box);
      },
    }),
    xr = function (e) {
      return e instanceof _r;
    };
  (_r.fresh = function (e) {
    return new _r(!1, e);
  }),
    (_r.done = function (e) {
      return new _r(!0, e);
    });
  const kr = function (e, n, r, i) {
      var o = function (t, o) {
        return (
          (function (e, t, n, r) {
            if (t < n)
              throw new hn(
                r && r == n
                  ? e +
                    ": wrong number of arguments (expected: " +
                    n +
                    " got: " +
                    t +
                    ")"
                  : e +
                    ": too few arguments (at least: " +
                    n +
                    " got: " +
                    t +
                    ")"
              );
            if (r && r < t)
              throw new hn(
                e + ": too many arguments (at most: " + r + " got: " + t + ")"
              );
          })(e, t.length, n, r),
          i(t, o)
        );
      };
      (i.fname = e),
        (o.inspect = function () {
          return this.fname;
        }),
        (t[e] = o);
    },
    jr = function (e, n) {
      t[e]
        ? V(n)
          ? Tt(n, function (t) {
              jr(e, t);
            })
          : A(n)
          ? (t[n] = t[e])
          : console.error(
              "[BUG] bad alias for library function `" +
                e +
                "': " +
                n.toString()
            )
        : console.error(
            "[BUG] library function `" +
              e +
              "' does not exist, so can't alias it."
          );
    },
    Sr = function (e, n) {
      var r = new rr(e, n);
      t[e] = r;
    },
    Tr = sr("number", function (e) {
      return "number" == typeof e || e instanceof pr;
    }),
    Cr = sr("integer", function (e) {
      return "number" == typeof e && e % 1 == 0;
    }),
    Er = sr("real number", function (e) {
      return "number" == typeof e;
    }),
    qr = cr(function (e, t, n, r) {
      if ("number" != typeof t || t != Math.round(t))
        throw new hn(e + ": number required, but got " + Mn(t));
      if (t < n || r < t)
        throw new hn(
          e +
            ": number must be between " +
            n +
            " and " +
            r +
            ", but got " +
            Mn(t)
        );
    }),
    Ar = sr("string", Wn),
    Dr = sr("character", Vn),
    Nr = sr("symbol", Xn),
    Or = sr("port", Yn),
    Mr = sr("pair", yn),
    Lr = sr("list", wn),
    Rr = sr("vector", Gn),
    Ir = sr("hashtable", lr),
    Hr = sr("promise", xr),
    Pr = sr("JavaScript function", Un),
    zr = sr("scheme closure", Fn),
    Fr = sr("scheme/js function", function (e) {
      return Fn(e) || Un(e);
    }),
    Br = sr("date", function (e) {
      return e instanceof Date;
    }),
    $r = cr(function (e, t, n, r) {
      if (!t) throw new hn((r || e) + ": " + n);
    }),
    Wr = function (e, t, n) {
      var r =
        e +
        " is deprecated and will be removed in BiwaScheme " +
        t +
        ". Please use " +
        n +
        " instead";
      console.warn(r);
    },
    Ur = function (e) {
      Ar(e);
      var t = e.split("/");
      if (2 !== t.length) return !1;
      var n = t[0],
        r = t[1],
        i = Xr(n, 10),
        o = Xr(r, 10);
      return !1 !== i && !1 !== o && !(o <= 0) && i / o;
    },
    Vr = function (e, t) {
      if ((Ar(e), Cr(t), t < 2 || t > 36)) return !1;
      var n = "0123456789abcdefghijklmnopqrstuvwxyz".slice(0, t);
      return new RegExp("^[+-]?[" + n + "]+$", "ig").test(e);
    },
    Xr = function (e, t) {
      if ((Ar(e), Cr(t), t < 2 || t > 36)) return !1;
      if (!Vr(e, t)) return !1;
      var n = parseInt(e, t);
      return !Number.isNaN(n) && n;
    },
    Yr = function (e) {
      Ar(e);
      return (
        !(
          !/^[+-]?[0-9]+[.]?[0-9]*e[+-]?[0-9]+$/i.test(e) &&
          !/(^[+-]?[0-9]*[.][0-9]+$)|(^[+-]?[0-9]+[.][0-9]*$)/.test(e)
        ) || Vr(e, 10)
      );
    },
    Gr = function (e) {
      if ((Ar(e), !Yr(e))) return !1;
      var t = new Number(e).valueOf();
      return !Number.isNaN(t) && !!Number.isFinite(t) && t;
    },
    Jr = {};
  (Jr.EnumType = cn.create({
    initialize: function (e) {
      this.members = en(e);
    },
    universe: function () {
      return new Jr.EnumSet(this, this.members);
    },
    indexer: function () {
      return at(function (e) {
        Nr(e[0], "(enum-set indexer)");
        var t = xt(this.members, e[0]);
        return -1 !== t && t;
      }, this);
    },
    constructor: function () {
      return at(function (e) {
        Lr(e[0], "(enum-set constructor)");
        var t = e[0].to_array();
        return (
          St(t, function (e) {
            Nr(e, "(enum-set constructor)");
          }),
          new Jr.EnumSet(this, t)
        );
      }, this);
    },
  })),
    cn.memoize(Jr.EnumType, ["universe", "indexer", "constructor"]),
    (Jr.EnumSet = cn.create({
      initialize: function (e, t) {
        (this.enum_type = e),
          (this.symbols = At(e.members, function (e) {
            return Ot(t, e);
          }));
      },
      symbol_list: function () {
        return xn(this.symbols);
      },
      is_member: function (e) {
        return Ot(this.symbols, e);
      },
      is_subset: function (e) {
        return (
          !Nt(this.symbols, function (t) {
            return !Ot(e.symbols, t);
          }) &&
          (this.enum_type === e.enum_type ||
            Dt(this.enum_type.members, function (t) {
              return Ot(e.enum_type.members, t);
            }))
        );
      },
      equal_to: function (e) {
        return this.is_subset(e) && e.is_subset(this);
      },
      union: function (e) {
        var t = At(
          this.enum_type.members,
          at(function (t) {
            return Ot(this.symbols, t) || Ot(e.symbols, t);
          }, this)
        );
        return new Jr.EnumSet(this.enum_type, t);
      },
      intersection: function (e) {
        var t = At(this.symbols, function (t) {
          return Ot(e.symbols, t);
        });
        return new Jr.EnumSet(this.enum_type, t);
      },
      difference: function (e) {
        var t = At(this.symbols, function (t) {
          return !Ot(e.symbols, t);
        });
        return new Jr.EnumSet(this.enum_type, t);
      },
      complement: function () {
        var e = At(
          this.enum_type.members,
          at(function (e) {
            return !Ot(this.symbols, e);
          }, this)
        );
        return new Jr.EnumSet(this.enum_type, e);
      },
      projection: function (e) {
        var t = At(this.symbols, function (t) {
          return Ot(e.enum_type.members, t);
        });
        return new Jr.EnumSet(e.enum_type, t);
      },
      toString: function () {
        return "#<EnumSet " + An(this.symbols) + ">";
      },
    })),
    cn.memoize(Jr.EnumSet, "symbol_list");
  const Qr = function (e) {
      return e instanceof Jr.EnumSet;
    },
    Kr = sr("enum_set", Qr),
    Zr = cn.create({
      initialize: function (e, t) {
        ii(e, "new Record"), (this.rtd = e), (this.fields = t);
      },
      get: function (e) {
        return this.fields[e];
      },
      set: function (e, t) {
        this.fields[e] = t;
      },
      toString: function () {
        var e = Mn(this.fields);
        return "#<Record " + this.rtd.name + " " + e + ">";
      },
    }),
    ei = function (e) {
      return e instanceof Zr;
    };
  (Zr._DefinedTypes = {}),
    (Zr.define_type = function (e, t, n) {
      return (Zr._DefinedTypes[e] = { rtd: t, cd: n });
    }),
    (Zr.get_type = function (e) {
      return Zr._DefinedTypes[e];
    }),
    (Zr.RTD = cn.create({
      initialize: function (e, t, n, r, i, o) {
        (this.name = e),
          (this.parent_rtd = t),
          (this.is_base_type = !t),
          n
            ? ((this.uid = n), (this.generative = !1))
            : ((this.uid = this._generate_new_uid()), (this.generative = !0)),
          (this.sealed = !!r),
          (this.opaque = t.opaque || !!i),
          (this.fields = Tt(o, function (e) {
            return { name: e[0], mutable: !!e[1] };
          }));
      },
      field_name: function (e) {
        for (
          var t = this._field_names(), n = this.parent_rtd;
          n;
          n = n.parent_rtd
        )
          t = n._field_names() + t;
        return t[e];
      },
      _field_names: function () {
        return Tt(this.fields, function (e) {
          return e.name;
        });
      },
      _generate_new_uid: function () {
        return ln(it("__record_td_uid"));
      },
      toString: function () {
        return "#<RecordTD " + name + ">";
      },
    })),
    (Zr.RTD.NongenerativeRecords = {});
  const ti = function (e) {
    return e instanceof Zr.RTD;
  };
  Zr.CD = cn.create({
    initialize: function (e, t, n) {
      this._check(e, t, n),
        (this.rtd = e),
        (this.parent_cd = t),
        n
          ? ((this.has_custom_protocol = !0), (this.protocol = n))
          : ((this.has_custom_protocol = !1),
            e.parent_rtd
              ? (this.protocol = this._default_protocol_for_derived_types())
              : (this.protocol = this._default_protocol_for_base_types()));
    },
    _check: function (e, t, n) {
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
    },
    _default_protocol_for_base_types: function () {
      return function (e) {
        var t = e[0];
        return Fr(t, "_default_protocol/base"), t;
      };
    },
    _default_protocol_for_derived_types: function () {
      var e = this.rtd;
      return function (t) {
        var n = t[0];
        Fr(n, "_default_protocol/n");
        return function (t) {
          var r = e.fields.length,
            i = t.length - r,
            o = t.slice(0, i),
            u = t.slice(i);
          return new tr(n, o, function (e) {
            var t = e[0];
            return (
              Fr(t, "_default_protocol/p"),
              new tr(t, u, function (e) {
                var t = e[0];
                return ri(t, "_default_protocol/result"), t;
              })
            );
          });
        };
      };
    },
    toString: function () {
      return "#<RecordCD " + this.rtd.name + ">";
    },
    record_constructor: function () {
      var e = this.parent_cd ? this._make_n([], this.rtd) : this._make_p();
      return (
        (e = at(e, this)),
        new tr(this.protocol, [e], function (e) {
          var t = e[0];
          return Fr(t, "record_constructor"), t;
        })
      );
    },
    _make_p: function () {
      return function (e) {
        return new Zr(this.rtd, e);
      };
    },
    _make_n: function (e, t) {
      var n = this.parent_cd;
      return n
        ? function (r) {
            return function (i) {
              var o = [].concat(i[0]).concat(e),
                u = n._make_n(o, t);
              return new tr(n.protocol, [u], function (e) {
                var t = e[0];
                return (
                  Fr(t, "_make_n"),
                  new tr(t, r, function (e) {
                    var t = e[0];
                    return ri(t), t;
                  })
                );
              });
            };
          }
        : function (n) {
            var r = n.concat(e);
            return new Zr(t, r);
          };
    },
  });
  const ni = function (e) {
      return e instanceof Zr.CD;
    },
    ri = sr("record", ei),
    ii = sr("record type descriptor", ti),
    oi = sr("record constructor descriptor", ni),
    ui = cn.create({
      initialize: function (e) {
        this.content = e;
      },
      to_write: function () {
        return "#<Values " + Tt(this.content, Mn).join(" ") + ">";
      },
    }),
    ai = {};
  kr("html-escape", 1, 1, function (e) {
    return Ar(e[0]), Ge(e[0]);
  });
  const ci = function (e) {
    return Tt(e, An).join(", ");
  };
  kr("inspect", 1, null, function (e) {
    return ci(e);
  }),
    kr("inspect!", 1, null, function (e) {
      return ai.puts(ci(e)), r;
    });
  const si = function (e) {
    switch (!0) {
      case D(e) || A(e) || !0 === e || !1 === e:
        return e;
      case V(e):
        return xn(Tt(e, si));
      case "object" == typeof e:
        var t = n;
        for (key in e) t = new gn(new gn(key, si(e[key])), t);
        return t;
      default:
        throw new Error(
          "json->sexp: detected invalid value for json: " + An(e)
        );
    }
  };
  kr("json->sexp", 1, 1, function (e) {
    return si(e[0]);
  }),
    kr("vector-push!", 2, null, function (e) {
      Rr(e[0]);
      for (var t = 1; t < e.length; t++) e[0].push(e[t]);
      return e[0];
    }),
    kr("identity", 1, 1, function (e) {
      return e[0];
    }),
    Sr("inc!", function (e) {
      var t = e.cdr.car;
      return _n(ln("begin"), _n(ln("set!"), t, _n(ln("+"), t, 1)), t);
    }),
    Sr("dec!", function (e) {
      var t = e.cdr.car;
      return _n(ln("begin"), _n(ln("set!"), t, _n(ln("-"), t, 1)), t);
    }),
    kr("string-concat", 1, 1, function (e) {
      return Lr(e[0]), e[0].to_array().join("");
    }),
    kr("string-split", 2, 2, function (e) {
      return Ar(e[0]), Ar(e[1]), xn(e[0].split(e[1]));
    }),
    kr("string-join", 1, 2, function (e) {
      Lr(e[0]);
      var t = "";
      return e[1] && (Ar(e[1]), (t = e[1])), e[0].to_array().join(t);
    }),
    kr("intersperse", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      Lr(n);
      var r = [];
      return (
        St(n.to_array().reverse(), function (e) {
          r.push(e), r.push(t);
        }),
        r.pop(),
        xn(r)
      );
    }),
    kr("map-with-index", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      St(n, Lr);
      var r = [],
        i = 0;
      return tr.multi_foreach(n, {
        call: function (e) {
          var n = Tt(e, function (e) {
            return e.car;
          });
          return n.unshift(i), i++, new tr(t, n);
        },
        result: function (e) {
          r.push(e);
        },
        finish: function () {
          return xn(r);
        },
      });
    }),
    Sr("dotimes", function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr,
        r = t.car,
        i = t.cdr.car,
        o = t.cdr.cdr.car,
        u = dn(),
        a = kn([
          [u, i],
          [r, 0, [ln("+"), r, 1]],
        ]),
        c = kn([[ln(">="), r, u], o]);
      return new gn(ln("do"), new gn(a, new gn(c, n)));
    });
  var fi = function (e, t, n) {
    return e.sort(function (e, r) {
      return new hr(n).invoke_closure(t, [e, r]);
    });
  };
  kr("list-sort/comp", 1, 2, function (e, t) {
    return Fr(e[0]), Lr(e[1]), xn(fi(e[1].to_array(), e[0], t));
  }),
    kr("vector-sort/comp", 1, 2, function (e, t) {
      return Fr(e[0]), Rr(e[1]), fi(De(e[1]), e[0], t);
    }),
    kr("vector-sort/comp!", 1, 2, function (e, t) {
      return Fr(e[0]), Rr(e[1]), fi(e[1], e[0], t), r;
    });
  Sr("define-macro", function (t) {
    var n,
      i = t.cdr.car;
    if (i instanceof gn) {
      var o = i.car;
      n = i.cdr;
      var u = t.cdr.cdr,
        a = new gn(ln("lambda"), new gn(n, u));
    } else {
      (o = i), (a = t.cdr.cdr.car);
      n = a.cdr.car;
    }
    var c = or.compile(a).il;
    if (0 != c[2])
      throw new pn(
        "you cannot use free variables in macro expander (or define-macro must be on toplevel)"
      );
    var s = new zn(c[3], [], -1, void 0);
    return (
      (e[o.name] = new rr(o.name, function (e) {
        var t = e.to_array();
        t.shift();
        var r = new hr(),
          i = (function (e, t) {
            var n = [],
              r = new or().find_dot_pos(e);
            if (-1 == r) n = t;
            else {
              for (var i = 0; i < r; i++) n[i] = t[i];
              n[i] = xn(t.slice(i));
            }
            return n;
          })(n, t);
        return r.invoke_closure(s, i);
      })),
      r
    );
  });
  var li = function (t) {
    if (t instanceof gn) {
      if (!(t.car instanceof fn && e[t.car.name] instanceof rr))
        throw new Error("macroexpand-1: `" + Mn(t) + "' is not a macro");
      t = e[t.car.name].transform(t);
    }
    return t;
  };
  Sr("%macroexpand", function (e) {
    var t = or.expand(e.cdr.car);
    return _n(ln("quote"), t);
  }),
    Sr("%macroexpand-1", function (e) {
      var t = li(e.cdr.car);
      return _n(ln("quote"), t);
    }),
    kr("macroexpand", 1, 1, function (e) {
      return or.expand(e[0]);
    }),
    kr("macroexpand-1", 1, 1, function (e) {
      return li(e[0]);
    }),
    kr("gensym", 0, 0, function (e) {
      return dn();
    }),
    kr("print", 1, null, function (e) {
      return (
        Tt(e, function (e) {
          ai.puts(qn(e), !0);
        }),
        ai.puts(""),
        r
      );
    }),
    kr("write-to-string", 1, 1, function (e) {
      return Mn(e[0]);
    }),
    kr("read-from-string", 1, 1, function (e) {
      return Ar(e[0]), hr.read(e[0]);
    }),
    kr("port-closed?", 1, 1, function (e) {
      return Or(e[0]), !e[0].is_open;
    }),
    kr("with-output-to-port", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      Or(t), Fr(n);
      var r = Pn.current_output;
      return (
        (Pn.current_output = t),
        new tr(n, [t], function (e) {
          return t.close(), (Pn.current_output = r), e[0];
        })
      );
    }),
    Sr("let1", function (e) {
      var t = e.cdr.car,
        r = e.cdr.cdr.car,
        i = e.cdr.cdr.cdr;
      return new gn(
        new gn(ln("lambda"), new gn(new gn(t, n), i)),
        new gn(r, n)
      );
    });
  var di = function (e, t) {
    if (!(e instanceof RegExp))
      throw new Error(t + ": regexp required, but got " + Mn(e));
  };
  kr("string->regexp", 1, 1, function (e) {
    return Ar(e[0], "string->regexp"), new RegExp(e[0]);
  }),
    kr("regexp?", 1, 1, function (e) {
      return e[0] instanceof RegExp;
    }),
    kr("regexp->string", 1, 1, function (e) {
      return di(e[0], "regexp->string"), e[0].toString().slice(1, -1);
    }),
    kr("regexp-exec", 2, 2, function (e) {
      var t = e[0];
      A(e[0]) && (t = new RegExp(e[0])),
        di(t, "regexp-exec"),
        Ar(e[1], "regexp-exec");
      var n = t.exec(e[1]);
      return null !== n && xn(n);
    }),
    kr("regexp-replace-all", 3, 3, function (e) {
      var t = e[0];
      if (A(t)) var n = new RegExp(t, "g");
      else {
        di(t);
        n = new RegExp(t.source, "g");
      }
      return Ar(e[1]), Ar(e[2]), e[1].replace(n, e[2]);
    });
  let hi = eval;
  kr("js-eval", 1, 1, function (e) {
    return hi(e[0]);
  }),
    kr("js-ref", 2, 2, function (e) {
      return A(e[1]) ? e[0][e[1]] : (Nr(e[1]), e[0][e[1].name]);
    }),
    kr("js-set!", 3, 3, function (e) {
      return Ar(e[1]), (e[0][e[1]] = e[2]), r;
    }),
    kr("js-call", 1, null, function (e) {
      var t = e.shift();
      Pr(t);
      return t.apply(null, e);
    }),
    kr("js-invoke", 2, null, function (e) {
      var t = e.shift(),
        n = e.shift();
      if ((A(n) || (Nr(n), (n = n.name)), t[n])) return t[n].apply(t, e);
      throw new Error("js-invoke: function " + n + " is not defined");
    }),
    kr("js-invocation", 2, null, function (e, t) {
      var n = e.shift();
      Xn(n) && (n = hi(n.name));
      var r = n;
      return (
        St(e, function (e) {
          if (Xn(e)) r = r[e.name];
          else {
            if (!wn(e))
              throw new hn(
                "js-invocation: expected list or symbol for callspec but got " +
                  An(e)
              );
            var n = e.to_array();
            Nr(n[0]);
            var i = n.shift().name;
            if (
              ((n = Tt(n, function (e) {
                if (Fn(e)) return pi(e, t);
                if (wn(e)) {
                  var n = {};
                  return (
                    e.foreach(function (e) {
                      Nr(e.car), (n[e.car.name] = e.cdr);
                    }),
                    n
                  );
                }
                return e;
              })),
              !P(r[i]))
            )
              throw new hn("js-invocation: the method `" + i + "' not found");
            r = r[i].apply(r, n);
          }
        }),
        r
      );
    }),
    Sr("..", function (e) {
      if (e.cdr == n) throw new Error("malformed ..");
      return new gn(ln("js-invocation"), e.cdr);
    }),
    kr("js-new", 1, null, function (e, t) {
      var n = function (e) {
          if (e.length % 2 != 0)
            throw new Error("js-new: odd number of key-value pair");
          for (var n = {}, r = 0; r < e.length; r += 2) {
            var i = e[r],
              o = e[r + 1];
            Nr(i), Fn(o) && (o = pi(o, t)), (n[i.name] = o);
          }
          return n;
        },
        r = e.shift();
      if ((A(r) && (r = hi(r)), 0 == e.length)) return new r();
      for (var i = [], o = 0; o < e.length; o++) {
        if (e[o] instanceof fn) {
          i.push(n(e.slice(o)));
          break;
        }
        i.push(e[o]);
      }
      return new (Function.prototype.bind.apply(r, [null].concat(i)))();
    }),
    kr("js-obj", 0, null, function (e) {
      if (e.length % 2 != 0)
        throw new Error("js-obj: number of arguments must be even");
      var t = {};
      for (i = 0; i < e.length / 2; i++)
        Ar(e[2 * i]), (t[e[2 * i]] = e[2 * i + 1]);
      return t;
    });
  const pi = function (e, t) {
    var n = new hr(t);
    return function () {
      return n.invoke_closure(e, Wt(arguments));
    };
  };
  kr("js-closure", 1, 1, function (e, t) {
    return zr(e[0]), pi(e[0], t);
  }),
    kr("js-null?", 1, 1, function (e) {
      return null === e[0];
    }),
    kr("js-undefined?", 1, 1, function (e) {
      return void 0 === e[0];
    }),
    kr("js-function?", 1, 1, function (e) {
      return P(e[0]);
    }),
    kr("js-array-to-list", 1, 1, function (e) {
      return Wr("js-array-to-list", "1.0", "js-array->list"), xn(e[0]);
    }),
    kr("js-array->list", 1, 1, function (e) {
      return xn(e[0]);
    }),
    kr("list-to-js-array", 1, 1, function (e) {
      return Wr("list-to-js-array", "1.0", "list->js-array"), e[0].to_array();
    }),
    kr("list->js-array", 1, 1, function (e) {
      return e[0].to_array();
    }),
    kr("alist-to-js-obj", 1, 1, function (e) {
      return Wr("alist-to-js-obj", "1.0", "alist->js-obj"), Tn(e[0]);
    }),
    kr("alist->js-obj", 1, 1, function (e) {
      return Lr(e[0]), Tn(e[0]);
    }),
    kr("js-obj-to-alist", 1, 1, function (e) {
      return Wr("js-obj-to-alist", "1.0", "js-obj->alist"), Sn(e[0]);
    }),
    kr("js-obj->alist", 1, 1, function (e) {
      return Sn(e[0]);
    }),
    kr("timer", 2, 2, function (e, t) {
      var n = e[0],
        i = e[1];
      zr(n), Er(i);
      var o = new hr(t);
      return (
        setTimeout(function () {
          o.invoke_closure(n);
        }, 1e3 * i),
        r
      );
    }),
    kr("set-timer!", 2, 2, function (e, t) {
      var n = e[0],
        r = e[1];
      zr(n), Er(r);
      var i = new hr(t);
      return setInterval(function () {
        i.invoke_closure(n);
      }, 1e3 * r);
    }),
    kr("clear-timer!", 1, 1, function (e) {
      var t = e[0];
      return clearInterval(t), r;
    }),
    kr("sleep", 1, 1, function (e) {
      var t = e[0];
      return (
        Er(t),
        new In(function (e) {
          setTimeout(function () {
            e.resume(n);
          }, 1e3 * t);
        })
      );
    });
  var mi = function (e) {
    kr("console-" + e, 1, null, function (t) {
      var n = window.console;
      if (n) {
        var r = Tt(t, function (e) {
          return An(e, { fallback: e });
        });
        n[e].apply(n, r);
      }
      return t[0];
    });
  };
  mi("debug"),
    mi("log"),
    mi("info"),
    mi("warn"),
    mi("error"),
    Sr("cond", function (e) {
      var t = e.cdr;
      if (!(t instanceof gn) || t === n)
        throw new hn("malformed cond: cond needs list but got " + write_ss(t));
      var r = null;
      return (
        St(t.to_array().reverse(), function (e) {
          if (!(e instanceof gn))
            throw new hn("bad clause in cond: " + write_ss(e));
          if (e.car === ln("else")) {
            if (null !== r)
              throw new hn(
                "'else' clause of cond followed by more clauses: " + write_ss(t)
              );
            r =
              e.cdr !== n &&
              (e.cdr.cdr === n ? e.cdr.car : new gn(ln("begin"), e.cdr));
          } else {
            var i = e.car;
            if (e.cdr === n) r = _n(ln("or"), i, r);
            else if (e.cdr.cdr === n) r = _n(ln("if"), i, e.cdr.car, r);
            else if (e.cdr.car === ln("=>")) {
              i = e.car;
              var o = e.cdr.cdr.car,
                u = dn();
              r = _n(ln("let"), _n(_n(u, i)), _n(ln("if"), i, _n(o, u), r));
            } else r = _n(ln("if"), i, new gn(ln("begin"), e.cdr), r);
          }
        }),
        r
      );
    }),
    Sr("case", function (e) {
      var t = dn();
      if (e.cdr === n) throw new hn("case: at least one clause is required");
      if (e.cdr instanceof gn) {
        var r = e.cdr.car,
          i = e.cdr.cdr,
          o = void 0;
        return (
          St(i.to_array().reverse(), function (e) {
            if (e.car === ln("else")) {
              if (void 0 !== o)
                throw new hn(
                  "case: 'else' clause followed by more clauses: " + write_ss(i)
                );
              o = new gn(ln("begin"), e.cdr);
            } else
              o = _n(
                ln("if"),
                new gn(
                  ln("or"),
                  xn(
                    Tt(e.car.to_array(), function (e) {
                      return _n(ln("eqv?"), t, _n(ln("quote"), e));
                    })
                  )
                ),
                new gn(ln("begin"), e.cdr),
                o
              );
          }),
          new gn(ln("let1"), new gn(t, new gn(r, new gn(o, n))))
        );
      }
      throw new hn("case: proper list is required");
    }),
    Sr("and", function (e) {
      if (e.cdr == n) return !0;
      var t = e.cdr.to_array(),
        r = t.length - 1,
        i = t[r];
      for (r -= 1; r >= 0; r--) i = _n(ln("if"), t[r], i, !1);
      return i;
    }),
    Sr("or", function (e) {
      for (var t = e.cdr.to_array(), n = !1, r = t.length - 1; r >= 0; r--)
        n = _n(ln("if"), t[r], t[r], n);
      return n;
    }),
    Sr("let", function (e) {
      var t = null;
      e.cdr.car instanceof fn && ((t = e.cdr.car), (e = e.cdr));
      var r = e.cdr.car,
        i = e.cdr.cdr;
      if (!(r instanceof gn) && r != n)
        throw new hn("let: need a pair for bindings: got " + Mn(r));
      for (var o = n, u = n, a = r; a instanceof gn; a = a.cdr) {
        if (!(a.car instanceof gn))
          throw new hn("let: need a pair for bindings: got " + Mn(a.car));
        (o = new gn(a.car.car, o)), (u = new gn(a.car.cdr.car, u));
      }
      var c = null;
      if (t) {
        (o = xn(o.to_array().reverse())), (u = xn(u.to_array().reverse()));
        var s = new gn(ln("lambda"), new gn(o, i)),
          f = new gn(t, u);
        c = _n(ln("letrec"), new gn(_n(t, s), n), f);
      } else c = new gn(new gn(ln("lambda"), new gn(o, i)), u);
      return c;
    }),
    Sr("let*", function (e) {
      var t = e.cdr.car,
        r = e.cdr.cdr;
      if (t === n) return new gn(ln("let"), new gn(n, r));
      if (!(t instanceof gn))
        throw new hn("let*: need a pair for bindings: got " + Mn(t));
      var i = null;
      return (
        St(t.to_array().reverse(), function (e) {
          i = new gn(
            ln("let"),
            new gn(new gn(e, n), null == i ? r : new gn(i, n))
          );
        }),
        i
      );
    });
  var vi = function (e) {
    var t = e.cdr.car,
      i = e.cdr.cdr;
    if (!(t instanceof gn))
      throw new hn("letrec*: need a pair for bindings: got " + Mn(t));
    var o = i;
    St(t.to_array().reverse(), function (e) {
      o = new gn(new gn(ln("set!"), e), o);
    });
    var u = n;
    return (
      St(t.to_array().reverse(), function (e) {
        u = new gn(new gn(e.car, new gn(r, n)), u);
      }),
      new gn(ln("let"), new gn(u, o))
    );
  };
  Sr("letrec", vi),
    Sr("letrec*", vi),
    Sr("let-values", function (e) {
      var t = e.cdr.car,
        r = e.cdr.cdr,
        i = n,
        o = n;
      St(t.to_array().reverse(), function (e) {
        var t = e.cdr.car,
          r = dn(),
          u = new gn(
            r,
            new gn(new gn(ln("lambda"), new gn(n, new gn(t, n))), n)
          );
        i = new gn(u, i);
        var a = e.car;
        o = new gn(new gn(a, new gn(new gn(r, n), n)), o);
      });
      var u = new gn(ln("let*-values"), new gn(o, r));
      return new gn(ln("let"), new gn(i, new gn(u, n)));
    }),
    Sr("let*-values", function (e) {
      var t = e.cdr.car,
        r = e.cdr.cdr,
        i = null;
      return (
        St(t.to_array().reverse(), function (e) {
          var t = e.car,
            o = e.cdr.car;
          i = new gn(
            ln("call-with-values"),
            new gn(
              new gn(ln("lambda"), new gn(n, new gn(o, n))),
              new gn(
                new gn(ln("lambda"), new gn(t, null == i ? r : new gn(i, n))),
                n
              )
            )
          );
        }),
        i
      );
    }),
    kr("eqv?", 2, 2, function (e) {
      return Kn(e[0], e[1]);
    }),
    kr("eq?", 2, 2, function (e) {
      return Qn(e[0], e[1]);
    }),
    kr("equal?", 2, 2, function (e) {
      return Zn(e[0], e[1]);
    }),
    kr("procedure?", 1, 1, function (e) {
      return Jn(e[0]);
    }),
    kr("number?", 1, 1, function (e) {
      return vr(e[0]);
    }),
    kr("complex?", 1, 1, function (e) {
      return gr(e[0]);
    }),
    kr("real?", 1, 1, function (e) {
      return yr(e[0]);
    }),
    kr("rational?", 1, 1, function (e) {
      return wr(e[0]);
    }),
    kr("integer?", 1, 1, function (e) {
      return br(e[0]);
    }),
    kr("=", 2, null, function (e) {
      var t = e[0];
      Tr(e[0]);
      for (var n = 1; n < e.length; n++) {
        if ((Tr(e[n]), Si(e[n]) != Si(t))) return !1;
        if (Ti(e[n]) != Ti(t)) return !1;
      }
      return !0;
    }),
    kr("<", 2, null, function (e) {
      Tr(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((Tr(e[t]), !(e[t - 1] < e[t]))) return !1;
      return !0;
    }),
    kr(">", 2, null, function (e) {
      Tr(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((Tr(e[t]), !(e[t - 1] > e[t]))) return !1;
      return !0;
    }),
    kr("<=", 2, null, function (e) {
      Tr(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((Tr(e[t]), !(e[t - 1] <= e[t]))) return !1;
      return !0;
    }),
    kr(">=", 2, null, function (e) {
      Tr(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((Tr(e[t]), !(e[t - 1] >= e[t]))) return !1;
      return !0;
    }),
    kr("zero?", 1, 1, function (e) {
      return Tr(e[0]), 0 === e[0];
    }),
    kr("positive?", 1, 1, function (e) {
      return Tr(e[0]), e[0] > 0;
    }),
    kr("negative?", 1, 1, function (e) {
      return Tr(e[0]), e[0] < 0;
    }),
    kr("odd?", 1, 1, function (e) {
      return Tr(e[0]), e[0] % 2 == 1 || e[0] % 2 == -1;
    }),
    kr("even?", 1, 1, function (e) {
      return Tr(e[0]), e[0] % 2 == 0;
    }),
    kr("finite?", 1, 1, function (e) {
      return Tr(e[0]), e[0] != 1 / 0 && e[0] != -1 / 0 && !isNaN(e[0]);
    }),
    kr("infinite?", 1, 1, function (e) {
      return Tr(e[0]), e[0] == 1 / 0 || e[0] == -1 / 0;
    }),
    kr("nan?", 1, 1, function (e) {
      return Tr(e[0]), isNaN(e[0]);
    }),
    kr("max", 2, null, function (e) {
      for (var t = 0; t < e.length; t++) Tr(e[t]);
      return Math.max.apply(null, e);
    }),
    kr("min", 2, null, function (e) {
      for (var t = 0; t < e.length; t++) Tr(e[t]);
      return Math.min.apply(null, e);
    });
  var gi = function (e, t) {
      return 0 === t ? e : new pr(e, t);
    },
    yi = function (e, t) {
      return 0 === t ? e : pr.from_polar(e, t);
    };
  kr("+", 0, null, function (e) {
    for (var t = 0, n = 0, r = 0; r < e.length; r++)
      Tr(e[r]), (t += Si(e[r])), (n += Ti(e[r]));
    return gi(t, n);
  });
  var wi = function (e) {
      return e instanceof pr ? e.magnitude() : e;
    },
    bi = function (e) {
      return e instanceof pr ? e.angle() : 0;
    };
  kr("*", 0, null, function (e) {
    for (var t = 1, n = 0, r = 0; r < e.length; r++)
      Tr(e[r]), (t *= wi(e[r])), (n += bi(e[r]));
    return yi(t, n);
  }),
    kr("-", 1, null, function (e) {
      var t = e.length;
      if ((Tr(e[0]), 1 == t))
        return e[0] instanceof pr ? new pr(-Si(e[0]), -Ti(e[0])) : -e[0];
      for (var n = Si(e[0]), r = Ti(e[0]), i = 1; i < t; i++)
        Tr(e[i]), (n -= Si(e[i])), (r -= Ti(e[i]));
      return gi(n, r);
    }),
    kr("/", 1, null, function (e) {
      var t = e.length;
      if ((Tr(e[0]), 1 == t))
        return e[0] instanceof pr
          ? pr.from_polar(1 / wi(e[0]), -bi(e[0]))
          : 1 / e[0];
      for (var n = wi(e[0]), r = bi(e[0]), i = 1; i < t; i++)
        Tr(e[i]), (n /= wi(e[i])), (r -= bi(e[i]));
      return yi(n, r);
    }),
    kr("abs", 1, 1, function (e) {
      return Tr(e[0]), Math.abs(e[0]);
    });
  var _i = function (e, t) {
      return Math.floor(e / t);
    },
    xi = function (e, t) {
      return e - Math.floor(e / t) * t;
    },
    ki = function (e, t) {
      return e > 0 ? Math.floor(e / t) : Math.ceil(e / t);
    },
    ji = function (e, t) {
      return e > 0 ? e - Math.floor(e / t) * t : e - Math.ceil(e / t) * t;
    };
  kr("div0-and-mod0", 2, 2, function (e) {
    return Tr(e[0]), Tr(e[1]), new ui([_i(e[0], e[1]), xi(e[0], e[1])]);
  }),
    kr("div", 2, 2, function (e) {
      return Tr(e[0]), Tr(e[1]), _i(e[0], e[1]);
    }),
    kr("mod", 2, 2, function (e) {
      return Tr(e[0]), Tr(e[1]), xi(e[0], e[1]);
    }),
    kr("div0-and-mod0", 2, 2, function (e) {
      return Tr(e[0]), Tr(e[1]), new ui([ki(e[0], e[1]), ji(e[0], e[1])]);
    }),
    kr("div0", 2, 2, function (e) {
      return Tr(e[0]), Tr(e[1]), ki(e[0], e[1]);
    }),
    kr("mod0", 2, 2, function (e) {
      return Tr(e[0]), Tr(e[1]), ji(e[0], e[1]);
    }),
    kr("numerator", 1, 1, function (e) {
      if ((Tr(e[0]), e[0] instanceof mr)) return e[0].numerator;
      throw new pn("todo");
    }),
    kr("denominator", 1, 1, function (e) {
      if ((Tr(e[0]), e[0] instanceof mr)) return e[0].denominator;
      throw new pn("todo");
    }),
    kr("floor", 1, 1, function (e) {
      return Tr(e[0]), Math.floor(e[0]);
    }),
    kr("ceiling", 1, 1, function (e) {
      return Tr(e[0]), Math.ceil(e[0]);
    }),
    kr("truncate", 1, 1, function (e) {
      return Tr(e[0]), e[0] < 0 ? Math.ceil(e[0]) : Math.floor(e[0]);
    }),
    kr("round", 1, 1, function (e) {
      return Tr(e[0]), Math.round(e[0]);
    }),
    kr("exp", 1, 1, function (e) {
      return Tr(e[0]), Math.exp(e[0]);
    }),
    kr("log", 1, 2, function (e) {
      var t = e[0],
        n = e[1];
      return Tr(t), n ? (Tr(n), Math.log(t) / Math.log(n)) : Math.log(t);
    }),
    kr("sin", 1, 1, function (e) {
      return Tr(e[0]), Math.sin(e[0]);
    }),
    kr("cos", 1, 1, function (e) {
      return Tr(e[0]), Math.cos(e[0]);
    }),
    kr("tan", 1, 1, function (e) {
      return Tr(e[0]), Math.tan(e[0]);
    }),
    kr("asin", 1, 1, function (e) {
      return Tr(e[0]), Math.asin(e[0]);
    }),
    kr("acos", 1, 1, function (e) {
      return Tr(e[0]), Math.acos(e[0]);
    }),
    kr("atan", 1, 2, function (e) {
      return (
        Tr(e[0]),
        2 == e.length ? (Tr(e[1]), Math.atan2(e[0], e[1])) : Math.atan(e[0])
      );
    }),
    kr("sqrt", 1, 1, function (e) {
      return Tr(e[0]), Math.sqrt(e[0]);
    }),
    kr("exact-integer-sqrt", 1, 1, function (e) {
      Tr(e[0]);
      var t = Math.sqrt(e[0]),
        n = t - (t % 1),
        r = e[0] - n * n;
      return new ui([n, r]);
    }),
    kr("expt", 2, 2, function (e) {
      return Tr(e[0]), Tr(e[1]), Math.pow(e[0], e[1]);
    }),
    kr("make-rectangular", 2, 2, function (e) {
      return Tr(e[0]), Tr(e[1]), new pr(e[0], e[1]);
    }),
    kr("make-polar", 2, 2, function (e) {
      return Tr(e[0]), Tr(e[1]), pr.from_polar(e[0], e[1]);
    });
  var Si = function (e) {
      return pr.assure(e).real;
    },
    Ti = function (e) {
      return pr.assure(e).imag;
    };
  kr("real-part", 1, 1, function (e) {
    return Tr(e[0]), Si(e[0]);
  }),
    kr("imag-part", 1, 1, function (e) {
      return Tr(e[0]), pr.assure(e[0]).imag;
    }),
    kr("magnitude", 1, 1, function (e) {
      return Tr(e[0]), pr.assure(e[0]).magnitude();
    }),
    kr("angle", 1, 1, function (e) {
      return Tr(e[0]), pr.assure(e[0]).angle();
    }),
    kr("number->string", 1, 3, function (e) {
      var t = e[0],
        n = e[1];
      if (e[2])
        throw new pn("number->string: precision is not yet implemented");
      return (n = n || 10), t.toString(n);
    }),
    kr("string->number", 1, 3, function (e) {
      var t = e[0];
      if ("+inf.0" === t) return 1 / 0;
      if ("-inf.0" === t) return -1 / 0;
      if ("+nan.0" === t) return NaN;
      var n = e[1],
        r = Xr(t, 0 === n ? 0 : n || 10);
      if (!1 !== r) return r;
      if (void 0 !== n && 10 !== n) return !1;
      var i = Gr(t);
      if (!1 !== i) return i;
      var o = Ur(t);
      return !1 !== o && o;
    }),
    kr("not", 1, 1, function (e) {
      return !1 === e[0];
    }),
    kr("boolean?", 1, 1, function (e) {
      return !1 === e[0] || !0 === e[0];
    }),
    kr("boolean=?", 2, null, function (e) {
      for (var t = e.length, n = 1; n < t; n++) if (e[n] != e[0]) return !1;
      return !0;
    }),
    kr("pair?", 1, 1, function (e) {
      return e[0] instanceof gn;
    }),
    kr("cons", 2, 2, function (e) {
      return new gn(e[0], e[1]);
    }),
    kr("car", 1, 1, function (e) {
      if (!(e[0] instanceof gn))
        throw new hn("Attempt to apply car on " + e[0]);
      return e[0].car;
    }),
    kr("cdr", 1, 1, function (e) {
      if (!(e[0] instanceof gn))
        throw new hn("Attempt to apply cdr on " + e[0]);
      return e[0].cdr;
    }),
    kr("set-car!", 2, 2, function (e) {
      if (!(e[0] instanceof gn))
        throw new hn("Attempt to apply set-car! on " + e[0]);
      return (e[0].car = e[1]), r;
    }),
    kr("set-cdr!", 2, 2, function (e) {
      if (!(e[0] instanceof gn))
        throw new hn("Attempt to apply set-cdr! on " + e[0]);
      return (e[0].cdr = e[1]), r;
    }),
    (function () {
      var e = function (e, t, n) {
        var r = n;
        return (
          St(t, function (t) {
            if (!(r instanceof gn))
              throw new hn(
                e + ": attempt to get " + (t ? "cdr" : "car") + " of " + r
              );
            r = t ? r.cdr : r.car;
          }),
          r
        );
      };
      kr("caar", 1, 1, function (t) {
        return e("caar", [0, 0], t[0]);
      }),
        kr("cadr", 1, 1, function (t) {
          return e("cadr", [1, 0], t[0]);
        }),
        kr("cdar", 1, 1, function (t) {
          return e("cadr", [0, 1], t[0]);
        }),
        kr("cddr", 1, 1, function (t) {
          return e("cadr", [1, 1], t[0]);
        }),
        kr("caaar", 1, 1, function (t) {
          return e("caaar", [0, 0, 0], t[0]);
        }),
        kr("caadr", 1, 1, function (t) {
          return e("caadr", [1, 0, 0], t[0]);
        }),
        kr("cadar", 1, 1, function (t) {
          return e("cadar", [0, 1, 0], t[0]);
        }),
        kr("caddr", 1, 1, function (t) {
          return e("caddr", [1, 1, 0], t[0]);
        }),
        kr("cdaar", 1, 1, function (t) {
          return e("cdaar", [0, 0, 1], t[0]);
        }),
        kr("cdadr", 1, 1, function (t) {
          return e("cdadr", [1, 0, 1], t[0]);
        }),
        kr("cddar", 1, 1, function (t) {
          return e("cddar", [0, 1, 1], t[0]);
        }),
        kr("cdddr", 1, 1, function (t) {
          return e("cdddr", [1, 1, 1], t[0]);
        }),
        kr("caaaar", 1, 1, function (t) {
          return e("caaaar", [0, 0, 0, 0], t[0]);
        }),
        kr("caaadr", 1, 1, function (t) {
          return e("caaadr", [1, 0, 0, 0], t[0]);
        }),
        kr("caadar", 1, 1, function (t) {
          return e("caadar", [0, 1, 0, 0], t[0]);
        }),
        kr("caaddr", 1, 1, function (t) {
          return e("caaddr", [1, 1, 0, 0], t[0]);
        }),
        kr("cadaar", 1, 1, function (t) {
          return e("cadaar", [0, 0, 1, 0], t[0]);
        }),
        kr("cadadr", 1, 1, function (t) {
          return e("cadadr", [1, 0, 1, 0], t[0]);
        }),
        kr("caddar", 1, 1, function (t) {
          return e("caddar", [0, 1, 1, 0], t[0]);
        }),
        kr("cadddr", 1, 1, function (t) {
          return e("cadddr", [1, 1, 1, 0], t[0]);
        }),
        kr("cdaaar", 1, 1, function (t) {
          return e("cdaaar", [0, 0, 0, 1], t[0]);
        }),
        kr("cdaadr", 1, 1, function (t) {
          return e("cdaadr", [1, 0, 0, 1], t[0]);
        }),
        kr("cdadar", 1, 1, function (t) {
          return e("cdadar", [0, 1, 0, 1], t[0]);
        }),
        kr("cdaddr", 1, 1, function (t) {
          return e("cdaddr", [1, 1, 0, 1], t[0]);
        }),
        kr("cddaar", 1, 1, function (t) {
          return e("cddaar", [0, 0, 1, 1], t[0]);
        }),
        kr("cddadr", 1, 1, function (t) {
          return e("cddadr", [1, 0, 1, 1], t[0]);
        }),
        kr("cdddar", 1, 1, function (t) {
          return e("cdddar", [0, 1, 1, 1], t[0]);
        }),
        kr("cddddr", 1, 1, function (t) {
          return e("cddddr", [1, 1, 1, 1], t[0]);
        });
    })(),
    kr("null?", 1, 1, function (e) {
      return e[0] === n;
    }),
    kr("list?", 1, 1, function (e) {
      return wn(e[0]);
    }),
    kr("list", 0, null, function (e) {
      for (var t = n, r = e.length - 1; r >= 0; r--) t = new gn(e[r], t);
      return t;
    }),
    kr("length", 1, 1, function (e) {
      Lr(e[0]);
      for (var t = 0, r = e[0]; r != n; r = r.cdr) t++;
      return t;
    }),
    kr("append", 1, null, function (e) {
      for (var t = e.length, n = e[--t]; t--; )
        St(e[t].to_array().reverse(), function (e) {
          n = new gn(e, n);
        });
      return n;
    }),
    kr("reverse", 1, 1, function (e) {
      if (e[0] == n) return n;
      Mr(e[0]);
      for (var t = n, r = e[0]; r != n; r = r.cdr) t = new gn(r.car, t);
      return t;
    }),
    kr("list-tail", 2, 2, function (e) {
      if ((Mr(e[0]), Cr(e[1]), e[1] < 0))
        throw new hn("list-tail: index out of range (" + e[1] + ")");
      for (var t = e[0], n = 0; n < e[1]; n++) {
        if (!(t instanceof gn))
          throw new hn("list-tail: the list is shorter than " + e[1]);
        t = t.cdr;
      }
      return t;
    }),
    kr("list-ref", 2, 2, function (e) {
      if ((Mr(e[0]), Cr(e[1]), e[1] < 0))
        throw new hn("list-tail: index out of range (" + e[1] + ")");
      for (var t = e[0], n = 0; n < e[1]; n++) {
        if (!(t instanceof gn))
          throw new hn("list-ref: the list is shorter than " + e[1]);
        t = t.cdr;
      }
      return t.car;
    }),
    kr("map", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      St(n, Lr);
      var r = [];
      return tr.multi_foreach(n, {
        call: function (e) {
          return new tr(
            t,
            Tt(e, function (e) {
              return e.car;
            })
          );
        },
        result: function (e) {
          r.push(e);
        },
        finish: function () {
          return xn(r);
        },
      });
    }),
    kr("for-each", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      return (
        St(n, Lr),
        tr.multi_foreach(n, {
          call: function (e) {
            return new tr(
              t,
              Tt(e, function (e) {
                return e.car;
              })
            );
          },
          finish: function () {
            return r;
          },
        })
      );
    }),
    kr("symbol?", 1, 1, function (e) {
      return e[0] instanceof fn;
    }),
    kr("symbol->string", 1, 1, function (e) {
      return Nr(e[0]), e[0].name;
    }),
    kr("symbol=?", 2, null, function (e) {
      Nr(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((Nr(e[t]), e[t] != e[0])) return !1;
      return !0;
    }),
    kr("string->symbol", 1, 1, function (e) {
      return Ar(e[0]), ln(e[0]);
    }),
    kr("char?", 1, 1, function (e) {
      return e[0] instanceof Rn;
    }),
    kr("char->integer", 1, 1, function (e) {
      return Dr(e[0]), e[0].value.charCodeAt(0);
    }),
    kr("integer->char", 1, 1, function (e) {
      return Cr(e[0]), Rn.get(String.fromCharCode(e[0]));
    });
  var Ci = function (e) {
    return function (t) {
      Dr(t[0]);
      for (var n = 1; n < t.length; n++)
        if ((Dr(t[n]), !e(t[n - 1].value, t[n].value))) return !1;
      return !0;
    };
  };
  kr(
    "char=?",
    2,
    null,
    Ci(function (e, t) {
      return e == t;
    })
  ),
    kr(
      "char<?",
      2,
      null,
      Ci(function (e, t) {
        return e < t;
      })
    ),
    kr(
      "char>?",
      2,
      null,
      Ci(function (e, t) {
        return e > t;
      })
    ),
    kr(
      "char<=?",
      2,
      null,
      Ci(function (e, t) {
        return e <= t;
      })
    ),
    kr(
      "char>=?",
      2,
      null,
      Ci(function (e, t) {
        return e >= t;
      })
    ),
    kr("string?", 1, 1, function (e) {
      return "string" == typeof e[0];
    }),
    kr("make-string", 1, 2, function (e) {
      Cr(e[0]);
      var t = " ";
      e[1] && (Dr(e[1]), (t = e[1].value));
      var n = "";
      return (
        We(e[0], function () {
          n += t;
        }),
        n
      );
    }),
    kr("string", 0, null, function (e) {
      if (0 == e.length) return "";
      for (var t = 0; t < e.length; t++) Dr(e[t]);
      return Tt(e, function (e) {
        return e.value;
      }).join("");
    }),
    kr("string-length", 1, 1, function (e) {
      return Ar(e[0]), e[0].length;
    }),
    kr("string-ref", 2, 2, function (e) {
      return (
        Ar(e[0]), qr(e[1], 0, e[0].length - 1), Rn.get(e[0].charAt([e[1]]))
      );
    }),
    kr("string=?", 2, null, function (e) {
      Ar(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((Ar(e[t]), e[0] != e[t])) return !1;
      return !0;
    }),
    kr("string<?", 2, null, function (e) {
      Ar(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((Ar(e[t]), !(e[t - 1] < e[t]))) return !1;
      return !0;
    }),
    kr("string>?", 2, null, function (e) {
      Ar(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((Ar(e[t]), !(e[t - 1] > e[t]))) return !1;
      return !0;
    }),
    kr("string<=?", 2, null, function (e) {
      Ar(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((Ar(e[t]), !(e[t - 1] <= e[t]))) return !1;
      return !0;
    }),
    kr("string>=?", 2, null, function (e) {
      Ar(e[0]);
      for (var t = 1; t < e.length; t++)
        if ((Ar(e[t]), !(e[t - 1] >= e[t]))) return !1;
      return !0;
    }),
    kr("substring", 3, 3, function (e) {
      if ((Ar(e[0]), Cr(e[1]), Cr(e[2]), e[1] < 0))
        throw new hn("substring: start too small: " + e[1]);
      if (e[2] < 0) throw new hn("substring: end too small: " + e[2]);
      if (e[0].length + 1 <= e[1])
        throw new hn("substring: start too big: " + e[1]);
      if (e[0].length + 1 <= e[2])
        throw new hn("substring: end too big: " + e[2]);
      if (!(e[1] <= e[2]))
        throw new hn("substring: not start <= end: " + e[1] + ", " + e[2]);
      return e[0].substring(e[1], e[2]);
    }),
    kr("string-append", 0, null, function (e) {
      for (var t = 0; t < e.length; t++) Ar(e[t]);
      return e.join("");
    }),
    kr("string->list", 1, 1, function (e) {
      return (
        Ar(e[0]),
        xn(
          Tt(e[0].split(""), function (e) {
            return Rn.get(e[0]);
          })
        )
      );
    }),
    kr("list->string", 1, 1, function (e) {
      return (
        Lr(e[0]),
        Tt(e[0].to_array(), function (e) {
          return e.value;
        }).join("")
      );
    }),
    kr("string-for-each", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      return (
        St(n, Ar),
        tr.multi_foreach(n, {
          call: function (e) {
            return new tr(t, e);
          },
          finish: function () {
            return r;
          },
        })
      );
    }),
    kr("string-copy", 1, 1, function (e) {
      return Ar(e[0]), e[0];
    }),
    kr("vector?", 1, 1, function (e) {
      return Gn(e[0]);
    }),
    kr("make-vector", 1, 2, function (e) {
      Cr(e[0]);
      var t = new Array(e[0]);
      if (2 == e.length) for (var n = 0; n < e[0]; n++) t[n] = e[1];
      return t;
    }),
    kr("vector", 0, null, function (e) {
      return e;
    }),
    kr("vector-length", 1, 1, function (e) {
      return Rr(e[0]), e[0].length;
    }),
    kr("vector-ref", 2, 2, function (e) {
      return Rr(e[0]), Cr(e[1]), qr(e[1], 0, e[0].length - 1), e[0][e[1]];
    }),
    kr("vector-set!", 3, 3, function (e) {
      return Rr(e[0]), Cr(e[1]), (e[0][e[1]] = e[2]), r;
    }),
    kr("vector->list", 1, 1, function (e) {
      return Rr(e[0]), xn(e[0]);
    }),
    kr("list->vector", 1, 1, function (e) {
      return Lr(e[0]), e[0].to_array();
    }),
    kr("vector-fill!", 2, 2, function (e) {
      Rr(e[0]);
      for (var t = e[0], n = e[1], r = 0; r < t.length; r++) t[r] = n;
      return t;
    }),
    kr("vector-map", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      St(n, Rr);
      var r = [];
      return tr.multi_foreach(n, {
        call: function (e) {
          return new tr(t, e);
        },
        result: function (e) {
          r.push(e);
        },
        finish: function () {
          return r;
        },
      });
    }),
    kr("vector-for-each", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      return (
        St(n, Rr),
        tr.multi_foreach(n, {
          call: function (e) {
            return new tr(t, e);
          },
          finish: function () {
            return r;
          },
        })
      );
    }),
    kr("apply", 2, null, function (e) {
      var t = e.shift(),
        n = e.pop(),
        r = e;
      return (r = r.concat(n.to_array())), new tr(t, r);
    }),
    Sr("call-with-current-continuation", function (e) {
      return new gn(ln("call/cc"), e.cdr);
    }),
    kr("values", 0, null, function (e) {
      return 1 == e.length ? e[0] : new ui(e);
    }),
    kr("call-with-values", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        Fr(t),
        Fr(n),
        new tr(t, [], function (e) {
          var t = e[0];
          return new tr(n, t instanceof ui ? t.content : [t]);
        })
      );
    }),
    kr("dynamic-wind", 3, 3, function (e, t) {
      var n = e[0],
        r = e[1],
        i = e[2];
      return new tr(n, [], function () {
        return (
          t.push_dynamic_winder(n, i),
          new tr(r, [], function (e) {
            var n = e[0];
            return (
              t.pop_dynamic_winder(),
              new tr(i, [], function () {
                return n;
              })
            );
          })
        );
      });
    });
  var Ei = function (e, t) {
    if (e instanceof fn || e === n) return _n(ln("quote"), e);
    if (e instanceof gn) {
      var r = e.car;
      return r instanceof gn && r.car === ln("unquote-splicing")
        ? 1 == t
          ? _n(ln("append"), e.car.cdr.car, Ei(e.cdr, t))
          : _n(
              ln("cons"),
              _n(
                ln("list"),
                _n(ln("quote"), ln("unquote-splicing")),
                Ei(e.car.cdr.car, t - 1)
              ),
              Ei(e.cdr, t)
            )
        : r === ln("unquote")
        ? 1 == t
          ? e.cdr.car
          : _n(ln("list"), _n(ln("quote"), ln("unquote")), Ei(e.cdr.car, t - 1))
        : r === ln("quasiquote")
        ? _n(
            ln("list"),
            _n(ln("quote"), ln("quasiquote")),
            Ei(e.cdr.car, t + 1)
          )
        : _n(ln("cons"), Ei(e.car, t), Ei(e.cdr, t));
    }
    if (e instanceof Array) {
      for (var i = [[]], o = 0; o < e.length; o++)
        if (e[o] instanceof gn && e[o].car === ln("unquote-splicing"))
          if (1 == t) {
            ((u = _n(ln("list->vector"), e[o].cdr.car)).splicing = !0),
              i.push(u),
              i.push([]);
          } else {
            var u = _n(
              ln("cons"),
              _n(
                ln("list"),
                _n(ln("quote"), ln("unquote-splicing")),
                Ei(e[o].car.cdr.car, t - 1)
              ),
              Ei(e[o].cdr, t)
            );
            Qt(i).push(u);
          }
        else Qt(i).push(Ei(e[o], t));
      var a = i.map(function (e) {
        return e.splicing ? e : jn(ln("vector"), xn(e));
      });
      return 1 == a.length
        ? jn(ln("vector"), xn(i[0]))
        : jn(ln("vector-append"), xn(a));
    }
    return e;
  };
  Sr("quasiquote", function (e) {
    return Ei(e.cdr.car, 1);
  }),
    Sr("unquote", function (e) {
      throw new hn("unquote(,) must be inside quasiquote(`)");
    }),
    Sr("unquote-splicing", function (e) {
      throw new hn("unquote-splicing(,@) must be inside quasiquote(`)");
    }),
    kr("string-upcase", 1, 1, function (e) {
      return Ar(e[0]), e[0].toUpperCase();
    }),
    kr("string-downcase", 1, 1, function (e) {
      return Ar(e[0]), e[0].toLowerCase();
    });
  const qi = function (e) {
    return function (t) {
      Ar(t[0]);
      for (var n = t[0].toUpperCase(), r = 1; r < t.length; r++)
        if ((Ar(t[r]), !e(n, t[r].toUpperCase()))) return !1;
      return !0;
    };
  };
  kr(
    "string-ci=?",
    2,
    null,
    qi(function (e, t) {
      return e == t;
    })
  ),
    kr(
      "string-ci<?",
      2,
      null,
      qi(function (e, t) {
        return e < t;
      })
    ),
    kr(
      "string-ci>?",
      2,
      null,
      qi(function (e, t) {
        return e > t;
      })
    ),
    kr(
      "string-ci<=?",
      2,
      null,
      qi(function (e, t) {
        return e <= t;
      })
    ),
    kr(
      "string-ci>=?",
      2,
      null,
      qi(function (e, t) {
        return e >= t;
      })
    ),
    kr("find", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        Lr(n),
        tr.foreach(n, {
          call: function (e) {
            return new tr(t, [e.car]);
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
    kr("for-all", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      St(n, Lr);
      var r = !0;
      return tr.multi_foreach(n, {
        call: function (e) {
          return new tr(
            t,
            Tt(e, function (e) {
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
    kr("exists", 2, null, function (e) {
      var t = e.shift(),
        n = e;
      return (
        St(n, Lr),
        tr.multi_foreach(n, {
          call: function (e) {
            return new tr(
              t,
              Tt(e, function (e) {
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
    kr("filter", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      Lr(n);
      var r = [];
      return tr.foreach(n, {
        call: function (e) {
          return new tr(t, [e.car]);
        },
        result: function (e, t) {
          e && r.push(t.car);
        },
        finish: function () {
          return xn(r);
        },
      });
    }),
    kr("partition", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      Lr(n);
      var r = [],
        i = [];
      return tr.foreach(n, {
        call: function (e) {
          return new tr(t, [e.car]);
        },
        result: function (e, t) {
          e ? r.push(t.car) : i.push(t.car);
        },
        finish: function () {
          return new ui([xn(r), xn(i)]);
        },
      });
    }),
    kr("fold-left", 3, null, function (e) {
      var t = e.shift(),
        n = e.shift(),
        r = e;
      return (
        St(r, Lr),
        tr.multi_foreach(r, {
          call: function (e) {
            var r = Tt(e, function (e) {
              return e.car;
            });
            return r.unshift(n), new tr(t, r);
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
    kr("fold-right", 3, null, function (e) {
      var t = e.shift(),
        n = e.shift(),
        r = Tt(e, function (e) {
          return Lr(e), xn(e.to_array().reverse());
        });
      return tr.multi_foreach(r, {
        call: function (e) {
          var r = Tt(e, function (e) {
            return e.car;
          });
          return r.push(n), new tr(t, r);
        },
        result: function (e, t) {
          n = e;
        },
        finish: function () {
          return n;
        },
      });
    }),
    kr("remp", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      Lr(n);
      var r = [];
      return tr.foreach(n, {
        call: function (e) {
          return new tr(t, [e.car]);
        },
        result: function (e, t) {
          e || r.push(t.car);
        },
        finish: function () {
          return xn(r);
        },
      });
    });
  var Ai = function (n) {
    return function (r) {
      var i = r[0],
        o = r[1];
      Lr(o);
      var u = [];
      return tr.foreach(o, {
        call: function (r) {
          return new tr(e[n] || t[n], [i, r.car]);
        },
        result: function (e, t) {
          e || u.push(t.car);
        },
        finish: function () {
          return xn(u);
        },
      });
    };
  };
  kr("remove", 2, 2, Ai("equal?")),
    kr("remv", 2, 2, Ai("eqv?")),
    kr("remq", 2, 2, Ai("eq?")),
    kr("memp", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        Lr(n),
        tr.foreach(n, {
          call: function (e) {
            return new tr(t, [e.car]);
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
  var Di = function (n) {
    return function (r) {
      var i = r[0],
        o = r[1];
      return (
        Lr(o),
        tr.foreach(o, {
          call: function (r) {
            return new tr(e[n] || t[n], [i, r.car]);
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
  kr("member", 2, 2, Di("equal?")),
    kr("memv", 2, 2, Di("eqv?")),
    kr("memq", 2, 2, Di("eq?")),
    kr("assp", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        Lr(n),
        tr.foreach(n, {
          call: function (e) {
            if (e.car.car) return new tr(t, [e.car.car]);
            throw new hn("ass*: pair required but got " + Mn(e.car));
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
  var Ni = function (n, r) {
    return function (i) {
      var o = i[0],
        u = i[1];
      return (
        Lr(u),
        tr.foreach(u, {
          call: function (i) {
            if (!yn(i.car))
              throw new hn(n + ": pair required but got " + Mn(i.car));
            var u = e[r] || t[r];
            return new tr(u, [o, i.car.car]);
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
  kr("assoc", 2, 2, Ni("assoc", "equal?")),
    kr("assv", 2, 2, Ni("assv", "eqv?")),
    kr("assq", 2, 2, Ni("assq", "eq?")),
    kr("cons*", 1, null, function (e) {
      if (1 == e.length) return e[0];
      var t = null;
      return (
        St(e.reverse(), function (e) {
          t = t ? new gn(e, t) : e;
        }),
        t
      );
    }),
    (function () {
      var e = function (e, n, r) {
          return e.length <= 1 ? r(e) : t(e, n, r, [[0, e.length, !1]], !1);
        },
        t = function (e, r, i, o, u) {
          for (;;) {
            var a = o[o.length - 1][0],
              c = o[o.length - 1][1],
              s = o[o.length - 1][2],
              f = c - a;
            if (f >= 2 && !u) o.push([a, a + (f >> 1), !0]);
            else {
              if (!s) {
                o.pop();
                var l = o[o.length - 1][0],
                  d = e.slice(l, a),
                  h = e.slice(a, c);
                return n(d, h, r, [], 0, 0, function (n) {
                  for (var u = 0; u < n.length; u++) e[l + u] = n[u];
                  return 1 == o.length ? i(e) : t(e, r, i, o, !0);
                });
              }
              o.pop();
              var p = o[o.length - 1][1];
              o.push([c, p, !1]), (u = !1);
            }
          }
        },
        n = function (e, t, r, i, o, u, a) {
          var c = e.length,
            s = t.length;
          if (o < c && u < s)
            return new tr(r, [t[u], e[o]], function (c) {
              return (
                c[0] ? (i.push(t[u]), (u += 1)) : (i.push(e[o]), (o += 1)),
                n(e, t, r, i, o, u, a)
              );
            });
          for (; o < c; ) i.push(e[o]), (o += 1);
          for (; u < s; ) i.push(t[u]), (u += 1);
          return a(i);
        },
        i = function (e, t) {
          return er(e, t) ? -1 : er(t, e) ? 1 : 0;
        };
      kr("list-sort", 1, 2, function (t) {
        return t[1]
          ? (Fr(t[0]),
            Lr(t[1]),
            e(t[1].to_array(), t[0], function (e) {
              return xn(e);
            }))
          : (Lr(t[0]), xn(t[0].to_array().sort(i)));
      }),
        kr("vector-sort", 1, 2, function (t) {
          return t[1]
            ? (Fr(t[0]),
              Rr(t[1]),
              e(De(t[1]), t[0], function (e) {
                return e;
              }))
            : (Rr(t[0]), De(t[0]).sort(i));
        }),
        kr("vector-sort!", 1, 2, function (t) {
          return t[1]
            ? (Fr(t[0]),
              Rr(t[1]),
              e(t[1], t[0], function (e) {
                return r;
              }))
            : (Rr(t[0]), t[0].sort(i), r);
        });
    })(),
    Sr("when", function (e) {
      var t = e.cdr.car,
        i = e.cdr.cdr;
      return new gn(
        ln("if"),
        new gn(t, new gn(new gn(ln("begin"), i), new gn(r, n)))
      );
    }),
    Sr("unless", function (e) {
      var t = e.cdr.car,
        i = e.cdr.cdr;
      return new gn(
        ln("if"),
        new gn(
          new gn(ln("not"), new gn(t, n)),
          new gn(new gn(ln("begin"), i), new gn(r, n))
        )
      );
    }),
    Sr("do", function (e) {
      if (!yn(e.cdr)) throw new hn("do: no variables of do");
      var t = e.cdr.car;
      if (!yn(t)) throw new hn("do: variables must be given as a list");
      if (!yn(e.cdr.cdr)) throw new hn("do: no resulting form of do");
      var n = e.cdr.cdr.car,
        r = e.cdr.cdr.cdr,
        i = dn(),
        o = xn(
          t.map(function (e) {
            var t = e.to_array();
            return _n(t[0], t[1]);
          })
        ),
        u = n.car,
        a = new gn(ln("begin"), n.cdr),
        c = new gn(
          i,
          xn(
            t.map(function (e) {
              var t = e.to_array();
              return t[2] || t[0];
            })
          )
        ),
        s = new gn(ln("begin"), r).concat(_n(c));
      return _n(ln("let"), i, o, _n(ln("if"), u, a, s));
    }),
    Sr("case-lambda", function (e) {
      if (!yn(e.cdr)) throw new hn("case-lambda: at least 1 clause required");
      var t = e.cdr.to_array(),
        r = dn(),
        i = _n(ln("raise"), "case-lambda: no matching clause found");
      return (
        t.reverse().forEach(function (e) {
          if (!yn(e))
            throw new hn("case-lambda: clause must be a pair: " + Mn(e));
          var t = e.car,
            o = e.cdr;
          if (t === n)
            i = _n(ln("if"), _n(ln("null?"), r), new gn(ln("begin"), o), i);
          else if (yn(t)) {
            var u = t.length(),
              a = t.last_cdr(),
              c = ln(a === n ? "=" : ">="),
              s = new gn(ln("lambda"), new gn(t, o));
            i = _n(
              ln("if"),
              _n(c, _n(ln("length"), r), u),
              _n(ln("apply"), s, r),
              i
            );
          } else {
            if (!Xn(t)) throw new hn("case-lambda: invalid formals: " + Mn(t));
            i = new gn(ln("let1"), new gn(t, new gn(r, o)));
          }
        }),
        _n(ln("lambda"), r, i)
      );
    }),
    Sr("define-record-type", function (e) {
      var t = e.cdr.car,
        n = e.cdr.cdr;
      if (Xn(t))
        var r = t,
          i = ln("make-" + t.name),
          o = ln(t.name + "?");
      else {
        Lr(t);
        (r = t.car), (i = t.cdr.car), (o = t.cdr.cdr.car);
        Nr(r), Nr(i), Nr(o);
      }
      var u,
        a = !1,
        c = !1,
        s = !1,
        f = !1,
        l = !1,
        d = !1,
        h = [];
      St(n.to_array(), function (e) {
        switch (e.car) {
          case ln("fields"):
            h = Tt(e.cdr.to_array(), function (e, t) {
              if (Xn(e))
                return {
                  name: e,
                  idx: t,
                  mutable: !1,
                  accessor_name: null,
                  mutator_name: null,
                };
              switch ((Lr(e), Nr(e.car), e.car)) {
                case ln("immutable"):
                  var n = e.cdr.car;
                  return (
                    Nr(n),
                    Bn(e.cdr.cdr)
                      ? { name: n, idx: t, mutable: !1 }
                      : {
                          name: n,
                          idx: t,
                          mutable: !1,
                          accessor_name: e.cdr.cdr.car,
                        }
                  );
                case ln("mutable"):
                  n = e.cdr.car;
                  return (
                    Nr(n),
                    Bn(e.cdr.cdr)
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
                  throw new hn(
                    "define-record-type: field definition must start with `immutable' or `mutable' but got " +
                      An(e.car)
                  );
              }
            });
            break;
          case ln("parent"):
            (u = e.cdr.car), Nr(u);
            break;
          case ln("protocol"):
            d = e.cdr.car;
            break;
          case ln("sealed"):
            a = !!e.cdr.car;
            break;
          case ln("opaque"):
            c = !!e.cdr.car;
            break;
          case ln("nongenerative"):
            s = e.cdr.car;
            break;
          case ln("parent-rtd"):
            (f = e.cdr.car), (l = e.cdr.cdr.car);
            break;
          default:
            throw new hn(
              "define-record-type: unknown clause `" + Mn(e.car) + "'"
            );
        }
      }),
        u &&
          ((f = [ln("record-type-descriptor"), u]),
          (l = [ln("record-constructor-descriptor"), u]));
      var p = [ln("record-type-descriptor"), r],
        m = [ln("record-constructor-descriptor"), r],
        v = Tt(h, function (e) {
          return _n(ln(e.mutable ? "mutable" : "immutable"), e.name);
        });
      v.is_vector = !0;
      var g = [
          ln("make-record-type-descriptor"),
          [ln("quote"), r],
          f,
          s,
          a,
          c,
          v,
        ],
        y = [ln("make-record-constructor-descriptor"), ln("__rtd"), l, d],
        w = [
          ln("let*"),
          [
            [ln("__rtd"), g],
            [ln("__cd"), y],
          ],
          [
            ln("_define-record-type"),
            [ln("quote"), r],
            ln("__rtd"),
            ln("__cd"),
          ],
        ],
        b = Tt(h, function (e) {
          var t = e.accessor_name || ln(r.name + "-" + e.name.name);
          return [ln("define"), t, [ln("record-accessor"), p, e.idx]];
        }),
        _ = At(h, function (e) {
          return e.mutable;
        });
      return (
        (_ = Tt(_, function (e) {
          var t = e.mutator_name || ln(r.name + "-" + e.name.name + "-set!");
          return [ln("define"), t, [ln("record-mutator"), p, e.idx]];
        })),
        kn(
          [
            ln("begin"),
            w,
            [ln("define"), i, [ln("record-constructor"), m]],
            [ln("define"), o, [ln("record-predicate"), p]],
          ]
            .concat(b)
            .concat(_)
        )
      );
    }),
    kr("_define-record-type", 3, 3, function (e) {
      return (
        Nr(e[0]), ii(e[1]), oi(e[2]), Zr.define_type(e[0].name, e[1], e[2]), r
      );
    }),
    Sr("record-type-descriptor", function (e) {
      return kn([ln("_record-type-descriptor"), [ln("quote"), e.cdr.car]]);
    }),
    kr("_record-type-descriptor", 1, 1, function (e) {
      Nr(e[0]);
      var t = Zr.get_type(e[0].name);
      if (t) return t.rtd;
      throw new hn("record-type-descriptor: unknown record type " + e[0].name);
    }),
    Sr("record-constructor-descriptor", function (e) {
      return kn([
        ln("_record-constructor-descriptor"),
        [ln("quote"), e.cdr.car],
      ]);
    }),
    kr("_record-constructor-descriptor", 1, 1, function (e) {
      Nr(e[0]);
      var t = Zr.get_type(e[0].name);
      if (t) return t.cd;
      throw new hn(
        "record-constructor-descriptor: unknown record type " + e[0].name
      );
    }),
    kr("make-record-type-descriptor", 6, 6, function (e) {
      var t = e[0],
        n = e[1],
        r = e[2],
        i = e[3],
        o = e[4],
        u = e[5];
      if ((Nr(t), n && ii(n), r)) {
        Nr(r);
        var a = Zr.RTD.NongenerativeRecords[r.name];
        if (a) return a;
      }
      (i = !!i), (o = !!o), Rr(u);
      for (var c = 0; c < u.length; c++) {
        var s = u[c];
        Nr(s.car, "mutability"),
          Nr(s.cdr.car, "field name"),
          (u[c] = [s.cdr.car.name, s.car == ln("mutable")]);
      }
      var f = new Zr.RTD(t, n, r, i, o, u);
      return r && (Zr.RTD.NongenerativeRecords[r.name] = f), f;
    }),
    kr("record-type-descriptor?", 1, 1, function (e) {
      return e[0] instanceof Zr.RTD;
    }),
    kr("make-record-constructor-descriptor", 3, 3, function (e) {
      var t = e[0],
        n = e[1],
        r = e[2];
      return ii(t), n && oi(n), r && Fr(r), new Zr.CD(t, n, r);
    }),
    kr("record-constructor", 1, 1, function (e) {
      var t = e[0];
      return oi(t), t.record_constructor();
    }),
    kr("record-predicate", 1, 1, function (e) {
      var t = e[0];
      return (
        ii(t),
        function (e) {
          var n = e[0];
          return n instanceof Zr && n.rtd === t;
        }
      );
    }),
    kr("record-accessor", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      ii(t), Cr(n);
      for (var r = t.parent_rtd; r; r = r.parent_rtd) n += r.fields.length;
      return function (e) {
        var r = e[0],
          i =
            t.name.name +
            "-" +
            t.field_name(n) +
            ": " +
            Mn(r) +
            " is not a " +
            t.name.name;
        $r(ei(r), i);
        for (var o = !1, u = r.rtd; u; u = u.parent_rtd) u == t && (o = !0);
        return $r(o, i), r.get(n);
      };
    }),
    kr("record-mutator", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      ii(t), Cr(n);
      for (var r = t.parent_rtd; r; r = r.parent_rtd) n += r.fields.length;
      return function (e) {
        var r = e[0],
          i = e[1],
          o = t.field_name(n);
        ri(r),
          $r(r.rtd === t, o + ": " + Mn(r) + " is not a " + t.name.name),
          $r(
            !r.rtd.sealed,
            o + ": " + t.name.name + " is sealed (can't mutate)"
          ),
          r.set(n, i);
      };
    }),
    kr("record?", 1, 1, function (e) {
      var t = e[0];
      return !!ei(t) && !t.rtd.opaque;
    }),
    kr("record-rtd", 1, 1, function (e) {
      return ri(e[0]), e[0].rtd;
    }),
    kr("record-type-name", 1, 1, function (e) {
      return ii(e[0]), e[0].name;
    }),
    kr("record-type-parent", 1, 1, function (e) {
      return ii(e[0]), e[0].parent_rtd;
    }),
    kr("record-type-uid", 1, 1, function (e) {
      return ii(e[0]), e[0].uid;
    }),
    kr("record-type-generative?", 1, 1, function (e) {
      return ii(e[0]), e[0].generative;
    }),
    kr("record-type-sealed?", 1, 1, function (e) {
      return ii(e[0]), e[0].sealed;
    }),
    kr("record-type-opaque?", 1, 1, function (e) {
      return ii(e[0]), e[0].opaque;
    }),
    kr("record-type-field-names", 1, 1, function (e) {
      return (
        ii(e[0]),
        Tt(e[0].fields, function (e) {
          return e.name;
        })
      );
    }),
    kr("record-field-mutable?", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      ii(e[0]), Cr(n);
      for (var r = t.parent_rtd; r; r = r.parent_rtd) n += r.fields.length;
      return e[0].fields[n].mutable;
    }),
    kr("raise", 1, 1, function (e) {
      throw new mn(Mn(e[0]));
    }),
    kr("port?", 1, 1, function (e) {
      return e[0] instanceof Pn;
    }),
    kr("textual-port?", 1, 1, function (e) {
      return Or(e[0]), !e[0].is_binary;
    }),
    kr("binary-port?", 1, 1, function (e) {
      return Or(e[0]), e[0].is_binary;
    }),
    kr("close-port", 1, 1, function (e) {
      return Or(e[0]), e[0].close(), r;
    }),
    kr("call-with-port", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        Or(t),
        zr(n),
        new tr(n, [t], function (e) {
          return t.close(), e[0];
        })
      );
    }),
    kr("call-with-string-output-port", 1, 1, function (e) {
      var t = e[0];
      Fr(t);
      var n = new Pn.StringOutput();
      return new tr(t, [n], function (e) {
        return n.close(), n.output_string();
      });
    }),
    kr("put-char", 2, 2, function (e) {
      return Or(e[0]), Dr(e[1]), e[0].put_string(e[1].value), r;
    }),
    kr("put-string", 2, 2, function (e) {
      return Or(e[0]), Ar(e[1]), e[0].put_string(e[1]), r;
    }),
    kr("put-datum", 2, 2, function (e) {
      return Or(e[0]), e[0].put_string(Mn(e[1])), r;
    }),
    kr("eof-object", 0, 0, function (e) {
      return Hn;
    }),
    kr("eof-object?", 1, 1, function (e) {
      return e[0] === Hn;
    }),
    kr("input-port?", 1, 1, function (e) {
      return Or(e[0]), e[0].is_input;
    }),
    kr("output-port?", 1, 1, function (e) {
      return Or(e[0]), e[0].is_output;
    }),
    kr("current-input-port", 0, 0, function (e) {
      return Pn.current_input;
    }),
    kr("current-output-port", 0, 0, function (e) {
      return Pn.current_output;
    }),
    kr("current-error-port", 0, 0, function (e) {
      return Pn.current_error;
    }),
    kr("close-input-port", 1, 1, function (e) {
      if ((Or(e[0]), !e[0].is_input))
        throw new hn("close-input-port: port is not input port");
      return e[0].close(), r;
    }),
    kr("close-output-port", 1, 1, function (e) {
      if ((Or(e[0]), !e[0].is_output))
        throw new hn("close-output-port: port is not output port");
      return e[0].close(), r;
    }),
    kr("read", 0, 1, function (e) {
      var t = e[0] || Pn.current_input;
      return (
        Or(t),
        t.get_string(function (e) {
          return hr.read(e);
        })
      );
    }),
    kr("write-char", 1, 2, function (e) {
      var t = e[1] || Pn.current_output;
      return Dr(e[0]), t.put_string(e[0].value), r;
    }),
    kr("newline", 0, 1, function (e) {
      return (e[0] || Pn.current_output).put_string("\n"), r;
    }),
    kr("display", 1, 2, function (e) {
      return (e[1] || Pn.current_output).put_string(qn(e[0])), r;
    }),
    kr("write", 1, 2, function (e) {
      var t = e[1] || Pn.current_output;
      return Or(t), t.put_string(Mn(e[0])), r;
    }),
    kr("write-shared", 1, 2, function (e) {
      var t = e[1] || Pn.current_output;
      return Or(t), t.put_string(Nn(e[0])), r;
    }),
    kr("write-simple", 1, 2, function (e) {
      var t = e[1] || Pn.current_output;
      return Or(t), t.put_string(En(e[0])), r;
    }),
    kr("bitwise-not", 1, 1, function (e) {
      return ~e[0];
    }),
    kr("bitwise-and", 1, null, function (e) {
      return Et(e, function (e, t) {
        return e & t;
      });
    }),
    kr("bitwise-ior", 1, null, function (e) {
      return Et(e, function (e, t) {
        return e | t;
      });
    }),
    kr("bitwise-xor", 1, null, function (e) {
      return Et(e, function (e, t) {
        return e ^ t;
      });
    }),
    kr("bitwise-if", 3, 3, function (e) {
      return (e[0] & e[1]) | (~e[0] & e[2]);
    }),
    kr("bitwise-bit-count", 1, 1, function (e) {
      for (var t = Math.abs(e[0]), n = 0; 0 != t; t >>= 1) 1 & t && n++;
      return n;
    }),
    kr("bitwise-length", 1, 1, function (e) {
      for (var t = Math.abs(e[0]), n = 0; 0 != t; t >>= 1) n++;
      return n;
    }),
    kr("bitwise-first-bit-set", 1, 1, function (e) {
      var t = Math.abs(e[0]),
        n = 0;
      if (0 == t) return -1;
      for (; 0 != t; t >>= 1) {
        if (1 & t) return n;
        n++;
      }
    }),
    kr("bitwise-bit-set?", 2, 2, function (e) {
      return !!(e[0] & (1 << e[1]));
    }),
    kr("bitwise-copy-bit", 3, 3, function (e) {
      var t = 1 << e[1];
      return (t & (e[2] << e[1])) | (~t & e[0]);
    }),
    kr("bitwise-bit-field", 3, 3, function (e) {
      return (~(-1 << e[2]) & e[0]) >> e[1];
    }),
    kr("bitwise-copy-bit-field", 4, 4, function (e) {
      var t = e[0],
        n = e[1],
        r = ~(-1 << e[2]) & (-1 << n);
      return (r & (e[3] << n)) | (~r & t);
    }),
    kr("bitwise-arithmetic-shift", 2, 2, function (e) {
      return e[1] >= 0 ? e[0] << e[1] : e[0] >> -e[1];
    }),
    kr("bitwise-arithmetic-shift-left", 2, 2, function (e) {
      return e[0] << e[1];
    }),
    kr("bitwise-arithmetic-shift-right", 2, 2, function (e) {
      return e[0] >> e[1];
    }),
    kr("bitwise-rotate-bit-field", 4, 4, function (e) {
      var t = e[0],
        n = e[1],
        r = e[2],
        i = e[3],
        o = r - n;
      if (o <= 0) return t;
      var u = (~(-1 << r) & t) >> n,
        a = ~(-1 << r) & (-1 << n);
      return (a & (((u << (i %= o)) | (u >> (o - i))) << n)) | (~a & t);
    }),
    kr("bitwise-reverse-bit-field", 3, 3, function (e) {
      for (
        var t = e[0],
          n = e[0],
          r = e[1],
          i = e[2],
          o = (~(-1 << i) & n) >> r,
          u = 0;
        u < i - r;
        u++, o >>= 1
      ) {
        var a = i - 1 - u,
          c = 1 << a;
        t = (c & ((1 & o) << a)) | (~c & t);
      }
      return t;
    }),
    kr("make-eq-hashtable", 0, 1, function (e) {
      return new fr(fr.eq_hash, fr.eq_equiv);
    }),
    kr("make-eqv-hashtable", 0, 1, function (e) {
      return new fr(fr.eqv_hash, fr.eqv_equiv);
    }),
    kr("make-hashtable", 2, 3, function (e) {
      return Fr(e[0]), Fr(e[1]), new fr(e[0], e[1]);
    }),
    kr("hashtable?", 1, 1, function (e) {
      return e[0] instanceof fr;
    }),
    kr("hashtable-size", 1, 1, function (e) {
      return Ir(e[0]), e[0].keys().length;
    });
  const Oi = function (e, t, n) {
    return new tr(e.hash_proc, [t], function (r) {
      var i = r[0],
        o = e.candidate_pairs(i);
      return o
        ? tr.foreach(o, {
            call: function (n) {
              return new tr(e.equiv_proc, [t, n[0]]);
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
  kr("hashtable-ref", 3, 3, function (e) {
    var t = e[0],
      n = e[1],
      r = e[2];
    return (
      Ir(t),
      Oi(t, n, {
        on_found: function (e) {
          return e[1];
        },
        on_not_found: function (e) {
          return r;
        },
      })
    );
  }),
    kr("hashtable-set!", 3, 3, function (e) {
      var t = e[0],
        n = e[1],
        i = e[2];
      return (
        Ir(t),
        $r(t.mutable, "hashtable is not mutable"),
        Oi(t, n, {
          on_found: function (e) {
            return (e[1] = i), r;
          },
          on_not_found: function (e) {
            return t.add_pair(e, n, i), r;
          },
        })
      );
    }),
    kr("hashtable-delete!", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        Ir(t),
        $r(t.mutable, "hashtable is not mutable"),
        Oi(t, n, {
          on_found: function (e, n) {
            return t.remove_pair(n, e), r;
          },
          on_not_found: function (e) {
            return r;
          },
        })
      );
    }),
    kr("hashtable-contains?", 2, 2, function (e) {
      var t = e[0],
        n = e[1];
      return (
        Ir(t),
        Oi(t, n, {
          on_found: function (e) {
            return !0;
          },
          on_not_found: function (e) {
            return !1;
          },
        })
      );
    }),
    kr("hashtable-update!", 4, 4, function (e) {
      var t = e[0],
        n = e[1],
        i = e[2],
        o = e[3];
      return (
        Ir(t),
        $r(t.mutable, "hashtable is not mutable"),
        Fr(i),
        Oi(t, n, {
          on_found: function (e, t) {
            return new tr(i, [e[1]], function (t) {
              return (e[1] = t[0]), r;
            });
          },
          on_not_found: function (e) {
            return new tr(i, [o], function (i) {
              return t.add_pair(e, n, i[0]), r;
            });
          },
        })
      );
    }),
    kr("hashtable-copy", 1, 2, function (e) {
      var t = void 0 !== e[1] && !!e[1];
      return Ir(e[0]), e[0].create_copy(t);
    }),
    kr("hashtable-clear!", 0, 1, function (e) {
      return (
        Ir(e[0]), $r(e[0].mutable, "hashtable is not mutable"), e[0].clear(), r
      );
    }),
    kr("hashtable-keys", 1, 1, function (e) {
      return Ir(e[0]), e[0].keys();
    }),
    kr("hashtable-entries", 1, 1, function (e) {
      return Ir(e[0]), new ui([e[0].keys(), e[0].values()]);
    }),
    kr("hashtable-equivalence-function", 1, 1, function (e) {
      return Ir(e[0]), e[0].equiv_proc;
    }),
    kr("hashtable-hash-function", 1, 1, function (e) {
      return Ir(e[0]), e[0].hash_proc;
    }),
    kr("hashtable-mutable?", 1, 1, function (e) {
      return Ir(e[0]), e[0].mutable;
    }),
    kr("equal-hash", 0, 0, function (e) {
      return fr.equal_hash;
    }),
    kr("string-hash", 0, 0, function (e) {
      return fr.string_hash;
    }),
    kr("string-ci-hash", 0, 0, function (e) {
      return fr.string_ci_hash;
    }),
    kr("symbol-hash", 0, 0, function (e) {
      return fr.symbol_hash;
    }),
    kr("make-enumeration", 1, 1, function (e) {
      Lr(e[0]);
      var t = e[0].to_array();
      return new Jr.EnumType(t).universe();
    }),
    kr("enum-set-universe", 1, 1, function (e) {
      return Kr(e[0]), e[0].enum_type.universe();
    }),
    kr("enum-set-indexer", 1, 1, function (e) {
      return Kr(e[0]), e[0].enum_type.indexer();
    }),
    kr("enum-set-constructor", 1, 1, function (e) {
      return Kr(e[0]), e[0].enum_type.constructor();
    }),
    kr("enum-set->list", 1, 1, function (e) {
      return Kr(e[0]), e[0].symbol_list();
    }),
    kr("enum-set-member?", 2, 2, function (e) {
      return Nr(e[0]), Kr(e[1]), e[1].is_member(e[0]);
    }),
    kr("enum-set-subset?", 2, 2, function (e) {
      return Kr(e[0]), Kr(e[1]), e[0].is_subset(e[1]);
    }),
    kr("enum-set=?", 2, 2, function (e) {
      return Kr(e[0]), Kr(e[1]), e[0].equal_to(e[1]);
    }),
    kr("enum-set-union", 2, 2, function (e) {
      return (
        Kr(e[0]),
        Kr(e[1]),
        $r(
          e[0].enum_type === e[1].enum_type,
          "two enum-sets must be the same enum-type",
          "enum-set-union"
        ),
        e[0].union(e[1])
      );
    }),
    kr("enum-set-intersection", 2, 2, function (e) {
      return Kr(e[0]), Kr(e[1]), e[0].intersection(e[1]);
    }),
    kr("enum-set-difference", 2, 2, function (e) {
      return Kr(e[0]), Kr(e[1]), e[0].difference(e[1]);
    }),
    kr("enum-set-complement", 1, 1, function (e) {
      return Kr(e[0]), e[0].complement();
    }),
    kr("enum-set-projection", 2, 2, function (e) {
      return Kr(e[0]), Kr(e[1]), e[0].projection(e[1]);
    }),
    Sr("define-enumeration", function (e) {
      var t = e.cdr.car;
      $r(Xn(t), "expected symbol for type_name", "define-enumeration"),
        (t = t.name);
      var n = e.cdr.cdr.car;
      $r(wn(n), "expected list of symbol for members", "define-enumeration"),
        (n = n.to_array());
      var r = e.cdr.cdr.cdr.car;
      $r(Xn(r), "expected symbol for constructor_name", "define-enumeration"),
        (r = r.name);
      var i = new Jr.EnumType(n);
      Sr(t, function (e) {
        $r(!Bn(e.cdr), "an argument is needed", t);
        var n = e.cdr.car;
        return (
          Nr(n, t),
          $r(
            Ot(i.members, n),
            n.name + " is not included in the universe: " + Mn(i.members),
            t
          ),
          _n(ln("quote"), n)
        );
      }),
        Sr(r, function (e) {
          Lr(e.cdr, r);
          var t = e.cdr.to_array();
          return (
            St(t, function (e) {
              Nr(e, r),
                $r(
                  Ot(i.members, e),
                  e.name + " is not included in the universe: " + Mn(i.members),
                  r
                );
            }),
            new Jr.EnumSet(i, t)
          );
        });
    }),
    kr("eval", 1, 1, function (e, t) {
      var n = e[0];
      return new hr(t).evaluate(Mn(n));
    }),
    Sr("delay", function (e) {
      if (e.cdr === n) throw new hn("malformed delay: no argument");
      if (e.cdr.cdr !== n)
        throw new hn("malformed delay: too many arguments: " + write_ss(e));
      var t = e.cdr.car;
      return new gn(
        ln(" procedure->promise"),
        new gn(
          new gn(
            ln("lambda"),
            new gn(n, new gn(new gn(ln("make-promise"), new gn(t, n)), n))
          )
        )
      );
    }),
    Sr("delay-force", function (e) {
      if (e.cdr === n) throw new hn("malformed delay-force: no argument");
      if (e.cdr.cdr !== n)
        throw new hn(
          "malformed delay-force: too many arguments: " + write_ss(e)
        );
      var t = e.cdr.car;
      return new gn(
        ln(" procedure->promise"),
        new gn(new gn(ln("lambda"), new gn(n, new gn(t, n))), n)
      );
    });
  var Mi = function (e) {
    return e.is_done()
      ? e.value()
      : new tr(e.thunk(), [], function (t) {
          Hr(t[0]);
          var n = t[0];
          return e.is_done() ? e.value() : (e.update_with(n), Mi(n));
        });
  };
  kr("force", 1, 1, function (e, t) {
    return Hr(e[0]), Mi(e[0]);
  }),
    kr("promise?", 1, 1, function (e, t) {
      return e[0] instanceof _r;
    }),
    kr("make-promise", 1, 1, function (e, t) {
      var n = e[0];
      return n instanceof _r ? n : _r.done(n);
    }),
    kr(" procedure->promise", 1, 1, function (e, t) {
      return Fr(e[0]), _r.fresh(e[0]);
    }),
    kr("make-parameter", 1, 2, function (e, t) {
      let n;
      const r = e[1],
        i = function (e) {
          if (0 == e.length) return n;
          {
            const t = n;
            return r
              ? new tr(r, [e[0]], (e) => ((n = e[0]), t))
              : ((n = e[0]), t);
          }
        };
      if (r) return new tr(r, [e[0]], (e) => ((n = e), i));
      {
        const t = e[0];
        return (n = t), i;
      }
    }),
    Sr("parameterize", function (e) {
      const t = e.cdr.car.to_array(),
        r = e.cdr.cdr,
        i = t.map(() => dn()),
        o = _n(...t.map((e, t) => _n(i[t], e.cdr.car))),
        u = jn(
          ln("begin"),
          _n(...t.map((e, t) => _n(ln("set!"), i[t], _n(e.car, i[t]))))
        ),
        a = _n(ln("lambda"), n, u),
        c = jn(ln("lambda"), jn(n, r)),
        s = _n(ln("lambda"), n, u);
      return _n(ln("let"), o, _n(ln("dynamic-wind"), a, c, s));
    }),
    kr("iota", 1, 3, function (e) {
      var t = e[0],
        n = e[1] || 0,
        r = void 0 === e[2] ? 1 : e[2];
      Cr(t), Tr(n), Tr(r);
      for (var i = [], o = n, u = 0; u < t; u++) i.push(o), (o += r);
      return xn(i);
    });
  var Li = function (e) {
    var t = yn(e.car) ? Li(e.car) : e.car,
      n = yn(e.cdr) ? Li(e.cdr) : e.cdr;
    return new gn(t, n);
  };
  kr("list-copy", 1, 1, function (e) {
    return yn(e[0]) ? Li(e[0]) : n;
  }),
    kr("open-input-string", 1, 1, function (e) {
      return Ar(e[0]), new Pn.StringInput(e[0]);
    }),
    kr("open-output-string", 0, 0, function (e) {
      return new Pn.StringOutput();
    }),
    kr("get-output-string", 1, 1, function (e) {
      if ((Or(e[0]), !(e[0] instanceof Pn.StringOutput)))
        throw new Error(
          "get-output-string: port must be made by 'open-output-string'"
        );
      return e[0].output_string();
    }),
    Sr("receive", function (e) {
      $r(yn(e.cdr), "missing formals", "receive");
      var t = e.cdr.car;
      $r(yn(e.cdr.cdr), "missing expression", "receive");
      var r = e.cdr.cdr.car,
        i = e.cdr.cdr.cdr;
      return kn([
        ln("call-with-values"),
        [ln("lambda"), n, r],
        new gn(ln("lambda"), new gn(t, i)),
      ]);
    }),
    kr("current-date", 0, 1, function (e) {
      return new Date();
    }),
    kr("date?", 1, 1, function (e) {
      return e[0] instanceof Date;
    }),
    kr("date-nanosecond", 1, 1, function (e) {
      return Br(e[0]), 1e6 * e[0].getMilliseconds();
    }),
    kr("date-millisecond", 1, 1, function (e) {
      return Br(e[0]), e[0].getMilliseconds();
    }),
    kr("date-second", 1, 1, function (e) {
      return Br(e[0]), e[0].getSeconds();
    }),
    kr("date-minute", 1, 1, function (e) {
      return Br(e[0]), e[0].getMinutes();
    }),
    kr("date-hour", 1, 1, function (e) {
      return Br(e[0]), e[0].getHours();
    }),
    kr("date-day", 1, 1, function (e) {
      return Br(e[0]), e[0].getDate();
    }),
    kr("date-month", 1, 1, function (e) {
      return Br(e[0]), e[0].getMonth() + 1;
    }),
    kr("date-year", 1, 1, function (e) {
      return Br(e[0]), e[0].getFullYear();
    }),
    kr("date-week-day", 1, 1, function (e) {
      return Br(e[0]), e[0].getDay();
    });
  const Ri = {
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
    Ii = function (e, t) {
      var n = function (e) {
          return e < 10 ? "0" + e : "" + e;
        },
        r = function (e) {
          return e < 10 ? " " + e : "" + e;
        },
        i = {
          a: function (e) {
            return Ri.weekday[e.getDay()];
          },
          A: function (e) {
            return Ri.full_weekday[e.getDay()];
          },
          b: function (e) {
            return Ri.month[e.getMonth()];
          },
          B: function (e) {
            return Ri.full_month[e.getMonth()];
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
            return Ri.month[e.getMonth()];
          },
          H: function (e) {
            return n(e.getHours());
          },
          I: function (e) {
            var t = e.getHours();
            return n(t < 13 ? t : t - 12);
          },
          j: function (e) {
            throw new pn("not implemented: day of year");
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
            throw new pn("not implemented: nanoseconds");
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
            throw new pn("not implemented: weeknum(0~, Sun)");
          },
          V: function (e) {
            return (function (e) {
              var t = new Date(e.getFullYear(), 0, 4),
                n = new Date(e.getFullYear(), 0, 4);
              return (
                t.getDay() >= Ri.weekday.indexOf("Thu")
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
            throw new pn("not implemented: weeknum(0~, Mon)");
          },
          x: function (e) {
            throw new pn("not implemented: weeknum(1~, Mon)");
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
            throw new pn("not implemented: time-zone");
          },
          Z: function (e) {
            throw new pn("not implemented: symbol time zone");
          },
          1: function (e) {
            throw new pn("not implemented: ISO-8601 year-month-day format");
          },
          2: function (e) {
            throw new pn(
              "not implemented: ISO-8601 hour-minute-second-timezone format"
            );
          },
          3: function (e) {
            throw new pn("not implemented: ISO-8601 hour-minute-second format");
          },
          4: function (e) {
            throw new pn(
              "not implemented: ISO-8601 year-month-day-hour-minute-second-timezone format"
            );
          },
          5: function (e) {
            throw new pn(
              "not implemented: ISO-8601 year-month-day-hour-minute-second format"
            );
          },
        };
      return t.replace(/~([\w1-5~])/g, function (t, n) {
        var r = i[n];
        return r ? r(e) : "~" == n ? "~" : n;
      });
    };
  kr("date->string", 1, 2, function (e) {
    return Br(e[0]), e[1] ? (Ar(e[1]), Ii(e[0], e[1])) : e[0].toString();
  }),
    kr("parse-date", 1, 1, function (e) {
      return Ar(e[0]), new Date(Date.parse(e[0]));
    }),
    kr("random-integer", 1, 1, function (e) {
      var t = e[0];
      if ((Cr(t), t < 0))
        throw new Error("random-integer: the argument must be >= 0");
      return Math.floor(Math.random() * e[0]);
    }),
    kr("random-real", 0, 0, function (e) {
      return Math.random();
    }),
    kr("format", 1, null, function (e) {
      if (A(e[0]))
        var t = null,
          n = e.shift();
      else if (!1 === e[0]) {
        e.shift();
        (t = null), (n = e.shift());
      } else if (!0 === e[0]) {
        e.shift();
        (t = Pn.current_output), (n = e.shift());
      } else {
        (t = e.shift()), (n = e.shift());
        Or(t);
      }
      var i = n
        .replace(/~[as]/g, function (t) {
          return (
            $r(e.length > 0, "insufficient number of arguments", "format"),
            "~a" == t ? qn(e.shift()) : Mn(e.shift())
          );
        })
        .replace(/~%/, "\n")
        .replace(/~~/, "~");
      return t ? (t.put_string(i), r) : i;
    });
  const Hi = function (e) {
    return ai.puts(Nn(e[0]), !0), r;
  };
  kr("write/ss", 1, 2, Hi),
    kr("write-with-shared-structure", 1, 2, Hi),
    kr("write*", 1, 2, Hi),
    kr("vector-append", 2, null, function (e) {
      var t = [];
      return t.concat.apply(t, e);
    }),
    kr("vector-copy", 1, 1, function (e) {
      return Rr(e[0]), De(e[0]);
    }),
    (n.to_set = function () {
      return new vn();
    });
  var Pi = {
    TopEnv: e,
    CoreEnv: t,
    nil: n,
    undef: r,
    max_trace_size: 40,
    suppress_deprecation_warning: !1,
    Version: "0.7.4",
    VERSION: "0.7.4",
    GitCommit: "09aecb2d4ed634a08ff03c3dee807ef2bad4ca50",
    isNil: Bn,
    isUndef: function (e) {
      return e === r;
    },
    isBoolean: $n,
    isString: Wn,
    isChar: Vn,
    isSymbol: Xn,
    isPort: Yn,
    isPair: yn,
    isList: wn,
    isVector: Gn,
    isHashtable: lr,
    isMutableHashtable: function (e) {
      return e instanceof fr && e.mutable;
    },
    isProcedure: Jn,
    isSelfEvaluating: function (e) {
      return $n(e) || isNumber(e) || Wn(e) || Vn(e);
    },
    eq: Qn,
    eqv: Kn,
    equal: Zn,
    lt: er,
    to_write: Mn,
    to_display: qn,
    inspect: An,
    write_ss: Nn,
    to_write_ss: Nn,
    Call: tr,
    Char: Rn,
    Closure: zn,
    isClosure: Fn,
    Compiler: or,
    Enumeration: Jr,
    isEnumSet: Qr,
    Error: hn,
    Bug: pn,
    UserError: mn,
    Hashtable: fr,
    Interpreter: hr,
    Complex: pr,
    Rational: mr,
    isNumber: vr,
    isComplex: gr,
    isReal: yr,
    isRational: wr,
    isInteger: br,
    Pair: gn,
    List: _n,
    array_to_list: xn,
    deep_array_to_list: kn,
    Cons: jn,
    Parser: dr,
    Pause: In,
    Port: Pn,
    eof: Hn,
    Promise: _r,
    isPromise: xr,
    Record: Zr,
    isRecord: ei,
    isRecordTD: ti,
    isRecordCD: ni,
    Set: vn,
    Symbol: fn,
    Sym: ln,
    gensym: dn,
    Syntax: rr,
    Values: ui,
    VMCode: ir,
    define_libfunc: kr,
    define_scmfunc: function (e, t, n, r) {
      new hr().evaluate("(define " + e + " " + r + "\n)");
    },
    parse_fraction: Ur,
    is_valid_integer_notation: Vr,
    parse_integer: Xr,
    is_valid_float_notation: Yr,
    parse_float: Gr,
  };
  (ai.puts = function (e, t) {
    Pn.current_output.put_string(e + (t ? "" : "\n"));
  }),
    (ai.p = function () {
      Pn.current_output.put_string("p> " + Tt(Wt(arguments), An).join(" "));
    });
  const zi = new Pn.CustomInput(function (e) {
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
    Fi = new Pn.CustomOutput(function (e) {
      const t = document.querySelector("#bs-console");
      if (!t) return;
      const n = document.createElement("span");
      (n.innerHTML = Ge(e).replace(/\n/g, "<br>").replace(/ /g, "&nbsp;")),
        t.appendChild(n);
    }),
    Bi = Fi,
    $i = window.jQuery;
  kr("read-line", 0, 1, function (e) {
    var t = e[0] || Pn.current_input;
    return Or(t), t.get_string((e) => e);
  }),
    kr("element-empty!", 1, 1, function (e) {
      return $i(e[0]).prop("value") ? $i(e[0]).val("") : $i(e[0]).empty();
    }),
    jr("element-empty!", "element-clear!"),
    kr("element-visible?", 1, 1, function (e) {
      return $i(e[0]).is(":visible");
    }),
    kr("element-toggle!", 1, 1, function (e) {
      return $i(e[0]).toggle();
    }),
    kr("element-hide!", 1, 1, function (e) {
      return $i(e[0]).hide();
    }),
    kr("element-show!", 1, 1, function (e) {
      return $i(e[0]).show();
    }),
    kr("element-remove!", 1, 1, function (e) {
      return $i(e[0]).remove();
    }),
    kr("element-update!", 2, 2, function (e) {
      return $i(e[0]).html(e[1]);
    }),
    kr("element-replace!", 2, 2, function (e) {
      return $i(e[0]).replaceWith(e[1]);
    }),
    kr("element-insert!", 2, 2, function (e) {
      return $i(e[0]).append(e[1]);
    }),
    kr("element-wrap!", 3, 3, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-ancestors", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-descendants", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-first-descendant", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-immediate-descendants", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-previous-sibling", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-next-sibling", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-siblings", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-match?", 2, 2, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-up", 3, 3, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-down", 3, 3, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-previous", 3, 3, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-next", 3, 3, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-select", 1, 1, function (e) {
      $i(e[0]).select();
    }),
    kr("element-adjacent", 0, 0, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-identify", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-read-attribute", 2, 2, function (e) {
      return Ar(e[1]), $i(e[0]).prop(e[1]);
    });
  var Wi = function (e) {
    return Ar(e[1]), $i(e[0]).prop(e[1], e[2]);
  };
  kr("element-write-attribute", 3, 3, function (e) {
    return (
      Wr("element-write-attribute", "1.0", "element-write-attribute!"), Wi(e)
    );
  }),
    kr("element-write-attribute!", 3, 3, Wi),
    kr("element-height", 1, 1, function (e) {
      return $i(e[0]).height();
    }),
    kr("element-width", 1, 1, function (e) {
      return $i(e[0]).width();
    }),
    kr("element-class-names", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-has-class-name?", 2, 2, function (e) {
      return Ar(e[1]), $i(e[0]).hasClass(e[1]);
    });
  var Ui = function (e) {
    return Ar(e[1]), $i(e[0]).addClass(e[1]);
  };
  kr("element-add-class-name", 2, 2, function (e) {
    return (
      Wr("element-add-class-name", "1.0", "element-add-class-name!"), Ui(e)
    );
  }),
    kr("element-add-class-name!", 2, 2, Ui);
  var Vi = function (e) {
    return Ar(e[1]), $i(e[0]).removeClass(e[1]);
  };
  kr("element-remove-class-name", 2, 2, function (e) {
    return (
      Wr("element-remove-class-name", "1.0", "element-remove-class-name!"),
      Vi(e)
    );
  }),
    kr("element-remove-class-name!", 2, 2, Vi);
  var Xi = function (e) {
    return Ar(e[1]), $i(e[0]).toggleClass(e[1]);
  };
  kr("element-toggle-class-name", 2, 2, function (e) {
    return (
      Wr("element-toggle-class-name", "1.0", "element-toggle-class-name!"),
      Xi(e)
    );
  }),
    kr("element-toggle-class-name!", 2, 2, Xi),
    kr("element-clean-whitespace!", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-empty?", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-descendant-of!", 2, 2, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("scroll-to-element!", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-style", 2, 2, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-opacity", 2, 2, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-style-set!", 2, 2, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-opacity-set!", 2, 2, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-dimensions", 1, 1, function (e) {
      return new Values($i(e[0]).width(), $i(e[0]).height());
    }),
    kr("element-make-positioned!", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-undo-positioned!", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-make-clipping!", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-undo-clipping!", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-cumulative-offset", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-positioned-offset", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-absolutize!", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-relativize!", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-cumulative-scroll-offset", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-offset-parent", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-viewport-offset", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-clone-position!", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-absolutize!", 1, 1, function (e) {
      throw new pn("not yet implemented");
    }),
    kr("element-focus!", 1, 1, function (e) {
      return $i(e[0]).focus();
    });
  const Yi = function (e) {
      var t = (e = e.to_array()).shift();
      t instanceof fn && (t = t.name);
      var n = t.match(/(.*)\.(.*)/);
      n && ((t = n[1]), e.unshift(ln("class"), n[2])),
        (n = t.match(/(.*)\#(.*)/)) && ((t = n[1]), e.unshift(ln("id"), n[2]));
      for (var r = [], i = ["<" + t], o = 0; o < e.length; o++)
        e[o] instanceof fn
          ? (i.push(" " + e[o].name + '="' + e[o + 1] + '"'), o++)
          : e[o] instanceof gn
          ? r.push(Yi(e[o]))
          : r.push(e[o]);
      return (
        i.push(">"), i.push(r.join("")), i.push("</" + t + ">"), i.join("")
      );
    },
    Gi = function (e, t) {
      return e === n || (!1 !== t(e.car) && Gi(e.cdr, t));
    };
  kr("element-new", 1, 1, function (e) {
    return Gi(e[0], function (e) {
      return A(e) || e instanceof fn || e instanceof gn;
    })
      ? $i(Yi(e[0]))[0]
      : n;
  });
  const Ji = function (e) {
    return $i(e).prop("value") ? $i(e).val() : Ge($i(e).html());
  };
  kr("element-content", 1, 1, function (e) {
    return Ji(e[0]);
  }),
    kr("load", 1, 1, function (e, t) {
      var n = e[0];
      Ar(n);
      var i = new hr(t);
      return new In(function (e) {
        $i.ajax(n, {
          dataType: "text",
          mimeType: "text/plain; charset=UTF-8",
          success: function (t) {
            i.evaluate(t, function () {
              return e.resume(r);
            });
          },
          error: function () {
            throw new Error("load: network error: failed to load " + n);
          },
        });
      });
    });
  kr("js-load", 2, 2, function (e) {
    var t = e[0],
      n = e[1];
    return (
      Ar(t),
      Ar(n),
      new In(function (e) {
        !(function (e, t, n) {
          var r = $i("<script/>", { src: e });
          $i("body").append(r);
          var i = new Function("return !!(" + t + ")");
          i()
            ? n()
            : setTimeout(function () {
                i() ? n() : setTimeout(arguments.callee, 10);
              }, 10);
        })(t, "window." + n, function () {
          e.resume(r);
        });
      })
    );
  });
  const Qi = function (e) {
    e.length > 1 && !1 === e[1] && (e[1] = []);
    var t = $i.apply(this, e);
    return t.length > 0 && t;
  };
  kr("$", 1, 2, Qi),
    kr("getelem", 1, 2, Qi),
    kr("dom-element", 1, 1, function (e) {
      return $i(e[0])[0];
    }),
    kr("set-style!", 3, 3, function (e) {
      return Ar(e[1]), $i(e[0]).css(e[1], e[2]), r;
    }),
    kr("get-style", 2, 2, function (e) {
      return Ar(e[1]), $i(e[0]).css(e[1]);
    }),
    kr("set-content!", 2, 2, function (e) {
      Ar(e[1]);
      var t = e[1].replace(/\n/g, "<br>").replace(/\t/g, "&nbsp;&nbsp;&nbsp;");
      return $i(e[0]).html(t), r;
    }),
    kr("get-content", 1, 1, function (e) {
      return Ji(e[0]);
    }),
    kr("set-handler!", 3, 3, function (e, t) {
      throw new Error(
        "set-handler! is obsolete, please use add-handler! instead"
      );
    }),
    kr("add-handler!", 3, 3, function (e, t) {
      var n = e[0],
        r = e[1],
        i = e[2],
        o = new hr(t),
        u = function (e) {
          return De(o).invoke_closure(i, [e]);
        };
      return $i(n).on(r, u), u;
    }),
    kr("remove-handler!", 3, 3, function (e, t) {
      var n = e[0],
        i = e[1],
        o = e[2];
      return $i(n).off(i, o), r;
    }),
    kr("wait-for", 2, 2, function (e) {
      var t = e[0],
        n = e[1],
        r = $i(t);
      r.biwascheme_wait_for = r.biwascheme_wait_for || {};
      var i = r.biwascheme_wait_for[n];
      return (
        i && r.off(n, i),
        new In(function (e) {
          var t = function (i) {
            return (
              (r.biwascheme_wait_for[n] = void 0), r.off(n, t), e.resume(i)
            );
          };
          (r.biwascheme_wait_for[n] = t), r.on(n, t);
        })
      );
    }),
    kr("domelem", 1, null, function (e) {
      throw new Error("obsolete");
    }),
    kr("dom-remove-children!", 1, 1, function (e) {
      return (
        ai.puts(
          "warning: dom-remove-children! is obsolete. use element-empty! instead"
        ),
        $i(e[0]).empty(),
        r
      );
    }),
    kr("dom-create-element", 1, 1, function (e) {
      throw new Error("obsolete");
    }),
    kr("element-append-child!", 2, 2, function (e) {
      return $i(e[0]).append(e[1]);
    }),
    kr("dom-remove-child!", 2, 2, function (e) {
      throw new Error("obsolete");
    }),
    kr("http-request", 1, 1, function (e) {
      var t = e[0];
      return (
        Ar(t),
        new In(function (e) {
          $i.get(
            t,
            function (t) {
              e.resume(t);
            },
            "text"
          );
        })
      );
    }),
    kr("http-post", 2, 2, function (e) {
      var t = e[0];
      Ar(t);
      var n = e[1];
      Lr(n);
      var r = Tn(n);
      return new In(function (e) {
        $i.post(
          t,
          r,
          function (t) {
            e.resume(t);
          },
          "text"
        );
      });
    });
  const Ki = [];
  kr("receive-jsonp", 1, 1, function (e) {
    var t = e[0];
    Ar(t);
    for (var n = Ki, r = 0; r < n.length && null !== n[r]; r++);
    var i = r;
    return (
      (t += "?callback=BiwaScheme.jsonp_receiver[" + i + "]"),
      new In(function (e) {
        n[i] = function (t) {
          e.resume(t), (n[i] = null);
        };
        var r = $i("<script/>", { src: t });
        $i("body").append(r);
      })
    );
  }),
    kr("alert", 1, 1, function (e) {
      return alert(e[0]), r;
    }),
    kr("confirm", 1, 1, function (e) {
      return confirm(e[0]);
    });
  const Zi = cn.create({
    initialize: function (e) {
      (this.dumparea = e || $("#dumparea")[0] || null), this.reset();
    },
    reset: function () {
      this.dumparea && $(this.dumparea).empty(),
        (this.n_folds = 0),
        (this.closures = []),
        (this.n_dumps = 0),
        (this.cur = -1),
        (this.is_folded = !0);
    },
    is_opc: function (e) {
      return e instanceof Array && "string" == typeof e[0];
    },
    dump_pad: "&nbsp;&nbsp;&nbsp;",
    dump_opc: function (e, t, n) {
      var r = "",
        i = "",
        o = "";
      n = n || !1;
      We(
        (t = t || 0),
        at(function () {
          i += this.dump_pad;
        }, this)
      ),
        We(
          t + 1,
          at(function () {
            o += this.dump_pad;
          }, this)
        ),
        (r += i + '[<span class="dump_opecode">' + e[0] + "</span>");
      for (var u = 1; !(e[u] instanceof Array) && u < e.length; )
        "constant" == e[0]
          ? (r +=
              "&nbsp;<span class='dump_constant'>" +
              this.dump_obj(e[u]) +
              "</span>")
          : (r += "&nbsp;" + this.dump_obj(e[u])),
          u++;
      for (u < e.length && (r += "<br>\n"); u < e.length; u++)
        this.is_opc(e[u])
          ? (r += this.dump_opc(e[u], u == e.length - 1 ? t : t + 1, !0))
          : ((r += u == e.length - 1 ? i : o), (r += this.dump_obj(e[u]))),
          u != e.length - 1 && (r += "<br>\n");
      return (r += "]"), n ? r : this.add_fold(r);
    },
    fold_limit: 20,
    add_fold: function (e) {
      var t = e.split(/<br>/gim);
      if (t.length > this.fold_limit) {
        var n =
            " <span style='text-decoration:underline; color:blue; cursor:pointer;'onclick='BiwaScheme.Dumper.toggle_fold(" +
            this.n_folds +
            ")'>more</span>",
          r = "<div style='display:none' class='fold" + this.n_folds + "'>";
        return (
          this.n_folds++,
          [
            t.slice(0, this.fold_limit).join("<br>"),
            n,
            r,
            t.slice(this.fold_limit).join("<br>"),
            "</div>",
          ].join("")
        );
      }
      return e;
    },
    stack_max_len: 80,
    dump_stack: function (e, t) {
      if (null == e) return An(e);
      var n = "<table>";
      if (0 == e.length)
        n += "<tr><td class='dump_dead'>(stack is empty)</td></tr>";
      else if (t < e.length) {
        var r = e.length - 1;
        n +=
          "<tr><td class='dump_dead'>[" +
          r +
          "]</td><td class='dump_dead'>" +
          Cn(this.dump_obj(e[r]), this.stack_max_len) +
          "</td></tr>";
      }
      for (var i = t - 1; i >= 0; i--)
        n +=
          "<tr><td class='dump_stknum'>[" +
          i +
          "]</td><td>" +
          Cn(this.dump_obj(e[i]), this.stack_max_len) +
          "</td></tr>";
      return n + "</table>";
    },
    dump_object: function (e) {
      var t = [];
      for (var n in e) t.push(n.toString());
      return "#<Object{" + t.join(",") + "}>";
    },
    dump_closure: function (e) {
      if (!e) return "**BROKEN**";
      if (0 == e.length) return "[]";
      for (var t = null, n = 0; n < this.closures.length; n++)
        this.closures[n] == e && (t = n);
      null == t && ((t = this.closures.length), this.closures.push(e));
      var r = De(e),
        i = r.shift && r.shift();
      return [
        "c",
        t,
        " <span class='dump_closure'>free vars :</span> ",
        this.dump_obj(r),
        " <span class='dump_closure'>body :</span> ",
        Cn(this.dump_obj(i), 100),
      ].join("");
    },
    dump_obj: function (e) {
      if (e && "function" == typeof e.to_html) return e.to_html();
      var t = Mn(e);
      return "[object Object]" == t && (t = this.dump_object(e)), Ge(t);
    },
    dump: function (e) {
      var t = "";
      e instanceof Object
        ? ((t += "<table>"),
          (t +=
            "<tr><td colspan='4'><a href='#' class='header'>#" +
            this.n_dumps +
            "</a></td></tr>"),
          St(
            ue(e),
            at(function (n) {
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
            }, this)
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
        : (t = Ge(An(e)) + "<br>\n");
      var n = $("<div/>", { class: "dump" + this.n_dumps });
      n.html(t),
        $(this.dumparea).append(n),
        at(function (e) {
          $(".header", this.dump_el(this.n_dumps)).click(
            at(function () {
              this.dump_move_to(e), this.dump_fold();
            }, this)
          );
        }, this)(this.n_dumps),
        n.hide(),
        this.n_dumps++;
    },
    dump_el: function (e) {
      return $(".dump" + e, this.dumparea);
    },
    dump_move_to: function (e) {
      e < 0 && (e = this.n_dumps + e),
        0 <= e &&
          e <= this.n_dumps &&
          (this.dump_el(this.cur).hide(),
          (this.cur = e),
          this.dump_el(this.cur).show());
    },
    dump_move: function (e) {
      0 <= this.cur && this.cur < this.n_dumps && this.dump_el(this.cur).hide(),
        0 <= this.cur + e && this.cur + e < this.n_dumps && (this.cur += e),
        this.dump_el(this.cur).show();
    },
    dump_fold: function () {
      for (var e = 0; e < this.n_dumps; e++)
        e != this.cur && this.dump_el(e).hide();
      this.is_folded = !0;
    },
    dump_unfold: function () {
      for (var e = 0; e < this.n_dumps; e++) this.dump_el(e).show();
      this.is_folded = !1;
    },
    dump_toggle_fold: function () {
      this.is_folded ? this.dump_unfold() : this.dump_fold();
    },
  });
  Zi.toggle_fold = function (e) {
    $(".fold" + e, this.dumparea).toggle();
  };
  return (
    (Pi.on_node = !1),
    (Pi.Console = ai),
    (Pi.Port.current_input = zi),
    (Pi.Port.current_output = Fi),
    (Pi.Port.current_error = Bi),
    (Pi.jsonp_receiver = Ki),
    (Pi.Dumper = Zi),
    (window.BiwaScheme = window.BiwaScheme || {}),
    Object.assign(window.BiwaScheme, Pi),
    (function () {
      const e = null,
        t = document.querySelector("#biwascheme-debugger");
      t && (e = new Zi(t));
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
    })(),
    Pi
  );
})();
