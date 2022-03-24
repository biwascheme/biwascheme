/*
 * BiwaScheme 0.7.5 - R6RS/R7RS Scheme in JavaScript
 *
 * Copyright (c) 2007-2022 Yutaka HARA (http://www.biwascheme.org/)
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
    o = n.push,
    s = n.indexOf,
    u = {},
    c = u.toString,
    l = u.hasOwnProperty,
    f = l.toString,
    d = f.call(Object),
    p = {},
    h = function (e) {
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
    g = { type: !0, src: !0, nonce: !0, noModule: !0 };
  function y(e, t, n) {
    var r,
      i,
      a = (n = n || _).createElement("script");
    if (((a.text = e), t))
      for (r in g)
        (i = t[r] || (t.getAttribute && t.getAttribute(r))) &&
          a.setAttribute(r, i);
    n.head.appendChild(a).parentNode.removeChild(a);
  }
  function b(e) {
    return null == e
      ? e + ""
      : "object" == typeof e || "function" == typeof e
      ? u[c.call(e)] || "object"
      : typeof e;
  }
  var v = function (e, t) {
    return new v.fn.init(e, t);
  };
  function w(e) {
    var t = !!e && "length" in e && e.length,
      n = b(e);
    return (
      !h(e) &&
      !m(e) &&
      ("array" === n ||
        0 === t ||
        ("number" == typeof t && t > 0 && t - 1 in e))
    );
  }
  (v.fn = v.prototype = {
    jquery: "3.6.0",
    constructor: v,
    length: 0,
    toArray: function () {
      return i.call(this);
    },
    get: function (e) {
      return null == e ? i.call(this) : e < 0 ? this[e + this.length] : this[e];
    },
    pushStack: function (e) {
      var t = v.merge(this.constructor(), e);
      return (t.prevObject = this), t;
    },
    each: function (e) {
      return v.each(this, e);
    },
    map: function (e) {
      return this.pushStack(
        v.map(this, function (t, n) {
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
        v.grep(this, function (e, t) {
          return (t + 1) % 2;
        })
      );
    },
    odd: function () {
      return this.pushStack(
        v.grep(this, function (e, t) {
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
    push: o,
    sort: n.sort,
    splice: n.splice,
  }),
    (v.extend = v.fn.extend = function () {
      var e,
        t,
        n,
        r,
        i,
        a,
        o = arguments[0] || {},
        s = 1,
        u = arguments.length,
        c = !1;
      for (
        "boolean" == typeof o && ((c = o), (o = arguments[s] || {}), s++),
          "object" == typeof o || h(o) || (o = {}),
          s === u && ((o = this), s--);
        s < u;
        s++
      )
        if (null != (e = arguments[s]))
          for (t in e)
            (r = e[t]),
              "__proto__" !== t &&
                o !== r &&
                (c && r && (v.isPlainObject(r) || (i = Array.isArray(r)))
                  ? ((n = o[t]),
                    (a =
                      i && !Array.isArray(n)
                        ? []
                        : i || v.isPlainObject(n)
                        ? n
                        : {}),
                    (i = !1),
                    (o[t] = v.extend(c, a, r)))
                  : void 0 !== r && (o[t] = r));
      return o;
    }),
    v.extend({
      expando: "jQuery" + ("3.6.0" + Math.random()).replace(/\D/g, ""),
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
        y(e, { nonce: t && t.nonce }, n);
      },
      each: function (e, t) {
        var n,
          r = 0;
        if (w(e))
          for (n = e.length; r < n && !1 !== t.call(e[r], r, e[r]); r++);
        else for (r in e) if (!1 === t.call(e[r], r, e[r])) break;
        return e;
      },
      makeArray: function (e, t) {
        var n = t || [];
        return (
          null != e &&
            (w(Object(e))
              ? v.merge(n, "string" == typeof e ? [e] : e)
              : o.call(n, e)),
          n
        );
      },
      inArray: function (e, t, n) {
        return null == t ? -1 : s.call(t, e, n);
      },
      merge: function (e, t) {
        for (var n = +t.length, r = 0, i = e.length; r < n; r++) e[i++] = t[r];
        return (e.length = i), e;
      },
      grep: function (e, t, n) {
        for (var r = [], i = 0, a = e.length, o = !n; i < a; i++)
          !t(e[i], i) !== o && r.push(e[i]);
        return r;
      },
      map: function (e, t, n) {
        var r,
          i,
          o = 0,
          s = [];
        if (w(e))
          for (r = e.length; o < r; o++)
            null != (i = t(e[o], o, n)) && s.push(i);
        else for (o in e) null != (i = t(e[o], o, n)) && s.push(i);
        return a(s);
      },
      guid: 1,
      support: p,
    }),
    "function" == typeof Symbol && (v.fn[Symbol.iterator] = n[Symbol.iterator]),
    v.each(
      "Boolean Number String Function Array Date RegExp Object Error Symbol".split(
        " "
      ),
      function (e, t) {
        u["[object " + t + "]"] = t.toLowerCase();
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
        a,
        o,
        s,
        u,
        c,
        l,
        f,
        d,
        p,
        h,
        m,
        _,
        g,
        y,
        b,
        v = "sizzle" + 1 * new Date(),
        w = e.document,
        x = 0,
        S = 0,
        C = ue(),
        k = ue(),
        E = ue(),
        P = ue(),
        j = function (e, t) {
          return e === t && (f = !0), 0;
        },
        $ = {}.hasOwnProperty,
        T = [],
        A = T.pop,
        B = T.push,
        q = T.push,
        L = T.slice,
        D = function (e, t) {
          for (var n = 0, r = e.length; n < r; n++) if (e[n] === t) return n;
          return -1;
        },
        N =
          "checked|selected|async|autofocus|autoplay|controls|defer|disabled|hidden|ismap|loop|multiple|open|readonly|required|scoped",
        O = "[\\x20\\t\\r\\n\\f]",
        I =
          "(?:\\\\[\\da-fA-F]{1,6}" +
          O +
          "?|\\\\[^\\r\\n\\f]|[\\w-]|[^\0-\\x7f])+",
        M =
          "\\[" +
          O +
          "*(" +
          I +
          ")(?:" +
          O +
          "*([*^$|!~]?=)" +
          O +
          "*(?:'((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\"|(" +
          I +
          "))|)" +
          O +
          "*\\]",
        R =
          ":(" +
          I +
          ")(?:\\((('((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\")|((?:\\\\.|[^\\\\()[\\]]|" +
          M +
          ")*)|.*)\\)|)",
        H = new RegExp(O + "+", "g"),
        F = new RegExp("^" + O + "+|((?:^|[^\\\\])(?:\\\\.)*)" + O + "+$", "g"),
        W = new RegExp("^" + O + "*," + O + "*"),
        z = new RegExp("^" + O + "*([>+~]|" + O + ")" + O + "*"),
        V = new RegExp(O + "|>"),
        U = new RegExp(R),
        X = new RegExp("^" + I + "$"),
        K = {
          ID: new RegExp("^#(" + I + ")"),
          CLASS: new RegExp("^\\.(" + I + ")"),
          TAG: new RegExp("^(" + I + "|[*])"),
          ATTR: new RegExp("^" + M),
          PSEUDO: new RegExp("^" + R),
          CHILD: new RegExp(
            "^:(only|first|last|nth|nth-last)-(child|of-type)(?:\\(" +
              O +
              "*(even|odd|(([+-]|)(\\d*)n|)" +
              O +
              "*(?:([+-]|)" +
              O +
              "*(\\d+)|))" +
              O +
              "*\\)|)",
            "i"
          ),
          bool: new RegExp("^(?:" + N + ")$", "i"),
          needsContext: new RegExp(
            "^" +
              O +
              "*[>+~]|:(even|odd|eq|gt|lt|nth|first|last)(?:\\(" +
              O +
              "*((?:-\\d)?\\d*)" +
              O +
              "*\\)|)(?=[^-]|$)",
            "i"
          ),
        },
        G = /HTML$/i,
        Y = /^(?:input|select|textarea|button)$/i,
        J = /^h\d$/i,
        Q = /^[^{]+\{\s*\[native \w/,
        Z = /^(?:#([\w-]+)|(\w+)|\.([\w-]+))$/,
        ee = /[+~]/,
        te = new RegExp(
          "\\\\[\\da-fA-F]{1,6}" + O + "?|\\\\([^\\r\\n\\f])",
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
        oe = ve(
          function (e) {
            return !0 === e.disabled && "fieldset" === e.nodeName.toLowerCase();
          },
          { dir: "parentNode", next: "legend" }
        );
      try {
        q.apply((T = L.call(w.childNodes)), w.childNodes),
          T[w.childNodes.length].nodeType;
      } catch (e) {
        q = {
          apply: T.length
            ? function (e, t) {
                B.apply(e, L.call(t));
              }
            : function (e, t) {
                for (var n = e.length, r = 0; (e[n++] = t[r++]); );
                e.length = n - 1;
              },
        };
      }
      function se(e, t, r, i) {
        var a,
          s,
          c,
          l,
          f,
          h,
          g,
          y = t && t.ownerDocument,
          w = t ? t.nodeType : 9;
        if (
          ((r = r || []),
          "string" != typeof e || !e || (1 !== w && 9 !== w && 11 !== w))
        )
          return r;
        if (!i && (d(t), (t = t || p), m)) {
          if (11 !== w && (f = Z.exec(e)))
            if ((a = f[1])) {
              if (9 === w) {
                if (!(c = t.getElementById(a))) return r;
                if (c.id === a) return r.push(c), r;
              } else if (
                y &&
                (c = y.getElementById(a)) &&
                b(t, c) &&
                c.id === a
              )
                return r.push(c), r;
            } else {
              if (f[2]) return q.apply(r, t.getElementsByTagName(e)), r;
              if (
                (a = f[3]) &&
                n.getElementsByClassName &&
                t.getElementsByClassName
              )
                return q.apply(r, t.getElementsByClassName(a)), r;
            }
          if (
            n.qsa &&
            !P[e + " "] &&
            (!_ || !_.test(e)) &&
            (1 !== w || "object" !== t.nodeName.toLowerCase())
          ) {
            if (((g = e), (y = t), 1 === w && (V.test(e) || z.test(e)))) {
              for (
                ((y = (ee.test(e) && ge(t.parentNode)) || t) === t &&
                  n.scope) ||
                  ((l = t.getAttribute("id"))
                    ? (l = l.replace(re, ie))
                    : t.setAttribute("id", (l = v))),
                  s = (h = o(e)).length;
                s--;

              )
                h[s] = (l ? "#" + l : ":scope") + " " + be(h[s]);
              g = h.join(",");
            }
            try {
              return q.apply(r, y.querySelectorAll(g)), r;
            } catch (t) {
              P(e, !0);
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
        var t = p.createElement("fieldset");
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
      function pe(e) {
        return function (t) {
          return "input" === t.nodeName.toLowerCase() && t.type === e;
        };
      }
      function he(e) {
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
                : t.isDisabled === e || (t.isDisabled !== !e && oe(t) === e)
              : t.disabled === e
            : "label" in t && t.disabled === e;
        };
      }
      function _e(e) {
        return ce(function (t) {
          return (
            (t = +t),
            ce(function (n, r) {
              for (var i, a = e([], n.length, t), o = a.length; o--; )
                n[(i = a[o])] && (n[i] = !(r[i] = n[i]));
            })
          );
        });
      }
      function ge(e) {
        return e && void 0 !== e.getElementsByTagName && e;
      }
      for (t in ((n = se.support = {}),
      (a = se.isXML = function (e) {
        var t = e && e.namespaceURI,
          n = e && (e.ownerDocument || e).documentElement;
        return !G.test(t || (n && n.nodeName) || "HTML");
      }),
      (d = se.setDocument = function (e) {
        var t,
          i,
          o = e ? e.ownerDocument || e : w;
        return o != p && 9 === o.nodeType && o.documentElement
          ? ((h = (p = o).documentElement),
            (m = !a(p)),
            w != p &&
              (i = p.defaultView) &&
              i.top !== i &&
              (i.addEventListener
                ? i.addEventListener("unload", ae, !1)
                : i.attachEvent && i.attachEvent("onunload", ae)),
            (n.scope = le(function (e) {
              return (
                h.appendChild(e).appendChild(p.createElement("div")),
                void 0 !== e.querySelectorAll &&
                  !e.querySelectorAll(":scope fieldset div").length
              );
            })),
            (n.attributes = le(function (e) {
              return (e.className = "i"), !e.getAttribute("className");
            })),
            (n.getElementsByTagName = le(function (e) {
              return (
                e.appendChild(p.createComment("")),
                !e.getElementsByTagName("*").length
              );
            })),
            (n.getElementsByClassName = Q.test(p.getElementsByClassName)),
            (n.getById = le(function (e) {
              return (
                (h.appendChild(e).id = v),
                !p.getElementsByName || !p.getElementsByName(v).length
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
            (g = []),
            (_ = []),
            (n.qsa = Q.test(p.querySelectorAll)) &&
              (le(function (e) {
                var t;
                (h.appendChild(e).innerHTML =
                  "<a id='" +
                  v +
                  "'></a><select id='" +
                  v +
                  "-\r\\' msallowcapture=''><option selected=''></option></select>"),
                  e.querySelectorAll("[msallowcapture^='']").length &&
                    _.push("[*^$]=" + O + "*(?:''|\"\")"),
                  e.querySelectorAll("[selected]").length ||
                    _.push("\\[" + O + "*(?:value|" + N + ")"),
                  e.querySelectorAll("[id~=" + v + "-]").length || _.push("~="),
                  (t = p.createElement("input")).setAttribute("name", ""),
                  e.appendChild(t),
                  e.querySelectorAll("[name='']").length ||
                    _.push("\\[" + O + "*name" + O + "*=" + O + "*(?:''|\"\")"),
                  e.querySelectorAll(":checked").length || _.push(":checked"),
                  e.querySelectorAll("a#" + v + "+*").length ||
                    _.push(".#.+[+~]"),
                  e.querySelectorAll("\\\f"),
                  _.push("[\\r\\n\\f]");
              }),
              le(function (e) {
                e.innerHTML =
                  "<a href='' disabled='disabled'></a><select disabled='disabled'><option/></select>";
                var t = p.createElement("input");
                t.setAttribute("type", "hidden"),
                  e.appendChild(t).setAttribute("name", "D"),
                  e.querySelectorAll("[name=d]").length &&
                    _.push("name" + O + "*[*^$|!~]?="),
                  2 !== e.querySelectorAll(":enabled").length &&
                    _.push(":enabled", ":disabled"),
                  (h.appendChild(e).disabled = !0),
                  2 !== e.querySelectorAll(":disabled").length &&
                    _.push(":enabled", ":disabled"),
                  e.querySelectorAll("*,:x"),
                  _.push(",.*:");
              })),
            (n.matchesSelector = Q.test(
              (y =
                h.matches ||
                h.webkitMatchesSelector ||
                h.mozMatchesSelector ||
                h.oMatchesSelector ||
                h.msMatchesSelector)
            )) &&
              le(function (e) {
                (n.disconnectedMatch = y.call(e, "*")),
                  y.call(e, "[s!='']:x"),
                  g.push("!=", R);
              }),
            (_ = _.length && new RegExp(_.join("|"))),
            (g = g.length && new RegExp(g.join("|"))),
            (t = Q.test(h.compareDocumentPosition)),
            (b =
              t || Q.test(h.contains)
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
            (j = t
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
                      ? e == p || (e.ownerDocument == w && b(w, e))
                        ? -1
                        : t == p || (t.ownerDocument == w && b(w, t))
                        ? 1
                        : l
                        ? D(l, e) - D(l, t)
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
                    o = [e],
                    s = [t];
                  if (!i || !a)
                    return e == p
                      ? -1
                      : t == p
                      ? 1
                      : i
                      ? -1
                      : a
                      ? 1
                      : l
                      ? D(l, e) - D(l, t)
                      : 0;
                  if (i === a) return de(e, t);
                  for (n = e; (n = n.parentNode); ) o.unshift(n);
                  for (n = t; (n = n.parentNode); ) s.unshift(n);
                  for (; o[r] === s[r]; ) r++;
                  return r
                    ? de(o[r], s[r])
                    : o[r] == w
                    ? -1
                    : s[r] == w
                    ? 1
                    : 0;
                }),
            p)
          : p;
      }),
      (se.matches = function (e, t) {
        return se(e, null, null, t);
      }),
      (se.matchesSelector = function (e, t) {
        if (
          (d(e),
          n.matchesSelector &&
            m &&
            !P[t + " "] &&
            (!g || !g.test(t)) &&
            (!_ || !_.test(t)))
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
            P(t, !0);
          }
        return se(t, p, null, [e]).length > 0;
      }),
      (se.contains = function (e, t) {
        return (e.ownerDocument || e) != p && d(e), b(e, t);
      }),
      (se.attr = function (e, t) {
        (e.ownerDocument || e) != p && d(e);
        var i = r.attrHandle[t.toLowerCase()],
          a = i && $.call(r.attrHandle, t.toLowerCase()) ? i(e, t, !m) : void 0;
        return void 0 !== a
          ? a
          : n.attributes || !m
          ? e.getAttribute(t)
          : (a = e.getAttributeNode(t)) && a.specified
          ? a.value
          : null;
      }),
      (se.escape = function (e) {
        return (e + "").replace(re, ie);
      }),
      (se.error = function (e) {
        throw new Error("Syntax error, unrecognized expression: " + e);
      }),
      (se.uniqueSort = function (e) {
        var t,
          r = [],
          i = 0,
          a = 0;
        if (
          ((f = !n.detectDuplicates),
          (l = !n.sortStable && e.slice(0)),
          e.sort(j),
          f)
        ) {
          for (; (t = e[a++]); ) t === e[a] && (i = r.push(a));
          for (; i--; ) e.splice(r[i], 1);
        }
        return (l = null), e;
      }),
      (i = se.getText = function (e) {
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
      ((r = se.selectors = {
        cacheLength: 50,
        createPseudo: ce,
        match: K,
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
                ? (e[3] || se.error(e[0]),
                  (e[4] = +(e[4]
                    ? e[5] + (e[6] || 1)
                    : 2 * ("even" === e[3] || "odd" === e[3]))),
                  (e[5] = +(e[7] + e[8] || "odd" === e[3])))
                : e[3] && se.error(e[0]),
              e
            );
          },
          PSEUDO: function (e) {
            var t,
              n = !e[6] && e[2];
            return K.CHILD.test(e[0])
              ? null
              : (e[3]
                  ? (e[2] = e[4] || e[5] || "")
                  : n &&
                    U.test(n) &&
                    (t = o(n, !0)) &&
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
              ((t = new RegExp("(^|" + O + ")" + e + "(" + O + "|$)")) &&
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
              var i = se.attr(r, e);
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
              o = "last" !== e.slice(-4),
              s = "of-type" === t;
            return 1 === r && 0 === i
              ? function (e) {
                  return !!e.parentNode;
                }
              : function (t, n, u) {
                  var c,
                    l,
                    f,
                    d,
                    p,
                    h,
                    m = a !== o ? "nextSibling" : "previousSibling",
                    _ = t.parentNode,
                    g = s && t.nodeName.toLowerCase(),
                    y = !u && !s,
                    b = !1;
                  if (_) {
                    if (a) {
                      for (; m; ) {
                        for (d = t; (d = d[m]); )
                          if (
                            s
                              ? d.nodeName.toLowerCase() === g
                              : 1 === d.nodeType
                          )
                            return !1;
                        h = m = "only" === e && !h && "nextSibling";
                      }
                      return !0;
                    }
                    if (((h = [o ? _.firstChild : _.lastChild]), o && y)) {
                      for (
                        b =
                          (p =
                            (c =
                              (l =
                                (f = (d = _)[v] || (d[v] = {}))[d.uniqueID] ||
                                (f[d.uniqueID] = {}))[e] || [])[0] === x &&
                            c[1]) && c[2],
                          d = p && _.childNodes[p];
                        (d = (++p && d && d[m]) || (b = p = 0) || h.pop());

                      )
                        if (1 === d.nodeType && ++b && d === t) {
                          l[e] = [x, p, b];
                          break;
                        }
                    } else if (
                      (y &&
                        (b = p =
                          (c =
                            (l =
                              (f = (d = t)[v] || (d[v] = {}))[d.uniqueID] ||
                              (f[d.uniqueID] = {}))[e] || [])[0] === x && c[1]),
                      !1 === b)
                    )
                      for (
                        ;
                        (d = (++p && d && d[m]) || (b = p = 0) || h.pop()) &&
                        ((s
                          ? d.nodeName.toLowerCase() !== g
                          : 1 !== d.nodeType) ||
                          !++b ||
                          (y &&
                            ((l =
                              (f = d[v] || (d[v] = {}))[d.uniqueID] ||
                              (f[d.uniqueID] = {}))[e] = [x, b]),
                          d !== t));

                      );
                    return (b -= i) === r || (b % r == 0 && b / r >= 0);
                  }
                };
          },
          PSEUDO: function (e, t) {
            var n,
              i =
                r.pseudos[e] ||
                r.setFilters[e.toLowerCase()] ||
                se.error("unsupported pseudo: " + e);
            return i[v]
              ? i(t)
              : i.length > 1
              ? ((n = [e, e, "", t]),
                r.setFilters.hasOwnProperty(e.toLowerCase())
                  ? ce(function (e, n) {
                      for (var r, a = i(e, t), o = a.length; o--; )
                        e[(r = D(e, a[o]))] = !(n[r] = a[o]);
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
              r = s(e.replace(F, "$1"));
            return r[v]
              ? ce(function (e, t, n, i) {
                  for (var a, o = r(e, null, i, []), s = e.length; s--; )
                    (a = o[s]) && (e[s] = !(t[s] = a));
                })
              : function (e, i, a) {
                  return (t[0] = e), r(t, null, a, n), (t[0] = null), !n.pop();
                };
          }),
          has: ce(function (e) {
            return function (t) {
              return se(e, t).length > 0;
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
              X.test(e || "") || se.error("unsupported lang: " + e),
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
            return e === h;
          },
          focus: function (e) {
            return (
              e === p.activeElement &&
              (!p.hasFocus || p.hasFocus()) &&
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
            return Y.test(e.nodeName);
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
      }).pseudos.nth = r.pseudos.eq),
      { radio: !0, checkbox: !0, file: !0, password: !0, image: !0 }))
        r.pseudos[t] = pe(t);
      for (t in { submit: !0, reset: !0 }) r.pseudos[t] = he(t);
      function ye() {}
      function be(e) {
        for (var t = 0, n = e.length, r = ""; t < n; t++) r += e[t].value;
        return r;
      }
      function ve(e, t, n) {
        var r = t.dir,
          i = t.next,
          a = i || r,
          o = n && "parentNode" === a,
          s = S++;
        return t.first
          ? function (t, n, i) {
              for (; (t = t[r]); ) if (1 === t.nodeType || o) return e(t, n, i);
              return !1;
            }
          : function (t, n, u) {
              var c,
                l,
                f,
                d = [x, s];
              if (u) {
                for (; (t = t[r]); )
                  if ((1 === t.nodeType || o) && e(t, n, u)) return !0;
              } else
                for (; (t = t[r]); )
                  if (1 === t.nodeType || o)
                    if (
                      ((l =
                        (f = t[v] || (t[v] = {}))[t.uniqueID] ||
                        (f[t.uniqueID] = {})),
                      i && i === t.nodeName.toLowerCase())
                    )
                      t = t[r] || t;
                    else {
                      if ((c = l[a]) && c[0] === x && c[1] === s)
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
        for (var a, o = [], s = 0, u = e.length, c = null != t; s < u; s++)
          (a = e[s]) && ((n && !n(a, r, i)) || (o.push(a), c && t.push(s)));
        return o;
      }
      function Se(e, t, n, r, i, a) {
        return (
          r && !r[v] && (r = Se(r)),
          i && !i[v] && (i = Se(i, a)),
          ce(function (a, o, s, u) {
            var c,
              l,
              f,
              d = [],
              p = [],
              h = o.length,
              m =
                a ||
                (function (e, t, n) {
                  for (var r = 0, i = t.length; r < i; r++) se(e, t[r], n);
                  return n;
                })(t || "*", s.nodeType ? [s] : s, []),
              _ = !e || (!a && t) ? m : xe(m, d, e, s, u),
              g = n ? (i || (a ? e : h || r) ? [] : o) : _;
            if ((n && n(_, g, s, u), r))
              for (c = xe(g, p), r(c, [], s, u), l = c.length; l--; )
                (f = c[l]) && (g[p[l]] = !(_[p[l]] = f));
            if (a) {
              if (i || e) {
                if (i) {
                  for (c = [], l = g.length; l--; )
                    (f = g[l]) && c.push((_[l] = f));
                  i(null, (g = []), c, u);
                }
                for (l = g.length; l--; )
                  (f = g[l]) &&
                    (c = i ? D(a, f) : d[l]) > -1 &&
                    (a[c] = !(o[c] = f));
              }
            } else (g = xe(g === o ? g.splice(h, g.length) : g)), i ? i(null, o, g, u) : q.apply(o, g);
          })
        );
      }
      function Ce(e) {
        for (
          var t,
            n,
            i,
            a = e.length,
            o = r.relative[e[0].type],
            s = o || r.relative[" "],
            u = o ? 1 : 0,
            l = ve(
              function (e) {
                return e === t;
              },
              s,
              !0
            ),
            f = ve(
              function (e) {
                return D(t, e) > -1;
              },
              s,
              !0
            ),
            d = [
              function (e, n, r) {
                var i =
                  (!o && (r || n !== c)) ||
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
                  be(
                    e
                      .slice(0, u - 1)
                      .concat({ value: " " === e[u - 2].type ? "*" : "" })
                  ).replace(F, "$1"),
                n,
                u < i && Ce(e.slice(u, i)),
                i < a && Ce((e = e.slice(i))),
                i < a && be(e)
              );
            }
            d.push(n);
          }
        return we(d);
      }
      return (
        (ye.prototype = r.filters = r.pseudos),
        (r.setFilters = new ye()),
        (o = se.tokenize = function (e, t) {
          var n,
            i,
            a,
            o,
            s,
            u,
            c,
            l = k[e + " "];
          if (l) return t ? 0 : l.slice(0);
          for (s = e, u = [], c = r.preFilter; s; ) {
            for (o in ((n && !(i = W.exec(s))) ||
              (i && (s = s.slice(i[0].length) || s), u.push((a = []))),
            (n = !1),
            (i = z.exec(s)) &&
              ((n = i.shift()),
              a.push({ value: n, type: i[0].replace(F, " ") }),
              (s = s.slice(n.length))),
            r.filter))
              !(i = K[o].exec(s)) ||
                (c[o] && !(i = c[o](i))) ||
                ((n = i.shift()),
                a.push({ value: n, type: o, matches: i }),
                (s = s.slice(n.length)));
            if (!n) break;
          }
          return t ? s.length : s ? se.error(e) : k(e, u).slice(0);
        }),
        (s = se.compile = function (e, t) {
          var n,
            i = [],
            a = [],
            s = E[e + " "];
          if (!s) {
            for (t || (t = o(e)), n = t.length; n--; )
              (s = Ce(t[n]))[v] ? i.push(s) : a.push(s);
            (s = E(
              e,
              (function (e, t) {
                var n = t.length > 0,
                  i = e.length > 0,
                  a = function (a, o, s, u, l) {
                    var f,
                      h,
                      _,
                      g = 0,
                      y = "0",
                      b = a && [],
                      v = [],
                      w = c,
                      S = a || (i && r.find.TAG("*", l)),
                      C = (x += null == w ? 1 : Math.random() || 0.1),
                      k = S.length;
                    for (
                      l && (c = o == p || o || l);
                      y !== k && null != (f = S[y]);
                      y++
                    ) {
                      if (i && f) {
                        for (
                          h = 0, o || f.ownerDocument == p || (d(f), (s = !m));
                          (_ = e[h++]);

                        )
                          if (_(f, o || p, s)) {
                            u.push(f);
                            break;
                          }
                        l && (x = C);
                      }
                      n && ((f = !_ && f) && g--, a && b.push(f));
                    }
                    if (((g += y), n && y !== g)) {
                      for (h = 0; (_ = t[h++]); ) _(b, v, o, s);
                      if (a) {
                        if (g > 0)
                          for (; y--; ) b[y] || v[y] || (v[y] = A.call(u));
                        v = xe(v);
                      }
                      q.apply(u, v),
                        l &&
                          !a &&
                          v.length > 0 &&
                          g + t.length > 1 &&
                          se.uniqueSort(u);
                    }
                    return l && ((x = C), (c = w)), b;
                  };
                return n ? ce(a) : a;
              })(a, i)
            )).selector = e;
          }
          return s;
        }),
        (u = se.select = function (e, t, n, i) {
          var a,
            u,
            c,
            l,
            f,
            d = "function" == typeof e && e,
            p = !i && o((e = d.selector || e));
          if (((n = n || []), 1 === p.length)) {
            if (
              (u = p[0] = p[0].slice(0)).length > 2 &&
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
              a = K.needsContext.test(e) ? 0 : u.length;
              a-- && ((c = u[a]), !r.relative[(l = c.type)]);

            )
              if (
                (f = r.find[l]) &&
                (i = f(
                  c.matches[0].replace(te, ne),
                  (ee.test(u[0].type) && ge(t.parentNode)) || t
                ))
              ) {
                if ((u.splice(a, 1), !(e = i.length && be(u))))
                  return q.apply(n, i), n;
                break;
              }
          }
          return (
            (d || s(e, p))(
              i,
              t,
              !m,
              n,
              !t || (ee.test(e) && ge(t.parentNode)) || t
            ),
            n
          );
        }),
        (n.sortStable = v.split("").sort(j).join("") === v),
        (n.detectDuplicates = !!f),
        d(),
        (n.sortDetached = le(function (e) {
          return 1 & e.compareDocumentPosition(p.createElement("fieldset"));
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
          fe(N, function (e, t, n) {
            var r;
            if (!n)
              return !0 === e[t]
                ? t.toLowerCase()
                : (r = e.getAttributeNode(t)) && r.specified
                ? r.value
                : null;
          }),
        se
      );
    })(e);
  (v.find = x),
    (v.expr = x.selectors),
    (v.expr[":"] = v.expr.pseudos),
    (v.uniqueSort = v.unique = x.uniqueSort),
    (v.text = x.getText),
    (v.isXMLDoc = x.isXML),
    (v.contains = x.contains),
    (v.escapeSelector = x.escape);
  var S = function (e, t, n) {
      for (var r = [], i = void 0 !== n; (e = e[t]) && 9 !== e.nodeType; )
        if (1 === e.nodeType) {
          if (i && v(e).is(n)) break;
          r.push(e);
        }
      return r;
    },
    C = function (e, t) {
      for (var n = []; e; e = e.nextSibling)
        1 === e.nodeType && e !== t && n.push(e);
      return n;
    },
    k = v.expr.match.needsContext;
  function E(e, t) {
    return e.nodeName && e.nodeName.toLowerCase() === t.toLowerCase();
  }
  var P = /^<([a-z][^\/\0>:\x20\t\r\n\f]*)[\x20\t\r\n\f]*\/?>(?:<\/\1>|)$/i;
  function j(e, t, n) {
    return h(t)
      ? v.grep(e, function (e, r) {
          return !!t.call(e, r, e) !== n;
        })
      : t.nodeType
      ? v.grep(e, function (e) {
          return (e === t) !== n;
        })
      : "string" != typeof t
      ? v.grep(e, function (e) {
          return s.call(t, e) > -1 !== n;
        })
      : v.filter(t, e, n);
  }
  (v.filter = function (e, t, n) {
    var r = t[0];
    return (
      n && (e = ":not(" + e + ")"),
      1 === t.length && 1 === r.nodeType
        ? v.find.matchesSelector(r, e)
          ? [r]
          : []
        : v.find.matches(
            e,
            v.grep(t, function (e) {
              return 1 === e.nodeType;
            })
          )
    );
  }),
    v.fn.extend({
      find: function (e) {
        var t,
          n,
          r = this.length,
          i = this;
        if ("string" != typeof e)
          return this.pushStack(
            v(e).filter(function () {
              for (t = 0; t < r; t++) if (v.contains(i[t], this)) return !0;
            })
          );
        for (n = this.pushStack([]), t = 0; t < r; t++) v.find(e, i[t], n);
        return r > 1 ? v.uniqueSort(n) : n;
      },
      filter: function (e) {
        return this.pushStack(j(this, e || [], !1));
      },
      not: function (e) {
        return this.pushStack(j(this, e || [], !0));
      },
      is: function (e) {
        return !!j(this, "string" == typeof e && k.test(e) ? v(e) : e || [], !1)
          .length;
      },
    });
  var $,
    T = /^(?:\s*(<[\w\W]+>)[^>]*|#([\w-]+))$/;
  ((v.fn.init = function (e, t, n) {
    var r, i;
    if (!e) return this;
    if (((n = n || $), "string" == typeof e)) {
      if (
        !(r =
          "<" === e[0] && ">" === e[e.length - 1] && e.length >= 3
            ? [null, e, null]
            : T.exec(e)) ||
        (!r[1] && t)
      )
        return !t || t.jquery ? (t || n).find(e) : this.constructor(t).find(e);
      if (r[1]) {
        if (
          ((t = t instanceof v ? t[0] : t),
          v.merge(
            this,
            v.parseHTML(r[1], t && t.nodeType ? t.ownerDocument || t : _, !0)
          ),
          P.test(r[1]) && v.isPlainObject(t))
        )
          for (r in t) h(this[r]) ? this[r](t[r]) : this.attr(r, t[r]);
        return this;
      }
      return (
        (i = _.getElementById(r[2])) && ((this[0] = i), (this.length = 1)), this
      );
    }
    return e.nodeType
      ? ((this[0] = e), (this.length = 1), this)
      : h(e)
      ? void 0 !== n.ready
        ? n.ready(e)
        : e(v)
      : v.makeArray(e, this);
  }).prototype = v.fn),
    ($ = v(_));
  var A = /^(?:parents|prev(?:Until|All))/,
    B = { children: !0, contents: !0, next: !0, prev: !0 };
  function q(e, t) {
    for (; (e = e[t]) && 1 !== e.nodeType; );
    return e;
  }
  v.fn.extend({
    has: function (e) {
      var t = v(e, this),
        n = t.length;
      return this.filter(function () {
        for (var e = 0; e < n; e++) if (v.contains(this, t[e])) return !0;
      });
    },
    closest: function (e, t) {
      var n,
        r = 0,
        i = this.length,
        a = [],
        o = "string" != typeof e && v(e);
      if (!k.test(e))
        for (; r < i; r++)
          for (n = this[r]; n && n !== t; n = n.parentNode)
            if (
              n.nodeType < 11 &&
              (o
                ? o.index(n) > -1
                : 1 === n.nodeType && v.find.matchesSelector(n, e))
            ) {
              a.push(n);
              break;
            }
      return this.pushStack(a.length > 1 ? v.uniqueSort(a) : a);
    },
    index: function (e) {
      return e
        ? "string" == typeof e
          ? s.call(v(e), this[0])
          : s.call(this, e.jquery ? e[0] : e)
        : this[0] && this[0].parentNode
        ? this.first().prevAll().length
        : -1;
    },
    add: function (e, t) {
      return this.pushStack(v.uniqueSort(v.merge(this.get(), v(e, t))));
    },
    addBack: function (e) {
      return this.add(null == e ? this.prevObject : this.prevObject.filter(e));
    },
  }),
    v.each(
      {
        parent: function (e) {
          var t = e.parentNode;
          return t && 11 !== t.nodeType ? t : null;
        },
        parents: function (e) {
          return S(e, "parentNode");
        },
        parentsUntil: function (e, t, n) {
          return S(e, "parentNode", n);
        },
        next: function (e) {
          return q(e, "nextSibling");
        },
        prev: function (e) {
          return q(e, "previousSibling");
        },
        nextAll: function (e) {
          return S(e, "nextSibling");
        },
        prevAll: function (e) {
          return S(e, "previousSibling");
        },
        nextUntil: function (e, t, n) {
          return S(e, "nextSibling", n);
        },
        prevUntil: function (e, t, n) {
          return S(e, "previousSibling", n);
        },
        siblings: function (e) {
          return C((e.parentNode || {}).firstChild, e);
        },
        children: function (e) {
          return C(e.firstChild);
        },
        contents: function (e) {
          return null != e.contentDocument && r(e.contentDocument)
            ? e.contentDocument
            : (E(e, "template") && (e = e.content || e),
              v.merge([], e.childNodes));
        },
      },
      function (e, t) {
        v.fn[e] = function (n, r) {
          var i = v.map(this, t, n);
          return (
            "Until" !== e.slice(-5) && (r = n),
            r && "string" == typeof r && (i = v.filter(r, i)),
            this.length > 1 &&
              (B[e] || v.uniqueSort(i), A.test(e) && i.reverse()),
            this.pushStack(i)
          );
        };
      }
    );
  var L = /[^\x20\t\r\n\f]+/g;
  function D(e) {
    return e;
  }
  function N(e) {
    throw e;
  }
  function O(e, t, n, r) {
    var i;
    try {
      e && h((i = e.promise))
        ? i.call(e).done(t).fail(n)
        : e && h((i = e.then))
        ? i.call(e, t, n)
        : t.apply(void 0, [e].slice(r));
    } catch (e) {
      n.apply(void 0, [e]);
    }
  }
  (v.Callbacks = function (e) {
    e =
      "string" == typeof e
        ? (function (e) {
            var t = {};
            return (
              v.each(e.match(L) || [], function (e, n) {
                t[n] = !0;
              }),
              t
            );
          })(e)
        : v.extend({}, e);
    var t,
      n,
      r,
      i,
      a = [],
      o = [],
      s = -1,
      u = function () {
        for (i = i || e.once, r = t = !0; o.length; s = -1)
          for (n = o.shift(); ++s < a.length; )
            !1 === a[s].apply(n[0], n[1]) &&
              e.stopOnFalse &&
              ((s = a.length), (n = !1));
        e.memory || (n = !1), (t = !1), i && (a = n ? [] : "");
      },
      c = {
        add: function () {
          return (
            a &&
              (n && !t && ((s = a.length - 1), o.push(n)),
              (function t(n) {
                v.each(n, function (n, r) {
                  h(r)
                    ? (e.unique && c.has(r)) || a.push(r)
                    : r && r.length && "string" !== b(r) && t(r);
                });
              })(arguments),
              n && !t && u()),
            this
          );
        },
        remove: function () {
          return (
            v.each(arguments, function (e, t) {
              for (var n; (n = v.inArray(t, a, n)) > -1; )
                a.splice(n, 1), n <= s && s--;
            }),
            this
          );
        },
        has: function (e) {
          return e ? v.inArray(e, a) > -1 : a.length > 0;
        },
        empty: function () {
          return a && (a = []), this;
        },
        disable: function () {
          return (i = o = []), (a = n = ""), this;
        },
        disabled: function () {
          return !a;
        },
        lock: function () {
          return (i = o = []), n || t || (a = n = ""), this;
        },
        locked: function () {
          return !!i;
        },
        fireWith: function (e, n) {
          return (
            i ||
              ((n = [e, (n = n || []).slice ? n.slice() : n]),
              o.push(n),
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
    v.extend({
      Deferred: function (t) {
        var n = [
            [
              "notify",
              "progress",
              v.Callbacks("memory"),
              v.Callbacks("memory"),
              2,
            ],
            [
              "resolve",
              "done",
              v.Callbacks("once memory"),
              v.Callbacks("once memory"),
              0,
              "resolved",
            ],
            [
              "reject",
              "fail",
              v.Callbacks("once memory"),
              v.Callbacks("once memory"),
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
              return v
                .Deferred(function (t) {
                  v.each(n, function (n, r) {
                    var i = h(e[r[4]]) && e[r[4]];
                    a[r[1]](function () {
                      var e = i && i.apply(this, arguments);
                      e && h(e.promise)
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
              function o(t, n, r, i) {
                return function () {
                  var s = this,
                    u = arguments,
                    c = function () {
                      var e, c;
                      if (!(t < a)) {
                        if ((e = r.apply(s, u)) === n.promise())
                          throw new TypeError("Thenable self-resolution");
                        (c =
                          e &&
                          ("object" == typeof e || "function" == typeof e) &&
                          e.then),
                          h(c)
                            ? i
                              ? c.call(e, o(a, n, D, i), o(a, n, N, i))
                              : (a++,
                                c.call(
                                  e,
                                  o(a, n, D, i),
                                  o(a, n, N, i),
                                  o(a, n, D, n.notifyWith)
                                ))
                            : (r !== D && ((s = void 0), (u = [e])),
                              (i || n.resolveWith)(s, u));
                      }
                    },
                    l = i
                      ? c
                      : function () {
                          try {
                            c();
                          } catch (e) {
                            v.Deferred.exceptionHook &&
                              v.Deferred.exceptionHook(e, l.stackTrace),
                              t + 1 >= a &&
                                (r !== N && ((s = void 0), (u = [e])),
                                n.rejectWith(s, u));
                          }
                        };
                  t
                    ? l()
                    : (v.Deferred.getStackHook &&
                        (l.stackTrace = v.Deferred.getStackHook()),
                      e.setTimeout(l));
                };
              }
              return v
                .Deferred(function (e) {
                  n[0][3].add(o(0, e, h(i) ? i : D, e.notifyWith)),
                    n[1][3].add(o(0, e, h(t) ? t : D)),
                    n[2][3].add(o(0, e, h(r) ? r : N));
                })
                .promise();
            },
            promise: function (e) {
              return null != e ? v.extend(e, i) : i;
            },
          },
          a = {};
        return (
          v.each(n, function (e, t) {
            var o = t[2],
              s = t[5];
            (i[t[1]] = o.add),
              s &&
                o.add(
                  function () {
                    r = s;
                  },
                  n[3 - e][2].disable,
                  n[3 - e][3].disable,
                  n[0][2].lock,
                  n[0][3].lock
                ),
              o.add(t[3].fire),
              (a[t[0]] = function () {
                return (
                  a[t[0] + "With"](this === a ? void 0 : this, arguments), this
                );
              }),
              (a[t[0] + "With"] = o.fireWith);
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
          o = v.Deferred(),
          s = function (e) {
            return function (n) {
              (r[e] = this),
                (a[e] = arguments.length > 1 ? i.call(arguments) : n),
                --t || o.resolveWith(r, a);
            };
          };
        if (
          t <= 1 &&
          (O(e, o.done(s(n)).resolve, o.reject, !t),
          "pending" === o.state() || h(a[n] && a[n].then))
        )
          return o.then();
        for (; n--; ) O(a[n], s(n), o.reject);
        return o.promise();
      },
    });
  var I = /^(Eval|Internal|Range|Reference|Syntax|Type|URI)Error$/;
  (v.Deferred.exceptionHook = function (t, n) {
    e.console &&
      e.console.warn &&
      t &&
      I.test(t.name) &&
      e.console.warn("jQuery.Deferred exception: " + t.message, t.stack, n);
  }),
    (v.readyException = function (t) {
      e.setTimeout(function () {
        throw t;
      });
    });
  var M = v.Deferred();
  function R() {
    _.removeEventListener("DOMContentLoaded", R),
      e.removeEventListener("load", R),
      v.ready();
  }
  (v.fn.ready = function (e) {
    return (
      M.then(e).catch(function (e) {
        v.readyException(e);
      }),
      this
    );
  }),
    v.extend({
      isReady: !1,
      readyWait: 1,
      ready: function (e) {
        (!0 === e ? --v.readyWait : v.isReady) ||
          ((v.isReady = !0),
          (!0 !== e && --v.readyWait > 0) || M.resolveWith(_, [v]));
      },
    }),
    (v.ready.then = M.then),
    "complete" === _.readyState ||
    ("loading" !== _.readyState && !_.documentElement.doScroll)
      ? e.setTimeout(v.ready)
      : (_.addEventListener("DOMContentLoaded", R),
        e.addEventListener("load", R));
  var H = function (e, t, n, r, i, a, o) {
      var s = 0,
        u = e.length,
        c = null == n;
      if ("object" === b(n))
        for (s in ((i = !0), n)) H(e, t, s, n[s], !0, a, o);
      else if (
        void 0 !== r &&
        ((i = !0),
        h(r) || (o = !0),
        c &&
          (o
            ? (t.call(e, r), (t = null))
            : ((c = t),
              (t = function (e, t, n) {
                return c.call(v(e), n);
              }))),
        t)
      )
        for (; s < u; s++) t(e[s], n, o ? r : r.call(e[s], s, t(e[s], n)));
      return i ? e : c ? t.call(e) : u ? t(e[0], n) : a;
    },
    F = /^-ms-/,
    W = /-([a-z])/g;
  function z(e, t) {
    return t.toUpperCase();
  }
  function V(e) {
    return e.replace(F, "ms-").replace(W, z);
  }
  var U = function (e) {
    return 1 === e.nodeType || 9 === e.nodeType || !+e.nodeType;
  };
  function X() {
    this.expando = v.expando + X.uid++;
  }
  (X.uid = 1),
    (X.prototype = {
      cache: function (e) {
        var t = e[this.expando];
        return (
          t ||
            ((t = {}),
            U(e) &&
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
              : t.match(L) || []).length;
            for (; n--; ) delete r[t[n]];
          }
          (void 0 === t || v.isEmptyObject(r)) &&
            (e.nodeType ? (e[this.expando] = void 0) : delete e[this.expando]);
        }
      },
      hasData: function (e) {
        var t = e[this.expando];
        return void 0 !== t && !v.isEmptyObject(t);
      },
    });
  var K = new X(),
    G = new X(),
    Y = /^(?:\{[\w\W]*\}|\[[\w\W]*\])$/,
    J = /[A-Z]/g;
  function Q(e, t, n) {
    var r;
    if (void 0 === n && 1 === e.nodeType)
      if (
        ((r = "data-" + t.replace(J, "-$&").toLowerCase()),
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
                  : Y.test(e)
                  ? JSON.parse(e)
                  : e))
            );
          })(n);
        } catch (e) {}
        G.set(e, t, n);
      } else n = void 0;
    return n;
  }
  v.extend({
    hasData: function (e) {
      return G.hasData(e) || K.hasData(e);
    },
    data: function (e, t, n) {
      return G.access(e, t, n);
    },
    removeData: function (e, t) {
      G.remove(e, t);
    },
    _data: function (e, t, n) {
      return K.access(e, t, n);
    },
    _removeData: function (e, t) {
      K.remove(e, t);
    },
  }),
    v.fn.extend({
      data: function (e, t) {
        var n,
          r,
          i,
          a = this[0],
          o = a && a.attributes;
        if (void 0 === e) {
          if (
            this.length &&
            ((i = G.get(a)), 1 === a.nodeType && !K.get(a, "hasDataAttrs"))
          ) {
            for (n = o.length; n--; )
              o[n] &&
                0 === (r = o[n].name).indexOf("data-") &&
                ((r = V(r.slice(5))), Q(a, r, i[r]));
            K.set(a, "hasDataAttrs", !0);
          }
          return i;
        }
        return "object" == typeof e
          ? this.each(function () {
              G.set(this, e);
            })
          : H(
              this,
              function (t) {
                var n;
                if (a && void 0 === t)
                  return void 0 !== (n = G.get(a, e)) ||
                    void 0 !== (n = Q(a, e))
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
    v.extend({
      queue: function (e, t, n) {
        var r;
        if (e)
          return (
            (t = (t || "fx") + "queue"),
            (r = K.get(e, t)),
            n &&
              (!r || Array.isArray(n)
                ? (r = K.access(e, t, v.makeArray(n)))
                : r.push(n)),
            r || []
          );
      },
      dequeue: function (e, t) {
        t = t || "fx";
        var n = v.queue(e, t),
          r = n.length,
          i = n.shift(),
          a = v._queueHooks(e, t);
        "inprogress" === i && ((i = n.shift()), r--),
          i &&
            ("fx" === t && n.unshift("inprogress"),
            delete a.stop,
            i.call(
              e,
              function () {
                v.dequeue(e, t);
              },
              a
            )),
          !r && a && a.empty.fire();
      },
      _queueHooks: function (e, t) {
        var n = t + "queueHooks";
        return (
          K.get(e, n) ||
          K.access(e, n, {
            empty: v.Callbacks("once memory").add(function () {
              K.remove(e, [t + "queue", n]);
            }),
          })
        );
      },
    }),
    v.fn.extend({
      queue: function (e, t) {
        var n = 2;
        return (
          "string" != typeof e && ((t = e), (e = "fx"), n--),
          arguments.length < n
            ? v.queue(this[0], e)
            : void 0 === t
            ? this
            : this.each(function () {
                var n = v.queue(this, e, t);
                v._queueHooks(this, e),
                  "fx" === e && "inprogress" !== n[0] && v.dequeue(this, e);
              })
        );
      },
      dequeue: function (e) {
        return this.each(function () {
          v.dequeue(this, e);
        });
      },
      clearQueue: function (e) {
        return this.queue(e || "fx", []);
      },
      promise: function (e, t) {
        var n,
          r = 1,
          i = v.Deferred(),
          a = this,
          o = this.length,
          s = function () {
            --r || i.resolveWith(a, [a]);
          };
        for (
          "string" != typeof e && ((t = e), (e = void 0)), e = e || "fx";
          o--;

        )
          (n = K.get(a[o], e + "queueHooks")) &&
            n.empty &&
            (r++, n.empty.add(s));
        return s(), i.promise(t);
      },
    });
  var Z = /[+-]?(?:\d*\.|)\d+(?:[eE][+-]?\d+|)/.source,
    ee = new RegExp("^(?:([+-])=|)(" + Z + ")([a-z%]*)$", "i"),
    te = ["Top", "Right", "Bottom", "Left"],
    ne = _.documentElement,
    re = function (e) {
      return v.contains(e.ownerDocument, e);
    },
    ie = { composed: !0 };
  ne.getRootNode &&
    (re = function (e) {
      return (
        v.contains(e.ownerDocument, e) || e.getRootNode(ie) === e.ownerDocument
      );
    });
  var ae = function (e, t) {
    return (
      "none" === (e = t || e).style.display ||
      ("" === e.style.display && re(e) && "none" === v.css(e, "display"))
    );
  };
  function oe(e, t, n, r) {
    var i,
      a,
      o = 20,
      s = r
        ? function () {
            return r.cur();
          }
        : function () {
            return v.css(e, t, "");
          },
      u = s(),
      c = (n && n[3]) || (v.cssNumber[t] ? "" : "px"),
      l =
        e.nodeType &&
        (v.cssNumber[t] || ("px" !== c && +u)) &&
        ee.exec(v.css(e, t));
    if (l && l[3] !== c) {
      for (u /= 2, c = c || l[3], l = +u || 1; o--; )
        v.style(e, t, l + c),
          (1 - a) * (1 - (a = s() / u || 0.5)) <= 0 && (o = 0),
          (l /= a);
      (l *= 2), v.style(e, t, l + c), (n = n || []);
    }
    return (
      n &&
        ((l = +l || +u || 0),
        (i = n[1] ? l + (n[1] + 1) * n[2] : +n[2]),
        r && ((r.unit = c), (r.start = l), (r.end = i))),
      i
    );
  }
  var se = {};
  function ue(e) {
    var t,
      n = e.ownerDocument,
      r = e.nodeName,
      i = se[r];
    return (
      i ||
      ((t = n.body.appendChild(n.createElement(r))),
      (i = v.css(t, "display")),
      t.parentNode.removeChild(t),
      "none" === i && (i = "block"),
      (se[r] = i),
      i)
    );
  }
  function ce(e, t) {
    for (var n, r, i = [], a = 0, o = e.length; a < o; a++)
      (r = e[a]).style &&
        ((n = r.style.display),
        t
          ? ("none" === n &&
              ((i[a] = K.get(r, "display") || null),
              i[a] || (r.style.display = "")),
            "" === r.style.display && ae(r) && (i[a] = ue(r)))
          : "none" !== n && ((i[a] = "none"), K.set(r, "display", n)));
    for (a = 0; a < o; a++) null != i[a] && (e[a].style.display = i[a]);
    return e;
  }
  v.fn.extend({
    show: function () {
      return ce(this, !0);
    },
    hide: function () {
      return ce(this);
    },
    toggle: function (e) {
      return "boolean" == typeof e
        ? e
          ? this.show()
          : this.hide()
        : this.each(function () {
            ae(this) ? v(this).show() : v(this).hide();
          });
    },
  });
  var le,
    fe,
    de = /^(?:checkbox|radio)$/i,
    pe = /<([a-z][^\/\0>\x20\t\r\n\f]*)/i,
    he = /^$|^module$|\/(?:java|ecma)script/i;
  (le = _.createDocumentFragment().appendChild(_.createElement("div"))),
    (fe = _.createElement("input")).setAttribute("type", "radio"),
    fe.setAttribute("checked", "checked"),
    fe.setAttribute("name", "t"),
    le.appendChild(fe),
    (p.checkClone = le.cloneNode(!0).cloneNode(!0).lastChild.checked),
    (le.innerHTML = "<textarea>x</textarea>"),
    (p.noCloneChecked = !!le.cloneNode(!0).lastChild.defaultValue),
    (le.innerHTML = "<option></option>"),
    (p.option = !!le.lastChild);
  var me = {
    thead: [1, "<table>", "</table>"],
    col: [2, "<table><colgroup>", "</colgroup></table>"],
    tr: [2, "<table><tbody>", "</tbody></table>"],
    td: [3, "<table><tbody><tr>", "</tr></tbody></table>"],
    _default: [0, "", ""],
  };
  function _e(e, t) {
    var n;
    return (
      (n =
        void 0 !== e.getElementsByTagName
          ? e.getElementsByTagName(t || "*")
          : void 0 !== e.querySelectorAll
          ? e.querySelectorAll(t || "*")
          : []),
      void 0 === t || (t && E(e, t)) ? v.merge([e], n) : n
    );
  }
  function ge(e, t) {
    for (var n = 0, r = e.length; n < r; n++)
      K.set(e[n], "globalEval", !t || K.get(t[n], "globalEval"));
  }
  (me.tbody = me.tfoot = me.colgroup = me.caption = me.thead),
    (me.th = me.td),
    p.option ||
      (me.optgroup = me.option = [
        1,
        "<select multiple='multiple'>",
        "</select>",
      ]);
  var ye = /<|&#?\w+;/;
  function be(e, t, n, r, i) {
    for (
      var a,
        o,
        s,
        u,
        c,
        l,
        f = t.createDocumentFragment(),
        d = [],
        p = 0,
        h = e.length;
      p < h;
      p++
    )
      if ((a = e[p]) || 0 === a)
        if ("object" === b(a)) v.merge(d, a.nodeType ? [a] : a);
        else if (ye.test(a)) {
          for (
            o = o || f.appendChild(t.createElement("div")),
              s = (pe.exec(a) || ["", ""])[1].toLowerCase(),
              u = me[s] || me._default,
              o.innerHTML = u[1] + v.htmlPrefilter(a) + u[2],
              l = u[0];
            l--;

          )
            o = o.lastChild;
          v.merge(d, o.childNodes), ((o = f.firstChild).textContent = "");
        } else d.push(t.createTextNode(a));
    for (f.textContent = "", p = 0; (a = d[p++]); )
      if (r && v.inArray(a, r) > -1) i && i.push(a);
      else if (
        ((c = re(a)), (o = _e(f.appendChild(a), "script")), c && ge(o), n)
      )
        for (l = 0; (a = o[l++]); ) he.test(a.type || "") && n.push(a);
    return f;
  }
  var ve = /^([^.]*)(?:\.(.+)|)/;
  function we() {
    return !0;
  }
  function xe() {
    return !1;
  }
  function Se(e, t) {
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
  function Ce(e, t, n, r, i, a) {
    var o, s;
    if ("object" == typeof t) {
      for (s in ("string" != typeof n && ((r = r || n), (n = void 0)), t))
        Ce(e, s, n, r, t[s], a);
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
      1 === a &&
        ((o = i),
        ((i = function (e) {
          return v().off(e), o.apply(this, arguments);
        }).guid = o.guid || (o.guid = v.guid++))),
      e.each(function () {
        v.event.add(this, t, i, r, n);
      })
    );
  }
  function ke(e, t, n) {
    n
      ? (K.set(e, t, !1),
        v.event.add(e, t, {
          namespace: !1,
          handler: function (e) {
            var r,
              a,
              o = K.get(this, t);
            if (1 & e.isTrigger && this[t]) {
              if (o.length)
                (v.event.special[t] || {}).delegateType && e.stopPropagation();
              else if (
                ((o = i.call(arguments)),
                K.set(this, t, o),
                (r = n(this, t)),
                this[t](),
                o !== (a = K.get(this, t)) || r ? K.set(this, t, !1) : (a = {}),
                o !== a)
              )
                return (
                  e.stopImmediatePropagation(), e.preventDefault(), a && a.value
                );
            } else
              o.length &&
                (K.set(this, t, {
                  value: v.event.trigger(
                    v.extend(o[0], v.Event.prototype),
                    o.slice(1),
                    this
                  ),
                }),
                e.stopImmediatePropagation());
          },
        }))
      : void 0 === K.get(e, t) && v.event.add(e, t, we);
  }
  (v.event = {
    global: {},
    add: function (e, t, n, r, i) {
      var a,
        o,
        s,
        u,
        c,
        l,
        f,
        d,
        p,
        h,
        m,
        _ = K.get(e);
      if (U(e))
        for (
          n.handler && ((n = (a = n).handler), (i = a.selector)),
            i && v.find.matchesSelector(ne, i),
            n.guid || (n.guid = v.guid++),
            (u = _.events) || (u = _.events = Object.create(null)),
            (o = _.handle) ||
              (o = _.handle = function (t) {
                return void 0 !== v && v.event.triggered !== t.type
                  ? v.event.dispatch.apply(e, arguments)
                  : void 0;
              }),
            c = (t = (t || "").match(L) || [""]).length;
          c--;

        )
          (p = m = (s = ve.exec(t[c]) || [])[1]),
            (h = (s[2] || "").split(".").sort()),
            p &&
              ((f = v.event.special[p] || {}),
              (p = (i ? f.delegateType : f.bindType) || p),
              (f = v.event.special[p] || {}),
              (l = v.extend(
                {
                  type: p,
                  origType: m,
                  data: r,
                  handler: n,
                  guid: n.guid,
                  selector: i,
                  needsContext: i && v.expr.match.needsContext.test(i),
                  namespace: h.join("."),
                },
                a
              )),
              (d = u[p]) ||
                (((d = u[p] = []).delegateCount = 0),
                (f.setup && !1 !== f.setup.call(e, r, h, o)) ||
                  (e.addEventListener && e.addEventListener(p, o))),
              f.add &&
                (f.add.call(e, l), l.handler.guid || (l.handler.guid = n.guid)),
              i ? d.splice(d.delegateCount++, 0, l) : d.push(l),
              (v.event.global[p] = !0));
    },
    remove: function (e, t, n, r, i) {
      var a,
        o,
        s,
        u,
        c,
        l,
        f,
        d,
        p,
        h,
        m,
        _ = K.hasData(e) && K.get(e);
      if (_ && (u = _.events)) {
        for (c = (t = (t || "").match(L) || [""]).length; c--; )
          if (
            ((p = m = (s = ve.exec(t[c]) || [])[1]),
            (h = (s[2] || "").split(".").sort()),
            p)
          ) {
            for (
              f = v.event.special[p] || {},
                d = u[(p = (r ? f.delegateType : f.bindType) || p)] || [],
                s =
                  s[2] &&
                  new RegExp("(^|\\.)" + h.join("\\.(?:.*\\.|)") + "(\\.|$)"),
                o = a = d.length;
              a--;

            )
              (l = d[a]),
                (!i && m !== l.origType) ||
                  (n && n.guid !== l.guid) ||
                  (s && !s.test(l.namespace)) ||
                  (r && r !== l.selector && ("**" !== r || !l.selector)) ||
                  (d.splice(a, 1),
                  l.selector && d.delegateCount--,
                  f.remove && f.remove.call(e, l));
            o &&
              !d.length &&
              ((f.teardown && !1 !== f.teardown.call(e, h, _.handle)) ||
                v.removeEvent(e, p, _.handle),
              delete u[p]);
          } else for (p in u) v.event.remove(e, p + t[c], n, r, !0);
        v.isEmptyObject(u) && K.remove(e, "handle events");
      }
    },
    dispatch: function (e) {
      var t,
        n,
        r,
        i,
        a,
        o,
        s = new Array(arguments.length),
        u = v.event.fix(e),
        c = (K.get(this, "events") || Object.create(null))[u.type] || [],
        l = v.event.special[u.type] || {};
      for (s[0] = u, t = 1; t < arguments.length; t++) s[t] = arguments[t];
      if (
        ((u.delegateTarget = this),
        !l.preDispatch || !1 !== l.preDispatch.call(this, u))
      ) {
        for (
          o = v.event.handlers.call(this, u, c), t = 0;
          (i = o[t++]) && !u.isPropagationStopped();

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
                  (v.event.special[a.origType] || {}).handle || a.handler
                ).apply(i.elem, s)) &&
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
        o,
        s = [],
        u = t.delegateCount,
        c = e.target;
      if (u && c.nodeType && !("click" === e.type && e.button >= 1))
        for (; c !== this; c = c.parentNode || this)
          if (1 === c.nodeType && ("click" !== e.type || !0 !== c.disabled)) {
            for (a = [], o = {}, n = 0; n < u; n++)
              void 0 === o[(i = (r = t[n]).selector + " ")] &&
                (o[i] = r.needsContext
                  ? v(i, this).index(c) > -1
                  : v.find(i, this, null, [c]).length),
                o[i] && a.push(r);
            a.length && s.push({ elem: c, handlers: a });
          }
      return (
        (c = this), u < t.length && s.push({ elem: c, handlers: t.slice(u) }), s
      );
    },
    addProp: function (e, t) {
      Object.defineProperty(v.Event.prototype, e, {
        enumerable: !0,
        configurable: !0,
        get: h(t)
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
      return e[v.expando] ? e : new v.Event(e);
    },
    special: {
      load: { noBubble: !0 },
      click: {
        setup: function (e) {
          var t = this || e;
          return (
            de.test(t.type) && t.click && E(t, "input") && ke(t, "click", we),
            !1
          );
        },
        trigger: function (e) {
          var t = this || e;
          return (
            de.test(t.type) && t.click && E(t, "input") && ke(t, "click"), !0
          );
        },
        _default: function (e) {
          var t = e.target;
          return (
            (de.test(t.type) &&
              t.click &&
              E(t, "input") &&
              K.get(t, "click")) ||
            E(t, "a")
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
    (v.removeEvent = function (e, t, n) {
      e.removeEventListener && e.removeEventListener(t, n);
    }),
    (v.Event = function (e, t) {
      if (!(this instanceof v.Event)) return new v.Event(e, t);
      e && e.type
        ? ((this.originalEvent = e),
          (this.type = e.type),
          (this.isDefaultPrevented =
            e.defaultPrevented ||
            (void 0 === e.defaultPrevented && !1 === e.returnValue)
              ? we
              : xe),
          (this.target =
            e.target && 3 === e.target.nodeType
              ? e.target.parentNode
              : e.target),
          (this.currentTarget = e.currentTarget),
          (this.relatedTarget = e.relatedTarget))
        : (this.type = e),
        t && v.extend(this, t),
        (this.timeStamp = (e && e.timeStamp) || Date.now()),
        (this[v.expando] = !0);
    }),
    (v.Event.prototype = {
      constructor: v.Event,
      isDefaultPrevented: xe,
      isPropagationStopped: xe,
      isImmediatePropagationStopped: xe,
      isSimulated: !1,
      preventDefault: function () {
        var e = this.originalEvent;
        (this.isDefaultPrevented = we),
          e && !this.isSimulated && e.preventDefault();
      },
      stopPropagation: function () {
        var e = this.originalEvent;
        (this.isPropagationStopped = we),
          e && !this.isSimulated && e.stopPropagation();
      },
      stopImmediatePropagation: function () {
        var e = this.originalEvent;
        (this.isImmediatePropagationStopped = we),
          e && !this.isSimulated && e.stopImmediatePropagation(),
          this.stopPropagation();
      },
    }),
    v.each(
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
      v.event.addProp
    ),
    v.each({ focus: "focusin", blur: "focusout" }, function (e, t) {
      v.event.special[e] = {
        setup: function () {
          return ke(this, e, Se), !1;
        },
        trigger: function () {
          return ke(this, e), !0;
        },
        _default: function () {
          return !0;
        },
        delegateType: t,
      };
    }),
    v.each(
      {
        mouseenter: "mouseover",
        mouseleave: "mouseout",
        pointerenter: "pointerover",
        pointerleave: "pointerout",
      },
      function (e, t) {
        v.event.special[e] = {
          delegateType: t,
          bindType: t,
          handle: function (e) {
            var n,
              r = this,
              i = e.relatedTarget,
              a = e.handleObj;
            return (
              (i && (i === r || v.contains(r, i))) ||
                ((e.type = a.origType),
                (n = a.handler.apply(this, arguments)),
                (e.type = t)),
              n
            );
          },
        };
      }
    ),
    v.fn.extend({
      on: function (e, t, n, r) {
        return Ce(this, e, t, n, r);
      },
      one: function (e, t, n, r) {
        return Ce(this, e, t, n, r, 1);
      },
      off: function (e, t, n) {
        var r, i;
        if (e && e.preventDefault && e.handleObj)
          return (
            (r = e.handleObj),
            v(e.delegateTarget).off(
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
            v.event.remove(this, e, n, t);
          })
        );
      },
    });
  var Ee = /<script|<style|<link/i,
    Pe = /checked\s*(?:[^=]|=\s*.checked.)/i,
    je = /^\s*<!(?:\[CDATA\[|--)|(?:\]\]|--)>\s*$/g;
  function $e(e, t) {
    return (
      (E(e, "table") &&
        E(11 !== t.nodeType ? t : t.firstChild, "tr") &&
        v(e).children("tbody")[0]) ||
      e
    );
  }
  function Te(e) {
    return (e.type = (null !== e.getAttribute("type")) + "/" + e.type), e;
  }
  function Ae(e) {
    return (
      "true/" === (e.type || "").slice(0, 5)
        ? (e.type = e.type.slice(5))
        : e.removeAttribute("type"),
      e
    );
  }
  function Be(e, t) {
    var n, r, i, a, o, s;
    if (1 === t.nodeType) {
      if (K.hasData(e) && (s = K.get(e).events))
        for (i in (K.remove(t, "handle events"), s))
          for (n = 0, r = s[i].length; n < r; n++) v.event.add(t, i, s[i][n]);
      G.hasData(e) && ((a = G.access(e)), (o = v.extend({}, a)), G.set(t, o));
    }
  }
  function qe(e, t) {
    var n = t.nodeName.toLowerCase();
    "input" === n && de.test(e.type)
      ? (t.checked = e.checked)
      : ("input" !== n && "textarea" !== n) ||
        (t.defaultValue = e.defaultValue);
  }
  function Le(e, t, n, r) {
    t = a(t);
    var i,
      o,
      s,
      u,
      c,
      l,
      f = 0,
      d = e.length,
      m = d - 1,
      _ = t[0],
      g = h(_);
    if (g || (d > 1 && "string" == typeof _ && !p.checkClone && Pe.test(_)))
      return e.each(function (i) {
        var a = e.eq(i);
        g && (t[0] = _.call(this, i, a.html())), Le(a, t, n, r);
      });
    if (
      d &&
      ((o = (i = be(t, e[0].ownerDocument, !1, e, r)).firstChild),
      1 === i.childNodes.length && (i = o),
      o || r)
    ) {
      for (u = (s = v.map(_e(i, "script"), Te)).length; f < d; f++)
        (c = i),
          f !== m &&
            ((c = v.clone(c, !0, !0)), u && v.merge(s, _e(c, "script"))),
          n.call(e[f], c, f);
      if (u)
        for (l = s[s.length - 1].ownerDocument, v.map(s, Ae), f = 0; f < u; f++)
          (c = s[f]),
            he.test(c.type || "") &&
              !K.access(c, "globalEval") &&
              v.contains(l, c) &&
              (c.src && "module" !== (c.type || "").toLowerCase()
                ? v._evalUrl &&
                  !c.noModule &&
                  v._evalUrl(
                    c.src,
                    { nonce: c.nonce || c.getAttribute("nonce") },
                    l
                  )
                : y(c.textContent.replace(je, ""), c, l));
    }
    return e;
  }
  function De(e, t, n) {
    for (var r, i = t ? v.filter(t, e) : e, a = 0; null != (r = i[a]); a++)
      n || 1 !== r.nodeType || v.cleanData(_e(r)),
        r.parentNode &&
          (n && re(r) && ge(_e(r, "script")), r.parentNode.removeChild(r));
    return e;
  }
  v.extend({
    htmlPrefilter: function (e) {
      return e;
    },
    clone: function (e, t, n) {
      var r,
        i,
        a,
        o,
        s = e.cloneNode(!0),
        u = re(e);
      if (
        !(
          p.noCloneChecked ||
          (1 !== e.nodeType && 11 !== e.nodeType) ||
          v.isXMLDoc(e)
        )
      )
        for (o = _e(s), r = 0, i = (a = _e(e)).length; r < i; r++)
          qe(a[r], o[r]);
      if (t)
        if (n)
          for (a = a || _e(e), o = o || _e(s), r = 0, i = a.length; r < i; r++)
            Be(a[r], o[r]);
        else Be(e, s);
      return (
        (o = _e(s, "script")).length > 0 && ge(o, !u && _e(e, "script")), s
      );
    },
    cleanData: function (e) {
      for (var t, n, r, i = v.event.special, a = 0; void 0 !== (n = e[a]); a++)
        if (U(n)) {
          if ((t = n[K.expando])) {
            if (t.events)
              for (r in t.events)
                i[r] ? v.event.remove(n, r) : v.removeEvent(n, r, t.handle);
            n[K.expando] = void 0;
          }
          n[G.expando] && (n[G.expando] = void 0);
        }
    },
  }),
    v.fn.extend({
      detach: function (e) {
        return De(this, e, !0);
      },
      remove: function (e) {
        return De(this, e);
      },
      text: function (e) {
        return H(
          this,
          function (e) {
            return void 0 === e
              ? v.text(this)
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
        return Le(this, arguments, function (e) {
          (1 !== this.nodeType &&
            11 !== this.nodeType &&
            9 !== this.nodeType) ||
            $e(this, e).appendChild(e);
        });
      },
      prepend: function () {
        return Le(this, arguments, function (e) {
          if (
            1 === this.nodeType ||
            11 === this.nodeType ||
            9 === this.nodeType
          ) {
            var t = $e(this, e);
            t.insertBefore(e, t.firstChild);
          }
        });
      },
      before: function () {
        return Le(this, arguments, function (e) {
          this.parentNode && this.parentNode.insertBefore(e, this);
        });
      },
      after: function () {
        return Le(this, arguments, function (e) {
          this.parentNode && this.parentNode.insertBefore(e, this.nextSibling);
        });
      },
      empty: function () {
        for (var e, t = 0; null != (e = this[t]); t++)
          1 === e.nodeType && (v.cleanData(_e(e, !1)), (e.textContent = ""));
        return this;
      },
      clone: function (e, t) {
        return (
          (e = null != e && e),
          (t = null == t ? e : t),
          this.map(function () {
            return v.clone(this, e, t);
          })
        );
      },
      html: function (e) {
        return H(
          this,
          function (e) {
            var t = this[0] || {},
              n = 0,
              r = this.length;
            if (void 0 === e && 1 === t.nodeType) return t.innerHTML;
            if (
              "string" == typeof e &&
              !Ee.test(e) &&
              !me[(pe.exec(e) || ["", ""])[1].toLowerCase()]
            ) {
              e = v.htmlPrefilter(e);
              try {
                for (; n < r; n++)
                  1 === (t = this[n] || {}).nodeType &&
                    (v.cleanData(_e(t, !1)), (t.innerHTML = e));
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
        return Le(
          this,
          arguments,
          function (t) {
            var n = this.parentNode;
            v.inArray(this, e) < 0 &&
              (v.cleanData(_e(this)), n && n.replaceChild(t, this));
          },
          e
        );
      },
    }),
    v.each(
      {
        appendTo: "append",
        prependTo: "prepend",
        insertBefore: "before",
        insertAfter: "after",
        replaceAll: "replaceWith",
      },
      function (e, t) {
        v.fn[e] = function (e) {
          for (var n, r = [], i = v(e), a = i.length - 1, s = 0; s <= a; s++)
            (n = s === a ? this : this.clone(!0)),
              v(i[s])[t](n),
              o.apply(r, n.get());
          return this.pushStack(r);
        };
      }
    );
  var Ne = new RegExp("^(" + Z + ")(?!px)[a-z%]+$", "i"),
    Oe = function (t) {
      var n = t.ownerDocument.defaultView;
      return (n && n.opener) || (n = e), n.getComputedStyle(t);
    },
    Ie = function (e, t, n) {
      var r,
        i,
        a = {};
      for (i in t) (a[i] = e.style[i]), (e.style[i] = t[i]);
      for (i in ((r = n.call(e)), t)) e.style[i] = a[i];
      return r;
    },
    Me = new RegExp(te.join("|"), "i");
  function Re(e, t, n) {
    var r,
      i,
      a,
      o,
      s = e.style;
    return (
      (n = n || Oe(e)) &&
        ("" !== (o = n.getPropertyValue(t) || n[t]) ||
          re(e) ||
          (o = v.style(e, t)),
        !p.pixelBoxStyles() &&
          Ne.test(o) &&
          Me.test(t) &&
          ((r = s.width),
          (i = s.minWidth),
          (a = s.maxWidth),
          (s.minWidth = s.maxWidth = s.width = o),
          (o = n.width),
          (s.width = r),
          (s.minWidth = i),
          (s.maxWidth = a))),
      void 0 !== o ? o + "" : o
    );
  }
  function He(e, t) {
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
          ne.appendChild(c).appendChild(l);
        var t = e.getComputedStyle(l);
        (r = "1%" !== t.top),
          (u = 12 === n(t.marginLeft)),
          (l.style.right = "60%"),
          (o = 36 === n(t.right)),
          (i = 36 === n(t.width)),
          (l.style.position = "absolute"),
          (a = 12 === n(l.offsetWidth / 3)),
          ne.removeChild(c),
          (l = null);
      }
    }
    function n(e) {
      return Math.round(parseFloat(e));
    }
    var r,
      i,
      a,
      o,
      s,
      u,
      c = _.createElement("div"),
      l = _.createElement("div");
    l.style &&
      ((l.style.backgroundClip = "content-box"),
      (l.cloneNode(!0).style.backgroundClip = ""),
      (p.clearCloneStyle = "content-box" === l.style.backgroundClip),
      v.extend(p, {
        boxSizingReliable: function () {
          return t(), i;
        },
        pixelBoxStyles: function () {
          return t(), o;
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
            null == s &&
              ((t = _.createElement("table")),
              (n = _.createElement("tr")),
              (r = _.createElement("div")),
              (t.style.cssText =
                "position:absolute;left:-11111px;border-collapse:separate"),
              (n.style.cssText = "border:1px solid"),
              (n.style.height = "1px"),
              (r.style.height = "9px"),
              (r.style.display = "block"),
              ne.appendChild(t).appendChild(n).appendChild(r),
              (i = e.getComputedStyle(n)),
              (s =
                parseInt(i.height, 10) +
                  parseInt(i.borderTopWidth, 10) +
                  parseInt(i.borderBottomWidth, 10) ===
                n.offsetHeight),
              ne.removeChild(t)),
            s
          );
        },
      }));
  })();
  var Fe = ["Webkit", "Moz", "ms"],
    We = _.createElement("div").style,
    ze = {};
  function Ve(e) {
    var t = v.cssProps[e] || ze[e];
    return (
      t ||
      (e in We
        ? e
        : (ze[e] =
            (function (e) {
              for (
                var t = e[0].toUpperCase() + e.slice(1), n = Fe.length;
                n--;

              )
                if ((e = Fe[n] + t) in We) return e;
            })(e) || e))
    );
  }
  var Ue = /^(none|table(?!-c[ea]).+)/,
    Xe = /^--/,
    Ke = { position: "absolute", visibility: "hidden", display: "block" },
    Ge = { letterSpacing: "0", fontWeight: "400" };
  function Ye(e, t, n) {
    var r = ee.exec(t);
    return r ? Math.max(0, r[2] - (n || 0)) + (r[3] || "px") : t;
  }
  function Je(e, t, n, r, i, a) {
    var o = "width" === t ? 1 : 0,
      s = 0,
      u = 0;
    if (n === (r ? "border" : "content")) return 0;
    for (; o < 4; o += 2)
      "margin" === n && (u += v.css(e, n + te[o], !0, i)),
        r
          ? ("content" === n && (u -= v.css(e, "padding" + te[o], !0, i)),
            "margin" !== n &&
              (u -= v.css(e, "border" + te[o] + "Width", !0, i)))
          : ((u += v.css(e, "padding" + te[o], !0, i)),
            "padding" !== n
              ? (u += v.css(e, "border" + te[o] + "Width", !0, i))
              : (s += v.css(e, "border" + te[o] + "Width", !0, i)));
    return (
      !r &&
        a >= 0 &&
        (u +=
          Math.max(
            0,
            Math.ceil(
              e["offset" + t[0].toUpperCase() + t.slice(1)] - a - u - s - 0.5
            )
          ) || 0),
      u
    );
  }
  function Qe(e, t, n) {
    var r = Oe(e),
      i =
        (!p.boxSizingReliable() || n) &&
        "border-box" === v.css(e, "boxSizing", !1, r),
      a = i,
      o = Re(e, t, r),
      s = "offset" + t[0].toUpperCase() + t.slice(1);
    if (Ne.test(o)) {
      if (!n) return o;
      o = "auto";
    }
    return (
      ((!p.boxSizingReliable() && i) ||
        (!p.reliableTrDimensions() && E(e, "tr")) ||
        "auto" === o ||
        (!parseFloat(o) && "inline" === v.css(e, "display", !1, r))) &&
        e.getClientRects().length &&
        ((i = "border-box" === v.css(e, "boxSizing", !1, r)),
        (a = s in e) && (o = e[s])),
      (o = parseFloat(o) || 0) +
        Je(e, t, n || (i ? "border" : "content"), a, r, o) +
        "px"
    );
  }
  function Ze(e, t, n, r, i) {
    return new Ze.prototype.init(e, t, n, r, i);
  }
  v.extend({
    cssHooks: {
      opacity: {
        get: function (e, t) {
          if (t) {
            var n = Re(e, "opacity");
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
          o,
          s = V(t),
          u = Xe.test(t),
          c = e.style;
        if (
          (u || (t = Ve(s)), (o = v.cssHooks[t] || v.cssHooks[s]), void 0 === n)
        )
          return o && "get" in o && void 0 !== (i = o.get(e, !1, r)) ? i : c[t];
        "string" === (a = typeof n) &&
          (i = ee.exec(n)) &&
          i[1] &&
          ((n = oe(e, t, i)), (a = "number")),
          null != n &&
            n == n &&
            ("number" !== a ||
              u ||
              (n += (i && i[3]) || (v.cssNumber[s] ? "" : "px")),
            p.clearCloneStyle ||
              "" !== n ||
              0 !== t.indexOf("background") ||
              (c[t] = "inherit"),
            (o && "set" in o && void 0 === (n = o.set(e, n, r))) ||
              (u ? c.setProperty(t, n) : (c[t] = n)));
      }
    },
    css: function (e, t, n, r) {
      var i,
        a,
        o,
        s = V(t);
      return (
        Xe.test(t) || (t = Ve(s)),
        (o = v.cssHooks[t] || v.cssHooks[s]) &&
          "get" in o &&
          (i = o.get(e, !0, n)),
        void 0 === i && (i = Re(e, t, r)),
        "normal" === i && t in Ge && (i = Ge[t]),
        "" === n || n
          ? ((a = parseFloat(i)), !0 === n || isFinite(a) ? a || 0 : i)
          : i
      );
    },
  }),
    v.each(["height", "width"], function (e, t) {
      v.cssHooks[t] = {
        get: function (e, n, r) {
          if (n)
            return !Ue.test(v.css(e, "display")) ||
              (e.getClientRects().length && e.getBoundingClientRect().width)
              ? Qe(e, t, r)
              : Ie(e, Ke, function () {
                  return Qe(e, t, r);
                });
        },
        set: function (e, n, r) {
          var i,
            a = Oe(e),
            o = !p.scrollboxSize() && "absolute" === a.position,
            s = (o || r) && "border-box" === v.css(e, "boxSizing", !1, a),
            u = r ? Je(e, t, r, s, a) : 0;
          return (
            s &&
              o &&
              (u -= Math.ceil(
                e["offset" + t[0].toUpperCase() + t.slice(1)] -
                  parseFloat(a[t]) -
                  Je(e, t, "border", !1, a) -
                  0.5
              )),
            u &&
              (i = ee.exec(n)) &&
              "px" !== (i[3] || "px") &&
              ((e.style[t] = n), (n = v.css(e, t))),
            Ye(0, n, u)
          );
        },
      };
    }),
    (v.cssHooks.marginLeft = He(p.reliableMarginLeft, function (e, t) {
      if (t)
        return (
          (parseFloat(Re(e, "marginLeft")) ||
            e.getBoundingClientRect().left -
              Ie(e, { marginLeft: 0 }, function () {
                return e.getBoundingClientRect().left;
              })) + "px"
        );
    })),
    v.each({ margin: "", padding: "", border: "Width" }, function (e, t) {
      (v.cssHooks[e + t] = {
        expand: function (n) {
          for (
            var r = 0, i = {}, a = "string" == typeof n ? n.split(" ") : [n];
            r < 4;
            r++
          )
            i[e + te[r] + t] = a[r] || a[r - 2] || a[0];
          return i;
        },
      }),
        "margin" !== e && (v.cssHooks[e + t].set = Ye);
    }),
    v.fn.extend({
      css: function (e, t) {
        return H(
          this,
          function (e, t, n) {
            var r,
              i,
              a = {},
              o = 0;
            if (Array.isArray(t)) {
              for (r = Oe(e), i = t.length; o < i; o++)
                a[t[o]] = v.css(e, t[o], !1, r);
              return a;
            }
            return void 0 !== n ? v.style(e, t, n) : v.css(e, t);
          },
          e,
          t,
          arguments.length > 1
        );
      },
    }),
    (v.Tween = Ze),
    (Ze.prototype = {
      constructor: Ze,
      init: function (e, t, n, r, i, a) {
        (this.elem = e),
          (this.prop = n),
          (this.easing = i || v.easing._default),
          (this.options = t),
          (this.start = this.now = this.cur()),
          (this.end = r),
          (this.unit = a || (v.cssNumber[n] ? "" : "px"));
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
            ? (this.pos = t = v.easing[this.easing](
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
            : (t = v.css(e.elem, e.prop, "")) && "auto" !== t
            ? t
            : 0;
        },
        set: function (e) {
          v.fx.step[e.prop]
            ? v.fx.step[e.prop](e)
            : 1 !== e.elem.nodeType ||
              (!v.cssHooks[e.prop] && null == e.elem.style[Ve(e.prop)])
            ? (e.elem[e.prop] = e.now)
            : v.style(e.elem, e.prop, e.now + e.unit);
        },
      },
    }),
    (Ze.propHooks.scrollTop = Ze.propHooks.scrollLeft = {
      set: function (e) {
        e.elem.nodeType && e.elem.parentNode && (e.elem[e.prop] = e.now);
      },
    }),
    (v.easing = {
      linear: function (e) {
        return e;
      },
      swing: function (e) {
        return 0.5 - Math.cos(e * Math.PI) / 2;
      },
      _default: "swing",
    }),
    (v.fx = Ze.prototype.init),
    (v.fx.step = {});
  var et,
    tt,
    nt = /^(?:toggle|show|hide)$/,
    rt = /queueHooks$/;
  function it() {
    tt &&
      (!1 === _.hidden && e.requestAnimationFrame
        ? e.requestAnimationFrame(it)
        : e.setTimeout(it, v.fx.interval),
      v.fx.tick());
  }
  function at() {
    return (
      e.setTimeout(function () {
        et = void 0;
      }),
      (et = Date.now())
    );
  }
  function ot(e, t) {
    var n,
      r = 0,
      i = { height: e };
    for (t = t ? 1 : 0; r < 4; r += 2 - t)
      i["margin" + (n = te[r])] = i["padding" + n] = e;
    return t && (i.opacity = i.width = e), i;
  }
  function st(e, t, n) {
    for (
      var r,
        i = (ut.tweeners[t] || []).concat(ut.tweeners["*"]),
        a = 0,
        o = i.length;
      a < o;
      a++
    )
      if ((r = i[a].call(n, t, e))) return r;
  }
  function ut(e, t, n) {
    var r,
      i,
      a = 0,
      o = ut.prefilters.length,
      s = v.Deferred().always(function () {
        delete u.elem;
      }),
      u = function () {
        if (i) return !1;
        for (
          var t = et || at(),
            n = Math.max(0, c.startTime + c.duration - t),
            r = 1 - (n / c.duration || 0),
            a = 0,
            o = c.tweens.length;
          a < o;
          a++
        )
          c.tweens[a].run(r);
        return (
          s.notifyWith(e, [c, r, n]),
          r < 1 && o
            ? n
            : (o || s.notifyWith(e, [c, 1, 0]), s.resolveWith(e, [c]), !1)
        );
      },
      c = s.promise({
        elem: e,
        props: v.extend({}, t),
        opts: v.extend(!0, { specialEasing: {}, easing: v.easing._default }, n),
        originalProperties: t,
        originalOptions: n,
        startTime: et || at(),
        duration: n.duration,
        tweens: [],
        createTween: function (t, n) {
          var r = v.Tween(
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
              ? (s.notifyWith(e, [c, 1, 0]), s.resolveWith(e, [c, t]))
              : s.rejectWith(e, [c, t]),
            this
          );
        },
      }),
      l = c.props;
    for (
      !(function (e, t) {
        var n, r, i, a, o;
        for (n in e)
          if (
            ((i = t[(r = V(n))]),
            (a = e[n]),
            Array.isArray(a) && ((i = a[1]), (a = e[n] = a[0])),
            n !== r && ((e[r] = a), delete e[n]),
            (o = v.cssHooks[r]) && ("expand" in o))
          )
            for (n in ((a = o.expand(a)), delete e[r], a))
              (n in e) || ((e[n] = a[n]), (t[n] = i));
          else t[r] = i;
      })(l, c.opts.specialEasing);
      a < o;
      a++
    )
      if ((r = ut.prefilters[a].call(c, e, l, c.opts)))
        return (
          h(r.stop) &&
            (v._queueHooks(c.elem, c.opts.queue).stop = r.stop.bind(r)),
          r
        );
    return (
      v.map(l, st, c),
      h(c.opts.start) && c.opts.start.call(e, c),
      c
        .progress(c.opts.progress)
        .done(c.opts.done, c.opts.complete)
        .fail(c.opts.fail)
        .always(c.opts.always),
      v.fx.timer(v.extend(u, { elem: e, anim: c, queue: c.opts.queue })),
      c
    );
  }
  (v.Animation = v.extend(ut, {
    tweeners: {
      "*": [
        function (e, t) {
          var n = this.createTween(e, t);
          return oe(n.elem, e, ee.exec(t), n), n;
        },
      ],
    },
    tweener: function (e, t) {
      h(e) ? ((t = e), (e = ["*"])) : (e = e.match(L));
      for (var n, r = 0, i = e.length; r < i; r++)
        (n = e[r]),
          (ut.tweeners[n] = ut.tweeners[n] || []),
          ut.tweeners[n].unshift(t);
    },
    prefilters: [
      function (e, t, n) {
        var r,
          i,
          a,
          o,
          s,
          u,
          c,
          l,
          f = "width" in t || "height" in t,
          d = this,
          p = {},
          h = e.style,
          m = e.nodeType && ae(e),
          _ = K.get(e, "fxshow");
        for (r in (n.queue ||
          (null == (o = v._queueHooks(e, "fx")).unqueued &&
            ((o.unqueued = 0),
            (s = o.empty.fire),
            (o.empty.fire = function () {
              o.unqueued || s();
            })),
          o.unqueued++,
          d.always(function () {
            d.always(function () {
              o.unqueued--, v.queue(e, "fx").length || o.empty.fire();
            });
          })),
        t))
          if (((i = t[r]), nt.test(i))) {
            if (
              (delete t[r],
              (a = a || "toggle" === i),
              i === (m ? "hide" : "show"))
            ) {
              if ("show" !== i || !_ || void 0 === _[r]) continue;
              m = !0;
            }
            p[r] = (_ && _[r]) || v.style(e, r);
          }
        if ((u = !v.isEmptyObject(t)) || !v.isEmptyObject(p))
          for (r in (f &&
            1 === e.nodeType &&
            ((n.overflow = [h.overflow, h.overflowX, h.overflowY]),
            null == (c = _ && _.display) && (c = K.get(e, "display")),
            "none" === (l = v.css(e, "display")) &&
              (c
                ? (l = c)
                : (ce([e], !0),
                  (c = e.style.display || c),
                  (l = v.css(e, "display")),
                  ce([e]))),
            ("inline" === l || ("inline-block" === l && null != c)) &&
              "none" === v.css(e, "float") &&
              (u ||
                (d.done(function () {
                  h.display = c;
                }),
                null == c && ((l = h.display), (c = "none" === l ? "" : l))),
              (h.display = "inline-block"))),
          n.overflow &&
            ((h.overflow = "hidden"),
            d.always(function () {
              (h.overflow = n.overflow[0]),
                (h.overflowX = n.overflow[1]),
                (h.overflowY = n.overflow[2]);
            })),
          (u = !1),
          p))
            u ||
              (_
                ? "hidden" in _ && (m = _.hidden)
                : (_ = K.access(e, "fxshow", { display: c })),
              a && (_.hidden = !m),
              m && ce([e], !0),
              d.done(function () {
                for (r in (m || ce([e]), K.remove(e, "fxshow"), p))
                  v.style(e, r, p[r]);
              })),
              (u = st(m ? _[r] : 0, r, d)),
              r in _ ||
                ((_[r] = u.start), m && ((u.end = u.start), (u.start = 0)));
      },
    ],
    prefilter: function (e, t) {
      t ? ut.prefilters.unshift(e) : ut.prefilters.push(e);
    },
  })),
    (v.speed = function (e, t, n) {
      var r =
        e && "object" == typeof e
          ? v.extend({}, e)
          : {
              complete: n || (!n && t) || (h(e) && e),
              duration: e,
              easing: (n && t) || (t && !h(t) && t),
            };
      return (
        v.fx.off
          ? (r.duration = 0)
          : "number" != typeof r.duration &&
            (r.duration in v.fx.speeds
              ? (r.duration = v.fx.speeds[r.duration])
              : (r.duration = v.fx.speeds._default)),
        (null != r.queue && !0 !== r.queue) || (r.queue = "fx"),
        (r.old = r.complete),
        (r.complete = function () {
          h(r.old) && r.old.call(this), r.queue && v.dequeue(this, r.queue);
        }),
        r
      );
    }),
    v.fn.extend({
      fadeTo: function (e, t, n, r) {
        return this.filter(ae)
          .css("opacity", 0)
          .show()
          .end()
          .animate({ opacity: t }, e, n, r);
      },
      animate: function (e, t, n, r) {
        var i = v.isEmptyObject(e),
          a = v.speed(t, n, r),
          o = function () {
            var t = ut(this, v.extend({}, e), a);
            (i || K.get(this, "finish")) && t.stop(!0);
          };
        return (
          (o.finish = o),
          i || !1 === a.queue ? this.each(o) : this.queue(a.queue, o)
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
              a = v.timers,
              o = K.get(this);
            if (i) o[i] && o[i].stop && r(o[i]);
            else for (i in o) o[i] && o[i].stop && rt.test(i) && r(o[i]);
            for (i = a.length; i--; )
              a[i].elem !== this ||
                (null != e && a[i].queue !== e) ||
                (a[i].anim.stop(n), (t = !1), a.splice(i, 1));
            (!t && n) || v.dequeue(this, e);
          })
        );
      },
      finish: function (e) {
        return (
          !1 !== e && (e = e || "fx"),
          this.each(function () {
            var t,
              n = K.get(this),
              r = n[e + "queue"],
              i = n[e + "queueHooks"],
              a = v.timers,
              o = r ? r.length : 0;
            for (
              n.finish = !0,
                v.queue(this, e, []),
                i && i.stop && i.stop.call(this, !0),
                t = a.length;
              t--;

            )
              a[t].elem === this &&
                a[t].queue === e &&
                (a[t].anim.stop(!0), a.splice(t, 1));
            for (t = 0; t < o; t++)
              r[t] && r[t].finish && r[t].finish.call(this);
            delete n.finish;
          })
        );
      },
    }),
    v.each(["toggle", "show", "hide"], function (e, t) {
      var n = v.fn[t];
      v.fn[t] = function (e, r, i) {
        return null == e || "boolean" == typeof e
          ? n.apply(this, arguments)
          : this.animate(ot(t, !0), e, r, i);
      };
    }),
    v.each(
      {
        slideDown: ot("show"),
        slideUp: ot("hide"),
        slideToggle: ot("toggle"),
        fadeIn: { opacity: "show" },
        fadeOut: { opacity: "hide" },
        fadeToggle: { opacity: "toggle" },
      },
      function (e, t) {
        v.fn[e] = function (e, n, r) {
          return this.animate(t, e, n, r);
        };
      }
    ),
    (v.timers = []),
    (v.fx.tick = function () {
      var e,
        t = 0,
        n = v.timers;
      for (et = Date.now(); t < n.length; t++)
        (e = n[t])() || n[t] !== e || n.splice(t--, 1);
      n.length || v.fx.stop(), (et = void 0);
    }),
    (v.fx.timer = function (e) {
      v.timers.push(e), v.fx.start();
    }),
    (v.fx.interval = 13),
    (v.fx.start = function () {
      tt || ((tt = !0), it());
    }),
    (v.fx.stop = function () {
      tt = null;
    }),
    (v.fx.speeds = { slow: 600, fast: 200, _default: 400 }),
    (v.fn.delay = function (t, n) {
      return (
        (t = (v.fx && v.fx.speeds[t]) || t),
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
        (p.checkOn = "" !== e.value),
        (p.optSelected = t.selected),
        ((e = _.createElement("input")).value = "t"),
        (e.type = "radio"),
        (p.radioValue = "t" === e.value);
    })();
  var ct,
    lt = v.expr.attrHandle;
  v.fn.extend({
    attr: function (e, t) {
      return H(this, v.attr, e, t, arguments.length > 1);
    },
    removeAttr: function (e) {
      return this.each(function () {
        v.removeAttr(this, e);
      });
    },
  }),
    v.extend({
      attr: function (e, t, n) {
        var r,
          i,
          a = e.nodeType;
        if (3 !== a && 8 !== a && 2 !== a)
          return void 0 === e.getAttribute
            ? v.prop(e, t, n)
            : ((1 === a && v.isXMLDoc(e)) ||
                (i =
                  v.attrHooks[t.toLowerCase()] ||
                  (v.expr.match.bool.test(t) ? ct : void 0)),
              void 0 !== n
                ? null === n
                  ? void v.removeAttr(e, t)
                  : i && "set" in i && void 0 !== (r = i.set(e, n, t))
                  ? r
                  : (e.setAttribute(t, n + ""), n)
                : i && "get" in i && null !== (r = i.get(e, t))
                ? r
                : null == (r = v.find.attr(e, t))
                ? void 0
                : r);
      },
      attrHooks: {
        type: {
          set: function (e, t) {
            if (!p.radioValue && "radio" === t && E(e, "input")) {
              var n = e.value;
              return e.setAttribute("type", t), n && (e.value = n), t;
            }
          },
        },
      },
      removeAttr: function (e, t) {
        var n,
          r = 0,
          i = t && t.match(L);
        if (i && 1 === e.nodeType) for (; (n = i[r++]); ) e.removeAttribute(n);
      },
    }),
    (ct = {
      set: function (e, t, n) {
        return !1 === t ? v.removeAttr(e, n) : e.setAttribute(n, n), n;
      },
    }),
    v.each(v.expr.match.bool.source.match(/\w+/g), function (e, t) {
      var n = lt[t] || v.find.attr;
      lt[t] = function (e, t, r) {
        var i,
          a,
          o = t.toLowerCase();
        return (
          r ||
            ((a = lt[o]),
            (lt[o] = i),
            (i = null != n(e, t, r) ? o : null),
            (lt[o] = a)),
          i
        );
      };
    });
  var ft = /^(?:input|select|textarea|button)$/i,
    dt = /^(?:a|area)$/i;
  function pt(e) {
    return (e.match(L) || []).join(" ");
  }
  function ht(e) {
    return (e.getAttribute && e.getAttribute("class")) || "";
  }
  function mt(e) {
    return Array.isArray(e) ? e : ("string" == typeof e && e.match(L)) || [];
  }
  v.fn.extend({
    prop: function (e, t) {
      return H(this, v.prop, e, t, arguments.length > 1);
    },
    removeProp: function (e) {
      return this.each(function () {
        delete this[v.propFix[e] || e];
      });
    },
  }),
    v.extend({
      prop: function (e, t, n) {
        var r,
          i,
          a = e.nodeType;
        if (3 !== a && 8 !== a && 2 !== a)
          return (
            (1 === a && v.isXMLDoc(e)) ||
              ((t = v.propFix[t] || t), (i = v.propHooks[t])),
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
            var t = v.find.attr(e, "tabindex");
            return t
              ? parseInt(t, 10)
              : ft.test(e.nodeName) || (dt.test(e.nodeName) && e.href)
              ? 0
              : -1;
          },
        },
      },
      propFix: { for: "htmlFor", class: "className" },
    }),
    p.optSelected ||
      (v.propHooks.selected = {
        get: function (e) {
          var t = e.parentNode;
          return t && t.parentNode && t.parentNode.selectedIndex, null;
        },
        set: function (e) {
          var t = e.parentNode;
          t && (t.selectedIndex, t.parentNode && t.parentNode.selectedIndex);
        },
      }),
    v.each(
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
        v.propFix[this.toLowerCase()] = this;
      }
    ),
    v.fn.extend({
      addClass: function (e) {
        var t,
          n,
          r,
          i,
          a,
          o,
          s,
          u = 0;
        if (h(e))
          return this.each(function (t) {
            v(this).addClass(e.call(this, t, ht(this)));
          });
        if ((t = mt(e)).length)
          for (; (n = this[u++]); )
            if (((i = ht(n)), (r = 1 === n.nodeType && " " + pt(i) + " "))) {
              for (o = 0; (a = t[o++]); )
                r.indexOf(" " + a + " ") < 0 && (r += a + " ");
              i !== (s = pt(r)) && n.setAttribute("class", s);
            }
        return this;
      },
      removeClass: function (e) {
        var t,
          n,
          r,
          i,
          a,
          o,
          s,
          u = 0;
        if (h(e))
          return this.each(function (t) {
            v(this).removeClass(e.call(this, t, ht(this)));
          });
        if (!arguments.length) return this.attr("class", "");
        if ((t = mt(e)).length)
          for (; (n = this[u++]); )
            if (((i = ht(n)), (r = 1 === n.nodeType && " " + pt(i) + " "))) {
              for (o = 0; (a = t[o++]); )
                for (; r.indexOf(" " + a + " ") > -1; )
                  r = r.replace(" " + a + " ", " ");
              i !== (s = pt(r)) && n.setAttribute("class", s);
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
          : h(e)
          ? this.each(function (n) {
              v(this).toggleClass(e.call(this, n, ht(this), t), t);
            })
          : this.each(function () {
              var t, i, a, o;
              if (r)
                for (i = 0, a = v(this), o = mt(e); (t = o[i++]); )
                  a.hasClass(t) ? a.removeClass(t) : a.addClass(t);
              else
                (void 0 !== e && "boolean" !== n) ||
                  ((t = ht(this)) && K.set(this, "__className__", t),
                  this.setAttribute &&
                    this.setAttribute(
                      "class",
                      t || !1 === e ? "" : K.get(this, "__className__") || ""
                    ));
            });
      },
      hasClass: function (e) {
        var t,
          n,
          r = 0;
        for (t = " " + e + " "; (n = this[r++]); )
          if (1 === n.nodeType && (" " + pt(ht(n)) + " ").indexOf(t) > -1)
            return !0;
        return !1;
      },
    });
  var _t = /\r/g;
  v.fn.extend({
    val: function (e) {
      var t,
        n,
        r,
        i = this[0];
      return arguments.length
        ? ((r = h(e)),
          this.each(function (n) {
            var i;
            1 === this.nodeType &&
              (null == (i = r ? e.call(this, n, v(this).val()) : e)
                ? (i = "")
                : "number" == typeof i
                ? (i += "")
                : Array.isArray(i) &&
                  (i = v.map(i, function (e) {
                    return null == e ? "" : e + "";
                  })),
              ((t =
                v.valHooks[this.type] ||
                v.valHooks[this.nodeName.toLowerCase()]) &&
                "set" in t &&
                void 0 !== t.set(this, i, "value")) ||
                (this.value = i));
          }))
        : i
        ? (t = v.valHooks[i.type] || v.valHooks[i.nodeName.toLowerCase()]) &&
          "get" in t &&
          void 0 !== (n = t.get(i, "value"))
          ? n
          : "string" == typeof (n = i.value)
          ? n.replace(_t, "")
          : null == n
          ? ""
          : n
        : void 0;
    },
  }),
    v.extend({
      valHooks: {
        option: {
          get: function (e) {
            var t = v.find.attr(e, "value");
            return null != t ? t : pt(v.text(e));
          },
        },
        select: {
          get: function (e) {
            var t,
              n,
              r,
              i = e.options,
              a = e.selectedIndex,
              o = "select-one" === e.type,
              s = o ? null : [],
              u = o ? a + 1 : i.length;
            for (r = a < 0 ? u : o ? a : 0; r < u; r++)
              if (
                ((n = i[r]).selected || r === a) &&
                !n.disabled &&
                (!n.parentNode.disabled || !E(n.parentNode, "optgroup"))
              ) {
                if (((t = v(n).val()), o)) return t;
                s.push(t);
              }
            return s;
          },
          set: function (e, t) {
            for (
              var n, r, i = e.options, a = v.makeArray(t), o = i.length;
              o--;

            )
              ((r = i[o]).selected =
                v.inArray(v.valHooks.option.get(r), a) > -1) && (n = !0);
            return n || (e.selectedIndex = -1), a;
          },
        },
      },
    }),
    v.each(["radio", "checkbox"], function () {
      (v.valHooks[this] = {
        set: function (e, t) {
          if (Array.isArray(t))
            return (e.checked = v.inArray(v(e).val(), t) > -1);
        },
      }),
        p.checkOn ||
          (v.valHooks[this].get = function (e) {
            return null === e.getAttribute("value") ? "on" : e.value;
          });
    }),
    (p.focusin = "onfocusin" in e);
  var gt = /^(?:focusinfocus|focusoutblur)$/,
    yt = function (e) {
      e.stopPropagation();
    };
  v.extend(v.event, {
    trigger: function (t, n, r, i) {
      var a,
        o,
        s,
        u,
        c,
        f,
        d,
        p,
        g = [r || _],
        y = l.call(t, "type") ? t.type : t,
        b = l.call(t, "namespace") ? t.namespace.split(".") : [];
      if (
        ((o = p = s = r = r || _),
        3 !== r.nodeType &&
          8 !== r.nodeType &&
          !gt.test(y + v.event.triggered) &&
          (y.indexOf(".") > -1 &&
            ((b = y.split(".")), (y = b.shift()), b.sort()),
          (c = y.indexOf(":") < 0 && "on" + y),
          ((t = t[v.expando]
            ? t
            : new v.Event(y, "object" == typeof t && t)).isTrigger = i ? 2 : 3),
          (t.namespace = b.join(".")),
          (t.rnamespace = t.namespace
            ? new RegExp("(^|\\.)" + b.join("\\.(?:.*\\.|)") + "(\\.|$)")
            : null),
          (t.result = void 0),
          t.target || (t.target = r),
          (n = null == n ? [t] : v.makeArray(n, [t])),
          (d = v.event.special[y] || {}),
          i || !d.trigger || !1 !== d.trigger.apply(r, n)))
      ) {
        if (!i && !d.noBubble && !m(r)) {
          for (
            u = d.delegateType || y, gt.test(u + y) || (o = o.parentNode);
            o;
            o = o.parentNode
          )
            g.push(o), (s = o);
          s === (r.ownerDocument || _) &&
            g.push(s.defaultView || s.parentWindow || e);
        }
        for (a = 0; (o = g[a++]) && !t.isPropagationStopped(); )
          (p = o),
            (t.type = a > 1 ? u : d.bindType || y),
            (f =
              (K.get(o, "events") || Object.create(null))[t.type] &&
              K.get(o, "handle")) && f.apply(o, n),
            (f = c && o[c]) &&
              f.apply &&
              U(o) &&
              ((t.result = f.apply(o, n)),
              !1 === t.result && t.preventDefault());
        return (
          (t.type = y),
          i ||
            t.isDefaultPrevented() ||
            (d._default && !1 !== d._default.apply(g.pop(), n)) ||
            !U(r) ||
            (c &&
              h(r[y]) &&
              !m(r) &&
              ((s = r[c]) && (r[c] = null),
              (v.event.triggered = y),
              t.isPropagationStopped() && p.addEventListener(y, yt),
              r[y](),
              t.isPropagationStopped() && p.removeEventListener(y, yt),
              (v.event.triggered = void 0),
              s && (r[c] = s))),
          t.result
        );
      }
    },
    simulate: function (e, t, n) {
      var r = v.extend(new v.Event(), n, { type: e, isSimulated: !0 });
      v.event.trigger(r, null, t);
    },
  }),
    v.fn.extend({
      trigger: function (e, t) {
        return this.each(function () {
          v.event.trigger(e, t, this);
        });
      },
      triggerHandler: function (e, t) {
        var n = this[0];
        if (n) return v.event.trigger(e, t, n, !0);
      },
    }),
    p.focusin ||
      v.each({ focus: "focusin", blur: "focusout" }, function (e, t) {
        var n = function (e) {
          v.event.simulate(t, e.target, v.event.fix(e));
        };
        v.event.special[t] = {
          setup: function () {
            var r = this.ownerDocument || this.document || this,
              i = K.access(r, t);
            i || r.addEventListener(e, n, !0), K.access(r, t, (i || 0) + 1);
          },
          teardown: function () {
            var r = this.ownerDocument || this.document || this,
              i = K.access(r, t) - 1;
            i
              ? K.access(r, t, i)
              : (r.removeEventListener(e, n, !0), K.remove(r, t));
          },
        };
      });
  var bt = e.location,
    vt = { guid: Date.now() },
    wt = /\?/;
  v.parseXML = function (t) {
    var n, r;
    if (!t || "string" != typeof t) return null;
    try {
      n = new e.DOMParser().parseFromString(t, "text/xml");
    } catch (e) {}
    return (
      (r = n && n.getElementsByTagName("parsererror")[0]),
      (n && !r) ||
        v.error(
          "Invalid XML: " +
            (r
              ? v
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
    St = /\r?\n/g,
    Ct = /^(?:submit|button|image|reset|file)$/i,
    kt = /^(?:input|select|textarea|keygen)/i;
  function Et(e, t, n, r) {
    var i;
    if (Array.isArray(t))
      v.each(t, function (t, i) {
        n || xt.test(e)
          ? r(e, i)
          : Et(
              e + "[" + ("object" == typeof i && null != i ? t : "") + "]",
              i,
              n,
              r
            );
      });
    else if (n || "object" !== b(t)) r(e, t);
    else for (i in t) Et(e + "[" + i + "]", t[i], n, r);
  }
  (v.param = function (e, t) {
    var n,
      r = [],
      i = function (e, t) {
        var n = h(t) ? t() : t;
        r[r.length] =
          encodeURIComponent(e) + "=" + encodeURIComponent(null == n ? "" : n);
      };
    if (null == e) return "";
    if (Array.isArray(e) || (e.jquery && !v.isPlainObject(e)))
      v.each(e, function () {
        i(this.name, this.value);
      });
    else for (n in e) Et(n, e[n], t, i);
    return r.join("&");
  }),
    v.fn.extend({
      serialize: function () {
        return v.param(this.serializeArray());
      },
      serializeArray: function () {
        return this.map(function () {
          var e = v.prop(this, "elements");
          return e ? v.makeArray(e) : this;
        })
          .filter(function () {
            var e = this.type;
            return (
              this.name &&
              !v(this).is(":disabled") &&
              kt.test(this.nodeName) &&
              !Ct.test(e) &&
              (this.checked || !de.test(e))
            );
          })
          .map(function (e, t) {
            var n = v(this).val();
            return null == n
              ? null
              : Array.isArray(n)
              ? v.map(n, function (e) {
                  return { name: t.name, value: e.replace(St, "\r\n") };
                })
              : { name: t.name, value: n.replace(St, "\r\n") };
          })
          .get();
      },
    });
  var Pt = /%20/g,
    jt = /#.*$/,
    $t = /([?&])_=[^&]*/,
    Tt = /^(.*?):[ \t]*([^\r\n]*)$/gm,
    At = /^(?:GET|HEAD)$/,
    Bt = /^\/\//,
    qt = {},
    Lt = {},
    Dt = "*/".concat("*"),
    Nt = _.createElement("a");
  function Ot(e) {
    return function (t, n) {
      "string" != typeof t && ((n = t), (t = "*"));
      var r,
        i = 0,
        a = t.toLowerCase().match(L) || [];
      if (h(n))
        for (; (r = a[i++]); )
          "+" === r[0]
            ? ((r = r.slice(1) || "*"), (e[r] = e[r] || []).unshift(n))
            : (e[r] = e[r] || []).push(n);
    };
  }
  function It(e, t, n, r) {
    var i = {},
      a = e === Lt;
    function o(s) {
      var u;
      return (
        (i[s] = !0),
        v.each(e[s] || [], function (e, s) {
          var c = s(t, n, r);
          return "string" != typeof c || a || i[c]
            ? a
              ? !(u = c)
              : void 0
            : (t.dataTypes.unshift(c), o(c), !1);
        }),
        u
      );
    }
    return o(t.dataTypes[0]) || (!i["*"] && o("*"));
  }
  function Mt(e, t) {
    var n,
      r,
      i = v.ajaxSettings.flatOptions || {};
    for (n in t) void 0 !== t[n] && ((i[n] ? e : r || (r = {}))[n] = t[n]);
    return r && v.extend(!0, e, r), e;
  }
  (Nt.href = bt.href),
    v.extend({
      active: 0,
      lastModified: {},
      etag: {},
      ajaxSettings: {
        url: bt.href,
        type: "GET",
        isLocal: /^(?:about|app|app-storage|.+-extension|file|res|widget):$/.test(
          bt.protocol
        ),
        global: !0,
        processData: !0,
        async: !0,
        contentType: "application/x-www-form-urlencoded; charset=UTF-8",
        accepts: {
          "*": Dt,
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
          "text xml": v.parseXML,
        },
        flatOptions: { url: !0, context: !0 },
      },
      ajaxSetup: function (e, t) {
        return t ? Mt(Mt(e, v.ajaxSettings), t) : Mt(v.ajaxSettings, e);
      },
      ajaxPrefilter: Ot(qt),
      ajaxTransport: Ot(Lt),
      ajax: function (t, n) {
        "object" == typeof t && ((n = t), (t = void 0)), (n = n || {});
        var r,
          i,
          a,
          o,
          s,
          u,
          c,
          l,
          f,
          d,
          p = v.ajaxSetup({}, n),
          h = p.context || p,
          m = p.context && (h.nodeType || h.jquery) ? v(h) : v.event,
          g = v.Deferred(),
          y = v.Callbacks("once memory"),
          b = p.statusCode || {},
          w = {},
          x = {},
          S = "canceled",
          C = {
            readyState: 0,
            getResponseHeader: function (e) {
              var t;
              if (c) {
                if (!o)
                  for (o = {}; (t = Tt.exec(a)); )
                    o[t[1].toLowerCase() + " "] = (
                      o[t[1].toLowerCase() + " "] || []
                    ).concat(t[2]);
                t = o[e.toLowerCase() + " "];
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
                  (w[e] = t)),
                this
              );
            },
            overrideMimeType: function (e) {
              return null == c && (p.mimeType = e), this;
            },
            statusCode: function (e) {
              var t;
              if (e)
                if (c) C.always(e[C.status]);
                else for (t in e) b[t] = [b[t], e[t]];
              return this;
            },
            abort: function (e) {
              var t = e || S;
              return r && r.abort(t), k(0, t), this;
            },
          };
        if (
          (g.promise(C),
          (p.url = ((t || p.url || bt.href) + "").replace(
            Bt,
            bt.protocol + "//"
          )),
          (p.type = n.method || n.type || p.method || p.type),
          (p.dataTypes = (p.dataType || "*").toLowerCase().match(L) || [""]),
          null == p.crossDomain)
        ) {
          u = _.createElement("a");
          try {
            (u.href = p.url),
              (u.href = u.href),
              (p.crossDomain =
                Nt.protocol + "//" + Nt.host != u.protocol + "//" + u.host);
          } catch (e) {
            p.crossDomain = !0;
          }
        }
        if (
          (p.data &&
            p.processData &&
            "string" != typeof p.data &&
            (p.data = v.param(p.data, p.traditional)),
          It(qt, p, n, C),
          c)
        )
          return C;
        for (f in ((l = v.event && p.global) &&
          0 == v.active++ &&
          v.event.trigger("ajaxStart"),
        (p.type = p.type.toUpperCase()),
        (p.hasContent = !At.test(p.type)),
        (i = p.url.replace(jt, "")),
        p.hasContent
          ? p.data &&
            p.processData &&
            0 ===
              (p.contentType || "").indexOf(
                "application/x-www-form-urlencoded"
              ) &&
            (p.data = p.data.replace(Pt, "+"))
          : ((d = p.url.slice(i.length)),
            p.data &&
              (p.processData || "string" == typeof p.data) &&
              ((i += (wt.test(i) ? "&" : "?") + p.data), delete p.data),
            !1 === p.cache &&
              ((i = i.replace($t, "$1")),
              (d = (wt.test(i) ? "&" : "?") + "_=" + vt.guid++ + d)),
            (p.url = i + d)),
        p.ifModified &&
          (v.lastModified[i] &&
            C.setRequestHeader("If-Modified-Since", v.lastModified[i]),
          v.etag[i] && C.setRequestHeader("If-None-Match", v.etag[i])),
        ((p.data && p.hasContent && !1 !== p.contentType) || n.contentType) &&
          C.setRequestHeader("Content-Type", p.contentType),
        C.setRequestHeader(
          "Accept",
          p.dataTypes[0] && p.accepts[p.dataTypes[0]]
            ? p.accepts[p.dataTypes[0]] +
                ("*" !== p.dataTypes[0] ? ", " + Dt + "; q=0.01" : "")
            : p.accepts["*"]
        ),
        p.headers))
          C.setRequestHeader(f, p.headers[f]);
        if (p.beforeSend && (!1 === p.beforeSend.call(h, C, p) || c))
          return C.abort();
        if (
          ((S = "abort"),
          y.add(p.complete),
          C.done(p.success),
          C.fail(p.error),
          (r = It(Lt, p, n, C)))
        ) {
          if (((C.readyState = 1), l && m.trigger("ajaxSend", [C, p]), c))
            return C;
          p.async &&
            p.timeout > 0 &&
            (s = e.setTimeout(function () {
              C.abort("timeout");
            }, p.timeout));
          try {
            (c = !1), r.send(w, k);
          } catch (e) {
            if (c) throw e;
            k(-1, e);
          }
        } else k(-1, "No Transport");
        function k(t, n, o, u) {
          var f,
            d,
            _,
            w,
            x,
            S = n;
          c ||
            ((c = !0),
            s && e.clearTimeout(s),
            (r = void 0),
            (a = u || ""),
            (C.readyState = t > 0 ? 4 : 0),
            (f = (t >= 200 && t < 300) || 304 === t),
            o &&
              (w = (function (e, t, n) {
                for (
                  var r, i, a, o, s = e.contents, u = e.dataTypes;
                  "*" === u[0];

                )
                  u.shift(),
                    void 0 === r &&
                      (r = e.mimeType || t.getResponseHeader("Content-Type"));
                if (r)
                  for (i in s)
                    if (s[i] && s[i].test(r)) {
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
                    o || (o = i);
                  }
                  a = a || o;
                }
                if (a) return a !== u[0] && u.unshift(a), n[a];
              })(p, C, o)),
            !f &&
              v.inArray("script", p.dataTypes) > -1 &&
              v.inArray("json", p.dataTypes) < 0 &&
              (p.converters["text script"] = function () {}),
            (w = (function (e, t, n, r) {
              var i,
                a,
                o,
                s,
                u,
                c = {},
                l = e.dataTypes.slice();
              if (l[1])
                for (o in e.converters) c[o.toLowerCase()] = e.converters[o];
              for (a = l.shift(); a; )
                if (
                  (e.responseFields[a] && (n[e.responseFields[a]] = t),
                  !u && r && e.dataFilter && (t = e.dataFilter(t, e.dataType)),
                  (u = a),
                  (a = l.shift()))
                )
                  if ("*" === a) a = u;
                  else if ("*" !== u && u !== a) {
                    if (!(o = c[u + " " + a] || c["* " + a]))
                      for (i in c)
                        if (
                          (s = i.split(" "))[1] === a &&
                          (o = c[u + " " + s[0]] || c["* " + s[0]])
                        ) {
                          !0 === o
                            ? (o = c[i])
                            : !0 !== c[i] && ((a = s[0]), l.unshift(s[1]));
                          break;
                        }
                    if (!0 !== o)
                      if (o && e.throws) t = o(t);
                      else
                        try {
                          t = o(t);
                        } catch (e) {
                          return {
                            state: "parsererror",
                            error: o
                              ? e
                              : "No conversion from " + u + " to " + a,
                          };
                        }
                  }
              return { state: "success", data: t };
            })(p, w, C, f)),
            f
              ? (p.ifModified &&
                  ((x = C.getResponseHeader("Last-Modified")) &&
                    (v.lastModified[i] = x),
                  (x = C.getResponseHeader("etag")) && (v.etag[i] = x)),
                204 === t || "HEAD" === p.type
                  ? (S = "nocontent")
                  : 304 === t
                  ? (S = "notmodified")
                  : ((S = w.state), (d = w.data), (f = !(_ = w.error))))
              : ((_ = S), (!t && S) || ((S = "error"), t < 0 && (t = 0))),
            (C.status = t),
            (C.statusText = (n || S) + ""),
            f ? g.resolveWith(h, [d, S, C]) : g.rejectWith(h, [C, S, _]),
            C.statusCode(b),
            (b = void 0),
            l && m.trigger(f ? "ajaxSuccess" : "ajaxError", [C, p, f ? d : _]),
            y.fireWith(h, [C, S]),
            l &&
              (m.trigger("ajaxComplete", [C, p]),
              --v.active || v.event.trigger("ajaxStop")));
        }
        return C;
      },
      getJSON: function (e, t, n) {
        return v.get(e, t, n, "json");
      },
      getScript: function (e, t) {
        return v.get(e, void 0, t, "script");
      },
    }),
    v.each(["get", "post"], function (e, t) {
      v[t] = function (e, n, r, i) {
        return (
          h(n) && ((i = i || r), (r = n), (n = void 0)),
          v.ajax(
            v.extend(
              { url: e, type: t, dataType: i, data: n, success: r },
              v.isPlainObject(e) && e
            )
          )
        );
      };
    }),
    v.ajaxPrefilter(function (e) {
      var t;
      for (t in e.headers)
        "content-type" === t.toLowerCase() &&
          (e.contentType = e.headers[t] || "");
    }),
    (v._evalUrl = function (e, t, n) {
      return v.ajax({
        url: e,
        type: "GET",
        dataType: "script",
        cache: !0,
        async: !1,
        global: !1,
        converters: { "text script": function () {} },
        dataFilter: function (e) {
          v.globalEval(e, t, n);
        },
      });
    }),
    v.fn.extend({
      wrapAll: function (e) {
        var t;
        return (
          this[0] &&
            (h(e) && (e = e.call(this[0])),
            (t = v(e, this[0].ownerDocument).eq(0).clone(!0)),
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
        return h(e)
          ? this.each(function (t) {
              v(this).wrapInner(e.call(this, t));
            })
          : this.each(function () {
              var t = v(this),
                n = t.contents();
              n.length ? n.wrapAll(e) : t.append(e);
            });
      },
      wrap: function (e) {
        var t = h(e);
        return this.each(function (n) {
          v(this).wrapAll(t ? e.call(this, n) : e);
        });
      },
      unwrap: function (e) {
        return (
          this.parent(e)
            .not("body")
            .each(function () {
              v(this).replaceWith(this.childNodes);
            }),
          this
        );
      },
    }),
    (v.expr.pseudos.hidden = function (e) {
      return !v.expr.pseudos.visible(e);
    }),
    (v.expr.pseudos.visible = function (e) {
      return !!(e.offsetWidth || e.offsetHeight || e.getClientRects().length);
    }),
    (v.ajaxSettings.xhr = function () {
      try {
        return new e.XMLHttpRequest();
      } catch (e) {}
    });
  var Rt = { 0: 200, 1223: 204 },
    Ht = v.ajaxSettings.xhr();
  (p.cors = !!Ht && "withCredentials" in Ht),
    (p.ajax = Ht = !!Ht),
    v.ajaxTransport(function (t) {
      var n, r;
      if (p.cors || (Ht && !t.crossDomain))
        return {
          send: function (i, a) {
            var o,
              s = t.xhr();
            if (
              (s.open(t.type, t.url, t.async, t.username, t.password),
              t.xhrFields)
            )
              for (o in t.xhrFields) s[o] = t.xhrFields[o];
            for (o in (t.mimeType &&
              s.overrideMimeType &&
              s.overrideMimeType(t.mimeType),
            t.crossDomain ||
              i["X-Requested-With"] ||
              (i["X-Requested-With"] = "XMLHttpRequest"),
            i))
              s.setRequestHeader(o, i[o]);
            (n = function (e) {
              return function () {
                n &&
                  ((n = r = s.onload = s.onerror = s.onabort = s.ontimeout = s.onreadystatechange = null),
                  "abort" === e
                    ? s.abort()
                    : "error" === e
                    ? "number" != typeof s.status
                      ? a(0, "error")
                      : a(s.status, s.statusText)
                    : a(
                        Rt[s.status] || s.status,
                        s.statusText,
                        "text" !== (s.responseType || "text") ||
                          "string" != typeof s.responseText
                          ? { binary: s.response }
                          : { text: s.responseText },
                        s.getAllResponseHeaders()
                      ));
              };
            }),
              (s.onload = n()),
              (r = s.onerror = s.ontimeout = n("error")),
              void 0 !== s.onabort
                ? (s.onabort = r)
                : (s.onreadystatechange = function () {
                    4 === s.readyState &&
                      e.setTimeout(function () {
                        n && r();
                      });
                  }),
              (n = n("abort"));
            try {
              s.send((t.hasContent && t.data) || null);
            } catch (e) {
              if (n) throw e;
            }
          },
          abort: function () {
            n && n();
          },
        };
    }),
    v.ajaxPrefilter(function (e) {
      e.crossDomain && (e.contents.script = !1);
    }),
    v.ajaxSetup({
      accepts: {
        script:
          "text/javascript, application/javascript, application/ecmascript, application/x-ecmascript",
      },
      contents: { script: /\b(?:java|ecma)script\b/ },
      converters: {
        "text script": function (e) {
          return v.globalEval(e), e;
        },
      },
    }),
    v.ajaxPrefilter("script", function (e) {
      void 0 === e.cache && (e.cache = !1), e.crossDomain && (e.type = "GET");
    }),
    v.ajaxTransport("script", function (e) {
      var t, n;
      if (e.crossDomain || e.scriptAttrs)
        return {
          send: function (r, i) {
            (t = v("<script>")
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
  var Ft,
    Wt = [],
    zt = /(=)\?(?=&|$)|\?\?/;
  v.ajaxSetup({
    jsonp: "callback",
    jsonpCallback: function () {
      var e = Wt.pop() || v.expando + "_" + vt.guid++;
      return (this[e] = !0), e;
    },
  }),
    v.ajaxPrefilter("json jsonp", function (t, n, r) {
      var i,
        a,
        o,
        s =
          !1 !== t.jsonp &&
          (zt.test(t.url)
            ? "url"
            : "string" == typeof t.data &&
              0 ===
                (t.contentType || "").indexOf(
                  "application/x-www-form-urlencoded"
                ) &&
              zt.test(t.data) &&
              "data");
      if (s || "jsonp" === t.dataTypes[0])
        return (
          (i = t.jsonpCallback = h(t.jsonpCallback)
            ? t.jsonpCallback()
            : t.jsonpCallback),
          s
            ? (t[s] = t[s].replace(zt, "$1" + i))
            : !1 !== t.jsonp &&
              (t.url += (wt.test(t.url) ? "&" : "?") + t.jsonp + "=" + i),
          (t.converters["script json"] = function () {
            return o || v.error(i + " was not called"), o[0];
          }),
          (t.dataTypes[0] = "json"),
          (a = e[i]),
          (e[i] = function () {
            o = arguments;
          }),
          r.always(function () {
            void 0 === a ? v(e).removeProp(i) : (e[i] = a),
              t[i] && ((t.jsonpCallback = n.jsonpCallback), Wt.push(i)),
              o && h(a) && a(o[0]),
              (o = a = void 0);
          }),
          "script"
        );
    }),
    (p.createHTMLDocument =
      (((Ft = _.implementation.createHTMLDocument("").body).innerHTML =
        "<form></form><form></form>"),
      2 === Ft.childNodes.length)),
    (v.parseHTML = function (e, t, n) {
      return "string" != typeof e
        ? []
        : ("boolean" == typeof t && ((n = t), (t = !1)),
          t ||
            (p.createHTMLDocument
              ? (((r = (t = _.implementation.createHTMLDocument(
                  ""
                )).createElement("base")).href = _.location.href),
                t.head.appendChild(r))
              : (t = _)),
          (a = !n && []),
          (i = P.exec(e))
            ? [t.createElement(i[1])]
            : ((i = be([e], t, a)),
              a && a.length && v(a).remove(),
              v.merge([], i.childNodes)));
      var r, i, a;
    }),
    (v.fn.load = function (e, t, n) {
      var r,
        i,
        a,
        o = this,
        s = e.indexOf(" ");
      return (
        s > -1 && ((r = pt(e.slice(s))), (e = e.slice(0, s))),
        h(t)
          ? ((n = t), (t = void 0))
          : t && "object" == typeof t && (i = "POST"),
        o.length > 0 &&
          v
            .ajax({ url: e, type: i || "GET", dataType: "html", data: t })
            .done(function (e) {
              (a = arguments),
                o.html(r ? v("<div>").append(v.parseHTML(e)).find(r) : e);
            })
            .always(
              n &&
                function (e, t) {
                  o.each(function () {
                    n.apply(this, a || [e.responseText, t, e]);
                  });
                }
            ),
        this
      );
    }),
    (v.expr.pseudos.animated = function (e) {
      return v.grep(v.timers, function (t) {
        return e === t.elem;
      }).length;
    }),
    (v.offset = {
      setOffset: function (e, t, n) {
        var r,
          i,
          a,
          o,
          s,
          u,
          c = v.css(e, "position"),
          l = v(e),
          f = {};
        "static" === c && (e.style.position = "relative"),
          (s = l.offset()),
          (a = v.css(e, "top")),
          (u = v.css(e, "left")),
          ("absolute" === c || "fixed" === c) && (a + u).indexOf("auto") > -1
            ? ((o = (r = l.position()).top), (i = r.left))
            : ((o = parseFloat(a) || 0), (i = parseFloat(u) || 0)),
          h(t) && (t = t.call(e, n, v.extend({}, s))),
          null != t.top && (f.top = t.top - s.top + o),
          null != t.left && (f.left = t.left - s.left + i),
          "using" in t ? t.using.call(e, f) : l.css(f);
      },
    }),
    v.fn.extend({
      offset: function (e) {
        if (arguments.length)
          return void 0 === e
            ? this
            : this.each(function (t) {
                v.offset.setOffset(this, e, t);
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
          if ("fixed" === v.css(r, "position")) t = r.getBoundingClientRect();
          else {
            for (
              t = this.offset(),
                n = r.ownerDocument,
                e = r.offsetParent || n.documentElement;
              e &&
              (e === n.body || e === n.documentElement) &&
              "static" === v.css(e, "position");

            )
              e = e.parentNode;
            e &&
              e !== r &&
              1 === e.nodeType &&
              (((i = v(e).offset()).top += v.css(e, "borderTopWidth", !0)),
              (i.left += v.css(e, "borderLeftWidth", !0)));
          }
          return {
            top: t.top - i.top - v.css(r, "marginTop", !0),
            left: t.left - i.left - v.css(r, "marginLeft", !0),
          };
        }
      },
      offsetParent: function () {
        return this.map(function () {
          for (
            var e = this.offsetParent;
            e && "static" === v.css(e, "position");

          )
            e = e.offsetParent;
          return e || ne;
        });
      },
    }),
    v.each({ scrollLeft: "pageXOffset", scrollTop: "pageYOffset" }, function (
      e,
      t
    ) {
      var n = "pageYOffset" === t;
      v.fn[e] = function (r) {
        return H(
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
    v.each(["top", "left"], function (e, t) {
      v.cssHooks[t] = He(p.pixelPosition, function (e, n) {
        if (n)
          return (n = Re(e, t)), Ne.test(n) ? v(e).position()[t] + "px" : n;
      });
    }),
    v.each({ Height: "height", Width: "width" }, function (e, t) {
      v.each({ padding: "inner" + e, content: t, "": "outer" + e }, function (
        n,
        r
      ) {
        v.fn[r] = function (i, a) {
          var o = arguments.length && (n || "boolean" != typeof i),
            s = n || (!0 === i || !0 === a ? "margin" : "border");
          return H(
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
                ? v.css(t, n, s)
                : v.style(t, n, i, s);
            },
            t,
            o ? i : void 0,
            o
          );
        };
      });
    }),
    v.each(
      [
        "ajaxStart",
        "ajaxStop",
        "ajaxComplete",
        "ajaxError",
        "ajaxSuccess",
        "ajaxSend",
      ],
      function (e, t) {
        v.fn[t] = function (e) {
          return this.on(t, e);
        };
      }
    ),
    v.fn.extend({
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
    v.each(
      "blur focus focusin focusout resize scroll click dblclick mousedown mouseup mousemove mouseover mouseout mouseenter mouseleave change select submit keydown keypress keyup contextmenu".split(
        " "
      ),
      function (e, t) {
        v.fn[t] = function (e, n) {
          return arguments.length > 0
            ? this.on(t, null, e, n)
            : this.trigger(t);
        };
      }
    );
  var Vt = /^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g;
  (v.proxy = function (e, t) {
    var n, r, a;
    if (("string" == typeof t && ((n = e[t]), (t = e), (e = n)), h(e)))
      return (
        (r = i.call(arguments, 2)),
        ((a = function () {
          return e.apply(t || this, r.concat(i.call(arguments)));
        }).guid = e.guid = e.guid || v.guid++),
        a
      );
  }),
    (v.holdReady = function (e) {
      e ? v.readyWait++ : v.ready(!0);
    }),
    (v.isArray = Array.isArray),
    (v.parseJSON = JSON.parse),
    (v.nodeName = E),
    (v.isFunction = h),
    (v.isWindow = m),
    (v.camelCase = V),
    (v.type = b),
    (v.now = Date.now),
    (v.isNumeric = function (e) {
      var t = v.type(e);
      return ("number" === t || "string" === t) && !isNaN(e - parseFloat(e));
    }),
    (v.trim = function (e) {
      return null == e ? "" : (e + "").replace(Vt, "");
    }),
    "function" == typeof define &&
      define.amd &&
      define("jquery", [], function () {
        return v;
      });
  var Ut = e.jQuery,
    Xt = e.$;
  return (
    (v.noConflict = function (t) {
      return e.$ === v && (e.$ = Xt), t && e.jQuery === v && (e.jQuery = Ut), v;
    }),
    void 0 === t && (e.jQuery = e.$ = v),
    v
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
    VERSION$1 = "0.7.5",
    GitCommit = "6cb6a0cc9da5f2a40d4afe10d70713d73d44f3bd";
  var VERSION = "1.13.1",
    root =
      ("object" == typeof self && self.self === self && self) ||
      ("object" == typeof global && global.global === global && global) ||
      Function("return this")() ||
      {},
    ArrayProto = Array.prototype,
    ObjProto = Object.prototype,
    SymbolProto = "undefined" != typeof Symbol ? Symbol.prototype : null,
    push = ArrayProto.push,
    slice = ArrayProto.slice,
    toString = ObjProto.toString,
    hasOwnProperty = ObjProto.hasOwnProperty,
    supportsArrayBuffer = "undefined" != typeof ArrayBuffer,
    supportsDataView = "undefined" != typeof DataView,
    nativeIsArray = Array.isArray,
    nativeKeys = Object.keys,
    nativeCreate = Object.create,
    nativeIsView = supportsArrayBuffer && ArrayBuffer.isView,
    _isNaN = isNaN,
    _isFinite = isFinite,
    hasEnumBug = !{ toString: null }.propertyIsEnumerable("toString"),
    nonEnumerableProps = [
      "valueOf",
      "isPrototypeOf",
      "toString",
      "propertyIsEnumerable",
      "hasOwnProperty",
      "toLocaleString",
    ],
    MAX_ARRAY_INDEX = Math.pow(2, 53) - 1;
  function restArguments(e, t) {
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
        var a = Array(t + 1);
        for (i = 0; i < t; i++) a[i] = arguments[i];
        return (a[t] = r), e.apply(this, a);
      }
    );
  }
  function isObject(e) {
    var t = typeof e;
    return "function" === t || ("object" === t && !!e);
  }
  function isNull(e) {
    return null === e;
  }
  function isUndefined(e) {
    return void 0 === e;
  }
  function isBoolean$1(e) {
    return !0 === e || !1 === e || "[object Boolean]" === toString.call(e);
  }
  function isElement(e) {
    return !(!e || 1 !== e.nodeType);
  }
  function tagTester(e) {
    var t = "[object " + e + "]";
    return function (e) {
      return toString.call(e) === t;
    };
  }
  var isString$1 = tagTester("String"),
    isNumber$1 = tagTester("Number"),
    isDate = tagTester("Date"),
    isRegExp = tagTester("RegExp"),
    isError = tagTester("Error"),
    isSymbol$1 = tagTester("Symbol"),
    isArrayBuffer = tagTester("ArrayBuffer"),
    isFunction$1 = tagTester("Function"),
    nodelist = root.document && root.document.childNodes;
  "function" != typeof /./ &&
    "object" != typeof Int8Array &&
    "function" != typeof nodelist &&
    (isFunction$1 = function (e) {
      return "function" == typeof e || !1;
    });
  var isFunction$1$1 = isFunction$1,
    hasObjectTag = tagTester("Object"),
    hasStringTagBug =
      supportsDataView && hasObjectTag(new DataView(new ArrayBuffer(8))),
    isIE11 = "undefined" != typeof Map && hasObjectTag(new Map()),
    isDataView = tagTester("DataView");
  function ie10IsDataView(e) {
    return null != e && isFunction$1$1(e.getInt8) && isArrayBuffer(e.buffer);
  }
  var isDataView$1 = hasStringTagBug ? ie10IsDataView : isDataView,
    isArray = nativeIsArray || tagTester("Array");
  function has$1(e, t) {
    return null != e && hasOwnProperty.call(e, t);
  }
  var isArguments = tagTester("Arguments");
  !(function () {
    isArguments(arguments) ||
      (isArguments = function (e) {
        return has$1(e, "callee");
      });
  })();
  var isArguments$1 = isArguments;
  function isFinite$1(e) {
    return !isSymbol$1(e) && _isFinite(e) && !isNaN(parseFloat(e));
  }
  function isNaN$1(e) {
    return isNumber$1(e) && _isNaN(e);
  }
  function constant(e) {
    return function () {
      return e;
    };
  }
  function createSizePropertyCheck(e) {
    return function (t) {
      var n = e(t);
      return "number" == typeof n && n >= 0 && n <= MAX_ARRAY_INDEX;
    };
  }
  function shallowProperty(e) {
    return function (t) {
      return null == t ? void 0 : t[e];
    };
  }
  var getByteLength = shallowProperty("byteLength"),
    isBufferLike = createSizePropertyCheck(getByteLength),
    typedArrayPattern = /\[object ((I|Ui)nt(8|16|32)|Float(32|64)|Uint8Clamped|Big(I|Ui)nt64)Array\]/;
  function isTypedArray(e) {
    return nativeIsView
      ? nativeIsView(e) && !isDataView$1(e)
      : isBufferLike(e) && typedArrayPattern.test(toString.call(e));
  }
  var isTypedArray$1 = supportsArrayBuffer ? isTypedArray : constant(!1),
    getLength = shallowProperty("length");
  function emulatedSet(e) {
    for (var t = {}, n = e.length, r = 0; r < n; ++r) t[e[r]] = !0;
    return {
      contains: function (e) {
        return t[e];
      },
      push: function (n) {
        return (t[n] = !0), e.push(n);
      },
    };
  }
  function collectNonEnumProps(e, t) {
    t = emulatedSet(t);
    var n = nonEnumerableProps.length,
      r = e.constructor,
      i = (isFunction$1$1(r) && r.prototype) || ObjProto,
      a = "constructor";
    for (has$1(e, a) && !t.contains(a) && t.push(a); n--; )
      (a = nonEnumerableProps[n]) in e &&
        e[a] !== i[a] &&
        !t.contains(a) &&
        t.push(a);
  }
  function keys(e) {
    if (!isObject(e)) return [];
    if (nativeKeys) return nativeKeys(e);
    var t = [];
    for (var n in e) has$1(e, n) && t.push(n);
    return hasEnumBug && collectNonEnumProps(e, t), t;
  }
  function isEmpty(e) {
    if (null == e) return !0;
    var t = getLength(e);
    return "number" == typeof t &&
      (isArray(e) || isString$1(e) || isArguments$1(e))
      ? 0 === t
      : 0 === getLength(keys(e));
  }
  function isMatch(e, t) {
    var n = keys(t),
      r = n.length;
    if (null == e) return !r;
    for (var i = Object(e), a = 0; a < r; a++) {
      var o = n[a];
      if (t[o] !== i[o] || !(o in i)) return !1;
    }
    return !0;
  }
  function _$1(e) {
    return e instanceof _$1
      ? e
      : this instanceof _$1
      ? void (this._wrapped = e)
      : new _$1(e);
  }
  function toBufferView(e) {
    return new Uint8Array(e.buffer || e, e.byteOffset || 0, getByteLength(e));
  }
  (_$1.VERSION = VERSION),
    (_$1.prototype.value = function () {
      return this._wrapped;
    }),
    (_$1.prototype.valueOf = _$1.prototype.toJSON = _$1.prototype.value),
    (_$1.prototype.toString = function () {
      return String(this._wrapped);
    });
  var tagDataView = "[object DataView]";
  function eq$1(e, t, n, r) {
    if (e === t) return 0 !== e || 1 / e == 1 / t;
    if (null == e || null == t) return !1;
    if (e != e) return t != t;
    var i = typeof e;
    return (
      ("function" === i || "object" === i || "object" == typeof t) &&
      deepEq(e, t, n, r)
    );
  }
  function deepEq(e, t, n, r) {
    e instanceof _$1 && (e = e._wrapped), t instanceof _$1 && (t = t._wrapped);
    var i = toString.call(e);
    if (i !== toString.call(t)) return !1;
    if (hasStringTagBug && "[object Object]" == i && isDataView$1(e)) {
      if (!isDataView$1(t)) return !1;
      i = tagDataView;
    }
    switch (i) {
      case "[object RegExp]":
      case "[object String]":
        return "" + e == "" + t;
      case "[object Number]":
        return +e != +e ? +t != +t : 0 == +e ? 1 / +e == 1 / t : +e == +t;
      case "[object Date]":
      case "[object Boolean]":
        return +e == +t;
      case "[object Symbol]":
        return SymbolProto.valueOf.call(e) === SymbolProto.valueOf.call(t);
      case "[object ArrayBuffer]":
      case tagDataView:
        return deepEq(toBufferView(e), toBufferView(t), n, r);
    }
    var a = "[object Array]" === i;
    if (!a && isTypedArray$1(e)) {
      if (getByteLength(e) !== getByteLength(t)) return !1;
      if (e.buffer === t.buffer && e.byteOffset === t.byteOffset) return !0;
      a = !0;
    }
    if (!a) {
      if ("object" != typeof e || "object" != typeof t) return !1;
      var o = e.constructor,
        s = t.constructor;
      if (
        o !== s &&
        !(
          isFunction$1$1(o) &&
          o instanceof o &&
          isFunction$1$1(s) &&
          s instanceof s
        ) &&
        "constructor" in e &&
        "constructor" in t
      )
        return !1;
    }
    r = r || [];
    for (var u = (n = n || []).length; u--; ) if (n[u] === e) return r[u] === t;
    if ((n.push(e), r.push(t), a)) {
      if ((u = e.length) !== t.length) return !1;
      for (; u--; ) if (!eq$1(e[u], t[u], n, r)) return !1;
    } else {
      var c,
        l = keys(e);
      if (((u = l.length), keys(t).length !== u)) return !1;
      for (; u--; )
        if (!has$1(t, (c = l[u])) || !eq$1(e[c], t[c], n, r)) return !1;
    }
    return n.pop(), r.pop(), !0;
  }
  function isEqual(e, t) {
    return eq$1(e, t);
  }
  function allKeys(e) {
    if (!isObject(e)) return [];
    var t = [];
    for (var n in e) t.push(n);
    return hasEnumBug && collectNonEnumProps(e, t), t;
  }
  function ie11fingerprint(e) {
    var t = getLength(e);
    return function (n) {
      if (null == n) return !1;
      var r = allKeys(n);
      if (getLength(r)) return !1;
      for (var i = 0; i < t; i++) if (!isFunction$1$1(n[e[i]])) return !1;
      return e !== weakMapMethods || !isFunction$1$1(n[forEachName]);
    };
  }
  var forEachName = "forEach",
    hasName = "has",
    commonInit = ["clear", "delete"],
    mapTail = ["get", hasName, "set"],
    mapMethods = commonInit.concat(forEachName, mapTail),
    weakMapMethods = commonInit.concat(mapTail),
    setMethods = ["add"].concat(commonInit, forEachName, hasName),
    isMap = isIE11 ? ie11fingerprint(mapMethods) : tagTester("Map"),
    isWeakMap = isIE11 ? ie11fingerprint(weakMapMethods) : tagTester("WeakMap"),
    isSet = isIE11 ? ie11fingerprint(setMethods) : tagTester("Set"),
    isWeakSet = tagTester("WeakSet");
  function values(e) {
    for (var t = keys(e), n = t.length, r = Array(n), i = 0; i < n; i++)
      r[i] = e[t[i]];
    return r;
  }
  function pairs(e) {
    for (var t = keys(e), n = t.length, r = Array(n), i = 0; i < n; i++)
      r[i] = [t[i], e[t[i]]];
    return r;
  }
  function invert(e) {
    for (var t = {}, n = keys(e), r = 0, i = n.length; r < i; r++)
      t[e[n[r]]] = n[r];
    return t;
  }
  function functions(e) {
    var t = [];
    for (var n in e) isFunction$1$1(e[n]) && t.push(n);
    return t.sort();
  }
  function createAssigner(e, t) {
    return function (n) {
      var r = arguments.length;
      if ((t && (n = Object(n)), r < 2 || null == n)) return n;
      for (var i = 1; i < r; i++)
        for (var a = arguments[i], o = e(a), s = o.length, u = 0; u < s; u++) {
          var c = o[u];
          (t && void 0 !== n[c]) || (n[c] = a[c]);
        }
      return n;
    };
  }
  var extend = createAssigner(allKeys),
    extendOwn = createAssigner(keys),
    defaults = createAssigner(allKeys, !0);
  function ctor() {
    return function () {};
  }
  function baseCreate(e) {
    if (!isObject(e)) return {};
    if (nativeCreate) return nativeCreate(e);
    var t = ctor();
    t.prototype = e;
    var n = new t();
    return (t.prototype = null), n;
  }
  function create(e, t) {
    var n = baseCreate(e);
    return t && extendOwn(n, t), n;
  }
  function clone(e) {
    return isObject(e) ? (isArray(e) ? e.slice() : extend({}, e)) : e;
  }
  function tap(e, t) {
    return t(e), e;
  }
  function toPath$1(e) {
    return isArray(e) ? e : [e];
  }
  function toPath(e) {
    return _$1.toPath(e);
  }
  function deepGet(e, t) {
    for (var n = t.length, r = 0; r < n; r++) {
      if (null == e) return;
      e = e[t[r]];
    }
    return n ? e : void 0;
  }
  function get(e, t, n) {
    var r = deepGet(e, toPath(t));
    return isUndefined(r) ? n : r;
  }
  function has(e, t) {
    for (var n = (t = toPath(t)).length, r = 0; r < n; r++) {
      var i = t[r];
      if (!has$1(e, i)) return !1;
      e = e[i];
    }
    return !!n;
  }
  function identity(e) {
    return e;
  }
  function matcher(e) {
    return (
      (e = extendOwn({}, e)),
      function (t) {
        return isMatch(t, e);
      }
    );
  }
  function property(e) {
    return (
      (e = toPath(e)),
      function (t) {
        return deepGet(t, e);
      }
    );
  }
  function optimizeCb(e, t, n) {
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
        return function (n, r, i, a) {
          return e.call(t, n, r, i, a);
        };
    }
    return function () {
      return e.apply(t, arguments);
    };
  }
  function baseIteratee(e, t, n) {
    return null == e
      ? identity
      : isFunction$1$1(e)
      ? optimizeCb(e, t, n)
      : isObject(e) && !isArray(e)
      ? matcher(e)
      : property(e);
  }
  function iteratee(e, t) {
    return baseIteratee(e, t, 1 / 0);
  }
  function cb(e, t, n) {
    return _$1.iteratee !== iteratee
      ? _$1.iteratee(e, t)
      : baseIteratee(e, t, n);
  }
  function mapObject(e, t, n) {
    t = cb(t, n);
    for (var r = keys(e), i = r.length, a = {}, o = 0; o < i; o++) {
      var s = r[o];
      a[s] = t(e[s], s, e);
    }
    return a;
  }
  function noop() {}
  function propertyOf(e) {
    return null == e
      ? noop
      : function (t) {
          return get(e, t);
        };
  }
  function times(e, t, n) {
    var r = Array(Math.max(0, e));
    t = optimizeCb(t, n, 1);
    for (var i = 0; i < e; i++) r[i] = t(i);
    return r;
  }
  function random(e, t) {
    return (
      null == t && ((t = e), (e = 0)),
      e + Math.floor(Math.random() * (t - e + 1))
    );
  }
  (_$1.toPath = toPath$1), (_$1.iteratee = iteratee);
  var now =
    Date.now ||
    function () {
      return new Date().getTime();
    };
  function createEscaper(e) {
    var t = function (t) {
        return e[t];
      },
      n = "(?:" + keys(e).join("|") + ")",
      r = RegExp(n),
      i = RegExp(n, "g");
    return function (e) {
      return (e = null == e ? "" : "" + e), r.test(e) ? e.replace(i, t) : e;
    };
  }
  var escapeMap = {
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#x27;",
      "`": "&#x60;",
    },
    _escape = createEscaper(escapeMap),
    unescapeMap = invert(escapeMap),
    _unescape = createEscaper(unescapeMap),
    templateSettings = (_$1.templateSettings = {
      evaluate: /<%([\s\S]+?)%>/g,
      interpolate: /<%=([\s\S]+?)%>/g,
      escape: /<%-([\s\S]+?)%>/g,
    }),
    noMatch = /(.)^/,
    escapes = {
      "'": "'",
      "\\": "\\",
      "\r": "r",
      "\n": "n",
      "\u2028": "u2028",
      "\u2029": "u2029",
    },
    escapeRegExp = /\\|'|\r|\n|\u2028|\u2029/g;
  function escapeChar(e) {
    return "\\" + escapes[e];
  }
  var bareIdentifier = /^\s*(\w|\$)+\s*$/;
  function template(e, t, n) {
    !t && n && (t = n), (t = defaults({}, t, _$1.templateSettings));
    var r = RegExp(
        [
          (t.escape || noMatch).source,
          (t.interpolate || noMatch).source,
          (t.evaluate || noMatch).source,
        ].join("|") + "|$",
        "g"
      ),
      i = 0,
      a = "__p+='";
    e.replace(r, function (t, n, r, o, s) {
      return (
        (a += e.slice(i, s).replace(escapeRegExp, escapeChar)),
        (i = s + t.length),
        n
          ? (a += "'+\n((__t=(" + n + "))==null?'':_.escape(__t))+\n'")
          : r
          ? (a += "'+\n((__t=(" + r + "))==null?'':__t)+\n'")
          : o && (a += "';\n" + o + "\n__p+='"),
        t
      );
    }),
      (a += "';\n");
    var o,
      s = t.variable;
    if (s) {
      if (!bareIdentifier.test(s))
        throw new Error("variable is not a bare identifier: " + s);
    } else (a = "with(obj||{}){\n" + a + "}\n"), (s = "obj");
    a =
      "var __t,__p='',__j=Array.prototype.join,print=function(){__p+=__j.call(arguments,'');};\n" +
      a +
      "return __p;\n";
    try {
      o = new Function(s, "_", a);
    } catch (e) {
      throw ((e.source = a), e);
    }
    var u = function (e) {
      return o.call(this, e, _$1);
    };
    return (u.source = "function(" + s + "){\n" + a + "}"), u;
  }
  function result(e, t, n) {
    var r = (t = toPath(t)).length;
    if (!r) return isFunction$1$1(n) ? n.call(e) : n;
    for (var i = 0; i < r; i++) {
      var a = null == e ? void 0 : e[t[i]];
      void 0 === a && ((a = n), (i = r)),
        (e = isFunction$1$1(a) ? a.call(e) : a);
    }
    return e;
  }
  var idCounter = 0;
  function uniqueId(e) {
    var t = ++idCounter + "";
    return e ? e + t : t;
  }
  function chain(e) {
    var t = _$1(e);
    return (t._chain = !0), t;
  }
  function executeBound(e, t, n, r, i) {
    if (!(r instanceof t)) return e.apply(n, i);
    var a = baseCreate(e.prototype),
      o = e.apply(a, i);
    return isObject(o) ? o : a;
  }
  var partial = restArguments(function (e, t) {
    var n = partial.placeholder,
      r = function () {
        for (var i = 0, a = t.length, o = Array(a), s = 0; s < a; s++)
          o[s] = t[s] === n ? arguments[i++] : t[s];
        for (; i < arguments.length; ) o.push(arguments[i++]);
        return executeBound(e, r, this, this, o);
      };
    return r;
  });
  partial.placeholder = _$1;
  var bind = restArguments(function (e, t, n) {
      if (!isFunction$1$1(e))
        throw new TypeError("Bind must be called on a function");
      var r = restArguments(function (i) {
        return executeBound(e, r, t, this, n.concat(i));
      });
      return r;
    }),
    isArrayLike = createSizePropertyCheck(getLength);
  function flatten$1(e, t, n, r) {
    if (((r = r || []), t || 0 === t)) {
      if (t <= 0) return r.concat(e);
    } else t = 1 / 0;
    for (var i = r.length, a = 0, o = getLength(e); a < o; a++) {
      var s = e[a];
      if (isArrayLike(s) && (isArray(s) || isArguments$1(s)))
        if (t > 1) flatten$1(s, t - 1, n, r), (i = r.length);
        else for (var u = 0, c = s.length; u < c; ) r[i++] = s[u++];
      else n || (r[i++] = s);
    }
    return r;
  }
  var bindAll = restArguments(function (e, t) {
    var n = (t = flatten$1(t, !1, !1)).length;
    if (n < 1) throw new Error("bindAll must be passed function names");
    for (; n--; ) {
      var r = t[n];
      e[r] = bind(e[r], e);
    }
    return e;
  });
  function memoize$1(e, t) {
    var n = function (r) {
      var i = n.cache,
        a = "" + (t ? t.apply(this, arguments) : r);
      return has$1(i, a) || (i[a] = e.apply(this, arguments)), i[a];
    };
    return (n.cache = {}), n;
  }
  var delay = restArguments(function (e, t, n) {
      return setTimeout(function () {
        return e.apply(null, n);
      }, t);
    }),
    defer = partial(delay, _$1, 1);
  function throttle(e, t, n) {
    var r,
      i,
      a,
      o,
      s = 0;
    n || (n = {});
    var u = function () {
        (s = !1 === n.leading ? 0 : now()),
          (r = null),
          (o = e.apply(i, a)),
          r || (i = a = null);
      },
      c = function () {
        var c = now();
        s || !1 !== n.leading || (s = c);
        var l = t - (c - s);
        return (
          (i = this),
          (a = arguments),
          l <= 0 || l > t
            ? (r && (clearTimeout(r), (r = null)),
              (s = c),
              (o = e.apply(i, a)),
              r || (i = a = null))
            : r || !1 === n.trailing || (r = setTimeout(u, l)),
          o
        );
      };
    return (
      (c.cancel = function () {
        clearTimeout(r), (s = 0), (r = i = a = null);
      }),
      c
    );
  }
  function debounce(e, t, n) {
    var r,
      i,
      a,
      o,
      s,
      u = function () {
        var c = now() - i;
        t > c
          ? (r = setTimeout(u, t - c))
          : ((r = null), n || (o = e.apply(s, a)), r || (a = s = null));
      },
      c = restArguments(function (c) {
        return (
          (s = this),
          (a = c),
          (i = now()),
          r || ((r = setTimeout(u, t)), n && (o = e.apply(s, a))),
          o
        );
      });
    return (
      (c.cancel = function () {
        clearTimeout(r), (r = a = s = null);
      }),
      c
    );
  }
  function wrap(e, t) {
    return partial(t, e);
  }
  function negate(e) {
    return function () {
      return !e.apply(this, arguments);
    };
  }
  function compose() {
    var e = arguments,
      t = e.length - 1;
    return function () {
      for (var n = t, r = e[t].apply(this, arguments); n--; )
        r = e[n].call(this, r);
      return r;
    };
  }
  function after(e, t) {
    return function () {
      if (--e < 1) return t.apply(this, arguments);
    };
  }
  function before(e, t) {
    var n;
    return function () {
      return --e > 0 && (n = t.apply(this, arguments)), e <= 1 && (t = null), n;
    };
  }
  var once = partial(before, 2);
  function findKey(e, t, n) {
    t = cb(t, n);
    for (var r, i = keys(e), a = 0, o = i.length; a < o; a++)
      if (t(e[(r = i[a])], r, e)) return r;
  }
  function createPredicateIndexFinder(e) {
    return function (t, n, r) {
      n = cb(n, r);
      for (var i = getLength(t), a = e > 0 ? 0 : i - 1; a >= 0 && a < i; a += e)
        if (n(t[a], a, t)) return a;
      return -1;
    };
  }
  var findIndex = createPredicateIndexFinder(1),
    findLastIndex = createPredicateIndexFinder(-1);
  function sortedIndex(e, t, n, r) {
    for (var i = (n = cb(n, r, 1))(t), a = 0, o = getLength(e); a < o; ) {
      var s = Math.floor((a + o) / 2);
      n(e[s]) < i ? (a = s + 1) : (o = s);
    }
    return a;
  }
  function createIndexFinder(e, t, n) {
    return function (r, i, a) {
      var o = 0,
        s = getLength(r);
      if ("number" == typeof a)
        e > 0
          ? (o = a >= 0 ? a : Math.max(a + s, o))
          : (s = a >= 0 ? Math.min(a + 1, s) : a + s + 1);
      else if (n && a && s) return r[(a = n(r, i))] === i ? a : -1;
      if (i != i)
        return (a = t(slice.call(r, o, s), isNaN$1)) >= 0 ? a + o : -1;
      for (a = e > 0 ? o : s - 1; a >= 0 && a < s; a += e)
        if (r[a] === i) return a;
      return -1;
    };
  }
  var indexOf = createIndexFinder(1, findIndex, sortedIndex),
    lastIndexOf = createIndexFinder(-1, findLastIndex);
  function find(e, t, n) {
    var r = (isArrayLike(e) ? findIndex : findKey)(e, t, n);
    if (void 0 !== r && -1 !== r) return e[r];
  }
  function findWhere(e, t) {
    return find(e, matcher(t));
  }
  function each(e, t, n) {
    var r, i;
    if (((t = optimizeCb(t, n)), isArrayLike(e)))
      for (r = 0, i = e.length; r < i; r++) t(e[r], r, e);
    else {
      var a = keys(e);
      for (r = 0, i = a.length; r < i; r++) t(e[a[r]], a[r], e);
    }
    return e;
  }
  function map(e, t, n) {
    t = cb(t, n);
    for (
      var r = !isArrayLike(e) && keys(e),
        i = (r || e).length,
        a = Array(i),
        o = 0;
      o < i;
      o++
    ) {
      var s = r ? r[o] : o;
      a[o] = t(e[s], s, e);
    }
    return a;
  }
  function createReduce(e) {
    var t = function (t, n, r, i) {
      var a = !isArrayLike(t) && keys(t),
        o = (a || t).length,
        s = e > 0 ? 0 : o - 1;
      for (i || ((r = t[a ? a[s] : s]), (s += e)); s >= 0 && s < o; s += e) {
        var u = a ? a[s] : s;
        r = n(r, t[u], u, t);
      }
      return r;
    };
    return function (e, n, r, i) {
      var a = arguments.length >= 3;
      return t(e, optimizeCb(n, i, 4), r, a);
    };
  }
  var reduce = createReduce(1),
    reduceRight = createReduce(-1);
  function filter(e, t, n) {
    var r = [];
    return (
      (t = cb(t, n)),
      each(e, function (e, n, i) {
        t(e, n, i) && r.push(e);
      }),
      r
    );
  }
  function reject(e, t, n) {
    return filter(e, negate(cb(t)), n);
  }
  function every(e, t, n) {
    t = cb(t, n);
    for (
      var r = !isArrayLike(e) && keys(e), i = (r || e).length, a = 0;
      a < i;
      a++
    ) {
      var o = r ? r[a] : a;
      if (!t(e[o], o, e)) return !1;
    }
    return !0;
  }
  function some(e, t, n) {
    t = cb(t, n);
    for (
      var r = !isArrayLike(e) && keys(e), i = (r || e).length, a = 0;
      a < i;
      a++
    ) {
      var o = r ? r[a] : a;
      if (t(e[o], o, e)) return !0;
    }
    return !1;
  }
  function contains(e, t, n, r) {
    return (
      isArrayLike(e) || (e = values(e)),
      ("number" != typeof n || r) && (n = 0),
      indexOf(e, t, n) >= 0
    );
  }
  var invoke = restArguments(function (e, t, n) {
    var r, i;
    return (
      isFunction$1$1(t)
        ? (i = t)
        : ((t = toPath(t)), (r = t.slice(0, -1)), (t = t[t.length - 1])),
      map(e, function (e) {
        var a = i;
        if (!a) {
          if ((r && r.length && (e = deepGet(e, r)), null == e)) return;
          a = e[t];
        }
        return null == a ? a : a.apply(e, n);
      })
    );
  });
  function pluck(e, t) {
    return map(e, property(t));
  }
  function where(e, t) {
    return filter(e, matcher(t));
  }
  function max(e, t, n) {
    var r,
      i,
      a = -1 / 0,
      o = -1 / 0;
    if (
      null == t ||
      ("number" == typeof t && "object" != typeof e[0] && null != e)
    )
      for (
        var s = 0, u = (e = isArrayLike(e) ? e : values(e)).length;
        s < u;
        s++
      )
        null != (r = e[s]) && r > a && (a = r);
    else
      (t = cb(t, n)),
        each(e, function (e, n, r) {
          ((i = t(e, n, r)) > o || (i === -1 / 0 && a === -1 / 0)) &&
            ((a = e), (o = i));
        });
    return a;
  }
  function min(e, t, n) {
    var r,
      i,
      a = 1 / 0,
      o = 1 / 0;
    if (
      null == t ||
      ("number" == typeof t && "object" != typeof e[0] && null != e)
    )
      for (
        var s = 0, u = (e = isArrayLike(e) ? e : values(e)).length;
        s < u;
        s++
      )
        null != (r = e[s]) && r < a && (a = r);
    else
      (t = cb(t, n)),
        each(e, function (e, n, r) {
          ((i = t(e, n, r)) < o || (i === 1 / 0 && a === 1 / 0)) &&
            ((a = e), (o = i));
        });
    return a;
  }
  function sample(e, t, n) {
    if (null == t || n)
      return isArrayLike(e) || (e = values(e)), e[random(e.length - 1)];
    var r = isArrayLike(e) ? clone(e) : values(e),
      i = getLength(r);
    t = Math.max(Math.min(t, i), 0);
    for (var a = i - 1, o = 0; o < t; o++) {
      var s = random(o, a),
        u = r[o];
      (r[o] = r[s]), (r[s] = u);
    }
    return r.slice(0, t);
  }
  function shuffle(e) {
    return sample(e, 1 / 0);
  }
  function sortBy(e, t, n) {
    var r = 0;
    return (
      (t = cb(t, n)),
      pluck(
        map(e, function (e, n, i) {
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
  }
  function group(e, t) {
    return function (n, r, i) {
      var a = t ? [[], []] : {};
      return (
        (r = cb(r, i)),
        each(n, function (t, i) {
          var o = r(t, i, n);
          e(a, t, o);
        }),
        a
      );
    };
  }
  var groupBy = group(function (e, t, n) {
      has$1(e, n) ? e[n].push(t) : (e[n] = [t]);
    }),
    indexBy = group(function (e, t, n) {
      e[n] = t;
    }),
    countBy = group(function (e, t, n) {
      has$1(e, n) ? e[n]++ : (e[n] = 1);
    }),
    partition = group(function (e, t, n) {
      e[n ? 0 : 1].push(t);
    }, !0),
    reStrSymbol = /[^\ud800-\udfff]|[\ud800-\udbff][\udc00-\udfff]|[\ud800-\udfff]/g;
  function toArray(e) {
    return e
      ? isArray(e)
        ? slice.call(e)
        : isString$1(e)
        ? e.match(reStrSymbol)
        : isArrayLike(e)
        ? map(e, identity)
        : values(e)
      : [];
  }
  function size(e) {
    return null == e ? 0 : isArrayLike(e) ? e.length : keys(e).length;
  }
  function keyInObj(e, t, n) {
    return t in n;
  }
  var pick = restArguments(function (e, t) {
      var n = {},
        r = t[0];
      if (null == e) return n;
      isFunction$1$1(r)
        ? (t.length > 1 && (r = optimizeCb(r, t[1])), (t = allKeys(e)))
        : ((r = keyInObj), (t = flatten$1(t, !1, !1)), (e = Object(e)));
      for (var i = 0, a = t.length; i < a; i++) {
        var o = t[i],
          s = e[o];
        r(s, o, e) && (n[o] = s);
      }
      return n;
    }),
    omit = restArguments(function (e, t) {
      var n,
        r = t[0];
      return (
        isFunction$1$1(r)
          ? ((r = negate(r)), t.length > 1 && (n = t[1]))
          : ((t = map(flatten$1(t, !1, !1), String)),
            (r = function (e, n) {
              return !contains(t, n);
            })),
        pick(e, r, n)
      );
    });
  function initial(e, t, n) {
    return slice.call(e, 0, Math.max(0, e.length - (null == t || n ? 1 : t)));
  }
  function first(e, t, n) {
    return null == e || e.length < 1
      ? null == t || n
        ? void 0
        : []
      : null == t || n
      ? e[0]
      : initial(e, e.length - t);
  }
  function rest(e, t, n) {
    return slice.call(e, null == t || n ? 1 : t);
  }
  function last(e, t, n) {
    return null == e || e.length < 1
      ? null == t || n
        ? void 0
        : []
      : null == t || n
      ? e[e.length - 1]
      : rest(e, Math.max(0, e.length - t));
  }
  function compact(e) {
    return filter(e, Boolean);
  }
  function flatten(e, t) {
    return flatten$1(e, t, !1);
  }
  var difference = restArguments(function (e, t) {
      return (
        (t = flatten$1(t, !0, !0)),
        filter(e, function (e) {
          return !contains(t, e);
        })
      );
    }),
    without = restArguments(function (e, t) {
      return difference(e, t);
    });
  function uniq(e, t, n, r) {
    isBoolean$1(t) || ((r = n), (n = t), (t = !1)), null != n && (n = cb(n, r));
    for (var i = [], a = [], o = 0, s = getLength(e); o < s; o++) {
      var u = e[o],
        c = n ? n(u, o, e) : u;
      t && !n
        ? ((o && a === c) || i.push(u), (a = c))
        : n
        ? contains(a, c) || (a.push(c), i.push(u))
        : contains(i, u) || i.push(u);
    }
    return i;
  }
  var union = restArguments(function (e) {
    return uniq(flatten$1(e, !0, !0));
  });
  function intersection(e) {
    for (
      var t = [], n = arguments.length, r = 0, i = getLength(e);
      r < i;
      r++
    ) {
      var a = e[r];
      if (!contains(t, a)) {
        var o;
        for (o = 1; o < n && contains(arguments[o], a); o++);
        o === n && t.push(a);
      }
    }
    return t;
  }
  function unzip(e) {
    for (
      var t = (e && max(e, getLength).length) || 0, n = Array(t), r = 0;
      r < t;
      r++
    )
      n[r] = pluck(e, r);
    return n;
  }
  var zip = restArguments(unzip);
  function object(e, t) {
    for (var n = {}, r = 0, i = getLength(e); r < i; r++)
      t ? (n[e[r]] = t[r]) : (n[e[r][0]] = e[r][1]);
    return n;
  }
  function range(e, t, n) {
    null == t && ((t = e || 0), (e = 0)), n || (n = t < e ? -1 : 1);
    for (
      var r = Math.max(Math.ceil((t - e) / n), 0), i = Array(r), a = 0;
      a < r;
      a++, e += n
    )
      i[a] = e;
    return i;
  }
  function chunk(e, t) {
    if (null == t || t < 1) return [];
    for (var n = [], r = 0, i = e.length; r < i; )
      n.push(slice.call(e, r, (r += t)));
    return n;
  }
  function chainResult(e, t) {
    return e._chain ? _$1(t).chain() : t;
  }
  function mixin(e) {
    return (
      each(functions(e), function (t) {
        var n = (_$1[t] = e[t]);
        _$1.prototype[t] = function () {
          var e = [this._wrapped];
          return push.apply(e, arguments), chainResult(this, n.apply(_$1, e));
        };
      }),
      _$1
    );
  }
  each(
    ["pop", "push", "reverse", "shift", "sort", "splice", "unshift"],
    function (e) {
      var t = ArrayProto[e];
      _$1.prototype[e] = function () {
        var n = this._wrapped;
        return (
          null != n &&
            (t.apply(n, arguments),
            ("shift" !== e && "splice" !== e) || 0 !== n.length || delete n[0]),
          chainResult(this, n)
        );
      };
    }
  ),
    each(["concat", "join", "slice"], function (e) {
      var t = ArrayProto[e];
      _$1.prototype[e] = function () {
        var e = this._wrapped;
        return null != e && (e = t.apply(e, arguments)), chainResult(this, e);
      };
    });
  var allExports = {
      __proto__: null,
      VERSION: VERSION,
      restArguments: restArguments,
      isObject: isObject,
      isNull: isNull,
      isUndefined: isUndefined,
      isBoolean: isBoolean$1,
      isElement: isElement,
      isString: isString$1,
      isNumber: isNumber$1,
      isDate: isDate,
      isRegExp: isRegExp,
      isError: isError,
      isSymbol: isSymbol$1,
      isArrayBuffer: isArrayBuffer,
      isDataView: isDataView$1,
      isArray: isArray,
      isFunction: isFunction$1$1,
      isArguments: isArguments$1,
      isFinite: isFinite$1,
      isNaN: isNaN$1,
      isTypedArray: isTypedArray$1,
      isEmpty: isEmpty,
      isMatch: isMatch,
      isEqual: isEqual,
      isMap: isMap,
      isWeakMap: isWeakMap,
      isSet: isSet,
      isWeakSet: isWeakSet,
      keys: keys,
      allKeys: allKeys,
      values: values,
      pairs: pairs,
      invert: invert,
      functions: functions,
      methods: functions,
      extend: extend,
      extendOwn: extendOwn,
      assign: extendOwn,
      defaults: defaults,
      create: create,
      clone: clone,
      tap: tap,
      get: get,
      has: has,
      mapObject: mapObject,
      identity: identity,
      constant: constant,
      noop: noop,
      toPath: toPath$1,
      property: property,
      propertyOf: propertyOf,
      matcher: matcher,
      matches: matcher,
      times: times,
      random: random,
      now: now,
      escape: _escape,
      unescape: _unescape,
      templateSettings: templateSettings,
      template: template,
      result: result,
      uniqueId: uniqueId,
      chain: chain,
      iteratee: iteratee,
      partial: partial,
      bind: bind,
      bindAll: bindAll,
      memoize: memoize$1,
      delay: delay,
      defer: defer,
      throttle: throttle,
      debounce: debounce,
      wrap: wrap,
      negate: negate,
      compose: compose,
      after: after,
      before: before,
      once: once,
      findKey: findKey,
      findIndex: findIndex,
      findLastIndex: findLastIndex,
      sortedIndex: sortedIndex,
      indexOf: indexOf,
      lastIndexOf: lastIndexOf,
      find: find,
      detect: find,
      findWhere: findWhere,
      each: each,
      forEach: each,
      map: map,
      collect: map,
      reduce: reduce,
      foldl: reduce,
      inject: reduce,
      reduceRight: reduceRight,
      foldr: reduceRight,
      filter: filter,
      select: filter,
      reject: reject,
      every: every,
      all: every,
      some: some,
      any: some,
      contains: contains,
      includes: contains,
      include: contains,
      invoke: invoke,
      pluck: pluck,
      where: where,
      max: max,
      min: min,
      shuffle: shuffle,
      sample: sample,
      sortBy: sortBy,
      groupBy: groupBy,
      indexBy: indexBy,
      countBy: countBy,
      partition: partition,
      toArray: toArray,
      size: size,
      pick: pick,
      omit: omit,
      first: first,
      head: first,
      take: first,
      initial: initial,
      last: last,
      rest: rest,
      tail: rest,
      drop: rest,
      compact: compact,
      flatten: flatten,
      without: without,
      uniq: uniq,
      unique: uniq,
      union: union,
      intersection: intersection,
      difference: difference,
      unzip: unzip,
      transpose: unzip,
      zip: zip,
      object: object,
      range: range,
      chunk: chunk,
      mixin: mixin,
      default: _$1,
    },
    _ = mixin(allExports);
  _._ = _;
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
      super(`${e}${null === t ? "" : ": " + to_write$1(t)}`), (this.form = t);
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
      return this._get(["cdr", "car"], e);
    }
    cddr(e) {
      return this._get(["cdr", "cdr"], e);
    }
    _get(e, t) {
      let n = this;
      return (
        e.forEach((e) => {
          if (n.hasOwnProperty(e)) return n[e];
          if (t) throw t;
        }),
        n
      );
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
    to_set() {
      for (var e = new BiwaSet(), t = this; t instanceof Pair; t = t.cdr)
        e.add(t.car);
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
        a = function (o) {
          if (r) {
            var s = t.result(o[0], i);
            if (void 0 !== s) return s;
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
        o = t[0],
        s = t[1];
      return null != (a = o.index(e))
        ? n(a)
        : null != (a = s.index(e))
        ? r(a)
        : i(e.name);
    }
    make_boxes(e, t, n) {
      t = t;
      for (var r = 0, i = []; t instanceof Pair; )
        e.member(t.car) && i.push(r), r++, (t = t.cdr);
      for (var a = n, o = i.length - 1; o >= 0; o--) a = ["box", i[o], a];
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
                ? this.find_sets(a, t.set_minus(i.to_set()))
                : this.find_sets(a, t.set_minus(new BiwaSet(i)));
            break;
          case Sym("if"):
            var o = e.second(),
              s = e.third(),
              u = e.fourth();
            n = this.find_sets(o, t).set_union(
              this.find_sets(s, t),
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
              o = e.cdr.cdr;
            r =
              a instanceof Pair
                ? this.find_free(o, t.set_union(a.to_set()), n)
                : this.find_free(o, t.set_cons(a), n);
            break;
          case Sym("if"):
            var s = e.second(),
              u = e.third(),
              c = e.fourth();
            r = this.find_free(s, t, n).set_union(
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
            for (var o = [], s = e.cdr; s instanceof Pair; s = s.cdr)
              o.push(s.car);
            for (var u = i, c = o.length - 1; c >= 0; c--)
              u = this.compile(o[c], t, n, r, u);
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
            var p = e.second(),
              h =
                ((e = e.third()),
                this.compile_lookup(
                  p,
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
            i = h;
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
            for (s = _; s instanceof Pair; s = s.cdr)
              u = this.compile(s.car, t, n, r, ["argument", u]);
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
        o = e.cdr.cdr,
        s = Compiler.transform_internal_define(o);
      if (isPair(s) && isSymbol(s.car) && "letrec*" == s.car.name)
        var u = Compiler.expand(s);
      else u = new Pair(Sym("begin"), e.cdr.cdr);
      var c = this.find_dot_pos(a),
        l = this.dotted2proper(a),
        f = this.find_free(u, l.to_set(), r),
        d = this.find_sets(u, l.to_set()),
        p = this.compile(
          u,
          [l.to_set(), f],
          d.set_union(n.set_intersect(f)),
          r.set_union(l.to_set()),
          ["return"]
        ),
        h = [
          "close",
          a instanceof Pair ? a.length() : 0,
          f.size(),
          this.make_boxes(d, l, p),
          i,
          c,
        ];
      return this.collect_free(f, t, h);
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
            var o = e.cdr.car,
              s = e.cdr.cdr;
            r = new Pair(Sym("lambda"), new Pair(o, n(s, t)));
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
              var p;
              for (
                t.modified = !0, r = d.transform(e);
                (r = n(r, (p = {}))), p.modified;

              );
            } else {
              var h,
                m = n(e.car, t);
              if (!(e.cdr instanceof Pair) && e.cdr !== nil)
                throw new BiwaError(
                  "proper list required for function application or macro use: " +
                    to_write(e)
                );
              (h = array_to_list(
                e.cdr.to_array().map(function (e) {
                  return n(e, t);
                })
              )),
                (r = new Pair(m, h));
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
        Object.keys(this.pairs_of).forEach(
          bind(function (e) {
            var n = this.pairs_of[e].map(function (e) {
              return [...e];
            });
            t.pairs_of[e] = n;
          }, this)
        ),
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
        values(this.pairs_of).forEach(function (n) {
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
  class Parser {
    constructor(e) {
      (this.tokens = this.tokenize(e)),
        (this.i = 0),
        (this.sexpCommentMarker = new Object());
    }
    insert(e) {
      this.tokens.splice(this.i, 0, ...this.tokenize(e));
    }
    inspect() {
      return [
        "#<Parser:",
        this.i,
        "/",
        this.tokens.length,
        " ",
        inspect(this.tokens),
        ">",
      ].join("");
    }
    tokenize(e) {
      for (var t = new Array(), n = null, r = 0; "" != e && n != e; )
        (n = e),
          (e = e.replace(
            /^\s*(;[^\r\n]*(\r|\n|$)|#;|#\||#\\[^\w]|#?(\(|\[|{)|\)|\]|}|\'|`|,@|,|\+inf\.0|-inf\.0|\+nan\.0|\"(\\(.|$)|[^\"\\])*(\"|$)|\|(\\(.|$)|[^\|\\])*(\||$)|[^\s()\[\]{}]+)/,
            function (e, n) {
              var i = n;
              if ("#|" == i) return r++, "";
              if (r > 0) {
                if (/(.*\|#)/.test(i)) {
                  if (--r < 0)
                    throw new BiwaError(
                      "Found an extra comment terminator: `|#'"
                    );
                  return i.substring(RegExp.$1.length, i.length);
                }
                return "";
              }
              return ";" != i.charAt(0) && (t[t.length] = i), "";
            }
          ));
      return t;
    }
    getObject() {
      var e = this.getObject0();
      if (e != this.sexpCommentMarker) return e;
      if ((e = this.getObject()) == Parser.EOS)
        throw new BiwaError(
          "Readable object not found after S exression comment"
        );
      return (e = this.getObject());
    }
    getList(e) {
      for (var t = nil, n = t; this.i < this.tokens.length; ) {
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
          var r = this.getObject();
          r != Parser.EOS && t != nil && (n.cdr = r);
        } else {
          var i = new Pair(this.getObject(), nil);
          t == nil ? (t = i) : (n.cdr = i), (n = i);
        }
      }
      return t;
    }
    getVector(e) {
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
    }
    eatObjectsInSexpComment(e) {
      for (; "#;" == this.tokens[this.i]; )
        if (
          (this.i++,
          this.getObject() == Parser.EOS || this.i >= this.tokens.length)
        )
          throw new BiwaError(e);
    }
    getObject0() {
      if (this.i >= this.tokens.length) return Parser.EOS;
      var e = this.tokens[this.i++];
      if ("#;" == e) return this.sexpCommentMarker;
      var t,
        n =
          "'" == e
            ? "quote"
            : "`" == e
            ? "quasiquote"
            : "," == e
            ? "unquote"
            : ",@" == e && "unquote-splicing";
      if (
        n ||
        "(" == e ||
        "#(" == e ||
        "[" == e ||
        "#[" == e ||
        "{" == e ||
        "#{" == e
      )
        return n
          ? new Pair(Sym(n), new Pair(this.getObject(), nil))
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
          : /^#o[0-9\.]+$/i.test(e)
          ? new Number(parseInt("0" + e.substring(2, e.length), 8))
          : /^#b[0-9\.]+$/i.test(e)
          ? new Number(parseInt(e.substring(2, e.length), 2))
          : new Number(e)),
        isNaN(t))
      ) {
        if ("#f" == e || "#F" == e) return !1;
        if ("#t" == e || "#T" == e) return !0;
        if ("#\\newline" == e.toLowerCase()) return Char.get("\n");
        if ("#\\space" == e.toLowerCase()) return Char.get(" ");
        if ("#\\tab" == e.toLowerCase()) return Char.get("\t");
        if (/^#\\.$/.test(e)) return Char.get(e.charAt(2));
        if (/^#\\x[a-zA-Z0-9]+$/.test(e)) {
          var r = parseInt(e.slice(3), 16);
          if (r >= 55296 && r <= 57343)
            throw new BiwaError("Character in Unicode excluded range.");
          if (r > 65535) throw new BiwaError("Character literal out of range.");
          return Char.get(String.fromCharCode(r));
        }
        if (/^\"(\\(.|$)|[^\"\\])*\"?$/.test(e))
          return e
            .replace(/(\r?\n|\\n)/g, "\n")
            .replace(/^\"|\\(.|$)|\"$/g, function (e, t) {
              return t || "";
            });
        if (/^\|[^\|]*\|/.test(e)) {
          const t = e.replace(/^\|/, "").replace(/\|$/, "");
          return Sym(t);
        }
        return Sym(e);
      }
      return t.valueOf();
    }
  }
  (Parser.EOS = new Object()),
    (Parser.parse = (e) => {
      const t = new Parser(e),
        n = [];
      for (;;) {
        var r = t.getObject();
        if (r === Parser.EOS) break;
        n.push(r);
      }
      return n;
    });
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
      for (var o = 0; o < n; o++) a[o] = this.index(r, o);
      return new Closure(e, a, i, -1 == i ? t : void 0);
    }
    run_dump_hook(e, t, n, r, i) {
      var a, o;
      if (this.dumper) a = this.dumper;
      else {
        if (!Interpreter.dumper) return;
        a = Interpreter.dumper;
      }
      a &&
        ((o = { a: e, f: n, c: r, s: i, x: t, stack: this.stack }), a.dump(o));
    }
    _execute(e, t, n, r, i) {
      for (var a = null; ; )
        switch ((this.run_dump_hook(e, t, n, r, i), t[0])) {
          case "halt":
            return e;
          case "refer-local":
            var o = t[1];
            t = t[2];
            (e = this.index(n, o + 1)), (this.last_refer = "(anon)");
            break;
          case "refer-free":
            (o = t[1]), (t = t[2]);
            (e = r.freevars[o]), (this.last_refer = "(anon)");
            break;
          case "refer-global":
            var s = t[1];
            t = t[2];
            if (TopEnv.hasOwnProperty(s)) var u = TopEnv[s];
            else {
              if (!CoreEnv.hasOwnProperty(s))
                throw new BiwaError("execute: unbound symbol: " + inspect(s));
              u = CoreEnv[s];
            }
            (e = u), (this.last_refer = s || "(anon)");
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
              d = ((o = l[2]), l[3]),
              p = ((t = l[4]), l[5]);
            (e = this.closure(d, f, o, i, p)), (i -= o);
            break;
          case "box":
            (o = t[1]), (t = t[2]);
            this.index_set(i, o + 1, [this.index(i, o + 1)]);
            break;
          case "test":
            var h = t[1],
              m = t[2];
            t = !1 !== e ? h : m;
            break;
          case "assign-global":
            var _ = t[1];
            t = t[2];
            if (!TopEnv.hasOwnProperty(_) && !CoreEnv.hasOwnProperty(_))
              throw new BiwaError("global variable '" + _ + "' is not defined");
            (TopEnv[_] = e), (e = undef);
            break;
          case "assign-local":
            (o = t[1]), (t = t[2]);
            (this.index(n, o + 1)[0] = e), (e = undef);
            break;
          case "assign-free":
            (o = t[1]), (t = t[2]);
            (r.freevars[o][0] = e), (e = undef);
            break;
          case "conti":
            (o = t[1]), (t = t[2]);
            e = this.capture_continuation(i, o);
            break;
          case "nuate1":
            var g = t[1],
              y = t[2],
              b = this.current_dynamic_winder,
              v = Interpreter.DynamicWind.listWinders(b, y);
            t = Interpreter.DynamicWind.joinWinders(v, [
              "refer-local",
              0,
              ["nuate2", g],
            ]);
            break;
          case "nuate2":
            (g = t[1]), (t = ["return"]);
            i = this.restore_stack(g);
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
            (o = t[1]), (t = t[2]);
            var w = this.index(i, o + 1);
            i = this.shift_args(o, w, i);
            break;
          case "tco_hinted_apply":
            this.tco_counter[this.tco_counter.length - 1]++,
              (t = ["apply"].concat(rest(t)));
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
                var k =
                  "Function call error: got " +
                  w +
                  " but wanted " +
                  x.expected_args;
                throw new BiwaError(k);
              }
              (n = i), (r = x);
            } else {
              if (!(x instanceof Function))
                throw new BiwaError(inspect(x) + " is not a function");
              var E = [];
              for (C = 0; C < w; C++) E.push(this.index(i, C + 1));
              var P = x(E, this);
              if (P instanceof Pause) {
                var j = P;
                return j.set_state(this, ["return"], n, r, i), j.ready(), j;
              }
              if (P instanceof Call) {
                var $ = [
                    "frame",
                    [
                      "argument",
                      [
                        "constant",
                        1,
                        ["argument", ["constant", P.after, ["apply"]]],
                      ],
                    ],
                    ["return"],
                  ],
                  T = [
                    "constant",
                    P.args.length,
                    [
                      "argument",
                      ["constant", P.proc, ["apply", P.args.length]],
                    ],
                  ];
                t = [
                  "frame",
                  reduce(
                    P.args,
                    function (e, t) {
                      return ["constant", t, ["argument", e]];
                    },
                    T
                  ),
                  $,
                ];
              } else (e = P), (t = ["return"]);
            }
            break;
          case "return":
            var A = i - (o = this.index(i, 0));
            (t = this.index(A, 1)),
              (n = this.index(A, 2)),
              (r = this.index(A, 3)),
              (i = A - 3 - 1);
            var B = 1 + this.tco_counter[this.tco_counter.length - 1];
            this.call_stack.splice(-B), this.tco_counter.pop();
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
      for (var o = undef; ; ) {
        if (e) (o = this._execute(t, n, r, i, a)), (e = !1);
        else {
          if (!this.parser) break;
          var s = this.parser.getObject();
          if (s === Parser.EOS) break;
          s = Compiler.expand(s);
          const e = this.compiler.run(s);
          o = this._execute(s, e.il, 0, [], 0);
        }
        if (o instanceof Pause) return o;
      }
      return this.after_evaluate(o), o;
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
      for (var o = [], s = 0; s < n.length && n[s] !== r; s++)
        o.push(n[s].after);
      return (
        i.reverse(),
        i.forEach(function (e) {
          o.push(e.before);
        }),
        o
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
  class Rational {
    constructor(e, t) {
      (this.numerator = e), (this.denominator = t);
    }
    isInteger() {}
  }
  const isNumber = function (e) {
      return (
        e instanceof Complex || e instanceof Rational || "number" == typeof e
      );
    },
    isComplex = isNumber,
    isReal = function (e) {
      return e instanceof Complex || e instanceof Rational
        ? e.isReal()
        : "number" == typeof e;
    },
    isRational = function (e) {
      return e instanceof Complex
        ? e.isRational()
        : e instanceof Rational || "number" == typeof e;
    },
    isInteger = function (e) {
      return e instanceof Complex || e instanceof Rational
        ? e.isInteger()
        : "number" == typeof e && e % 1 == 0;
    };
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
          this.members = uniq(e);
        }
        universe() {
          return new Enumeration.EnumSet(this, this.members);
        }
        indexer() {
          return bind(function (e) {
            assert_symbol(e[0], "(enum-set indexer)");
            var t = indexOf(this.members, e[0]);
            return -1 !== t && t;
          }, this);
        }
        constructor_() {
          return bind(function (e) {
            assert_list(e[0], "(enum-set constructor)");
            var t = e[0].to_array();
            return (
              t.forEach(function (e) {
                assert_symbol(e, "(enum-set constructor)");
              }),
              new Enumeration.EnumSet(this, t)
            );
          }, this);
        }
      },
      EnumSet: class {
        constructor(e, t) {
          (this.enum_type = e),
            (this.symbols = e.members.filter(function (e) {
              return contains(t, e);
            }));
        }
        symbol_list() {
          return array_to_list(this.symbols);
        }
        is_member(e) {
          return contains(this.symbols, e);
        }
        is_subset(e) {
          return (
            !some(this.symbols, function (t) {
              return !contains(e.symbols, t);
            }) &&
            (this.enum_type === e.enum_type ||
              every(this.enum_type.members, function (t) {
                return contains(e.enum_type.members, t);
              }))
          );
        }
        equal_to(e) {
          return this.is_subset(e) && e.is_subset(this);
        }
        union(e) {
          var t = this.enum_type.members.filter(
            bind(function (t) {
              return contains(this.symbols, t) || contains(e.symbols, t);
            }, this)
          );
          return new Enumeration.EnumSet(this.enum_type, t);
        }
        intersection(e) {
          var t = this.symbols.filter(function (t) {
            return contains(e.symbols, t);
          });
          return new Enumeration.EnumSet(this.enum_type, t);
        }
        difference(e) {
          var t = this.symbols.filter(function (t) {
            return !contains(e.symbols, t);
          });
          return new Enumeration.EnumSet(this.enum_type, t);
        }
        complement() {
          var e = this.enum_type.members.filter(
            bind(function (e) {
              return !contains(this.symbols, e);
            }, this)
          );
          return new Enumeration.EnumSet(this.enum_type, e);
        }
        projection(e) {
          var t = this.symbols.filter(function (t) {
            return contains(e.enum_type.members, t);
          });
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
            o = t.slice(i);
          return new Call(n, a, function (e) {
            var t = e[0];
            return (
              assert_procedure(t, "_default_protocol/p"),
              new Call(t, o, function (e) {
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
        (e = bind(e, this)),
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
                o = n._make_n(a, t);
              return new Call(n.protocol, [o], function (e) {
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
    return assert_string(e[0]), _escape(e[0]);
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
        o = gensym(),
        s = deep_array_to_list([
          [o, i],
          [r, 0, [Sym("+"), r, 1]],
        ]),
        u = deep_array_to_list([[Sym(">="), r, o], a]);
      return new Pair(Sym("do"), new Pair(s, new Pair(u, n)));
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
        sort_with_comp(clone(e[1]), e[0], t)
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
    var o = Compiler.compile(a).il;
    if (0 != o[2])
      throw new Bug(
        "you cannot use free variables in macro expander (or define-macro must be on toplevel)"
      );
    var s = new Closure(o[3], [], -1, void 0);
    return (
      (TopEnv[r.name] = new Syntax(r.name, function (e) {
        var n = e.to_array();
        n.shift();
        var r = new Interpreter(),
          i = rearrange_args(t, n);
        return r.invoke_closure(s, i);
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
      for (var i = nil, a = nil, o = n; o instanceof Pair; o = o.cdr) {
        if (!(o.car instanceof Pair))
          throw new BiwaError(
            "let: need a pair for bindings: got " + to_write$1(o.car)
          );
        (i = new Pair(o.car.car, i)), (a = new Pair(o.car.cdr.car, a));
      }
      var s = null;
      if (t) {
        (i = array_to_list(i.to_array().reverse())),
          (a = array_to_list(a.to_array().reverse()));
        var u = new Pair(Sym("lambda"), new Pair(i, r)),
          c = new Pair(t, a);
        s = List(Sym("letrec"), new Pair(List(t, u), nil), c);
      } else s = new Pair(new Pair(Sym("lambda"), new Pair(i, r)), a);
      return s;
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
          var o = e.car;
          i = new Pair(new Pair(o, new Pair(new Pair(n, nil), nil)), i);
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
      if ((assert_number(e[0]), e[0] instanceof Rational))
        return e[0].numerator;
      throw new Bug("todo");
    }),
    define_libfunc("denominator", 1, 1, function (e) {
      if ((assert_number(e[0]), e[0] instanceof Rational))
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
    };
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
    (function () {
      var e = function (e, t, n) {
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
      };
      define_libfunc("caar", 1, 1, function (t) {
        return e("caar", [0, 0], t[0]);
      }),
        define_libfunc("cadr", 1, 1, function (t) {
          return e("cadr", [1, 0], t[0]);
        }),
        define_libfunc("cdar", 1, 1, function (t) {
          return e("cadr", [0, 1], t[0]);
        }),
        define_libfunc("cddr", 1, 1, function (t) {
          return e("cadr", [1, 1], t[0]);
        }),
        define_libfunc("caaar", 1, 1, function (t) {
          return e("caaar", [0, 0, 0], t[0]);
        }),
        define_libfunc("caadr", 1, 1, function (t) {
          return e("caadr", [1, 0, 0], t[0]);
        }),
        define_libfunc("cadar", 1, 1, function (t) {
          return e("cadar", [0, 1, 0], t[0]);
        }),
        define_libfunc("caddr", 1, 1, function (t) {
          return e("caddr", [1, 1, 0], t[0]);
        }),
        define_libfunc("cdaar", 1, 1, function (t) {
          return e("cdaar", [0, 0, 1], t[0]);
        }),
        define_libfunc("cdadr", 1, 1, function (t) {
          return e("cdadr", [1, 0, 1], t[0]);
        }),
        define_libfunc("cddar", 1, 1, function (t) {
          return e("cddar", [0, 1, 1], t[0]);
        }),
        define_libfunc("cdddr", 1, 1, function (t) {
          return e("cdddr", [1, 1, 1], t[0]);
        }),
        define_libfunc("caaaar", 1, 1, function (t) {
          return e("caaaar", [0, 0, 0, 0], t[0]);
        }),
        define_libfunc("caaadr", 1, 1, function (t) {
          return e("caaadr", [1, 0, 0, 0], t[0]);
        }),
        define_libfunc("caadar", 1, 1, function (t) {
          return e("caadar", [0, 1, 0, 0], t[0]);
        }),
        define_libfunc("caaddr", 1, 1, function (t) {
          return e("caaddr", [1, 1, 0, 0], t[0]);
        }),
        define_libfunc("cadaar", 1, 1, function (t) {
          return e("cadaar", [0, 0, 1, 0], t[0]);
        }),
        define_libfunc("cadadr", 1, 1, function (t) {
          return e("cadadr", [1, 0, 1, 0], t[0]);
        }),
        define_libfunc("caddar", 1, 1, function (t) {
          return e("caddar", [0, 1, 1, 0], t[0]);
        }),
        define_libfunc("cadddr", 1, 1, function (t) {
          return e("cadddr", [1, 1, 1, 0], t[0]);
        }),
        define_libfunc("cdaaar", 1, 1, function (t) {
          return e("cdaaar", [0, 0, 0, 1], t[0]);
        }),
        define_libfunc("cdaadr", 1, 1, function (t) {
          return e("cdaadr", [1, 0, 0, 1], t[0]);
        }),
        define_libfunc("cdadar", 1, 1, function (t) {
          return e("cdadar", [0, 1, 0, 1], t[0]);
        }),
        define_libfunc("cdaddr", 1, 1, function (t) {
          return e("cdaddr", [1, 1, 0, 1], t[0]);
        }),
        define_libfunc("cddaar", 1, 1, function (t) {
          return e("cddaar", [0, 0, 1, 1], t[0]);
        }),
        define_libfunc("cddadr", 1, 1, function (t) {
          return e("cddadr", [1, 0, 1, 1], t[0]);
        }),
        define_libfunc("cdddar", 1, 1, function (t) {
          return e("cdddar", [0, 1, 1, 1], t[0]);
        }),
        define_libfunc("cddddr", 1, 1, function (t) {
          return e("cddddr", [1, 1, 1, 1], t[0]);
        });
    })(),
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
        times(e[0], function () {
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
          ? List(Sym("append"), e.car.cdr.car, expand_qq(e.cdr, t))
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
            last(r).push(a);
          }
        else last(r).push(expand_qq(e[i], t));
      var o = r.map(function (e) {
        return e.splicing ? e : Cons(Sym("vector"), array_to_list(e));
      });
      return 1 == o.length
        ? Cons(Sym("vector"), array_to_list(r[0]))
        : Cons(Sym("vector-append"), array_to_list(o));
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
        t = function (e, r, i, a, o) {
          for (;;) {
            var s = a[a.length - 1][0],
              u = a[a.length - 1][1],
              c = a[a.length - 1][2],
              l = u - s;
            if (l >= 2 && !o) a.push([s, s + (l >> 1), !0]);
            else {
              if (!c) {
                a.pop();
                var f = a[a.length - 1][0],
                  d = e.slice(f, s),
                  p = e.slice(s, u);
                return n(d, p, r, [], 0, 0, function (n) {
                  for (var o = 0; o < n.length; o++) e[f + o] = n[o];
                  return 1 == a.length ? i(e) : t(e, r, i, a, !0);
                });
              }
              a.pop();
              var h = a[a.length - 1][1];
              a.push([u, h, !1]), (o = !1);
            }
          }
        },
        n = function (e, t, r, i, a, o, s) {
          var u = e.length,
            c = t.length;
          if (a < u && o < c)
            return new Call(r, [t[o], e[a]], function (u) {
              return (
                u[0] ? (i.push(t[o]), (o += 1)) : (i.push(e[a]), (a += 1)),
                n(e, t, r, i, a, o, s)
              );
            });
          for (; a < u; ) i.push(e[a]), (a += 1);
          for (; o < c; ) i.push(t[o]), (o += 1);
          return s(i);
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
        o = n.car,
        s = new Pair(Sym("begin"), n.cdr),
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
      return List(Sym("let"), i, a, List(Sym("if"), o, s, c));
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
              o = t.last_cdr(),
              s = Sym(o === nil ? "=" : ">="),
              u = new Pair(Sym("lambda"), new Pair(t, i));
            r = List(
              Sym("if"),
              List(s, List(Sym("length"), n), a),
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
      var o,
        s = !1,
        u = !1,
        c = !1,
        l = !1,
        f = !1,
        d = !1,
        p = [];
      n.to_array().forEach(function (e) {
        switch (e.car) {
          case Sym("fields"):
            p = e.cdr.to_array().map(function (e, t) {
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
            (o = e.cdr.car), assert_symbol(o);
            break;
          case Sym("protocol"):
            d = e.cdr.car;
            break;
          case Sym("sealed"):
            s = !!e.cdr.car;
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
        o &&
          ((l = [Sym("record-type-descriptor"), o]),
          (f = [Sym("record-constructor-descriptor"), o]));
      var h = [Sym("record-type-descriptor"), r],
        m = [Sym("record-constructor-descriptor"), r],
        _ = p.map(function (e) {
          return List(Sym(e.mutable ? "mutable" : "immutable"), e.name);
        });
      _.is_vector = !0;
      var g = [
          Sym("make-record-type-descriptor"),
          [Sym("quote"), r],
          l,
          c,
          s,
          u,
          _,
        ],
        y = [Sym("make-record-constructor-descriptor"), Sym("__rtd"), f, d],
        b = [
          Sym("let*"),
          [
            [Sym("__rtd"), g],
            [Sym("__cd"), y],
          ],
          [
            Sym("_define-record-type"),
            [Sym("quote"), r],
            Sym("__rtd"),
            Sym("__cd"),
          ],
        ],
        v = p.map(function (e) {
          var t = e.accessor_name || Sym(r.name + "-" + e.name.name);
          return [Sym("define"), t, [Sym("record-accessor"), h, e.idx]];
        }),
        w = p.filter(function (e) {
          return e.mutable;
        });
      return (
        (w = w.map(function (e) {
          var t = e.mutator_name || Sym(r.name + "-" + e.name.name + "-set!");
          return [Sym("define"), t, [Sym("record-mutator"), h, e.idx]];
        })),
        deep_array_to_list(
          [
            Sym("begin"),
            b,
            [Sym("define"), i, [Sym("record-constructor"), m]],
            [Sym("define"), a, [Sym("record-predicate"), h]],
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
        o = e[5];
      if ((assert_symbol(t), n && assert_record_td(n), r)) {
        assert_symbol(r);
        var s = Record.RTD.NongenerativeRecords[r.name];
        if (s) return s;
      }
      (i = !!i), (a = !!a), assert_vector(o);
      for (var u = 0; u < o.length; u++) {
        var c = o[u];
        assert_symbol(c.car, "mutability"),
          assert_symbol(c.cdr.car, "field name"),
          (o[u] = [c.cdr.car.name, c.car == Sym("mutable")]);
      }
      var l = new Record.RTD(t, n, r, i, a, o);
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
          return n instanceof Record && n.rtd === t;
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
        for (var a = !1, o = r.rtd; o; o = o.parent_rtd) o == t && (a = !0);
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
      var o = (~(-1 << r) & t) >> n,
        s = ~(-1 << r) & (-1 << n);
      return (s & (((o << (i %= a)) | (o >> (a - i))) << n)) | (~s & t);
    }),
    define_libfunc("bitwise-reverse-bit-field", 3, 3, function (e) {
      for (
        var t = e[0],
          n = e[0],
          r = e[1],
          i = e[2],
          a = (~(-1 << i) & n) >> r,
          o = 0;
        o < i - r;
        o++, a >>= 1
      ) {
        var s = i - 1 - o,
          u = 1 << s;
        t = (u & ((1 & a) << s)) | (~u & t);
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
            contains(i.members, n),
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
                  contains(i.members, e),
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
        o = List(Sym("lambda"), nil, a),
        s = Cons(Sym("lambda"), Cons(nil, n)),
        u = List(Sym("lambda"), nil, a);
      return List(Sym("let"), i, List(Sym("dynamic-wind"), o, s, u));
    }),
    define_libfunc("iota", 1, 3, function (e) {
      var t = e[0],
        n = e[1] || 0,
        r = void 0 === e[2] ? 1 : e[2];
      assert_integer(t), assert_number(n), assert_number(r);
      for (var i = [], a = n, o = 0; o < t; o++) i.push(a), (a += r);
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
    }),
    (nil.to_set = function () {
      return new BiwaSet();
    });
  var BiwaScheme$1 = {
    TopEnv: TopEnv,
    CoreEnv: CoreEnv,
    nil: nil,
    undef: undef,
    max_trace_size: max_trace_size,
    suppress_deprecation_warning: suppress_deprecation_warning,
    Version: VERSION$1,
    VERSION: VERSION$1,
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
    Rational: Rational,
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
      (n.innerHTML = _escape(e).replace(/\n/g, "<br>").replace(/ /g, "&nbsp;")),
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
    return $$1(e).prop("value") ? $$1(e).val() : _escape($$1(e).html());
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
      n = n || !1;
      times(
        (t = t || 0),
        function () {
          i += DUMP_PAD;
        }.bind(this)
      ),
        times(
          t + 1,
          function () {
            a += DUMP_PAD;
          }.bind(this)
        ),
        (r += i + '[<span class="dump_opecode">' + e[0] + "</span>");
      for (var o = 1; !(e[o] instanceof Array) && o < e.length; )
        "constant" == e[0]
          ? (r +=
              "&nbsp;<span class='dump_constant'>" +
              this.dump_obj(e[o]) +
              "</span>")
          : (r += "&nbsp;" + this.dump_obj(e[o])),
          o++;
      for (o < e.length && (r += "<br>\n"); o < e.length; o++)
        this.is_opc(e[o])
          ? (r += this.dump_opc(e[o], o == e.length - 1 ? t : t + 1, !0))
          : ((r += o == e.length - 1 ? i : a), (r += this.dump_obj(e[o]))),
          o != e.length - 1 && (r += "<br>\n");
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
      return "[object Object]" == t && (t = this.dump_object(e)), _escape(t);
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
        : (t = _escape(inspect(e)) + "<br>\n");
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
