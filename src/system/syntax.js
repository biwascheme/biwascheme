(function(){
//
// Syntax
//
BiwaScheme.Syntax = BiwaScheme.Class.create({
  initialize: function(sname, func){
    this.sname = sname;
    this.func = func;
  },

  transform: function(x){
    if (!this.func) {
      throw new Error("misplaced syntax: "+this.sname);
    }
    return this.func(x);
  },

  inspect: function(){
    return "#<Syntax " + this.sname +">";
  }
});

// Register dummy syntaxes to raise error
BiwaScheme.CoreEnv["quote"]  = new BiwaScheme.Syntax("quote");
BiwaScheme.CoreEnv["define"] = new BiwaScheme.Syntax("define");
BiwaScheme.CoreEnv["define-syntax"] = new BiwaScheme.Syntax("define-syntax");
BiwaScheme.CoreEnv["begin"] = new BiwaScheme.Syntax("begin");
BiwaScheme.CoreEnv["set!"] = new BiwaScheme.Syntax("set!");
BiwaScheme.CoreEnv["call/cc"] = new BiwaScheme.Syntax("call/cc");
BiwaScheme.CoreEnv["call-with-current-continuation"]   = new BiwaScheme.Syntax("call-with-current-continuation");
BiwaScheme.CoreEnv["lambda"] = new BiwaScheme.Syntax("lambda");
BiwaScheme.CoreEnv["syntax-case"] = new BiwaScheme.Syntax("syntax-case");
BiwaScheme.CoreEnv["if"] = new BiwaScheme.Syntax("if");

//
// syntax-case
//

BiwaScheme.Syntax.TRACE_EXPANSION = false;

// Colors for debug print
var theme = {
  env: 'yellow',
  so: 'green',
  subst: 'blue',
  mark: 'yellow'
};
if (BiwaScheme.on_node && BiwaScheme.Syntax.TRACE_EXPANSION) {
  var colors = require('colors');
  colors.setTheme(theme);
}
else {
  var colors = {};
  for (name in theme) colors[name] = function(s){ return s};
}

// SyntaxObject (S-expr with some meta information)
//
// NB: Definition of "syntax object" is not limited to instances of `SyntaxObject`.
// A syntax object is either of:
// - A nonpair, nonvector, nonsymbol value (number, boolean, string, char)
// - A pair or vector of syntax objects
// - An instane of `SyntaxObject` ("wrapped" syntax object)
// Note that a raw symbol is not a syntax object. It needs to be wrapped with SyntaxObject.
BiwaScheme.Syntax.SyntaxObject = BiwaScheme.Class.create({
  // expr: S-expr
  // wrap: BiwaScheme.Syntax.Wrap
  initialize: function(expr, wrap){
    this.expr = expr;
    if (!wrap) { 
      throw new BiwaScheme.Error("[BUG] wrap not specified: "+ BiwaScheme.inspect(expr));
    }
    this.wrap = wrap;
  },

  // Return the label corresponding to this identifier
  // (`id-label` in "Beautiful Code")
  getLabel: function() {
    BiwaScheme.assert(this.expr instanceof BiwaScheme.Symbol,
                      "called getLabel on non-identifier SO");
    var sym = this.expr;
    var marks = this.wrap.marks();

    for (var i=0; i<this.wrap.markSubsts.length; i++) {
      var ms = this.wrap.markSubsts[i];
      if (ms instanceof Mark) {
        marks = _.rest(marks);
      }
      else {
        var subst = ms;
        if (subst.sym === sym && Mark.isSameMarks(subst.marks, marks)) {
          var marksStr = "["+marks.map(function(m){ return m.debugStr() }).join(", ")+"]";
          debug("getLabel", "ident:", this.inspect(), "marks:", marksStr,
            "matched:", subst.debugStr(), "result:", subst.label.debugStr());
          return subst.label;
        }
      }
    }

    if (BiwaScheme.SyntaxEnv.hasOwnProperty(sym.name)) {
      return BiwaScheme.Syntax.Label.TopLevel;
    }

    //console.error("getLabel notfound", BiwaScheme.to_write(this.expr), this.wrap.debugStr());
    throw new BiwaScheme.Error("undefined identifier: "+sym.name);
  },

  // Return true if this SO represents an identifier
  isIdentifier: function() {
    return this.expr instanceof BiwaScheme.Symbol;
  },

  // `free-identifier=?`
  freeIdentifierEquals: function(other) {
    if (!this.isIdentifier()) throw new Bug("not an identifier");
    if (!(other instanceof SyntaxObject) || !other.isIdentifier())
      throw new Bug("`other` is not an identifier");

    return this.getLabel() === other.getLabel();
  },

  // `bound-identifier=?`
  boundIdentifierEquals: function(other) {
    if (!this.isIdentifier()) throw new Bug("not an identifier");
    if (!(other instanceof SyntaxObject) || !other.isIdentifier())
      throw new Bug("`other` is not an identifier");

    return this.expr.name === other.expr.name &&
           Mark.isSameMarks(this.wrap.marks(), other.wrap.marks());
  },

  // Return true if this SO contains nil
  isNullSO: function() {
    return this.expr === BiwaScheme.nil;
  },

  // Return true if this SO contains a Pair
  isPairSO: function() {
    return this.expr instanceof BiwaScheme.Pair;
  },

  // Return true if this SO contains a vector
  isVectorSO: function() {
    return BiwaScheme.isVector(this.expr);
  },

  // Extract car as SO
  sCar: function() {
    if (!this.isPairSO()) throw new Error("sCar: not a pair SO");
    // Note: If `this.expr` has its own wrap(`innerWrap`),
    // new wrap will be `this.wrap` + `innerWrap`.
    // If the tail of `this.wrap` and the head of `innerWrap` is the same
    // mark, it is removed (cancels each other).
    return SyntaxObject.wrapWith(this.wrap, this.expr.car);
  },

  // Extract cdr as SO
  sCdr: function() {
    if (!this.isPairSO()) throw new Error("sCdr: not a pair SO");
    return SyntaxObject.wrapWith(this.wrap, this.expr.cdr);
  },

  // Decompose #SO<List> into an array of #<SO>
  // NB: Last cdr is just ignored (even if it is not nil)
  expose: function() {
    if (this.expr instanceof Pair) {
      var so = this, ary = [];
      for (var ary = [], so = this; so.isPairSO(); so = so.sCdr()) {
        ary.push(so.sCar());
      }
      return ary;
    }
    else {
      throw new BiwaScheme.Bug("this so is not a pair: "+BiwaScheme.to_write(this.expr));
    }
  },

  strip: function() {
    return SyntaxObject.strip(this);
  },

  // Returns a Symbol of the same context as this
  sym: function(name) {
    return new SyntaxObject(Sym(name), this.wrap);
  },
  
  inspect: function() {
    if (Syntax.inspecting) { // Simplify nested SyntaxObject (currently disabled)
      if (BiwaScheme.isSelfEvaluating(this.expr)) {
        return BiwaScheme.to_write(this.expr) + "{-}";
      }
      else {
        return BiwaScheme.to_write(this.expr) +
          "{" + this.wrap.debugStr() + "}";
      }
    }
    else {
      //Syntax.inspecting = true;
      var ret = colors.so("#<SO ") + BiwaScheme.to_write(this.expr) +
                " " + this.wrap.debugStr() + colors.so(">");
      Syntax.inspecting = false;
      return ret;
    }
  }
});

_.extend(BiwaScheme.Syntax.SyntaxObject, {
  // Return a SyntaxObject with a wrap
  // - x: SyntaxObject or scheme expr
  // Note: this is named `extend-wrap` in Beautiful Code
  wrapWith: function(wrap, x) {
    if (x instanceof SyntaxObject) {
      return new SyntaxObject(x.expr, wrap.joinWrap(x.wrap));
    }
    else {
      return new SyntaxObject(x, wrap);
    }
  },

  // Return a SyntaxObject with a mark
  // x : SyntaxObject or scheme expr
  addMark: function(x, mark) {
    return SyntaxObject.wrapWith(new Wrap([mark]), x);
  },

  // Return a SyntaxObject with a subst
  // id : identifier(SyntaxObject with Symbol)
  // label: Label
  // x : SyntaxObject or scheme expr
  addSubst: function(id, label, x) {
    var subst = new Subst(id, label);
    return SyntaxObject.wrapWith(new Wrap([subst]), x);
  },

  // Return a SyntaxObject with substs
  // - x : SyntaxObject or scheme expr
  // - substs: Array<Subst>
  addSubsts: function(x, substs) {
    return SyntaxObject.wrapWith(new Wrap(substs), x);
  },

  // Convert a SyntaxObject into pure S-expr by removing meta information
  strip: function(x) {
    if (x instanceof SyntaxObject) {
      return (x.wrap.isTopMarked() ? x.expr : SyntaxObject.strip(x.expr));
    }
    else if (x instanceof Pair) {
      var a = SyntaxObject.strip(x.car),
          d = SyntaxObject.strip(x.cdr);
      // Just return x if not changed (to avoid unnecessary memory allocation)
      return (a === x.car && d === x.cdr ? x : new Pair(a, d));
    }
    else if (_.isArray(x)) {
      return x.map(function(item){ return SyntaxObject.strip(item) });
    }
    else {
      return x;
    }
  }
});

BiwaScheme.isIdentifier = function(x) {
  return (x instanceof SyntaxObject) &&
         (x.expr instanceof Symbol);
};

BiwaScheme.Syntax.Mark = BiwaScheme.Class.create({
  initialize: function(){
    this.n = (BiwaScheme.Syntax.Mark.n++);
  },

  // For debug print
  debugStr: function(){
    opts = arguments[0] || {}
    return opts['nocolor'] ? "m"+this.n : colors.mark("m"+this.n);
  },

  toString: function() {
    return "m" + this.n;
  },

  inspect: function(){
    return "#<Mark "+this.n+">";
  }
});
BiwaScheme.Syntax.Mark.n = 0;
// A special Mark for InitialWrap (used by `strip`)
BiwaScheme.Syntax.Mark.TopMark = new BiwaScheme.Syntax.Mark();

// Return true if `this.marks` and `marks` are the same (ordered) list of Marks 
// - marks1, marks2: Array<Mark>
BiwaScheme.Syntax.Mark.isSameMarks = function(marks1, marks2) {
  var markPairs = _.zip(marks1, marks2);
  return _.all(markPairs, function(mm) {
    return mm[0] === mm[1];
  });
}

BiwaScheme.Syntax.Label = BiwaScheme.Class.create({
  initialize: function(suffix){
    this.n = (BiwaScheme.Syntax.Label.n++);
    this.suffix = suffix || "";

    this.name = "l" + this.n + this.suffix;
  },

  debugStr: function(){
    return this.name;
  },

  toString: function(){
    return "#<Label " + this.debugStr() + ">";
  }
});
BiwaScheme.Syntax.Label.n = 0;
BiwaScheme.Syntax.Label.TopLevel = new BiwaScheme.Syntax.Label();

BiwaScheme.Syntax.Subst = BiwaScheme.Class.create({
  initialize: function(id, label){
    this.sym = id.expr;           // Symbol
    this.marks = id.wrap.marks(); // Array<Mark>
    this.label = label;           // Label
  },

  debugStr: function() {
    var marks = this.marks.map(function(m){ return m.debugStr({nocolor: true}) }).join("");
    return colors.subst("[" + this.sym.name + "(" + marks + ")=" + this.label.debugStr() + "]");
  },

  inspect: function(){
    return "#<Subst " + this.debugStr() + ">";
  }
});

BiwaScheme.Syntax.Wrap = BiwaScheme.Class.create({
  initialize: function(markSubsts){
    this.markSubsts = markSubsts || [];   // Array<Mark|Subst>
  },

  // Return an array of the marks
  marks: function() {
    return _.filter(this.markSubsts,
                    function(x){ return x instanceof Mark });
  },

  // Return a new Wrap with `mark`
  addMark: function(mark) {
    return new Wrap([mark]).joinWrap(this);
  },

  // Return a new Wrap with `subst`
  addSubst: function(subst) {
    return new Wrap([subst].concat(this.markSubsts));
  },

  // Return a new wrap that is a combination of this and other
  // Note that consecutive same marks cancels each other
  joinWrap: function(other) {
    var a1 = this.markSubsts,
        a2 = other.markSubsts;
    if (this.markSubsts.length == 0 || other.markSubsts.length == 0) {
      return new Wrap(a1.concat(a2));
    }

    var x1 = _.last(this.markSubsts),
        x2 = _.first(other.markSubsts);
    if (x1 instanceof BiwaScheme.Syntax.Mark && x1 === x2) {
      a1 = _.initial(a1);  // Omit x1 and x2 from the result
      a2 = _.rest(a2);
    }
    return new Wrap(a1.concat(a2));
  },

  // Return true if this wrap has TopMark
  isTopMarked: function() {
    return _.some(this.markSubsts, function(ms) {
      return ms[0] === BiwaScheme.Syntax.Mark.TopMark;
    });
  },

  debugStr: function() {
    //return this.markSubsts.map(function(x){ return x.debugStr() }).join(",");
    return this.marks().map(function(x){ return x.debugStr() }).join("");
  },

  toString: function() {
    return "#<Wrap" + this.debugStr() + ">";
  }
});

BiwaScheme.Syntax.Binding = BiwaScheme.Class.create({
  initialize: function(type, value){
    switch(type) {
      case "macro":
      case "core":
        if (!BiwaScheme.isProcedure(value))
          throw new BiwaScheme.Bug("Binding.initialize: expected function but got "+BiwaScheme.inspect(value));
        break;
      case "lexical":
      case "global":
        if (!(value instanceof BiwaScheme.Symbol))
          throw new BiwaScheme.Bug("Binding.initialize: expected symbol but got "+BiwaScheme.inspect(value));
        break;
    }
    this.type = type;   // either of "macro" "lexical" "core" "global"
    this.value = value; // expander for "macro", "core". Symbol for "lexical", "global"
  },

  inspect: function(){
    if (this.type == "macro" || this.type == "core") {
      return "#<Binding("+this.type+")>";
    }
    else {
      return "#<Binding("+this.type+", "+this.value+")>";
    }
  }
});

BiwaScheme.Syntax.Env = BiwaScheme.Class.create({
  initialize: function(/*hash*/){
    this.hash = arguments[0] || {};  // Hash<Label name, Binding>
  },

  set: function(label, binding) {
    this.hash[label.name] = binding;
    return this;
  },

  // Return a (shallow) copy of this
  dup: function() {
    var newHash = {};
    _.extend(newHash, this.hash);
    return new Env(newHash);
  },

  // Return the binding corresponds to `label` (or `undefined` if none)
  // (`label-binding` in "Beautiful Code")
  get: function(label) {
    return this.hash[label.name];
  },

  // - id: identifier SO
  // (`id-binding` in "Beautiful Code")
  bindingOfId: function(id) {
    var label = id.getLabel();
    if (label == Label.TopLevel) {
      // REVIEW: should we lookup TopEnv/CoreEnv too?
      return BiwaScheme.SyntaxEnv[id.expr.name];
    }

    var binding = this.get(label);
    if (!binding) {
      throw new BiwaScheme.Error("displaced lexical: "+id.expr.name);
    }
    return binding;
  },

  inspect: function(){
    var keys = _.keys(this.hash);
    if (keys.length == 0) {
      return colors.env("#<Env>");
    }
    else {
      var contents = _.map(this.hash, function(v, k) {
        return k + "=" + v.inspect();
      }).join(", ");
      return colors.env("#<Env " + contents + ">");
    }
  }
});

BiwaScheme.Syntax.INITIAL_ENV_ITEMS = [
  ["quote", "core", function(so, env, metaEnv, after){
    var x = so.expr;
    if (!(x.cdr instanceof Pair)) throw new Error("quote: missing argument");
    if (x.cdr.cdr !== BiwaScheme.nil) throw new Error("quote: too many arguments");
    return after(x);
  }],

  ["define", "core", function(so, env, metaEnv, after){
    var ret = LambdaHelper.parseDefine(so, env);
    var name = ret[0], expr = ret[1], isDefun = ret[2];

    if (isDefun) { // `(define (f ...) ...)`
      var label = new Label();
      var binding = new Binding("lexical", name.expr);
      var lambdaSo = SyntaxObject.addSubst(
        name,
        label,
        new SyntaxObject(expr, new Wrap())); // REVIEW: not `so.wrap`?
      var newEnv = env.dup().set(label, binding);
      return Expander._exp(lambdaSo, newEnv, metaEnv, function(so2) {
        return after(List(Sym("define"), name, so2));
      });
    }
    else { // `(define x ...)`
      return Expander._exp(expr, env, metaEnv, function(so2) {
        return after(List(Sym("define"), name, so2));
      });
    }
  }],

  ["define-syntax", "core", function(so, env, metaEnv, after){
    var sos = so.expose();
    if (sos.length < 3) throw new Error("define-syntax: missing name or transformer");
    if (sos.length > 3) throw new Error("define-syntax: too many args");
    var name = sos[1], transformer = sos[2];

    // `(define-synax name (lambda (x) ...))`
    return Expander._exp(transformer, env, metaEnv, function(so2) {
      return after(List(Sym("define-syntax"), name, so2));
    });
  }],

  ["begin", "core", function(so, env, metaEnv, after){
    // (begin body...)
    var sos = so.expose();
    if (sos.length < 2) throw new Error("begin: missing body of begin");
    return Expander._expandExprs(_.rest(sos), env, metaEnv, function(newBody) {
      return after(new Pair(Sym("begin"), ListA(newBody)));
    });
  }],

  ["set!", "core", function(so, env, metaEnv, after){
    // (set! x v)
    var sos = so.expose();
    if (sos.length != 3) throw new Error("set!: set! takes var and val");

    return Expander._exp(sos[1], env, metaEnv, function(left) {
      return Expander._exp(sos[2], env, metaEnv, function(right) {
        return after(List(Sym("set!"), left, right));
      });
    });
  }],

  ["call/cc", "core", function(so, env, metaEnv, after){
    // (call/cc f)
    var sos = so.expose();
    if (sos.length < 2) throw new Error("call/cc: missing argument");
    return Expander._exp(sos[1], env, metaEnv, function(so2) {
      return after(List(Sym("call/cc"), so2));
    });
  }],

  ["call-with-current-continuation", "macro", function(x){
    var sos = x.expose();
    return List(Sym("call/cc"), sos[1]);
  }],

  ["if", "core", function(so, env, metaEnv, after){
    var sos = so.expose();
    if (sos.length < 3) throw new Error("if: missing then clause");
    if (sos.length > 4) throw new Error("if: too many clauses");

    return Expander._exp(sos[1], env, metaEnv, function(condc) {
      return Expander._exp(sos[2], env, metaEnv, function(thenc) {
        if (sos[3]) { 
          return Expander._exp(sos[3], env, metaEnv, function(elsec) {
            return after(List(Sym("if"), condc, thenc, elsec));
          });
        }
        else {
          return after(List(Sym("if"), condc, thenc, BiwaScheme.undef));
        }
      });
    });
  }],

  ["lambda", "core", function(so, env, metaEnv, after){
    var cdrSo = so.sCdr();
    if (!cdrSo.isPairSO()) throw new BiwaScheme.Error("malformed lambda");
    var paramSpecSo = cdrSo.sCar();
    var bodySos = cdrSo.sCdr().expose();

    // Check this lambda has internal definitions
    ret = LambdaHelper.collectInternalDefs(bodySos);
    var internalDefs = ret[0];
    bodySos = ret[1];
    
    if (internalDefs.length > 0) {
      // Convert "lambda + internal defs" into "lambda + letrec*"
      var letrec = LambdaHelper.buildLetRecStar(internalDefs, bodySos);
      var newLambda = new SyntaxObject(
        List(Sym("lambda"), paramSpecSo, letrec), so.wrap);
      return Expander._exp(newLambda, env, metaEnv, after);
    }
    else {
      return LambdaHelper.expandBody(paramSpecSo, bodySos, env, metaEnv,
                                     function(newParamSpec, newBodyExprs) {
        return after(new Pair(Sym("lambda"),
                       new Pair(newParamSpec,
                         ListA(newBodyExprs))));
      });
    }
  }],

  ["syntax-case", "core", function(so, env, metaEnv, after) {
    var sos = so.expose();
    if (sos.length < 4) {
      throw new Error("syntax-case: too few arguments");
    }
    var expr = sos[0];
    if (sos[1].isIdentifier()) {
      var myEllipsis = sos[1].expr, literals = sos[2], clauses = _.rest(sos, 3);
    }
    else {
      var myEllipsis = Sym("..."), literals = sos[1], clauses = _.rest(sos, 2);
    }
    var syntaxCase = new BiwaScheme.Syntax.SyntaxCase(myEllipsis, literals);
    return syntaxCase.compileClauses(clauses, env, metaEnv, function(compiledClauses) {
      compiledClauses.push(List(Sym("js-eval"), "throw 'syntax-case: no match'"));
      var expr = new Pair(Sym("or"), ListA(compiledClauses));
      var compiledSyntaxCase = new SyntaxObject(expr, so.wrap);
      // Expand `or`, etc.
      return Expander._exp(compiledSyntaxCase, env, metaEnv, after);
    });
  }],
];

// Misc

_.extend(BiwaScheme.Syntax, {
  // Return a unique name with the given `prefix`
  genVar: function(prefix){
    if (!prefix) throw new BiwaScheme.Bug("invalid prefix: "+BiwaScheme.inspect(prefix));
    var n = (Syntax._genVar++);
    return Sym(prefix + "." + n);
  },
  _genVar: 0,

  // datum->syntax
  // templateId: Identifier to copy context
  // expr: Scheme expr
  datumToSyntax: function(templateId, expr) {
    return new SyntaxObject(expr, templateId.wrap);
  }
});

//
// Expander
//

BiwaScheme.Expander = {
  InitialEnv: new BiwaScheme.Syntax.Env({}),
  InitialWrap: new BiwaScheme.Syntax.Wrap([BiwaScheme.Syntax.Mark.TopMark]),

  // Entry point of Expander
  // 
  // Normally returns a S-expr. If `expr` contains a macro use, returns
  // BiwaScheme.Call to evaluate the macro body (which is written in Scheme).
  expand: function(expr) {
    var so1 = new SyntaxObject(expr, Expander.InitialWrap),
        env = Expander.InitialEnv,
        menv = Expander.InitialEnv;
    var ret = Expander._exp(so1, env, menv, function(so2) {
      var stripped = SyntaxObject.strip(so2);
      debug("Expanded:", BiwaScheme.to_write(stripped), "\n");
      return stripped;
    });
    debug("(return from expand)", BiwaScheme.to_write(ret), "\n");
    return ret;
  },

  // Main loop of Expander (`exp` in "Beautiful Code")
  // Returns syntax object or BiwaScheme.Call
  _exp: function(so1, env, menv, after) {
    if (!BiwaScheme.isSelfEvaluating(so1.expr)) debug("_exp", so1.inspect(), env.inspect(), menv);

    var expr = so1.expr;
    if (expr instanceof BiwaScheme.Symbol) {
      // Variable reference or varref-like macro call
      var binding = env.bindingOfId(so1);
      switch(binding.type) {
        case "macro":
          return Expander._expandMacro(so1.expr.name, binding.value, so1, function(so2) {
            return Expander._exp(so2, env, menv, after);
          });
        case "lexical":
        case "global":
          return after(binding.value);
        default:
          throw new BiwaScheme.Error("undefined variable: "+expr.name);
      }
    }
    else if (expr instanceof BiwaScheme.Pair) {
      var sos = so1.expose();
      if (sos[0].expr instanceof BiwaScheme.Symbol) {
        // Funcion call or macro call
        var id = sos[0];
        var binding = env.bindingOfId(id);
        switch(binding.type) {
          case "macro":
            return Expander._expandMacro(id.expr.name, binding.value, so1, function(so2) {
              return Expander._exp(so2, env, menv, after);
            });
          case "lexical":
          case "global":
            var car = binding.value;
            return Expander._expandExprs(_.rest(sos), env, menv, function(newSos) {
              var cdr = ListA(newSos);
              var so2 = new SyntaxObject(new Pair(car, cdr), so1.wrap)
              return after(so2);
            });
          case "core":
            return Expander._expandCore(binding.value, so1, env, menv, after);
          default:
            throw "must not happen"
        }
      }
      else {
        // ((func expr...) args...)
        return Expander._exp(sos[0], env, menv, function(car) {
          return Expander._expandExprs(_.rest(sos), env, menv, function(newSos) {
            var cdr = ListA(newSos);
            var so2 = new SyntaxObject(new Pair(car, cdr), so1.wrap);
            return after(so2);
          });
        });
      }
    }
    else {
      // Constant
      var d = SyntaxObject.strip(so1);
      if (!BiwaScheme.isSelfEvaluating(d)) {
        throw new Error("misplaced non-self evaluating expr: "+BiwaScheme.to_write(d));
      }
      return after(d);
    }
  },
  
  // Expand macro use
  // (`exp-macro` in "Beautiful Code")
  // name: String (used only for debug print)
  // after: js-func that takes the resulting SO
  // Returns BiwaScheme.Call
  _expandMacro: function(name, transformer, so, after) {
    debug("_expandMacro", so.inspect());

    var m = new Mark();
    // Call `addMark` before and after the transformation.
    // Only "new" code are left marked because same marks cancel each other.
    var marked = SyntaxObject.addMark(so, m);
    return new BiwaScheme.Call(transformer, [marked], function(newSo) {
      var remarked = SyntaxObject.addMark(newSo, m);
      return after(remarked);
    });
  },

  // coreTrans: js function
  _expandCore: function(coreTrans, so, env, menv, after) {
    debug("_expandCore", so.inspect());

    return coreTrans(so, env, menv, function(newExpr) {
      return after(new SyntaxObject(newExpr, so.wrap));
    });
  },

  // - exprs: Array<SO>
  _expandExprs: function(exprs, env, menv, after) {
    if (exprs.length == 0) {
      return after([]);
    }
    var first = exprs[0], rest = _.rest(exprs);
    return Expander._exp(first, env, menv, function(newFirst) {
      return Expander._expandExprs(rest, env, menv, function(newRest) {
        return after([newFirst].concat(newRest));
      });
    });
  }
}

// Helpers for expanding `lambda`
BiwaScheme.Expander.LambdaHelper = {
  // Check parameter spec is valid
  // Return the last cdr of the parameter list
  // - paramSpecSo: SyntaxObject
  validateParamSpec: function(paramSpecSo) {
    // Validate parameter spec 
    var lastCdr;
    if (paramSpecSo.isPairSO()) {
      var x;
      for (x = paramSpecSo; x.isPairSO(); x = x.sCdr()) {
        if (!(x.sCar().isIdentifier())) {
          throw new Error("lambda: invalid parameter name: "+BiwaScheme.to_write(paramSpecSo.expr));
        }
      }
      lastCdr = x;
      if (!lastCdr.isNullSO() && !lastCdr.isIdentifier()) {
        throw new Error("lambda: invalid rest parameter: "+BiwaScheme.to_write(paramSpecSo.expr));
      }
    }
    else if (!paramSpecSo.isNullSO() && !paramSpecSo.isIdentifier()) {
      throw new Error("lambda: invalid parameter spec: "+BiwaScheme.inspect(paramSpecSo.expr));
    }
    return lastCdr;
  },

  // Generate new name for parameters
  // - paramSpecSo: SO
  // - lastCdr: SO(Symbol or nil)
  // Return:
  // - newParamSpec: scheme expr
  // - paramNames: Array of [oldname(SO), newname(Symbol)]
  generateVariables: function(paramSpecSo, lastCdr) {
    var newParamSpec, paramNames;
    if (paramSpecSo.isNullSO()) {
      newParamSpec = BiwaScheme.nil;
      paramNames = [];
    }
    else if (paramSpecSo.isIdentifier()) {
      newParamSpec = Syntax.genVar(paramSpecSo.expr.name);
      paramNames = [[paramSpecSo, newParamSpec]];
    }
    else {
      paramNames = paramSpecSo.expose().map(function(id){
        if (!id.isIdentifier()) throw new Error("expected identifier but got "+BiwaScheme.inspect(id));
        return [id, Syntax.genVar(id.expr.name)];
      });
      var lastIdx;
      if (lastCdr.isIdentifier()) {
        newParamSpec = Syntax.genVar(lastCdr.expr.name)
        paramNames.push([lastCdr, newParamSpec]);
        lastIdx = paramNames.length - 2;
      }
      else {
        newParamSpec = BiwaScheme.nil;
        lastIdx = paramNames.length - 1;
      }
      for (var i = lastIdx; i >= 0; i--) {
        newParamSpec = new Pair(paramNames[i][1], newParamSpec);
      }
    }
    return [newParamSpec, paramNames];
  },

  // Collect internal definitions (including `define` in `begin`)
  // - bodySos: Array of SO
  // Return:
  // - defs: Array of SO (each starts with `define`)
  // - rest: Array of SO
  collectInternalDefs: function(bodySos) {
    var defs = [];
    var rest = bodySos;
    while (rest.length > 0) {
      var so = rest[0];
      if (so.isPairSO() && so.sCar().expr === Sym("define")) {
        defs.push(so);
        rest = rest.slice(1, rest.length);
      }
      else if (so.isPairSO() && so.sCar().expr === Sym("begin")) {
        if (!so.sCdr().isPairSO() && !so.sCdr().isNullSO())
          throw new Error("lambda: malformed begin");
        // Splice `begin`
        rest = so.sCdr().expose().concat(rest.slice(1, rest.length));
      }
      else {
        // Found an expression. Stop looking for internal defs
        break;
      }
    }
    return [defs, rest];
  },

  // Build `(letrec* ((name expr)...) ...)`
  // - internalDefs: Array of SO
  // - bodySos: Array of SO
  buildLetRecStar: function(internalDefs, bodySos) {
    var defs = internalDefs.map(function(so) {
      var ret = LambdaHelper.parseDefine(so);
      var name = ret[0], expr = ret[1];
      return List(name, expr);
    });
    return new Pair(Sym("letrec*"),
             new Pair(ListA(defs),
               ListA(bodySos)));
  },

  // Parse `(define ...)` and return either
  // - variable name and the expression, or
  // - variable name and a lambda expression (for `(define (f ...) ...)`)
  // Returns `[name, expr, isLambda]`
  // - name: SO(Identifier)
  // - expr: SO
  // - isLambda: true if expr is lambda
  parseDefine: function(so) {
    var cdr = so.sCdr();
    if (!cdr.isPairSO()) throw new Error("define: missing name");
    var cadr = cdr.sCar(), cddr = cdr.sCdr();
    if (cadr.isIdentifier()) {
      // (define var expr)
      if (!cddr.isPairSO() || !cddr.sCdr().isNullSO())
        throw new Error("define: malformed expr: "+BiwaScheme.to_write(cddr.expr));
      return [cadr, cddr.sCar(), false]; 
    }
    else if (cadr.isPairSO()) {
      // (define (fname arg...) body...)
      // => (define fname (lambda (arg...) body...))
      // (define (fname arg... . rest) body...)
      // => (define fname (lambda (arg... . rest) body...))
      // (define (fname . args) body...)
      // => (define fname (lambda args body...))
      var fname = cadr.sCar(), argSpec = cadr.sCdr();
      if (!fname.isIdentifier()) throw new Error("define: invalid function name");
      var lambdaExpr = new Pair(Sym("lambda"),
                         new Pair(argSpec, 
                           cddr));
      return [fname, lambdaExpr, true]; 
    }
    else {
      throw new Error("define: invalid name form: "+BiwaScheme.to_write(so.expr));
    }
  },

  // Expand body expressions
  expandBody: function(paramSpecSo, bodySos, env, metaEnv, after) {
    var lastCdr = LambdaHelper.validateParamSpec(paramSpecSo);
    var ret = LambdaHelper.generateVariables(paramSpecSo, lastCdr);
    var newParamSpec = ret[0],
        paramNames = ret[1];

    var newEnv = env.dup(),
        substs = [];
    paramNames.forEach(function(names) {
      var label = new Label();
      var binding = new Binding("lexical", names[1]);
      newEnv.set(label, binding);
      substs.push(new Subst(names[0], label));
    });

    var newBodySos = bodySos.map(function(bodySo){
      return SyntaxObject.addSubsts(bodySo, substs);
    });
    return Expander._expandExprs(newBodySos, newEnv, metaEnv, function(newBodyExprs) {
      return after(newParamSpec, newBodyExprs);
    });
  }
};

//
// Helpers to implement syntax-case (see also: r6rs_lib.js)
//

BiwaScheme.Syntax.SyntaxCase = BiwaScheme.Class.create({
  initialize: function(myEllipsis, literals) {
    this.myEllipsis = myEllipsis;
    this.literals = literals;
    // Counter for _newSym
    this._n = 0;
  },

  // Return a symbol unique in this syntax-case
  _newSym: function() {
    this._n++;
    return Sym("v"+this._n);
  },

  compileClauses: function(clauses, env, menv, after) {
    if (clauses.length == 0) {
      return after([]);
    }
    var first = clauses[0], rest = _.rest(clauses);
    var clause = this._parseClause(first);
    var self = this;

    // Compile each clause by recursion
    return this._compileClause(clause.pattern, clause.outputExpr, env, menv, function(compiledFirst) {
      return self.compileClauses(rest, env, menv, function(compiledRest) {
        return after([compiledFirst].concat(compiledRest));
      });
    });
  },

  _parseClause: function(clause) {
    var sos = clause.expose();
    if (sos.length < 2) {
      throw new Error("syntax-case: clause has too few elements");
    }
    else if (sos.length == 2) {
      var pattern = sos[0], fenderExpr = null, outputExpr = sos[1];
    }
    else if (sos.length == 3) {
      var pattern = sos[0], fenderExpr = sos[1], outputExpr = sos[2];
      throw "TODO: fender";
    }
    else if (sos.length > 3) {
      throw new Error("syntax-case: clause has too many elements");
    }
    return {
      pattern: pattern,
      fenderExpr: fenderExpr,
      outputExpr: outputExpr,
    };
  },

  // pat: SO (the pattern)
  // outputExpr: SO (the output expression)
  _compileClause: function(pat, outputExpr, env, menv, after) {
    var self = this;
    return this._compile(
      pat.sCdr(), // p
      List(Sym("syntax-cdr"), Sym("x")), // x
      0, // dim
      [], // vars
      // next (cont: You'll get the compiled result by calling `cont`
      //       with a form to embed in it)
      function(vars, cont) {
        return self._compileOutputExpression(outputExpr, vars, env, menv, function(compiledOutExpr) {
          return after(cont(compiledOutExpr));
        });
      }
    );
  },

  _compile: function(p, x, dim, vars, next) {
    debug("_compile", BiwaScheme.to_write(p), BiwaScheme.to_write(x), dim, BiwaScheme.to_write(vars));
    var v = this._newSym();
    var matchThis;
    if (p.isIdentifier()) {
      // TODO: ellipsis, literal, underscore
      var newVars = [].concat(vars, [[p, dim]]); 
      return next(newVars, function(compiledNext) {
        var compiledThis = List(Sym("let"), List(List(v, x)),
                             List(Sym("let"), List(List(p, v)),
                               compiledNext));
        return compiledThis;
      });
    }
    else if (p.isPairSO()) {
      var self = this;
      return this._compile(
        p.sCar(),
        List(Sym("syntax-car"), v),
        dim,
        vars,
        function(vars2, cont2) {
          return self._compile(
            p.sCdr(),
            List(Sym("syntax-cdr"), v),
            dim,
            vars2,
            function(vars3, cont3) {
              return next(vars3, function(compiledNext) {
                var compiledCdr = cont3(compiledNext);
                var compiledCar = cont2(compiledCdr)
                var compiledThis = List(Sym("let"), List(List(v, x)),
                                   List(Sym("and"), List(Sym("syntax-pair?"), v),
                                      compiledCar));
                return compiledThis;
              });
            }
          );
        }
      );
    }
    else if (BiwaScheme.isVector(p.expr)) {
      throw "TODO";
    }
    else if (p.isNullSO()) {
      return next(vars, function(compiledNext) {
        var compiledThis = List(Sym("let"), List(List(v, x)),
                           List(Sym("and"), List(Sym("syntax-null?"), v),
                             compiledNext));
        return compiledThis;
      });
    }
    else { // Constant
      return next(vars, function(compiledNext) {
        var compiledThis = List(Sym("let"), List(List(v, x)),
                           List(Sym("and"), List(Sym("equal?"), v, p),
                             compiledNext));
        return compiledThis;
      });
    }
  },

  _compileOutputExpression: function(outputExpr, vars, env, menv, after) {
    debug("_compileOutputExpression", outputExpr, "vars:", vars)
    var ret = this._createOutExprEnv(outputExpr, vars, env);
    var newEnv = ret[0], substs = ret[1];

    // Use Expander to expand `syntax`
    var outputExpr_ = SyntaxObject.addSubsts(outputExpr, substs);
    return Expander._exp(outputExpr_, newEnv, menv, after);
  },

  // Create a Env for expanding output expression
  _createOutExprEnv: function(outputExpr, vars, origEnv) {
    var self = this;
    var newEnv = origEnv.dup(),
        substs = [];

    // Add pattern variables
    vars.forEach(function(item) {
      var identifier = item[0];
      var label = new Label();
      var binding = new Binding("lexical", identifier.expr);
      newEnv.set(label, binding);
      substs.push(new Subst(identifier, label));
    });

    // Add `syntax`
    var label = new Label();
    var binding = new Binding("macro", function(ar) {
      var so = ar[0];
      debug("(generated `syntax`)", so)
      var sos = so.expose();
      if (sos.length < 2) throw new Error("syntax: missing argument");
      if (sos.length > 2) throw new Error("syntax: too many args");
      var tmpl = sos[1];
      return self._compileTmpl(tmpl, vars, 0, false);
    });
    newEnv.set(label, binding);
    substs.push(new Subst(new SyntaxObject(Sym("syntax"), outputExpr.wrap), label));

    return [newEnv, substs];
  },

  // t: SyntaxObject
  // vars: `[[Symbol, dim], ...]`
  _compileTmpl: function(t, vars, dim, isEllipsisEscape) {
    if (t.isIdentifier()) {
      // Check this is a pattern variable
      var found = vars.find(function(x) { return x[0].expr === t.expr });
      if (found) {
        if (found[1] > dim) {
          throw new Error("syntax-case: to few ...'s");
        }
        return t;
      }
      else {
        // It's just a data (a symbol)
        return List(Sym("quote"), t);
      }
    }
    else if (t.isPairSO()) {
      // TODO: ellipsis-escape?, ellipsis?
      var compiledCar = this._compileTmpl(t.sCar(), vars, dim, isEllipsisEscape);
      var compiledCdr = this._compileTmpl(t.sCdr(), vars, dim, isEllipsisEscape);
      return List(Sym("cons"), compiledCar, compiledCdr);
    }
    else if (t.isVectorSO()) {
      throw "TODO"
    }
    else if (t.isNullSO()) {
      // REVIEW: should these be `id.sym()` ??
      return nil; // REVIEW: should be List(Sym("quote"), nil); ? 
    }
    else { // Constant
      return t;
    }
  }
});


// TODO: merge this with BiwaScheme.TopEnv and BiwaScheme.CoreEnv
// SyntaxEnv contains
// - user-defined global variables
// - stdlibs (like "let", "cond")
// - core forms (like "set!", "lambda")
BiwaScheme.SyntaxEnv = {};
BiwaScheme.global_variable_set = function(name, type, value) {
  BiwaScheme.SyntaxEnv[name] = new BiwaScheme.Syntax.Binding(type, value);
};
BiwaScheme.Syntax.INITIAL_ENV_ITEMS.forEach(function(item){
  var name = item[0], type = item[1], value = item[2];
  BiwaScheme.global_variable_set(name, type, value);
});

// Aliases for this file
var Syntax = BiwaScheme.Syntax,
    SyntaxObject = BiwaScheme.Syntax.SyntaxObject,
    Wrap = BiwaScheme.Syntax.Wrap,
    Mark = BiwaScheme.Syntax.Mark,
    Label = BiwaScheme.Syntax.Label,
    Binding = BiwaScheme.Syntax.Binding,
    Subst = BiwaScheme.Syntax.Subst,
    Env = BiwaScheme.Syntax.Env,
    Expander = BiwaScheme.Expander,
    LambdaHelper = BiwaScheme.Expander.LambdaHelper,

    nil = BiwaScheme.nil,
    inspect = BiwaScheme.inspect,
    List = BiwaScheme.List,
    ListA = BiwaScheme.ListA,
    Pair = BiwaScheme.Pair,
    Sym = BiwaScheme.Sym,

    debug = function(/*arguments*/){
      if(BiwaScheme.Syntax.TRACE_EXPANSION) {
        var args = Array.prototype.slice.call(arguments).map(function(x) {
          if (_.isString(x)) return x;
          else return inspect(x);
        });
        console.log.apply(null, args);
      }
    };

})();
