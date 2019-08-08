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
    if (!this.func){
      throw new BiwaScheme.Bug("sorry, syntax "+this.sname+
                               " is a pseudo syntax now");
    }
    return this.func(x);
  },
  inspect: function(){
    return "#<Syntax " + this.sname +">";
  }
});

// A built-in syntax did not have associated Syntax object.
// Following code installed dummy Syntax objects to built-in syntax.
BiwaScheme.CoreEnv["define"] = new BiwaScheme.Syntax("define");
BiwaScheme.CoreEnv["begin"]  = new BiwaScheme.Syntax("begin");
BiwaScheme.CoreEnv["quote"]  = new BiwaScheme.Syntax("quote");
BiwaScheme.CoreEnv["lambda"] = new BiwaScheme.Syntax("lambda");
BiwaScheme.CoreEnv["if"]     = new BiwaScheme.Syntax("if");
BiwaScheme.CoreEnv["set!"]   = new BiwaScheme.Syntax("set!");

//
// syntax-case
//

BiwaScheme.Syntax.TRACE_EXPANSION = true;

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
    if (!wrap) throw new BiwaScheme.Error("[BUG] wrap not specified");
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

    console.error("getLabel notfound", BiwaScheme.to_write(this.expr), this.wrap.debugStr());
    throw new BiwaScheme.Error("undefined identifier: "+sym.name);
  },

  // Return true if this SO represents an identifier
  isIdentifier: function() {
    return this.expr instanceof BiwaScheme.Symbol;
  },

  // Return true if this SO contains nil
  isNullSO: function() {
    return this.expr === BiwaScheme.nil;
  },

  // Return true if this SO contains a Pair
  isPairSO: function() {
    return this.expr instanceof BiwaScheme.Pair;
  },

  // Extract car as SO
  sCar: function() {
    return SyntaxObject.wrapWith(this.wrap, this.expr.car);
  },

  // Extract cdr as SO
  sCdr: function() {
    return SyntaxObject.wrapWith(this.wrap, this.expr.cdr);
  },

  // Decompose #SO<List> into an array of #<SO>
  // Note: Last cdr is just ignored (even if it is not nil)
  expose: function() {
    if (this.expr instanceof Pair) {
      var wrap = this.wrap;
      return this.expr.to_array().map(function(sub) {
        return new SyntaxObject.wrapWith(wrap, sub);
      });
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
    var subst = new Subst(id.expr, id.wrap.marks(), label);
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
  initialize: function(sym, marks, label){
    this.sym = sym;     // Symbol
    this.marks = marks; // Array<Mark>
    this.label = label; // Label
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
    return this.markSubsts.map(function(x){ return x.debugStr() }).join(",");
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
        if (!_.isFunction(value))
          throw new BiwaScheme.Bug("expected function but got "+BiwaScheme.inspect(value));
        break;
      case "lexical":
      case "global":
        if (!(value instanceof BiwaScheme.Symbol))
          throw new BiwaScheme.Bug("expected symbol but got "+BiwaScheme.inspect(value));
        break;
    }
    this.type = type;   // either of "macro" "lexical" "core" "global"
    this.value = value; // expander for "macro", "core". Symbol for "lexical", "global"
  },

  inspect: function(){
    return "#<Binding("+this.type+", "+this.value+")>";
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
  ["quote", "core", function(so, env, metaEnv){
    var x = so.expr;
    if (!(x.cdr instanceof Pair)) throw new Error("quote: missing argument");
    if (x.cdr.cdr !== BiwaScheme.nil) throw new Error("quote: too many arguments");
    return x;
  }],

  ["define", "core", function(so, env, metaEnv){
    var ret = LambdaHelper.parseDefine(so, env);
    var name = ret[0], expr = ret[1], isLambda = ret[2];

    if (isLambda) { // `(define (f ...) ...)`
      var label = new Label();
      var binding = new Binding("lexical", name.expr);
      var lambdaSo = SyntaxObject.addSubst(
        name,
        label,
        new SyntaxObject(expr, new Wrap()));
      var newEnv = env.dup().set(label, binding);
      return List(Sym("define"), name, Expander._exp(lambdaSo, newEnv, metaEnv));
    }
    else { 
      return List(Sym("define"), name, Expander._exp(expr, env, metaEnv));
    }
  }],

  ["begin", "core", function(so, env, metaEnv){
    // (begin body...)
    var sos = so.expose();
    if (sos.length < 2) throw new Error("begin: missing body of begin");
    var newBody = sos.slice(1, sos.length).map(function(so){
      return Expander._exp(so, env, metaEnv);
    });
    return new Pair(Sym("begin"), ListA(newBody));
  }],

  ["set!", "core", function(so, env, metaEnv){
    // (set! x v)
    var sos = so.expose();
    if (sos.length != 3) throw new Error("set!: set! takes var and val");
    return List(Sym("set!"),
                Expander._exp(sos[1], env, metaEnv),
                Expander._exp(sos[2], env, metaEnv));
  }],

  ["call/cc", "core", function(so, env, metaEnv){
    // (call/cc f)
    var sos = so.expose();
    if (sos.length < 2) throw new Error("call/cc: missing argument");
    return List(Sym("call/cc"),
                Expander._exp(sos[1], env, metaEnv));
  }],

  ["if", "core", function(so, env, metaEnv){
    var sos = so.expose();
    if (sos.length < 3) throw new Error("if: missing then clause");
    if (sos.length > 4) throw new Error("if: too many clauses");

    var condc = Expander._exp(sos[1], env, metaEnv);
    var thenc = Expander._exp(sos[2], env, metaEnv);
    if (sos[3]) { 
      var elsec = Expander._exp(sos[3], env, metaEnv);
      return List(Sym("if"), condc, thenc, elsec);
    }
    else {
      return List(Sym("if"), condc, thenc, BiwaScheme.undef);
    }
  }],

  ["lambda", "core", function(so, env, metaEnv){
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
      return Expander._exp(newLambda, env, metaEnv);
    }
    else {
      var ret = LambdaHelper.expandBody(paramSpecSo, bodySos, env, metaEnv);
      var newParamSpec = ret[0], newBodyExprs = ret[1];
      return new Pair(Sym("lambda"),
               new Pair(newParamSpec,
                 ListA(newBodyExprs)));
    }
  }],

  ["syntax", "core", function(so, env, metaEnv){
    var x = so.expr;
    if (!(x.cdr instanceof Pair)) throw new Error("quote: missing argument");
    if (x.cdr.cdr !== BiwaScheme.nil) throw new Error("quote: too many arguments");
    var target = x.cdr.car;
    var so = new BiwaScheme.Syntax.SyntaxObject(target);
    return List(Sym("quote"), so);
  }]
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

  // free-identifier=?
  isFreeIdentifierEqual: function(id1, id2) {
    return id1.getLabel() === id2.getLabel();
  },

  // bound-identifier=?
  isBoundIdentifierEqual: function(id1, id2) {
    return id1.expr.name == id2.expr.name &&
           Mark.isSameMarks(id1.wrap.marks(), id2.wrap.marks());
  },

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
  expand: function(expr) {
    var so = new SyntaxObject(expr, Expander.InitialWrap),
        env = Expander.InitialEnv,
        menv = Expander.InitialEnv;
    var ret = SyntaxObject.strip(Expander._exp(so, env, menv));
    debug("Expanded:", BiwaScheme.to_write(ret), "\n");
    return ret;
  },

  // Main loop of Expander (`exp` in "Beautiful Code")
  _exp: function(so, env, menv) {
    if (!BiwaScheme.isSelfEvaluating(so.expr)) debug("_exp", so.inspect(), env.inspect(), menv);

    var expr = so.expr;
    if (expr instanceof BiwaScheme.Symbol) {
      // Variable reference or varref-like macro call
      var binding = env.bindingOfId(so);
      switch(binding.type) {
        case "macro":
          var newx = Expander._expandMacro(so.expr.name, binding.value, so);
          return Expander._exp(newx, env, menv);
        case "lexical":
        case "global":
          return binding.value;
        default:
          throw new BiwaScheme.Error("undefined variable: "+expr.name);
      }
    }
    else if (expr instanceof BiwaScheme.Pair) {
      var sos = so.expose();
      if (sos[0].expr instanceof BiwaScheme.Symbol) {
        // Funcion call or macro call
        var id = sos[0];
        var binding = env.bindingOfId(id);
        switch(binding.type) {
          case "macro":
            var newx = Expander._expandMacro(id.expr.name, binding.value, so);
            return Expander._exp(newx, env, menv);
          case "lexical":
          case "global":
            var car = binding.value;
            var cdr = List.apply(null, Expander._expandExprs(_.rest(sos), env, menv));
            return new SyntaxObject(new Pair(car, cdr), so.wrap)
          case "core":
            return Expander._expandCore(binding.value, so, env, menv);
          default:
            throw "must not happen"
        }
      }
      else {
        // ((func expr...) args...)
        var car = Expander._exp(sos[0], env, menv);
        var cdr = List.apply(null, Expander._expandExprs(_.rest(sos), env, menv));
        return new SyntaxObject(new Pair(car, cdr), so.wrap);
      }
    }
    else {
      // Constant
      var d = SyntaxObject.strip(so);
      if (!BiwaScheme.isSelfEvaluating(d)) {
        throw new Error("misplaced non-self evaluating expr: "+BiwaScheme.to_write(d));
      }
      return d;
    }
  },
  
  // Expand macro use
  // (`exp-macro` in "Beautiful Code")
  // name: String (used only for debug print)
  _expandMacro: function(name, transformer, so) {
    debug("_expandMacro", so.inspect());

    var m = new Mark();
    // Call `addMark` before and after the transformation.
    // Only "new" code are left marked because same marks cancel each other.
    var marked = SyntaxObject.addMark(so, m);
    return SyntaxObject.addMark(transformer(marked), m);
  },

  _expandCore: function(coreTrans, so, env, menv) {
    debug("_expandCore", so.inspect());

    return new SyntaxObject(coreTrans(so, env, menv), so.wrap);
  },

  // - exprs: Array<SO>
  _expandExprs: function(exprs, env, menv) {
    //debug("_expandExprs", exprs);
    return exprs.map(function(so) {
      return Expander._exp(so, env, menv);
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
  // Return:
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
  expandBody: function(paramSpecSo, bodySos, env, metaEnv) {
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
      substs.push(new Subst(names[0].expr, names[0].wrap.marks(), label));
    });

    var newBodyExprs = bodySos.map(function(bodySo){
      var newBodyExpr = SyntaxObject.addSubsts(bodySo, substs);
      return Expander._exp(newBodyExpr, newEnv, metaEnv);
    });
    return [newParamSpec, newBodyExprs];
  }
};

// TODO: merge this with BiwaScheme.TopEnv and BiwaScheme.CoreEnv
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
    List = BiwaScheme.List,
    ListA = BiwaScheme.ListA,
    Pair = BiwaScheme.Pair,
    Sym = BiwaScheme.Sym,

    debug = function(/*arguments*/){
      if(BiwaScheme.Syntax.TRACE_EXPANSION) console.log.apply(null, Array.prototype.slice.call(arguments));
    };

})();
