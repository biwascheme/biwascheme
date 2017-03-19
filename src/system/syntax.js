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

BiwaScheme.Syntax.SyntaxObject = BiwaScheme.Class.create({
  initialize: function(expr, wrap){
    this.expr = expr;
    if (!wrap) throw new BiwaScheme.Error("[BUG] wrap not specified");
    this.wrap = wrap;
  },

  // Return the label corresponding to this identifier
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
          debug("getLabel", sym.name, subst.debugStr());
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

  // Decompose #SO<List> into an array of #<SO>
  expose: function() {
    if (this.expr instanceof Pair) {
      var wrap = this.wrap;
      return this.expr.to_array().map(function(sub) {
        if (sub instanceof SyntaxObject) {
          return new SyntaxObject(sub.expr, wrap.joinWrap(sub.wrap));
        }
        else {
          return new SyntaxObject(sub, wrap);
        }
      });
    }
    else {
      throw new BiwaScheme.Bug("this so is not a pair: "+BiwaScheme.to_write(this.expr));
    }
  },

  // Returns a Symbol of the same context as this
  sym: function(name) {
    return new SyntaxObject(Sym(name), this.wrap);
  },
  
  inspect: function() {
    if (Syntax.inspecting) { // Simplify nested SyntaxObject
      if (BiwaScheme.isSelfEvaluating(this.expr)) {
        return BiwaScheme.to_write(this.expr) + "{-}";
      }
      else {
        return BiwaScheme.to_write(this.expr) +
          "{" + this.wrap.debugStr() + "}";
      }
    }
    else {
      Syntax.inspecting = true;
      var ret = "#<SO " + BiwaScheme.to_write(this.expr) +
                " " + this.wrap.debugStr() + ">";
      Syntax.inspecting = false;
      return ret;
    }
  }
});

_.extend(BiwaScheme.Syntax.SyntaxObject, {
  // Return a SyntaxObject with a mark
  // x : SyntaxObject or scheme expr
  addMark: function(x, mark) {
    if (x instanceof SyntaxObject) {
      return new SyntaxObject(x.expr, x.wrap.addMark(mark));
    }
    else {
      return new SyntaxObject(x, new Wrap([mark]));
    }
  },

  // Return a SyntaxObject with a subst
  // id : identifier(SyntaxObject with Symbol)
  // label: Label
  // x : SyntaxObject or scheme expr
  addSubst: function(id, label, x) {
    var subst = new Subst(id.expr, id.wrap.marks(), label);
    if (x instanceof SyntaxObject) {
      return new SyntaxObject(x.expr, x.wrap.addSubst(subst));
    }
    else {
      return new SyntaxObject(x, new Wrap([subst]));
    }
  },

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
    return "m" + this.n;
  },

  toString: function() {
    return "m" + this.n;
  },

  inspect: function(){
    return "#<Mark "+this.n+">";
  }
});
BiwaScheme.Syntax.Mark.n = 0;
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
    var marks = this.marks.map(function(m){ return m.debugStr() }).join("");
    return "[" + this.sym.name + "(" + marks + ")" + this.label.debugStr() + "]";
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
    var ret = [];
    this.markSubsts.forEach(function(x){
      if (x instanceof Subst && x.marks[0] === Mark.TopMark) {
        // Omit INITIAL_ENV_ITEMS
        if (_.last(ret) != "...") ret.push("...");
      }
      else {
        ret.push(x.debugStr());
      }
    });
    return ret.join("");
  },

  toString: function() {
    return "#<Wrap" + this.debugStr() + ">";
  }
});

BiwaScheme.Syntax.Binding = BiwaScheme.Class.create({
  initialize: function(type, value){
    this.type = type;   // either of "macro" "lexical" "core"
    this.value = value;
  },

  inspect: function(){
    return "#<Binding("+this.type+")>";
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

  get: function(label) {
    return this.hash[label.name];
  },

  // - id: identifier SO
  bindingOfId: function(id) {
    var label = id.getLabel();
    if (label == Label.TopLevel) {
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
    if (keys.length == 0)
      return "#<Env>";
    else
      return "#<Env " + _.keys(this.hash).join(",") + ">";
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
    // TODO: is it really safe to access so.expr directly?
    var expr = so.expr;
    if (!(expr.cdr instanceof BiwaScheme.Pair))
      throw new Error("define: malformed define");
    var cadr = expr.cdr.car;
    if (cadr instanceof BiwaScheme.Symbol) {
      // (define var expr)
      var cddr = expr.cdr.cdr;
      if (!(cddr instanceof BiwaScheme.Pair) || cddr.cdr !== BiwaScheme.nil)
        throw new Error("define: malformed define");
      var newExpr = Expander._exp(new SyntaxObject(cddr.car, so.wrap), env, metaEnv);
      return List(Sym("define"), cadr, newExpr);
    }
    else if (cadr instanceof BiwaScheme.Pair) {
      // (define (fname arg...) body...)
      // => (define fname (lambda (arg...) body...))
      // (define (fname arg... . rest) body...)
      // => (define fname (lambda (arg... . rest) body...))
      // (define (fname . args) body...)
      // => (define fname (lambda args body...))
      var fname = cadr.car, argSpec = cadr.cdr;
      if (!(fname instanceof BiwaScheme.Symbol))
        throw new Error("define: invalid function name");
      var label = new Label(), binding = new Binding("lexical", fname);
      var lambdaExpr = new Pair(Sym("lambda"),
                         new Pair(argSpec, 
                           expr.cdr.cdr));
      var lambdaSo = SyntaxObject.addSubst(
        new SyntaxObject(fname, new Wrap()),
        label,
        new SyntaxObject(lambdaExpr, new Wrap()));
      var newEnv = env.dup().set(label, binding);
      var newLambdaExpr = Expander._exp(lambdaSo, newEnv, metaEnv);
      return List(Sym("define"), fname, newLambdaExpr);
    }
    else {
      throw new Error("define: malformed define");
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
    var sos = so.expose();
    if (sos.length < 3) throw new BiwaScheme.Error("malformed lambda");
    var paramSpec = sos[1].expr;
    var bodySos = _.rest(sos, 2);

    var lastCdr = Expander.LambdaHelper.validateParamSpec(paramSpec);

    var ret = Expander.LambdaHelper.generateVariables(paramSpec, lastCdr);
    var newParamSpec = ret[0],
        paramNames = ret[1];

    var newBodyExprs = Expander.LambdaHelper.expandBody(
      bodySos, paramNames, sos[1].wrap, env, metaEnv);

    return new Pair(Sym("lambda"),
             new Pair(newParamSpec,
               ListA(newBodyExprs)));
  }],

  ["syntax", "core", function(so, env, metaEnv){
    var x = so.expr;
    if (!(x.cdr instanceof Pair)) throw new Error("quote: missing argument");
    if (x.cdr.cdr !== BiwaScheme.nil) throw new Error("quote: too many arguments");
    var target = x.cdr.car;
    var so = new BiwaScheme.Syntax.SyntaxObject(target);
    return List(Sym("quote"), so);
  }],

  ["let", "macro", function(so){
    // (let ((a 1) (b 2)) (+ a b))
    // => ((lambda (a b) (+ a b)) 1 2)
    // (let loop ((a 1) (b 2)) body ..)
    //=> (letrec ((loop (lambda (a b) body ..))) (loop 1 2))
    var sos = so.expose(); // [let, (name?), binds, bodyExpr, ...]

    if (sos[1].expr instanceof Symbol) {
      // named let
      TODO
    }
    else {
      var vars = nil, vals = nil;
      var binds = sos[1].expose();
      binds.forEach(function(bind) {
        var a = bind.expose();
        vars = new Pair(a[0], vars);
        vals = new Pair(a[1], vals);
      });

      var bodies = _.rest(sos, 2);
                                          // Not sure
      var ret = new Pair(List.apply(null, [sos[0].sym("lambda"), vars].concat(bodies)),
                         vals);
      return new SyntaxObject(ret, new Wrap());
    }
  }],

  ["orr", "macro", function(so){
    //debug("orr", so.inspect());
    var sos = so.expose();
    var id = sos[0];
    var e1 = sos[1], e2 = sos[2];
    var ret = List(id.sym("let"),
                List(List(Sym("t"), e1)),
                List(id.sym("if"), Sym("t"), Sym("t"), e2));
    return new SyntaxObject(ret, new Wrap());
  }],

  ["swap!", "macro", function(so){
    //debug("swap!", so.inspect());
    // (swap! a b)
    // = (let ((temp #'a)) (set! #'a #'b) (set! b temp))
    var sos = so.expose();
    var id = sos[0];
    var a = sos[1], b = sos[2];
    var ret = List(id.sym("let"),
                List(List(Sym("temp"), a)),
                List(id.sym("set!"), a, b),
                List(id.sym("set!"), b, Sym("temp")));
    return new SyntaxObject(ret, new Wrap());
  }]
];

// Misc

_.extend(BiwaScheme.Syntax, {
  genVar: function(prefix){
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

  expand: function(expr) {
    var so = new SyntaxObject(expr, Expander.InitialWrap),
        env = Expander.InitialEnv,
        menv = Expander.InitialEnv;
    var ret = SyntaxObject.strip(Expander._exp(so, env, menv));
    debug("Expanded:", BiwaScheme.to_write(ret));
    return ret;
  },

  _exp: function(so, env, menv) {
    if (!BiwaScheme.isSelfEvaluating(so.expr)) debug("_exp", so.inspect(), env, menv);

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
  // - paramSpec: scheme expr
  validateParamSpec: function(paramSpec) {
    // Validate parameter spec 
    var lastCdr;
    if (paramSpec instanceof BiwaScheme.Pair) {
      var x;
      for (x = paramSpec; x instanceof BiwaScheme.Pair; x = x.cdr) {
        if (!(x.car instanceof BiwaScheme.Symbol)) {
          throw new Error("lambda: invalid parameter name: "+BiwaScheme.to_write(paramSpec));
        }
      }
      lastCdr = x;
      if (lastCdr instanceof BiwaScheme.Symbol) { // (lambda (x y . rest) ...)
        // ok
      }
      else if (lastCdr !== BiwaScheme.nil) {
        throw new Error("lambda: invalid rest parameter: "+BiwaScheme.to_write(paramSpec));
      }
    }
    else if (paramSpec === BiwaScheme.nil || // (lambda () ...)
             paramSpec instanceof BiwaScheme.Symbol) { // (lambda args ...)
      // ok
    }
    else {
      throw new Error("lambda: invalid parameter spec: "+BiwaScheme.inspec(paramSpec));
    }
    return lastCdr;
  },

  // Generate new name for parameters
  // - paramSpec: scheme expr
  // - lastCdr: Symbol or nil
  generateVariables: function(paramSpec, lastCdr) {
    var newParamSpec, paramNames;
    if (paramSpec === BiwaScheme.nil) {
      newParamSpec = BiwaScheme.nil;
      paramNames = [];
    }
    else if (paramSpec instanceof BiwaScheme.Symbol) {
      newParamSpec = Syntax.genVar(paramSpec.name);
      paramNames = [[paramSpec, newParamSpec]];
    }
    else {
      paramNames = paramSpec.to_array().map(function(sym){
        return [sym, Syntax.genVar(sym.name)];
      });
      var lastIdx;
      if (lastCdr instanceof BiwaScheme.Symbol) {
        newParamSpec = Syntax.genVar(lastCdr.name)
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

  expandBody: function(bodySos, paramNames, wrap, env, metaEnv) {
    var newEnv = env.dup();
    var params = paramNames.map(function(names) {
      var label = new Label();
      var binding = new Binding("lexical", names[1]);
      newEnv.set(label, binding);
      return {
        so: new SyntaxObject(names[0], wrap),
        label: label
      }
    });

    var newBodyExprs = bodySos.map(function(bodySo){
      var newBodyExpr = params.reduce(function(b, param) {
        return SyntaxObject.addSubst(param.so, param.label, b);
      }, bodySo);
      return Expander._exp(newBodyExpr, newEnv, metaEnv);
    });

    return newBodyExprs;
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

    nil = BiwaScheme.nil,
    List = BiwaScheme.List,
    ListA = BiwaScheme.ListA,
    Pair = BiwaScheme.Pair,
    Sym = BiwaScheme.Sym,

    debug = function(/*arguments*/){
      if(BiwaScheme.Syntax.TRACE_EXPANSION) console.log.apply(null, Array.prototype.slice.call(arguments));
    };

})();
