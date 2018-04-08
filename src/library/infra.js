///
/// infra.js - Basis for library functions
///

//
// define_*func - define library functions
//
BiwaScheme.check_arity = function(len, min, max){
  var fname = arguments.callee.caller
                ? arguments.callee.caller.fname
                : "(?)";
  if(len < min){
    if(max && max == min)
      throw new BiwaScheme.Error(fname+": wrong number of arguments (expected: "+min+" got: "+len+")");
    else
      throw new BiwaScheme.Error(fname+": too few arguments (at least: "+min+" got: "+len+")");
  }
  else if(max && max < len)
    throw new BiwaScheme.Error(fname+": too many arguments (at most: "+max+" got: "+len+")");
}
BiwaScheme.define_libfunc = function(fname, min, max, func){
  var f = function(ar, intp){
    BiwaScheme.check_arity(ar.length, min, max);
    return func(ar, intp);
  };

  func["fname"] = fname; // for assert_*
  f["fname"]    = fname; // for check_arity
  f["inspect"] = function(){ return this.fname; }
  BiwaScheme.CoreEnv[fname] = f;
}
BiwaScheme.alias_libfunc = function(fname, aliases) {
  if (BiwaScheme.CoreEnv[fname]) {
    if (_.isArray(aliases)) {
      _.map(aliases, function(a) { BiwaScheme.alias_libfunc(fname, a); });
    } else if (_.isString(aliases)) {
      BiwaScheme.CoreEnv[aliases] = BiwaScheme.CoreEnv[fname];
    } else {
      console.error("[BUG] bad alias for library function " +
                    "`" + fname + "': " + aliases.toString());
    }
  } else {
    console.error("[BUG] library function " +
                  "`" + fname + "'" +
                  " does not exist, so can't alias it.");
  }
};
BiwaScheme.define_syntax = function(sname, func) {
  var s = new BiwaScheme.Syntax(sname, func);
  BiwaScheme.CoreEnv[sname] = s;
}
BiwaScheme.define_scmfunc = function(fname, min, max, str){
  (new BiwaScheme.Interpreter).evaluate("(define "+fname+" "+str+"\n)");
}

//  define_scmfunc("map+", 2, null, 
//    "(lambda (proc ls) (if (null? ls) ls (cons (proc (car ls)) (map proc (cdr ls)))))");

//
// assertions - type checks
//
BiwaScheme.make_assert = function(check){
  return function(/*args*/){
    var fname = arguments.callee.caller
                  ? arguments.callee.caller.fname
                  : "";
    check.apply(this, [fname].concat(_.toArray(arguments)));
  }
}
BiwaScheme.make_simple_assert = function(type, test, _fname){
  return BiwaScheme.make_assert(function(fname, obj, opt){
    if(_fname) fname = _fname;
    option = opt ? ("("+opt+")") : ""
    if(!test(obj)){
      throw new BiwaScheme.Error(fname + option + ": " +
                                 type + " required, but got " +
                                 BiwaScheme.to_write(obj));
    }
  })
}

BiwaScheme.assert_number = BiwaScheme.make_simple_assert("number", function(obj){
  return typeof(obj) == 'number' || (obj instanceof BiwaScheme.Complex);
});

BiwaScheme.assert_integer = BiwaScheme.make_simple_assert("integer", function(obj){
  return typeof(obj) == 'number' && (obj % 1 == 0)
});

BiwaScheme.assert_real = BiwaScheme.make_simple_assert("real number", function(obj){
  return typeof(obj) == 'number';
});

BiwaScheme.assert_between = BiwaScheme.make_assert(function(fname, obj, from, to){
  if( typeof(obj) != 'number' || obj != Math.round(obj) ){
    throw new BiwaScheme.Error(fname + ": " +
                               "number required, but got " +
                               BiwaScheme.to_write(obj));
  }

  if( obj < from || to < obj ){
    throw new BiwaScheme.Error(fname + ": " +
                               "number must be between " +
                               from + " and " + to + ", but got " +
                               BiwaScheme.to_write(obj));
  }
});

BiwaScheme.assert_string = BiwaScheme.make_simple_assert("string", _.isString);

BiwaScheme.assert_char = BiwaScheme.make_simple_assert("character", BiwaScheme.isChar);
BiwaScheme.assert_symbol = BiwaScheme.make_simple_assert("symbol", BiwaScheme.isSymbol);
BiwaScheme.assert_port = BiwaScheme.make_simple_assert("port", BiwaScheme.isPort);
BiwaScheme.assert_pair = BiwaScheme.make_simple_assert("pair", BiwaScheme.isPair);
BiwaScheme.assert_list = BiwaScheme.make_simple_assert("list", BiwaScheme.isList);
BiwaScheme.assert_vector = BiwaScheme.make_simple_assert("vector", BiwaScheme.isVector);

BiwaScheme.assert_hashtable = BiwaScheme.make_simple_assert("hashtable",
                                          BiwaScheme.isHashtable);
BiwaScheme.assert_mutable_hashtable = BiwaScheme.make_simple_assert("mutable hashtable",
                                            BiwaScheme.isMutableHashtable);

BiwaScheme.assert_record = BiwaScheme.make_simple_assert("record",
                                          BiwaScheme.isRecord);
BiwaScheme.assert_record_td = BiwaScheme.make_simple_assert("record type descriptor",
                                          BiwaScheme.isRecordTD);
BiwaScheme.assert_record_cd = BiwaScheme.make_simple_assert("record constructor descriptor",
                                          BiwaScheme.isRecordCD);
BiwaScheme.assert_enum_set = BiwaScheme.make_simple_assert("enum_set",
                                          BiwaScheme.isEnumSet);
BiwaScheme.assert_promise = BiwaScheme.make_simple_assert("promise",
                                          BiwaScheme.isPromise);

BiwaScheme.assert_function = BiwaScheme.make_simple_assert("JavaScript function",
                                         _.isFunction);
BiwaScheme.assert_closure = BiwaScheme.make_simple_assert("scheme function",
                                        BiwaScheme.isClosure);
BiwaScheme.assert_procedure = BiwaScheme.make_simple_assert("scheme/js function", function(obj){
  return BiwaScheme.isClosure(obj) || _.isFunction(obj);
});

BiwaScheme.assert_date = BiwaScheme.make_simple_assert("date", function(obj){
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

BiwaScheme.assert = BiwaScheme.make_assert(function(fname, success, message, _fname){
  if(!success){
    throw new BiwaScheme.Error((_fname || fname)+": "+message);
  }
});

//
// deprecation
//

// Show deprecation warnig
// @param {string} title - feature to be deprecated
// @param {string} ver - when it will be removed (eg. "1.0")
// @param {string} alt - alternatives
BiwaScheme.deprecate = function(title, ver, alt){
  if(BiwaScheme.suppress_deprecation_warning) return;

  var msg = title+" is deprecated and will be removed in BiwaScheme "+ver+ ". "+
            "Please use "+alt+" instead";
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
BiwaScheme.parse_fraction = function(rep) {
  BiwaScheme.assert_string(rep);

  var frac_parts = rep.split('/');

  if (frac_parts.length !== 2)
    return false;

  var num_rep = frac_parts[0];
  var denom_rep = frac_parts[1];

  var num = BiwaScheme.parse_integer(num_rep, 10);
  var denom = BiwaScheme.parse_integer(denom_rep, 10);

  if (num === false || denom === false)
    return false;

  if (denom <= 0)
    return false;

  return num / denom;
};

// Given a string notation of an integer, and the radix, validates the
// notation: returns true if the notation is valid, otherwise false.
//
// @param {string} rep - the string representation of the integer
// @param {integer} rdx - the radix, where 2 <= rdx <= 36
// @return {boolean}
BiwaScheme.is_valid_integer_notation = function(rep, rdx) {
  BiwaScheme.assert_string(rep);
  BiwaScheme.assert_integer(rdx);

  if (rdx < 2 || rdx > 36)
    return false;

  var rdx_symbols = '0123456789abcdefghijklmnopqrstuvwxyz';

  var valid_symbols = rdx_symbols.slice(0, rdx);
  var sym_regex = new RegExp('^[+-]?' + '[' + valid_symbols + ']+$', 'ig');

  return sym_regex.test(rep);
};

// Parse an integer. If the integer does not have a valid representation, or
// produces NaN, - false is returned. If the radix is not within [2..36]
// range, false is returned as well.
//
// @param {string} rep - the string representation of the integer
// @param {integer} rdx - the radix, where 2 <= rdx <= 36
// @return {integer|false}
BiwaScheme.parse_integer = function(rep, rdx) {
  BiwaScheme.assert_string(rep);
  BiwaScheme.assert_integer(rdx);

  if (rdx < 2 || rdx > 36)
    return false;

  if (!BiwaScheme.is_valid_integer_notation(rep, rdx))
    return false;

  var res = parseInt(rep, rdx);

  if (Number.isNaN(res))
    return false;

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
BiwaScheme.is_valid_float_notation = function(rep) {
  BiwaScheme.assert_string(rep);

  var sci_regex = /^[+-]?[0-9]+[.]?[0-9]*e[+-]?[0-9]+$/i;
  var fp_regex  = /(^[+-]?[0-9]*[.][0-9]+$)|(^[+-]?[0-9]+[.][0-9]*$)/;

  if (sci_regex.test(rep) || fp_regex.test(rep))
    return true;

  return BiwaScheme.is_valid_integer_notation(rep, 10);
};

// Parse a floating-point number. If the floating-point number does not have a
// valid representation, or produces -Infinity, +Infinity or NaN, - false is
// returned.
//
// @param {string} rep - the string representation of the floating-point value
// @return {float|false}
BiwaScheme.parse_float = function(rep) {
  BiwaScheme.assert_string(rep);

  if (!BiwaScheme.is_valid_float_notation(rep))
    return false;

  var res = new Number(rep).valueOf();

  if (Number.isNaN(res))
    return false;

  if (!Number.isFinite(res))
    return false;

  return res;
};
