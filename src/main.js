import { TopEnv, CoreEnv, nil, undef, max_trace_size, suppress_deprecation_warning } from "./header.js";
import { VERSION, GitCommit } from "./version.js";

import { isNil, isUndef, isBoolean, isString, isFunction, isChar, isSymbol, isPort,
         isVector, isProcedure,
         isSelfEvaluating, eq, eqv, equal, lt } from "./system/_types.js"
import { to_write, to_display, inspect, write_shared } from "./system/_writer.js"
import Call from "./system/call.js"
import Char from "./system/char.js"
import { Closure, isClosure } from "./system/closure.js"
import Compiler from "./system/compiler.js"
import { Enumeration, isEnumSet } from "./system/enumeration.js"
import { BiwaError, Bug, UserError } from "./system/error.js"
import { Hashtable, isHashtable, isMutableHashtable } from "./system/hashtable.js"
import Interpreter from "./system/interpreter.js"
import { Complex, Rational, isNumber, isComplex, isReal, isRational, isInteger } from "./system/number.js"
import { Pair, List, isPair, isList, array_to_list, deep_array_to_list, Cons } from "./system/pair.js"
import Parser from "./system/parser.js"
import Pause from "./system/pause.js"
import { Port, eof } from "./system/port.js"
import { BiwaPromise, isPromise } from "./system/promise.js"
import { Record, isRecord, isRecordTD, isRecordCD } from "./system/record.js"
import BiwaSet from "./system/set.js"
import { BiwaSymbol, Sym, gensym } from "./system/symbol.js"
import Syntax from "./system/syntax.js"
import Values from "./system/values.js"
import VMCode from "./system/vmcode.js"

import "./library/extra_lib.js"
import "./library/js_interface.js"
import "./library/r6rs_lib.js"
import "./library/srfi.js"

// For unit testing
import { define_libfunc, define_scmfunc, parse_fraction, is_valid_integer_notation, parse_integer, is_valid_float_notation, parse_float } from "./library/infra.js"

// Avoid circular dependency
nil.to_set = function(){ return new BiwaSet(); };

export default {
  TopEnv, CoreEnv, nil, undef, max_trace_size, suppress_deprecation_warning,
  Version: VERSION, VERSION, GitCommit,
  isNil, isUndef, isBoolean, isString, isChar, isSymbol, isPort, isPair, isList,
    isVector, isHashtable, isMutableHashtable, isProcedure,
    isSelfEvaluating, eq, eqv, equal, lt,
  to_write, to_display, inspect,
  write_ss: write_shared, to_write_ss: write_shared, // For backward compatibility
  Call,
  Char,
  Closure, isClosure,
  Compiler,
  Enumeration, isEnumSet,
  Error: BiwaError, Bug, UserError,
  Hashtable,
  Interpreter,
  Complex, Rational, isNumber, isComplex, isReal, isRational, isInteger,
  Pair, List, array_to_list, deep_array_to_list, Cons,
  Parser,
  Pause,
  Port, eof,
  Promise: BiwaPromise, isPromise,
  Record, isRecord, isRecordTD, isRecordCD,
  Set: BiwaSet,
  Symbol: BiwaSymbol, Sym, gensym,
  Syntax,
  Values,
  VMCode,

  define_libfunc, define_scmfunc, parse_fraction, is_valid_integer_notation, parse_integer, is_valid_float_notation, parse_float,
};
