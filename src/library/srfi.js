import * as _ from "../deps/underscore-esm.js"
import { nil, undef } from "../header.js";
import { define_libfunc, alias_libfunc, define_syntax, define_scmfunc,
         assert_number, assert_integer, assert_real, assert_between, assert_string,
         assert_char, assert_symbol, assert_port, assert_pair, assert_list,
         assert_vector,
         assert_function, assert_closure, assert_procedure, assert_date, assert, deprecate } from "./infra.js"; 
import { to_write, to_display, write_shared } from "../system/_writer.js"
import Console from "../system/console.js"
import { Bug } from "../system/error.js"
import { Pair, isPair, array_to_list, deep_array_to_list } from "../system/pair.js"
import { Port } from "../system/port.js"
import { Sym } from "../system/symbol.js"

//
// srfi.js - SRFI libraries
//
// should be src/library/srfi/1.js, etc (in the future).
//
  
//
// srfi-1 (list)
//
// (iota count start? step?)
define_libfunc("iota", 1, 3, function(ar){
  var count = ar[0];
  var start = ar[1] || 0;
  var step = (ar[2]===undefined) ? 1 : ar[2];
  assert_integer(count);
  assert_number(start);
  assert_number(step);

  var a = [], n = start;
  for(var i=0; i<count; i++){
    a.push(n);
    n += step;
  }
  return array_to_list(a);
});

var copy_pair = function(pair){
  var car = isPair(pair.car) ? copy_pair(pair.car)
                             : pair.car;
  var cdr = isPair(pair.cdr) ? copy_pair(pair.cdr)
                             : pair.cdr;
  return new Pair(car, cdr);
};
// (list-copy list)
define_libfunc("list-copy", 1, 1, function(ar){
  if(isPair(ar[0])){
    return copy_pair(ar[0]);
  }
  else{
    return nil;
  }
});

//
// srfi-6 & Gauche (string port)
// 
define_libfunc("open-input-string", 1, 1, function(ar){
  assert_string(ar[0]);
  return new Port.StringInput(ar[0]);
})
define_libfunc("open-output-string", 0, 0, function(ar){
  return new Port.StringOutput();
})
define_libfunc("get-output-string", 1, 1, function(ar){
  assert_port(ar[0]);
  if(!(ar[0] instanceof Port.StringOutput))
    throw new Error("get-output-string: port must be made by 'open-output-string'");
  return ar[0].output_string();
})

//
// srfi-8 (receive)
//

// (receive <formals> <expression> <body>...)
// -> (call-with-values (lambda () expression)
//                        (lambda formals body ...))
define_syntax("receive", function(x){
  assert(isPair(x.cdr),
         "missing formals", "receive");
  var formals = x.cdr.car;
  assert(isPair(x.cdr.cdr),
         "missing expression", "receive");
  var expression = x.cdr.cdr.car;
  var body       = x.cdr.cdr.cdr;
  
  return deep_array_to_list([Sym("call-with-values"),
    [Sym("lambda"), nil, expression],
    new Pair(Sym("lambda"),
      new Pair(formals, body))]);
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
define_libfunc("current-date", 0, 1, function(ar){
  //todo: tz-offset (ar[1])
  return new Date();
})
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
define_libfunc("date?", 1, 1, function(ar){
  return (ar[0] instanceof Date);
})
define_libfunc("date-nanosecond", 1, 1, function(ar){
  assert_date(ar[0]);
  return ar[0].getMilliseconds() * 1000000;
})
define_libfunc("date-millisecond", 1, 1, function(ar){ // not srfi-19
  assert_date(ar[0]);
  return ar[0].getMilliseconds();
})
define_libfunc("date-second", 1, 1, function(ar){
  assert_date(ar[0]);
  return ar[0].getSeconds();
})
define_libfunc("date-minute", 1, 1, function(ar){
  assert_date(ar[0]);
  return ar[0].getMinutes();
})
define_libfunc("date-hour", 1, 1, function(ar){
  assert_date(ar[0]);
  return ar[0].getHours();
})
define_libfunc("date-day", 1, 1, function(ar){
  assert_date(ar[0]);
  return ar[0].getDate();
})
define_libfunc("date-month", 1, 1, function(ar){
  assert_date(ar[0]);
  return ar[0].getMonth() + 1; //Jan = 0 in javascript..
})
define_libfunc("date-year", 1, 1, function(ar){
  assert_date(ar[0]);
  return ar[0].getFullYear();
})
//date-zone-offset
//date-year-day
define_libfunc("date-week-day", 1, 1, function(ar){
  assert_date(ar[0]);
  return ar[0].getDay();
})
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
  full_weekday: ["Sunday", "Monday", "Tuesday", 
    "Wednesday", "Thursday", "Friday", "Saturday"],
  month: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
  full_month: ["January", "February", "March", "April",
    "May", "June", "July", "August", "September", 
    "Octorber", "November", "December"]
}

const date2string = function(date, format){
  var zeropad  = function(n){ return n<10 ? "0"+n : ""+n }; 
  var spacepad = function(n){ return n<10 ? " "+n : ""+n }; 
  var isoweeknum  = function(x){
    var janFour = new Date(x.getFullYear(), 0, 4);
    var weekone = new Date(x.getFullYear(), 0, 4);

    if (janFour.getDay() >= date_names.weekday.indexOf("Thu")) {
      weekone.setDate(janFour.getDate() - (janFour.getDay()+1));
    } else {
      weekone.setDate(janFour.getDate() + ((7 - janFour.getDay()) +1));
    }

    return Math.ceil(((x - weekone) / 86400000) / 7);
  };

  var getter = {
    a: function(x){ return date_names.weekday[x.getDay()] },
    A: function(x){ return date_names.full_weekday[x.getDay()] },
    b: function(x){ return date_names.month[x.getMonth()] },
    B: function(x){ return date_names.full_month[x.getMonth()] },
    c: function(x){ return x.toString() },
    d: function(x){ return zeropad(x.getDate()) },
    D: function(x){ return getter.d(x) + getter.m(x) + getter.y(x); },
    e: function(x){ return spacepad(x.getDate()) },
    f: function(x){ return x.getSeconds() + x.getMilliseconds()/1000; },
    h: function(x){ return date_names.month[x.getMonth()] },
    H: function(x){ return zeropad(x.getHours()) },
    I: function(x){ var h = x.getHours(); return zeropad(h<13 ? h : h-12) },
    j: function(x){ throw new Bug("not implemented: day of year") },
    k: function(x){ return spacepad(x.getHours()) },
    l: function(x){ var h = x.getHours(); return spacepad(h<13 ? h : h-12) },
    m: function(x){ return zeropad(x.getMonth()+1) },
    M: function(x){ return zeropad(x.getMinutes()) },
    n: function(x){ return "\n" },
    N: function(x){ throw new Bug("not implemented: nanoseconds") },
    p: function(x){ return x.getHours()<13 ? "AM" : "PM" },
    r: function(x){ return getter.I(x) + ":" + getter.M(x) + ":" + getter.S(x) + " " + getter.p(x) },
    s: function(x){ return Math.floor(x.getTime() / 1000) },
    S: function(x){ return zeropad(x.getSeconds()) },
    t: function(x){ return "\t" },
    T: function(x){ return getter.H(x) + ":" + getter.M(x) + ":" + getter.S(x) },
    U: function(x){ throw new Bug("not implemented: weeknum(0~, Sun)") },
    V: function(x){ return isoweeknum(x) },
    w: function(x){ return x.getDay() },
    W: function(x){ throw new Bug("not implemented: weeknum(0~, Mon)") },
    x: function(x){ throw new Bug("not implemented: weeknum(1~, Mon)") },
    X: function(x){ return getter.Y(x) + "/" + getter.m(x) + "/" + getter.d(x) },
    y: function(x){ return x.getFullYear() % 100 },
    Y: function(x){ return x.getFullYear() },
    z: function(x){ throw new Bug("not implemented: time-zone") },
    Z: function(x){ throw new Bug("not implemented: symbol time zone") },
    1: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day format") },
    2: function(x){ throw new Bug("not implemented: ISO-8601 hour-minute-second-timezone format") },
    3: function(x){ throw new Bug("not implemented: ISO-8601 hour-minute-second format") },
    4: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day-hour-minute-second-timezone format") },
    5: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day-hour-minute-second format") }
  }

  return format.replace(/~([\w1-5~])/g, function(str, x){
    var func = getter[x];
    if(func)
      return func(date);
    else if(x == "~")
      return "~";
    else
      return x;
  })
}

// date->string date template
define_libfunc("date->string", 1, 2, function(ar){
  assert_date(ar[0]);

  if(ar[1]){
    assert_string(ar[1]);
    return date2string(ar[0], ar[1]);
  }
  else
    return ar[0].toString();
})
// string->date

// parse-date date
define_libfunc("parse-date", 1, 1, function(ar){ // not srfi-19
  assert_string(ar[0]);
  return new Date(Date.parse(ar[0]));
})

//
// srfi-27
//
define_libfunc("random-integer", 1, 1, function(ar){
  var n = ar[0];
  assert_integer(n);
  if (n < 0)
    throw new Error("random-integer: the argument must be >= 0");
  else
    return Math.floor(Math.random() * ar[0]);
});
define_libfunc("random-real", 0, 0, function(ar){
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
define_libfunc("format", 1, null, function(ar){
  if (typeof ar[0] === "string") {
    var port = null, format_str = ar.shift();
  }
  else if (ar[0] === false) {
    ar.shift();
    var port = null, format_str = ar.shift();
  }
  else if (ar[0] === true) {
    ar.shift();
    var port = Port.current_output,
        format_str = ar.shift();
  }
  else {
    var port = ar.shift(), format_str = ar.shift();
    assert_port(port);
  }

  var str = format_str.replace(/~[as]/g, function(matched){
               assert(ar.length > 0,
                      "insufficient number of arguments", "format");
               if (matched == "~a")
                 return to_display(ar.shift());
               else
                 return to_write(ar.shift());
            }).replace(/~%/, "\n")
              .replace(/~~/, "~");
  if (port) {
    port.put_string(str);
    return undef;
  }
  else {
    return str;
  }
});

//
// srfi-38 (write/ss)
//
const user_write_ss = function(ar){
  Console.puts(write_shared(ar[0]), true);
  return undef;
}
define_libfunc("write/ss", 1, 2, user_write_ss);
define_libfunc("write-with-shared-structure", 1, 2, user_write_ss);
define_libfunc("write*", 1, 2, user_write_ss); //from Gauche(STklos)

//
// srfi-43 (vector library)
//
define_libfunc("vector-append", 2, null, function(ar){
  var vec = [];
  return vec.concat.apply(vec, ar);
});

// (vector-copy vector)
define_libfunc("vector-copy", 1, 1, function(ar){
  assert_vector(ar[0]);
  return [...ar[0]];
});

//
// see src/library/node_functions.js for:
// - srfi-98 (get-environment-variable)
//
