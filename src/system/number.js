import * as _ from "../deps/underscore-esm.js"
import Class from "./class.js"

//
// number.js
//

//
// Complex
//
const Complex = Class.create({
  initialize: function(real, imag){
    this.real = real;
    this.imag = imag;
  },
  magnitude: function(){
    return Math.sqrt(this.real * this.real + this.imag * this.imag);
  },
  angle: function(){
    return Math.atan2(this.imag, this.real);
  },
  isReal: function(){
    return this.imag == 0;
  },
  isRational: function() {
    return this.imag == 0 && isRational(this.real);
  },
  isInteger: function(){
    return this.imag == 0 && isInteger(this.real);
  },
  toString: function(radix){
    if (this.real === 0 && this.imag === 0)
      return "0";
    var img = "";
    if (this.imag !== 0) {
      if (this.imag > 0 && this.real !== 0){
          img+="+";
      }
      switch(this.imag) {
          case 1:
              break;
          case -1: img+="-";
               break;
          default: img+=this.imag.toString(radix);
      }
     img+="i";
    }
    var real = "";
    if (this.real !== 0){
      real += this.real.toString(radix);
    }
    return real+img;
  }
})
Complex.from_polar = function(r, theta){
  var real = r * Math.cos(theta);
  var imag = r * Math.sin(theta);
  return new Complex(real, imag);
}
Complex.assure = function(num){
  if(num instanceof Complex)
    return num
  else
    return new Complex(num, 0);
}

//
// Rational (unfinished)
//
const Rational = Class.create({
  initialize: function(numerator, denominator){
    this.numerator = numerator;
    this.denominator = denominator;
  },

  isInteger: function() {
     // FIXME
  }
})

//
// Predicates
//
const isNumber = function(x) {
  return (x instanceof Complex)  ||
         (x instanceof Rational) ||
         (typeof(x) == 'number');
};
const isComplex = isNumber;
const isReal = function(x) {
  if (x instanceof Complex || x instanceof Rational) {
    return x.isReal()
  }
  else {
    return (typeof(x) == 'number');
  }
};
const isRational = function(x) {
  if (x instanceof Complex) {
    return x.isRational();
  }
  else if (x instanceof Rational) {
    return true;
  }
  else {
    return (typeof(x) == 'number');
  }
};
const isInteger = function(x) {
  if (x instanceof Complex || x instanceof Rational) {
    return x.isInteger();
  }
  else {
    return (typeof(x) == 'number') && (x % 1 == 0);
  }
};

export { Complex, Rational, isNumber, isComplex, isReal, isRational, isInteger };
