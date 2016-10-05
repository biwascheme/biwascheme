//
// number.js
//

//
// Complex
//
BiwaScheme.Complex = BiwaScheme.Class.create({
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
    return this.imag == 0 && BiwaScheme.isRational(this.real);
  },
  isInteger: function(){
    return this.imag == 0 && BiwaScheme.isInteger(this.real);
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
BiwaScheme.Complex.from_polar = function(r, theta){
  var real = r * Math.cos(theta);
  var imag = r * Math.sin(theta);
  return new BiwaScheme.Complex(real, imag);
}
BiwaScheme.Complex.assure = function(num){
  if(num instanceof BiwaScheme.Complex)
    return num
  else
    return new BiwaScheme.Complex(num, 0);
}

//
// Rational (unfinished)
//
BiwaScheme.Rational = BiwaScheme.Class.create({
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
BiwaScheme.isNumber = function(x) {
  return (x instanceof BiwaScheme.Complex)  ||
         (x instanceof BiwaScheme.Rational) ||
         (typeof(x) == 'number');
};
BiwaScheme.isComplex = BiwaScheme.isNumber;
BiwaScheme.isReal = function(x) {
  if (x instanceof BiwaScheme.Complex || x instanceof BiwaScheme.Rational) {
    return x.isReal()
  }
  else {
    return (typeof(x) == 'number');
  }
};
BiwaScheme.isRational = function(x) {
  if (x instanceof BiwaScheme.Complex) {
    return x.isRational();
  }
  else if (x instanceof BiwaScheme.Rational) {
    return true;
  }
  else {
    return (typeof(x) == 'number');
  }
};
BiwaScheme.isInteger = function(x) {
  if (x instanceof BiwaScheme.Complex || x instanceof BiwaScheme.Rational) {
    return x.isInteger();
  }
  else {
    return (typeof(x) == 'number') && (x % 1 == 0);
  }
};
