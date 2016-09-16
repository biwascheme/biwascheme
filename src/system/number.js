//
// number.js - Complex and Rational
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

BiwaScheme.Rational = BiwaScheme.Class.create({
  initialize: function(numerator, denominator){
    this.numerator = numerator;
    this.denominator = denominator;
  }
})


