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
    return Math.acos(this.real / this.magnitude());
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


