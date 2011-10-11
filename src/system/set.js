//
// Set - set of string
// contents must be string (or at least sortable)
//
BiwaScheme.Set = BiwaScheme.Class.create({
  initialize : function(/*args*/){
    this.arr = [];
    var i;
    for(i=0; i<arguments.length; i++)
      this.arr[i] = arguments[i];
  },

  equals : function(other){
    if(this.arr.length != other.arr.length)
      return false;

    var a1 = _.clone(this.arr);
    var a2 = _.clone(other.arr);
    a1.sort();
    a2.sort();
    for(var i=0; i<this.arr.length; i++){
      if(a1[i] != a2[i]) return false;
    }
    return true;
  },
  set_cons : function(item){
    var o = new BiwaScheme.Set(item);
    o.arr = _.clone(this.arr);
    o.arr.push(item);
    return o;
  },
  set_union : function(/*args*/){
    var o = new BiwaScheme.Set();
    o.arr = _.clone(this.arr);

    for(var k=0; k<arguments.length; k++){
      var s2 = arguments[k];
      if(!(s2 instanceof BiwaScheme.Set))
        throw new BiwaScheme.Error("set_union: arguments must be a set");

      for(var i=0; i<s2.arr.length; i++)
        o.add(s2.arr[i]);
    }
    return o;
  },
  set_intersect : function(s2){
    if(!(s2 instanceof BiwaScheme.Set))
      throw new BiwaScheme.Error("set_intersect: arguments must be a set");

    var o = new BiwaScheme.Set();
    for(var i=0; i<this.arr.length; i++)
      if(s2.member(this.arr[i]))
        o.add(this.arr[i]);
    return o;
  },
  set_minus : function(s2){
    if(!(s2 instanceof BiwaScheme.Set))
      throw new BiwaScheme.Error("set_minus: arguments must be a set");

    var o = new BiwaScheme.Set();
    for(var i=0; i<this.arr.length; i++)
      if(!s2.member(this.arr[i]))
        o.add(this.arr[i]);
    return o;
  },
  add : function(item){
    if(!this.member(item)){
      this.arr.push(item);
    }
  },
  member : function(item){
    for(var i=0; i<this.arr.length; i++)
      if(this.arr[i] == item) return true;

    return false;
  },
  rindex : function(item){
    for(var i=this.arr.length-1; i>=0 ; i--)
      if(this.arr[i] == item) return (this.arr.length-1-i);

    return null;
  },
  index : function(item){
    for(var i=0; i<this.arr.length; i++)
      if(this.arr[i] == item) return i;

    return null;
  },
  inspect : function(){
    return "Set(" + this.arr.join(", ") + ")";
  },
  toString : function(){
    return this.inspect();
  },
  size : function(){
    return this.arr.length;
  }
});
