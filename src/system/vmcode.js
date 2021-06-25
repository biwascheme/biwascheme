import Class from "./class.js"
import { isVector } from "./_types.js"

// A compiled Scheme expression
const VMCode = Class.create({
  // il: Intermediate Language (JS array)
  initialize: function(il){
    if (!isVector(il)) { console.error(il); throw "not array" }
    this.il = il;
  },

  to_write: function() {
    return "#<VMCode>";
  }
})

export default VMCode;
