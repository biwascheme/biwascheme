import { isVector } from "./_types.js"

// A compiled Scheme expression
class VMCode {
  // il: Intermediate Language (JS array)
  constructor(il){
    if (!isVector(il)) { console.error(il); throw "not array" }
    this.il = il;
  }

  to_write() {
    return "#<VMCode>";
  }
}

export default VMCode;
