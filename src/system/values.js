import Class from "./class.js"
import _ from "../deps/underscore-1.10.2-esm.js"

//
// Values
//
const Values = Class.create({
  initialize: function(values){
    this.content = values;
  },
  to_write: function(){
    return "#<Values " +
             _.map(this.content, BiwaScheme.to_write).join(" ") +
           ">";
  }
});

export default Values;
