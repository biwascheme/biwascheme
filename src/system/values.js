import * as _ from "../deps/underscore-esm.js"
import Class from "./class.js"
import { to_write } from "./_writer.js"

//
// Values
//
const Values = Class.create({
  initialize: function(values){
    this.content = values;
  },
  to_write: function(){
    return "#<Values " +
             _.map(this.content, to_write).join(" ") +
           ">";
  }
});

export default Values;
