import * as _ from "../deps/underscore-1.10.2-esm.js"
import { to_write } from "./_writer.js"

//
// Values
//
class Values {
  constructor(values){
    this.content = values;
  }

  to_write(){
    return "#<Values " +
             _.map(this.content, to_write).join(" ") +
           ">";
  }
}

export default Values;
