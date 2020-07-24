import * as _ from "../../deps/underscore-1.10.2-esm.js"
import { inspect } from "../../system/_writer.js"
import { Port } from "../../system/port.js"

const Console = {};

Console.puts = function(str, no_newline) {
  Port.current_output.put_string(str + (no_newline ? "" : "\n"))
};

Console.p = function (/*ARGS*/){
  Port.current_output.put_string(
    "p> "+_.map(_.toArray(arguments), inspect).join(" ")
  );
};

export default Console;
