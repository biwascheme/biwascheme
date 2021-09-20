import * as _ from "../../deps/underscore-esm.js"
import { inspect } from "../../system/_writer.js"
import Console from "../../system/console.js"
import { Port } from "../../system/port.js"

Console.puts = function(str, no_newline) {
  Port.current_output.put_string(str + (no_newline ? "" : "\n"))
};

Console.p = function (/*ARGS*/){
  Port.current_output.put_string(
    "p> "+ Array.arrayFrom(arguments).map(inspect).join(" ")
  );
};

export default Console;
