import Console from "../../system/console.js"
import { Port } from "../../system/port.js"

Console.puts = function(str, no_newline) {
  Port.current_output.put_string(str + (no_newline ? "" : "\n"))
};

Console.p = function() {
  [].slice.call(arguments).forEach(Port.current_output.put_string);
};

export default Console;
