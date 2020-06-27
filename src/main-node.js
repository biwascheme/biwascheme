import BiwaScheme from "./main.js";
import { run, run_file, node_setup } from "./platforms/node/module_postamble.js"

BiwaScheme.on_node = true;
BiwaScheme.run = run;
BiwaScheme.run_file = run_file;
node_setup(BiwaScheme);
module.exports = BiwaScheme;
