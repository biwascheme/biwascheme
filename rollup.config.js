import { terser } from "rollup-plugin-terser"
import prettier from "rollup-plugin-prettier"
import fs from "fs"

export default {
  plugins: [
      terser({ output: { comments: /@license/i } }),
      prettier({}),
  ],
  input: "src/main.js",
  output: [
    {
      file: "release/biwascheme.js",
      format: "iife",
      name: "BiwaScheme",
      strict: false,
    },
    {
      file: "release/biwascheme-min.js",
      format: "iife",
      name: "BiwaScheme",
      strict: false,
      plugins: [terser()],
    },
    {
      file: "release/node_biwascheme.js",
      format: "cjs",
      name: "BiwaScheme",
      strict: false,
      intro: fs.readFileSync("src/platforms/node/module_preamble.js", 'utf8'),
      plugins: [prettier()],
    },
  ]
}
