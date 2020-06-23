import prettier from "rollup-plugin-prettier"
import fs from "fs"

export default {
  plugins: [
      prettier({}),
  ],
  input: "src/main-node.js",
  output: [
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

