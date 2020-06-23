import { terser } from "rollup-plugin-terser"

export default {
  plugins: [
      terser({ output: { comments: /@license/i } }),
  ],
  input: "src/main-browser.js",
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
  ]
}
