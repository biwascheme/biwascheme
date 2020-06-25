import { terser } from "rollup-plugin-terser"
import { VERSION } from "./src/version.js";

const banner = `/*
 * BiwaScheme ${VERSION} - R6RS/R7RS Scheme in JavaScript
 *
 * Copyright (c) 2007-${(new Date).getFullYear()} Yutaka HARA (http://www.biwascheme.org/)
 * Licensed under the MIT license.
 */`

export default {
  input: "src/main-browser.js",
  output: [
    {
      file: "release/biwascheme.js",
      format: "iife",
      name: "BiwaScheme",
      strict: false,
      banner: banner,
    },
    {
      file: "release/biwascheme-min.js",
      format: "iife",
      name: "BiwaScheme",
      strict: false,
      banner: banner,
      plugins: [terser({output: {comments: /Copyright/}})],
    },
  ]
}
