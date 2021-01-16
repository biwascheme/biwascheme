import prettier from "rollup-plugin-prettier";
import { terser } from "rollup-plugin-terser";
import replace from "@rollup/plugin-replace";
import child_process from "child_process";

const banner = `/*
 * BiwaScheme __VERSION__ - R6RS/R7RS Scheme in JavaScript
 *
 * Copyright (c) 2007-${new Date().getFullYear()} Yutaka HARA (http://www.biwascheme.org/)
 * Licensed under the MIT license.
 */`;

// Use the replace plugin in each configuration to get the version and git
// commit programmatically.
let replaceVersion = () =>
  replace({
    __VERSION__: process.env.npm_package_version,
    __GIT_COMMIT__: child_process
      .execSync("git rev-parse HEAD")
      .toString()
      .trim(),
  });

export default [
  {
    plugins: [prettier({}), replaceVersion()],
    input: "src/main-node.js",
    output: [
      {
        file: "release/node_biwascheme.js",
        format: "cjs",
        name: "BiwaScheme",
        strict: false,
        plugins: [prettier()],
      },
    ],
  },
  {
    input: "src/main-browser.js",
    plugins: [replaceVersion()],
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
        plugins: [terser({ output: { comments: /Copyright/ } })],
      },
    ],
  },
];
