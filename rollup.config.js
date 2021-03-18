import { readFileSync } from 'fs';
import prettier from "rollup-plugin-prettier";
import { terser } from "rollup-plugin-terser";
import replace from "@rollup/plugin-replace";
import child_process from "child_process";
import package_json from "./package.json"

const banner = `/*
 * BiwaScheme __DEVELOPMENT__ - R6RS/R7RS Scheme in JavaScript
 *
 * Copyright (c) 2007-${new Date().getFullYear()} Yutaka HARA (http://www.biwascheme.org/)
 * Licensed under the MIT license.
 */`;

// Use the replace plugin in each configuration to get the version and git
// commit programmatically.
let replaceVersion = () =>
  replace({
    __DEVELOPMENT__: package_json["version"],
    __GIT_COMMIT__: child_process
      .execSync("git rev-parse HEAD")
      .toString()
      .trim(),
  });

export default [
  {
    plugins: [prettier({ parser: "babel" }), replaceVersion()],
    input: "src/main-node.js",
    output: [
      {
        file: "release/node_biwascheme.js",
        format: "cjs",
        name: "BiwaScheme",
        strict: false,
      },
    ],
  },
  {
    input: "src/main-browser.js",
    plugins: [prettier({ parser: "babel" }), replaceVersion()],
    output: [
      {
        file: "release/biwascheme.js",
        format: "iife",
        name: "BiwaScheme",
        strict: false,
        banner: banner + readFileSync("src/deps/jquery.js"),
      },
      {
        file: "release/biwascheme-min.js",
        format: "iife",
        name: "BiwaScheme",
        strict: false,
        banner: banner + readFileSync("src/deps/jquery.js"),
        plugins: [terser({ output: { comments: /Copyright/ } })],
      },
      {
        file: "release/biwascheme.mjs",
        format: "esm",
        name: "BiwaScheme",
        strict: false,
        banner: banner
      },
    ],
  },
];
