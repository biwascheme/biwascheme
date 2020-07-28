import prettier from "rollup-plugin-prettier"

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
      plugins: [prettier()],
    },
  ]
}

