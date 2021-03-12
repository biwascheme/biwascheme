## How to build BiwaScheme

### Prerequisites

- Node.js
- npm

### Command

```
$ npm install
$ npm run build
```

This will create these js files.

- ./release/biwascheme.js (IIFE)
- ./release/biwascheme.mjs (ESM)
- ./release/biwascheme-min.js (IIFE, compressed)
- ./release/node_biwascheme.js (Contains some Node-only functions instead of those for browsers)
