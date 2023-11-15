## How to build BiwaScheme

### Prerequisites

- Node.js (>= 15.0.0 which supports String.prototype.replaceAll)
- npm

Currently Windows is not supported (because of symlink issue). WSL2 is OK.

### Command

```
$ npm install
$ npm run build
```

This will create these js files.

- ./release/biwascheme.js (IIFE, contains jQuery)
- ./release/biwascheme-min.js (IIFE, contains jQuery, compressed)
- ./release/biwascheme.mjs (ESM, without jQuery)
- ./release/node_biwascheme.js (Contains some Node-only functions instead of those for browsers)
