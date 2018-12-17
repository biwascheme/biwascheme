## How to make a release

1. Edit History.txt
1. Edit VERSION
1. Edit package.json
1. Build
  - $ npm install uglifyjs -g
  - $ make -B
1. Make sure it is working:
  - run tests
    - node ./local_website.js
    - open http://localhost:7001/test/spec.html
    - open http://localhost:7001/test/spec.html#release
    - open http://localhost:7001/test/spec.html#min
    - run test/browser_functions/
      - `$ cd test/browser_functions && node server.js && open http://localhost:7001/`
    - $ npm run node-test
  - run demos
    - demo/*
    - open http://localhost:7001/demo/repl.html
    - open http://localhost:7001/website/i.html
    - open http://localhost:7001/test/tracer.html
    - open http://localhost:7001/test/tracer_auto.html
  - try npm package
    - `$ npm pack`
    - `$ npm install biwascheme-x.y.z.tgz -g`
    - `$ biwas -v`
1. Create biwascheme-x.y.z.js
  - $ cp release/biwascheme{,-x.y.z}js
  - $ cp release/biwascheme{,-x.y.z}-min.js
1. Commit, `git tag` and push changes to github
1. Update website
  - see website.md
1. Publish npm package
  - `$ npm publish`
1. Update `VERSION` to x.y.z.dev
