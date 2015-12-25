## How to make a release

1. Edit History.txt
2. Edit VERSION
3. Edit package.json
4. Build
  - $ make -B
  - Commit as release/biwascheme-x.y.z.js
5. Make sure it is working:
  - run tests
    - open http://localhost:7001/test/spec.html
    - open http://localhost:7001/test/spec.html#release
    - open http://localhost:7001/test/spec.html#min
    - run test/browser_functions/
      - `$ cd test/browser_functions && node server.js && open http://localhost:7001/`
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
6. Push changes and make a release on github
  - Copy changelog from History.txt
  - Push "Publish" to make a git tag
7. Update website
  - see website.md
8. Publish npm package
  - `$ npm publish`
9. Update `VERSION` to x.y.z.dev
