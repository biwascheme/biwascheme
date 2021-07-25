## How to make a release

1. Edit CHANGELOG.md
1. Edit package.json
1. Build
  - $ rollup -c
1. Create release/biwascheme-x.y.z.js
  - $ npm run prepare-release
1. Rebuild website
  - $ make website -B
1. Make sure it is working:
  - run tests
    - npm run serve
    - open http://localhost:7001/test/spec.html
    - open http://localhost:7001/test/spec.html#release
    - open http://localhost:7001/test/spec.html#min
    - run test/browser_functions/
      - `$ cd test/browser_functions && node server.js && open http://localhost:7002/`
    - $ npm run node-test
  - run demos
    - open http://localhost:7001/
    - open http://localhost:7001/demo/repl.html
    - open http://localhost:7001/website/i.html
    - open http://localhost:7001/test/tracer.html
  - try npm package
    - `$ npm pack`
    - `$ npm install -g biwascheme-x.y.z.tgz`
    - `$ biwas -v`
1. Commit, `git tag` and push changes to github. Publish npm package
  - $ npm run release
1. Make a release on [github](https://github.com/biwascheme/biwascheme/releases)
1. Post release mail to the mailing list
