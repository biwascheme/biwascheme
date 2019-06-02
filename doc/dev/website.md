## Website

Website of BiwaScheme (https://www.biwascheme.org) is hosted on GitHub Pages.

### How to build *.html

Execute `$ make website -B` to 

1. convert `doc/**/*.md` to HTML, and
2. build index.html from index_.html, website/_header.html and website/_footer.html.

### How to update the website

Just push these html files to the `master` branch.

### Browse locally (with Node.js)

```
$ ./local_website.js`
$ open http://localhost:7001/
```

### Browse locally (with [Pow](http://pow.cx))

```
$ ln -s . ~/.pow/biwascheme
$ open http://biwascheme.dev/index.html
```
