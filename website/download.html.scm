(html-doctype)
(html
  (convert-file "_head.html.scm")
  (body
    (convert-file "_header.html.scm")

    (div :id "content"
      (h2 "Download")

      (h3 "On-line REPL")

      (p "If you just want to try it:"
         (link-to "BiwaScheme REPL" "repos/repl.html"))

      (h3 "Release version")

      (link-to "zip and tgz"
               "http://github.com/yhara/biwascheme/downloads")

      (h3 "Latest version")

      (p "You can also download the latest verison "
         "from the git repository:" (br)
         (link-to "http://github.com/yhara/biwascheme"))

      (convert-file "_footer.html.scm")
    )))
