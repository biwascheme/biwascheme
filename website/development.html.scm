(html-doctype)
(html
  (convert-file "_head.html.scm")
  (body
    (convert-file "_header.html.scm")

    (div :id "content"
      (h2 "Development")

      (h3 "Source code")

      (p "The repository of BiwaScheme is "
         (link-to "hosted in github" "http://github.com/biwascheme/biwascheme")
         ".")

      (h3 "Mailing lists")

      (dl
        (dt "English")
        (dd (link-to "http://groups.google.co.jp/group/biwascheme"))

        (dt "Japanese")
        (dd (link-to "http://groups.google.co.jp/group/biwascheme-ja")))

      (h3 "Issue tracker")

      (link-to "http://github.com/biwascheme/biwascheme/issues")

      (h3 "Technical documents")

      (link-to "http://wiki.github.com/yhara/biwascheme")

      (convert-file "_footer.html.scm")
    )))
