;; Load npm markdown package
(define marked (js-eval "require('marked')"))
(define fs (js-eval "require('fs')"))

;; Return list of entries in the directory `path`
(define (dir-ls path)
  (let1 ar (.. fs `(readdirSync ,path))
    (map (lambda (x) (string-append path "/" x))
         (vector->list ar))))

;; Return if `path` is a directory 
(define (directory? path)
  (.. fs `(statSync ,path) '(isDirectory)))

;; Yield every normal file under `dir`
(define (traverse proc dir)
  (for-each (lambda (path)
              (if (directory? path)
                (traverse proc path)
                (proc path)))
    (dir-ls dir)))

;; Return content of a file
(define (read-file path)
  (.. fs `(readFileSync ,path ((encoding . "utf-8")))))

(define (convert-file! mkd-path)
  (let* ((html-path (regexp-replace-all (string->regexp "\.md$")
                                        mkd-path ".html"))
         (mkd (read-file mkd-path))
         (body (js-call marked mkd))
         (html (string-append HEADER body FOOTER)))
    (.. fs `(writeFileSync ,html-path ,html))
    (print "Wrote " html-path)))

;; Load header and footer
(define HEADER (read-file "website/_header.html"))
(define FOOTER (read-file "website/_footer.html"))

;; Convert ./doc/**/*.md into *.html
(traverse (lambda (path)
            (when (regexp-exec "\.md$" path)
              (convert-file! path)))
          "./doc/")
