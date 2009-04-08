;;;
;;; presen.scm
;;;
;;; usage:
;;;  (load "presen.scm")
;;;  (define *presen*
;;;    '#(("hello"
;;;        ("foo"
;;;         "bar))
;;;       ("good bye"
;;;        ("thank you"
;;;         "very much"))))
;;;  (presen-start)

(define (wrap-by-tag tag content)
  (string-append "<" tag ">" content "</" tag ">"))
 
(define (make-li item)
  (wrap-by-tag "li" item))
(define (make-ul items)
  (wrap-by-tag "ul" (string-concat (map make-li items))))
(define (make-page title items)
  (string-append (wrap-by-tag "h1" title) (make-ul items)))

(define (presen-show n)
  (set-content! ($ "num") (number->string n))
  (let1 data (vector-ref *presen* n)
    (let ((title (car data))
          (items (cadr data)))
      (set-content! ($ "presen") 
        (make-page title items)))))

(define *num* 0)
(define (presen-start)
  (presen-show *num*))

(define (presen-page n)
  (set-content! ($ "bs-console") "") ;;temporary..
  (set! *num* (if (< n 0)
                0
                (if (<= (vector-length *presen*) n)
                  (- (vector-length *presen*) 1)
                  n)))
  (presen-show *num*))

(add-handler! ($ "head") "click"
  (lambda (e)
    (presen-page 0)))

(add-handler! ($ "next") "click"
  (lambda (e) 
    (presen-page (+ *num* 1))))

(add-handler! ($ "back") "click"
  (lambda (e) 
    (presen-page (- *num* 1))))

(add-handler! ($ "last") "click"
  (lambda (e)
    (presen-page (- (vector-length *presen*) 1))))

(element-hide! ($ "repl"))
(add-handler! ($ "repl-toggle") "click"
  (lambda (e)
    (element-toggle! ($ "repl"))))

(add-handler! ($ "repl-eval") "click"
  (lambda (e)             
    (set-content! ($ "repl-dst") 
                  (html-escape (write-to-string (eval (get-content ($ "repl-src"))))))
    #f))
