(use gauche.test)
(use gauche.threads)
(use util.match)

(load "./tuplespace.scm")
(import tuplespace)

;;; utility
(define (times proc n)
  (when (< 0 n)
    (proc) 
    (times proc (- n 1))))

;;; tuplespace
(define *ts* (tuplespace-init))

;;; class chatter
(define-class <chatter> () ; () = no super class
  ((name :init-keyword :name :accessor name-of)))

(define (make-chatter name)
  (make <chatter> :name name))

(define-method chatter-say ((chatter <chatter>) message)
  (tuplespace-write *ts* `(,(name-of chatter) ,message)))

(define-method chatter-listen ((chatter <chatter>))
  (let1 result (tuplespace-read *ts* `((not ,(name-of chatter)) _))
    (match result
      ((sender-name message)
       (print #`",(name-of chatter) : <,|sender-name|> ,message")))))

(define-method chatter-run ((chatter <chatter>))
  (thread-start!
    (chatter-thread chatter)))

(define-method chatter-thread ((chatter <chatter>))
  (make-thread
    (lambda ()
      (guard (e (else (print "Error !! " (ref e 'message))))
        (for-each (lambda (i)
                    (chatter-say chatter (number->string i))
                    (chatter-listen chatter))
                  '(1 2 3))))))

;;; main
(define chatters 
  (list (make-chatter "yhara")
        (make-chatter "ujihisa")
        (make-chatter "hakobe")))

(for-each thread-join!
          (map (cut chatter-run <>) chatters))

