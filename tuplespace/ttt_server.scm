(load "./inter_browser.scm")

(define client-id 100)
(define MAX_MESSAGES 100)
(define o #t)
; give latest msg
(add-manager 
  (lambda ()
    (while #t
      (tuplespace-take *ts* '(connect))
      (cond
        (o
         (set! o #f)
         (tuplespace-write *ts* '(connect maru)))
        (else
          (set! o #t)
          (tuplespace-write *ts* '(connect batu)))))))


;;; add msg-id, delete old message
;(add-manager 
;  (lambda ()
;    (while #t
;      (let ((tuple (tuplespace-take *ts* '(message _ _ _))))
;        (inc! message-id)
;        (tuplespace-write *ts* `(message ,message-id ,@(cdr tuple)))
;        ;(print "foo")
;        ;(print (tuplespace-takep *ts* `(,(- message-id MAX-MESSAGES) _ _ _)))
;        ;(print "bar")
;        
;        ))))
;

