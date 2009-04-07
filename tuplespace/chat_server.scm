(load "./inter_browser.scm")

(define message-id 0)
(define client-id 100)
(define MAX_MESSAGES 100)

;(set! whitelist '(lambda < x))

(add-manager 
  (lambda ()
    (while #t
      (let ((tuple (tuplespace-take *ts* '(message _ _ _))))
        (inc! message-id)
        (tuplespace-write *ts* `(message ,message-id ,@(cdr tuple)))
        ))))

(add-manager 
  (lambda ()
    (while #t
      (let ((tuple (tuplespace-take *ts* '(connect))))
        (inc! client-id)
        (tuplespace-write *ts* `(connect ,client-id ,message-id))))))

