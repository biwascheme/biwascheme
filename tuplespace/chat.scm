(js-load "custom_biwa_lib.js" "BiwaScheme.CoreEnv['ts-init']")

  (define *my-id* #f)
  (define *msg-id* #f)

  (add-handler! ($ "#send") "click"
   (lambda ()
      (console-log "sending " (get-content ($ "#message")))
      (let ((name (get-content ($ "#name")))
            (msg  (get-content ($ "#message"))))
        (ts-write (list 'message *my-id* name msg)))))

  (define (receive-message)
    (let ((v (ts-read `(message (? (lambda (x) (= (+ 1 ,*msg-id*) x))) _ _ _))))
      (let ((new-msg-id (cadr v))
            (sender-id  (caddr v))
            (name       (cadddr v))
            (message    (cadddr (cdr v))))
        (set! *msg-id* new-msg-id)
        (print "<" name ">" message)))
    (receive-message))

  (define (start-chat)
    (ts-write '(connect))
    (let ((state (ts-take '(connect _ _))))
      (set! *my-id* (cadr state))
      (set! *msg-id* (caddr state)))
    (receive-message))

  (ts-init)
  (start-chat)
