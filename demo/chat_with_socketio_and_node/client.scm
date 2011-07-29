(js-load "/socket.io/socket.io.js" "io")

(define (log-debug . args)
  (apply js-invoke (append (list (js-eval "console") "log") args)))

(define *io* (js-eval "io"))
(define *socket* (js-invoke *io* "connect"))

(define (handle type callback)
  (js-invoke *socket* "on" type (js-closure callback)))

(define (send . args)
  (let1 args1 (map (lambda (e) (if (procedure? e) (js-closure e) e)) args)
        (apply js-invoke (append (list *socket* "emit") args1))))

(define (clear!)
  (element-empty! "#message")
  (element-focus! "#message"))

(define (add-message! from msg)
  (element-append-child! "#lines" (element-new (list 'p (list 'b from) msg))))

(handle "connect"
        (lambda ()
          (element-add-class-name "#chat" "connected")
          (element-focus! "#nick")))

(handle "announcement"
        (lambda (msg)
          (element-append-child! "#lines" (element-new (list 'p (list 'em msg))))))

(handle "nicknames"
        (lambda (nicknames)
          (element-empty! "#nicknames")
          (for-each
           (lambda (n) (element-append-child! "#nicknames" (element-new (list 'b n))))
           (js-list-to-list nicknames))))

(handle "user message" add-message!)

(handle "reconnect"
        (lambda ()
          (element-empty! "#lines")
          (add-message! "System" "Reconnected to the server")))

(handle "reconnecting"
        (lambda ()
          (add-message! "System" "Attempting to reconnect to the server")))

(handle "error"
        (lambda (err)
          (if err
              (add-message! "System" err)
            (add-message! "System" "An unknown error occurred"))))

(add-handler! "#set-nickname"
              "submit"
              (lambda ()
                (send "nickname"
                      (element-content "#nick")
                      (lambda (taken)
                        (if taken
                            (element-show! "#nickname-err")
                          (begin
                           (clear!)
                           (element-add-class-name "#chat" "nickname-set")))))
                #f))

(add-handler! "#send-message"
              "submit"
              (lambda ()
                (add-message! "me" (element-content "#message"))
                (send "user message" (element-content "#message"))
                (clear!)
                (js-invoke ($ "#lines") "scrollTop" 10000000)
                #f))
