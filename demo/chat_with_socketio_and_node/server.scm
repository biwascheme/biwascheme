(load "socketio.scm")

;; general utils
(define (log-debug . args)
  (apply js-invoke (append (list (js-eval "console") "log") (map inspect args))))

(define (require path)
  (js-call (js-eval "require") path))

;; start a simple web-serving hosting static files from the current directory
(define (express-start port)
  (let* ((express (require "express"))
         (app (js-invoke express "createServer"))
         (dir (string-append (js-eval "__dirname") "/../../../")))
    (js-invoke app "configure"
               (js-closure (lambda () (js-invoke app "use" (js-invoke express "static" dir)))))
    (js-invoke app "listen" port
               (js-closure
                (lambda ()
                  (let1 addr (js-invoke app "address")
                        (log-debug (string-append "Server listening on http://"
                                                  (js-ref addr "address")
                                                  ":"
                                                  (number->string (js-ref addr "port"))))))))
    app))

;; extend a web server to accept Socket.IO connections
(define (socketio-start app connection-handler)
  (let* ((sio (require "socket.io"))
         (io (js-invoke sio "listen" app)))
    (js-invoke (js-ref io "sockets") "on" "connection" (js-closure connection-handler))
    io))

;; Socket.IO helpers
(define (socketio-broadcast socket . args)
  (apply socketio-emit (cons (js-ref socket "broadcast") args)))

(define (socketio-broadcast-all socket . args)
  (apply socketio-emit (cons (js-ref (js-ref socket "manager") "sockets") args)))

;; main
(log-debug "Server starting")

(define *nicknames* '())

(socketio-start
 (express-start 3333)
 (lambda (socket)
   (let ((handle (lambda (type callback) (socketio-on socket type callback)))
         (broadcast (lambda args (apply socketio-broadcast (cons socket args))))
         (broadcast-all (lambda args (apply socketio-broadcast-all (cons socket args))))
         (nick #f))

     (handle "user message"
             (lambda (msg)
               (broadcast "user message" nick msg)))

     (handle "nickname"
             (lambda (new-nick callback)
               (if (member new-nick *nicknames*)
                   (callback #t)
                 (begin
                  (callback #f)
                  (set! nick new-nick)
                  (set! *nicknames* (cons nick *nicknames*))
                  (broadcast "announcement" (string-append nick " connected"))
                  (broadcast-all "nicknames" *nicknames*)))))

     (handle "disconnect"
             (lambda ()
               (if nick
                   (begin
                    (set! *nicknames* (remove nick *nicknames*))
                    (broadcast "announcement" (string-append nick " disconnected"))
                    (broadcast-all "nicknames" *nicknames*))))))))
