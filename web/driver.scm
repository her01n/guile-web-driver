(define-module (web driver))

(use-modules
  (ice-9 iconv) (ice-9 match) (ice-9 popen)
  (json)
  (srfi srfi-1) (srfi srfi-27)
  (web client) (web request) (web response) (web server))

(define web-server #f)
(define current-handler #f)

(define-public (set-web-handler! handler)
  "Sets the current handler for the testing web server listening on localhost:8080."
  (set! current-handler handler) 
  (if (not web-server)
; Start listening in calling thread, so the client can connect as soon as this procedure returns
    (let ((server-socket (socket PF_INET SOCK_STREAM 0)))
      (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
      (bind server-socket AF_INET INADDR_LOOPBACK 8080)
      (listen server-socket 16)
      (set! web-server #t)
      (call-with-new-thread
        (lambda ()
          (run-server 
            (lambda (request body) (current-handler request body))
            'http
            (list #:socket server-socket)))))))

(define (request method uri body-scm)
  (define body-bytevector (and body-scm (string->bytevector (scm->json-string body-scm) "utf-8")))
  (call-with-values
    (lambda ()
      (http-request uri #:method method #:body body-bytevector))
    (lambda (response body)
      (let ((value (hash-ref (json-string->scm (bytevector->string body "utf-8")) "value")))
        (if (equal? 200 (response-code response))
          value
          (let ((error (hash-ref value "error"))
                (message (hash-ref value "message")))
            (throw 'web-driver-error
              (format #f "Request ~a ~a failed with status ~a ~a.\nError: ~a\nMessage: ~a\n" 
                method uri (response-code response) (response-reason-phrase response) error message))))))))

(define (close-driver-pipe driver-pipe)
; It is possible that the process has already terminated after the session was deleted
  (format driver-pipe "kill $DRIVERPID 2>/dev/null\n")
  (format driver-pipe "wait $DRIVERPID")
  (format driver-pipe "exit\n")
  (close-pipe driver-pipe))

(define (free-listen-port)
  "Find an unused port for server to listen on it"
  (define s (socket PF_INET SOCK_STREAM 0))
  (listen s 1)
  (let ((port (array-ref (getsockname s) 2)))
    (close-port s)
    port))
      
(define-public (open-web-driver)
  "Opens a web driver session. 
   Caller needs to close this session by calling *close-web-driver* when done."
  (let* ((port (free-listen-port))
         (driver-pipe (open-output-pipe "sh"))
         (driver-uri (format #f "http://localhost:~a" port)))
    (format driver-pipe "chromedriver --port=~a >/dev/null &\n" port)
    (format driver-pipe "DRIVERPID=$!\n")
; wait until the new process starts listening
    (find
      (lambda (try)
        (catch #t
          (lambda () (request 'GET (format #f "~a/status" driver-uri) #f) #t)
          (lambda (key . args) (usleep (* 10 1000)) #f)))
      (iota 100))
; start a new session
    (catch #t
      (lambda ()
        (let* ((uri (format #f "~a/session" driver-uri))
               (parameters (json (object ("capabilities" (object)))))
               (response (request 'POST uri parameters))
               (session-id (hash-ref response "sessionId")))
          (list 'web-driver driver-pipe driver-uri session-id)))
      (lambda (key . args)
        (close-driver-pipe driver-pipe)
        (apply throw key args)))))

(define-public (web-driver? object)
  (match object
    (('web-driver driver-pipe driver-uri session-id) #t)
    (else #f)))

(define (session-command driver method path body-scm)
  (match driver
    (('web-driver driver-pipe driver-uri session-id)
      (request method (format #f "~a/session/~a~a" driver-uri session-id path) body-scm))))

(define *default-driver* (make-thread-local-fluid))

(define (close driver)
  (match driver
    (('web-driver driver-pipe driver-uri session-id)
      (session-command driver 'DELETE "" #f)
      (close-driver-pipe driver-pipe))))

(define-public (close-web-driver . args)
  (if (null? args)
    (if (fluid-ref *default-driver*)
      (begin
        (close (fluid-ref *default-driver*))
        (fluid-set! *default-driver* #f)))
    (close (car args))))

(define (open-default-driver)
  (if (not (fluid-ref *default-driver*))
    (fluid-set! *default-driver* (open-web-driver)))
  (fluid-ref *default-driver*))

(define (with-default-driver proc)
  (lambda args
    (if (and (pair? args) (web-driver? (car args)))
      (apply proc args)
      (apply proc (open-default-driver) args))))

(define-public navigate-to
  (with-default-driver
    (lambda (driver url)
      (session-command driver 'POST "/url" (json (object ("url" ,url)))))))

(define-public current-url
  (with-default-driver
    (lambda (driver)
      (session-command driver 'GET "/url" #f))))

(define-public delete-all-cookies
  (with-default-driver
    (lambda (driver)
      (session-command driver 'DELETE "/cookie" #f))))

; XXX elements are returned as a json object with a single weird key
; with value of the actual element id/reference
(define (web-driver-element driver element-object)
  (list 'web-driver-element driver (hash-ref element-object "element-6066-11e4-a52e-4f735466cecf")))

(define-public element-by-css-selector
  (with-default-driver
    (lambda (driver selector)
      (web-driver-element driver
        (session-command driver 
        'POST "/element" 
        (json (object ("using" "css selector") ("value" ,selector))))))))

(define-public elements-by-css-selector
  (with-default-driver
    (lambda (driver selector)
      (map
        (lambda (element-object) (web-driver-element driver element-object))
        (session-command driver 
          'POST "/elements" 
          (json (object ("using" "css selector") ("value" ,selector))))))))

(define-public element-by-id
  (with-default-driver
    (lambda (driver id)
      (element-by-css-selector driver (string-append "#" id)))))

(define-public elements-by-id
  (with-default-driver
    (lambda (driver id)
      (elements-by-css-selector driver (string-append "#" id)))))
  
(define-public element-by-class-name
  (with-default-driver
    (lambda (driver class-name)
      (element-by-css-selector driver (string-append "." class-name)))))

(define-public elements-by-class-name
  (with-default-driver
    (lambda (driver class-name)
      (elements-by-css-selector driver (string-append "." class-name)))))

(define-public element-by-tag-name element-by-css-selector)

(define-public elements-by-tag-name elements-by-css-selector)

(define (element-command element method path body-scm)
  (match element
    (('web-driver-element driver element)
      (session-command driver method (format #f "/element/~a~a" element path) body-scm))))

(define-public (text element)
  (element-command element 'GET "/text" #f))

(define-public (attribute element name)
  (element-command element 'GET (format #f "/attribute/~a" name) #f))

(define-public (click element)
  (element-command element 'POST "/click" (json (object))))

(define-public (send-keys element text)
  (element-command element 'POST "/value" (json (object ("text" ,text)))))

