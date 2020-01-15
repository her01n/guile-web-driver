(define-module (web driver))

(use-modules
  (ice-9 hash-table) (ice-9 iconv) (ice-9 match) (ice-9 popen)
  (json)
  (srfi srfi-1) (srfi srfi-9) (srfi srfi-27)
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

(define-public (web-driver-open? driver)
  (match driver
    (('web-driver driver-pipe driver-uri session-id) (not (port-closed? driver-pipe)))))

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

(define-public (call-with-web-driver proc)
  (define driver (open-web-driver))
  (catch #t
    (lambda () 
      (let ((r (with-fluid* *default-driver* driver (lambda () (proc driver)))))
        (close-web-driver driver) r))
    (lambda args 
      (close-web-driver driver) (apply throw args))))

(define-public (get-default-driver)
  (let ((current (fluid-ref *default-driver*)))
    (if (or (not current) (not (web-driver-open? current)))
      (fluid-set! *default-driver* (open-web-driver))))
  (fluid-ref *default-driver*))

(define-syntax define-public-with-driver
  (syntax-rules ()
    ((define-public-with-driver (proc-name driver args* ...) body* ...)
     (define-public (proc-name . args)
       (let ((proc (lambda* (driver args* ...) body* ...)))
             (if (and (pair? args) (web-driver? (car args)))
               (apply proc args)
               (apply proc (get-default-driver) args)))))))

;;; Timeouts

(define-public-with-driver (set-script-timeout driver #:optional timeout)
  (let ((value (match timeout ((? number? n) n) (#f 30000) (#:never #nil))))
    (session-command driver 'POST "/timeouts" (json (object ("script" ,value))))))

(define-public-with-driver (get-script-timeout driver)
  (match (hash-ref (session-command driver 'GET "/timeouts" #f) "script")
    ((? number? n) n)
    (#nil #:never)))

(define-public-with-driver (set-page-load-timeout driver #:optional (timeout 300000))
  (session-command driver 'POST "/timeouts" (json (object ("pageLoad" ,timeout)))))

(define-public-with-driver (get-page-load-timeout driver)
  (hash-ref (session-command driver 'GET "/timeouts" #f) "pageLoad"))

(define-public-with-driver (set-implicit-timeout driver #:optional (timeout 0))
  (session-command driver 'POST "/timeouts" (json (object ("implicit" ,timeout)))))

(define-public-with-driver (get-implicit-timeout driver)
  (hash-ref (session-command driver 'GET "/timeouts" #f) "implicit"))

;;; Navigation

(define-public-with-driver (navigate-to driver url)
  (session-command driver 'POST "/url" (json (object ("url" ,url)))))

(define-public-with-driver (current-url driver) 
  (session-command driver 'GET "/url" #f))

(define-public-with-driver (back driver) 
  (session-command driver 'POST "/back" (json (object))))

(define-public-with-driver (forward driver)
  (session-command driver 'POST "/forward" (json (object))))

(define-public-with-driver (refresh driver)
  (session-command driver 'POST "/refresh" (json (object))))

(define-public-with-driver (title driver)
  (session-command driver 'GET "/title" #f))

;;; Windows

(define (web-driver-window driver window-object)
  (list 'web-driver-window driver window-object))

(define-public-with-driver (current-window driver)
  (web-driver-window driver
    (session-command driver 'GET "/window" #f)))

(define-public-with-driver (close-window driver)
  (session-command driver 'DELETE "/window" (json (object)))
  ; XXX chromedriver would keep the deleted window currect, 
  ; causing all following navigation calls to fail.
  (switch-to (first (all-windows driver))))

(define-public-with-driver (all-windows driver)
  (map
    (lambda (window-object) (web-driver-window driver window-object))
    (session-command driver 'GET "/window/handles" #f)))

(define (new-window driver type)
  (web-driver-window 
    driver
    (hash-ref
      (session-command driver 'POST "/window/new" (json (object ("type" ,type))))
      "handle")))

(define-public-with-driver (open-new-window driver)
  (new-window driver "window"))

(define-public-with-driver (open-new-tab driver)
  (new-window driver "tab"))

(define-public-with-driver (switch-to driver target)
  (match target
    (('web-driver-window driver handle)
     (session-command driver 'POST "/window" (json (object ("handle" ,handle)))))
    (('web-driver-element driver element)
     (session-command driver 'POST "/frame" 
       (json (object ("id" (object ("element-6066-11e4-a52e-4f735466cecf" ,element)))))))
    ((? number? n)
     (session-command driver 'POST "/frame" (json (object ("id" ,n)))))))

;;; Browsing Context

(define-public-with-driver (switch-to-parent driver)
  (session-command driver 'POST "/frame/parent" (json (object))))

(define-public-with-driver (switch-to-window driver)
  (session-command driver 'POST "/frame" (json (object ("id" #nil)))))

;;; Rectangle Record

(define-record-type <rect>
  (make-rect x y width height)
  rect?
  (x       rect-x)
  (y       rect-y)
  (width   rect-width)
  (height  rect-height))

(export make-rect rect? rect-x rect-y rect-width rect-height)

;;; Resizing and Positioning Windows

(define (result->rect result)
  (let* ((x (hash-ref result "x"))
         (y (hash-ref result "y"))
         (width (hash-ref result "width"))
         (height (hash-ref result "height")))
    (make-rect x y width height)))

(define-public-with-driver (window-rect driver)
  (result->rect (session-command driver 'GET "/window/rect" #f)))

(define-public-with-driver (set-window-position driver x y)
  (set-window-rect driver x y #nil #nil))

(define-public-with-driver (set-window-size driver width height)
  (set-window-rect driver #nil #nil width height))

(define-public-with-driver (set-window-rect driver #:rest args)
  (match args
    ((x y width height)
     (result->rect 
       (session-command driver 'POST "/window/rect" 
         (json (object ("x" ,x) ("y" ,y) ("width" ,width) ("height" ,height))))))
    ((($ <rect> x y width height))
     (set-window-rect driver x y width height))))

(define-public-with-driver (minimize driver)
  (session-command driver 'POST "/window/minimize" (json (object))))

(define-public-with-driver (maximize driver)
  (session-command driver 'POST "/window/maximize" (json (object))))

(define-public-with-driver (full-screen driver)
  (session-command driver 'POST "/window/fullscreen" (json (object))))

(define-public-with-driver (restore driver)
  (set-window-rect driver #nil #nil #nil #nil))

;;; Elements

; XXX elements are returned as a json object with a single weird key
; with value of the actual element id/reference
(define (web-driver-element driver element-object)
  (list 
    'web-driver-element driver 
    (hash-ref element-object "element-6066-11e4-a52e-4f735466cecf")))

(define (element-object? element-object)
  (and
    (hash-table? element-object)
    (hash-ref element-object "element-6066-11e4-a52e-4f735466cecf")))

(define (element-command element method path body-scm)
  (match element
    (('web-driver-element driver element)
      (session-command driver method (format #f "/element/~a~a" element path) body-scm))))

;;; Finding Elements

(define (find-element driver using value)
  (web-driver-element driver
    (session-command driver 
      'POST "/element" 
      (json (object ("using" ,using) ("value" ,value))))))

(define (find-element-from driver from using value)
  (web-driver-element driver
    (element-command from
      'POST "/element"
      (json (object ("using" ,using) ("value" ,value))))))

(define (find-elements driver using value)
  (map
    (lambda (element-object) (web-driver-element driver element-object))
    (session-command driver 
      'POST "/elements" 
      (json (object ("using" ,using) ("value" ,value))))))

(define (find-elements-from driver from using value)
  (map
    (lambda (element-object) (web-driver-element driver element-object))
    (element-command from 
      'POST "/elements" 
      (json (object ("using" ,using) ("value" ,value))))))

(define-syntax define-finder
  (syntax-rules ()
    ((define-finder element-by elements-by using filter)
     (begin
       (define-public-with-driver (element-by driver value #:key (from #f))
         (if from
           (find-element-from driver from using (filter value))
           (find-element driver using (filter value))))
       (define-public-with-driver (elements-by driver value #:key (from #f))
         (if from
           (find-elements-from driver from using (filter value))
           (find-elements driver using (filter value))))))
    ((define-finder element-by elements-by using)
     (define-finder element-by elements-by using identity))))

(define-finder element-by-css-selector elements-by-css-selector "css selector")

; TODO check that the id and class name are valid
; They should be at least one character and not contain any space characters

(define-finder element-by-id elements-by-id 
  "css selector" (lambda (id) (string-append "#" id)))
  
(define-finder element-by-class-name elements-by-class-name 
  "css selector" (lambda (class-name) (string-append "." class-name)))

(define-finder element-by-tag-name elements-by-tag-name "tag name")

(define-finder element-by-link-text elements-by-link-text "link text")

(define-finder element-by-partial-link-text elements-by-partial-link-text "partial link text")

(define-finder element-by-xpath elements-by-xpath "xpath")

(define-public-with-driver (active-element driver)
  (web-driver-element driver (session-command driver 'GET "/element/active" #f)))

;;; Element State

(define-public (selected? element)
  (element-command element 'GET "/selected" #f))

(define-public (attribute element name)
  (element-command element 'GET (format #f "/attribute/~a" name) #f))

(define-public (property element name)
  (element-command element 'GET (format #f "/property/~a" name) #f))

(define-public (css-value element name)
  (element-command element 'GET (format #f "/css/~a" name) #f))

(define-public (text element)
  (element-command element 'GET "/text" #f))

(define-public (tag-name element)
  (element-command element 'GET "/name" #f))

(define-public (rect element)
  (result->rect
    (element-command element 'GET "/rect" #f)))

(define-public (enabled? element)
  (element-command element 'GET "/enabled" #f))

;;; Interacting with elements

(define-public (click element)
  (element-command element 'POST "/click" (json (object))))

(define-public (clear element)
  (element-command element 'POST "/clear" (json (object))))

(define-public (send-keys element text)
  (element-command element 'POST "/value" (json (object ("text" ,text)))))

;;; Document

(define-public-with-driver (page-source driver)
  (session-command driver 'GET "/source" #f))

(define (scm->javascript value)
  (match value
    (#t #t)
    (#f #f)
    (#nil #nil)
    ((? number? n) n)
    ((? string? s) s)
    (('web-driver-element driver handle)
     (json (object ("element-6066-11e4-a52e-4f735466cecf" ,handle))))
    ((? list? l) (map scm->javascript l))))

(define (javascript->scm driver value)
  (match value
    (#t #t)
    (#f #f)
    (#nil #nil)
    ((? number? n) n)
    ((? string? s) s)
    ((? element-object? r) (web-driver-element driver r))
    ((? list? l) (map (lambda (value) (javascript->scm driver value)) l))
    ((? hash-table? t)
     (alist->hash-table
       (hash-map->list
         (lambda (key value)
           (cons key (javascript->scm driver value)))
         t)))))

(define (execute driver path body arguments)
  (let ((js-args (map scm->javascript arguments)))
    (javascript->scm driver
      (session-command 
        driver 'POST path
        (json (object ("script" ,body) ("args" ,js-args)))))))

(define-public-with-driver (execute-javascript driver body #:rest arguments)
  (execute driver "/execute/sync" body arguments))

(define-public-with-driver (execute-javascript-async driver body #:rest arguments)
  (execute driver "/execute/async" body arguments))

;;; Cookies

(define-record-type <cookie>
  (make-cookie name value path domain secure http-only expiry same-site)
  cookie?
  (name       cookie-name)
  (value      cookie-value)
  (path       cookie-path)
  (domain     cookie-domain)
  (secure     cookie-secure)
  (http-only  cookie-http-only)
  (expiry     cookie-expire)
  (same-site  cookie-same-site))

(export 
  cookie-name cookie-value cookie-path cookie-domain cookie-secure 
  cookie-http-only cookie-expire cookie-same-site)

(define (parse-cookie hash)
  (make-cookie 
    (hash-ref hash "name") 
    (hash-ref hash "value") 
    (hash-ref hash "path" "/")
    (hash-ref hash "domain")
    (hash-ref hash "secure" #f)
    (hash-ref hash "httpOnly" #f)
    (hash-ref hash "expiry" #f)
    (hash-ref hash "samesite" #f)))

(define-public-with-driver (get-all-cookies driver)
  (map
    parse-cookie
    (session-command driver 'GET "/cookie" #f)))

(define-public-with-driver (get-named-cookie driver name)
  (parse-cookie (session-command driver 'GET (format #f "/cookie/~a" name) #f)))

(define-public-with-driver 
  (add-cookie driver #:key name value path domain secure http-only expiry same-site)
  (let* ((add (lambda (key value) (if value (list (cons key value)) '())))
         (args
           (append 
             (add "name" name) (add "value" value) (add "path" path) (add "domain" domain) 
             (add "secure" secure) (add "httpOnly" http-only) (add "expiry" expiry) 
             (add "samesite" same-site)))
         (hash (alist->hash-table (list (cons "cookie" (alist->hash-table args))))))
    (session-command driver 'POST "/cookie" hash)))

(define-public-with-driver (delete-named-cookie driver name)
  (session-command driver 'DELETE (format #f "/cookie/~a" name) #f))

(define-public-with-driver (delete-all-cookies driver)
  (session-command driver 'DELETE "/cookie" #f))

