(define-module (test driver))

(use-modules
  (ice-9 iconv) (ice-9 match) (ice-9 threads)
  (hdt hdt)
  (srfi srfi-1)
  (web client) (web request) (web response) (web server) (web uri))

(use-modules
  (web driver))

(test set-web-handler!
  (set-web-handler! (lambda (request body) (values '() "test")))
  (call-with-values
    (lambda () (http-get "http://localhost:8080/"))
    (lambda (response body)
      (assert (equal? 200 (response-code response)))
      (assert (equal? "test" body)))))

(test call-with-web-driver
  (define closed #f)
  (assert
    (equal? 42
      (call-with-web-driver
        (lambda (driver)
          (assert (web-driver? driver))
          (assert (web-driver-open? driver))
          (assert (equal? driver (get-default-driver)))
          (set! closed driver)
          42))))
  (assert (not (web-driver-open? closed))))
  
; Use only one driver, default, to speed up the tests
(hook (close-web-driver))

(test timeouts
  (test script-timeout
    (test milliseconds
      (set-script-timeout 20000)
      (assert (equal? 20000 (get-script-timeout))))
    (test never
      (set-script-timeout #:never)
      (assert (equal? #:never (get-script-timeout))))
    (test default
      (set-script-timeout)
      (assert (equal? 30000 (get-script-timeout)))))
  (test page-load-timeout
    (test milliseconds
      (set-page-load-timeout 60000)
      (assert (equal? 60000 (get-page-load-timeout))))
    (test default)
      (set-page-load-timeout)
      (assert (equal? 300000 (get-page-load-timeout))))
  (test implicit-timeout
    (test milliseconds
      (set-implicit-timeout 1000)
      (assert (equal? 1000 (get-implicit-timeout))))
    (test default
      (set-implicit-timeout)
      (assert (equal? 0 (get-implicit-timeout)))))
  (test independent
    (set-script-timeout 10000)
    (set-page-load-timeout 20000)
    (assert (equal? 10000 (get-script-timeout)))))
  
(define (const-html html)
  (lambda (request body) (values '((content-type . (text/html))) html)))

(test navigation
  (test navigate-to
    ; To get current url we do not really need to run a web server
    (navigate-to "http://localhost:8080")
    (assert (equal? "http://localhost:8080/" (current-url))))
  (test back-forward 
    (set-web-handler! (const-html "<html><body></body></html>"))
    (navigate-to "http://localhost:8080/a")
    (navigate-to "http://localhost:8080/b")
    (back)
    (assert (equal? "http://localhost:8080/a" (current-url)))
    (forward)
    (assert (equal? "http://localhost:8080/b" (current-url))))
  (test refresh
    (set-web-handler! (const-html "<html><body></body></html>"))
    (navigate-to "http://localhost:8080")
    (set-web-handler! (const-html "<html><body><div id='theid' /></body></html>"))
    (refresh)
    (assert (element-by-id "theid")))
  (test title
    (set-web-handler! (const-html "<html><head><title>the title</title></head><body></body></html>"))
    (navigate-to "http://localhost:8080/")
    (assert (equal? "the title" (title)))))

(test windows
  (test open-close-windows
    (assert (equal? 1 (length (all-windows))))
    (open-new-window)
    (assert (equal? 2 (length (all-windows))))
    (close-window)
    (assert (equal? 1 (length (all-windows)))))
  (hook
    (define (close-other-windows)
      (when (> (length (all-windows)) 1) 
        (close-window)
        (close-other-windows)))
    (close-other-windows))
  (test open-new-window
    (let ((window (open-new-window)))
      (assert (member window (all-windows)))))
  (test open-new-tab
    (let ((window (open-new-tab)))
      (assert (member window (all-windows)))))
  (test switch-to-window
    (open-new-window)
    (match-let (((one two) (all-windows)))
      (assert (not (equal? one two)))
      (switch-to one)
      (assert (equal? one (current-window)))
      (switch-to two)
      (assert (equal? two (current-window)))))
  (test independent-navigation
    (open-new-window)
    (match-let (((one two) (all-windows)))
      (switch-to one)
      (navigate-to "http://localhost:9753/")
      (switch-to two)
      (navigate-to "http://localhost:8642/")
      (switch-to one)
      (assert (equal? "http://localhost:9753/" (current-url))))))

(test browsing-context
  (set-web-handler!
    (lambda (request body)
      (values 
        '((content-type . (text/html))) 
        (match (uri-path (request-uri request))
          ("/" "<p>top</p><iframe src='/inner.html'></iframe>")
          ("/inner.html" "<p>inner</p>")
          (_ "<p>not found</p>")))))
  (navigate-to "http://localhost:8080")
  (test switch-to-frame
    (switch-to (element-by-tag-name "iframe"))
    (assert (equal? "inner" (text (element-by-tag-name "p")))))
  (test switch-to-nth
    (switch-to 0)
    (assert (equal? "inner" (text (element-by-tag-name "p")))))
  (test switch-to-parent
    (switch-to (element-by-tag-name "iframe"))
    (switch-to-parent)
    (assert (equal? "top" (text (element-by-tag-name "p")))))
  (test switch-to-window
    (switch-to (element-by-tag-name "iframe"))
    (switch-to-window)
    (assert (equal? "top" (text (element-by-tag-name "p"))))))

(test resizing-and-positioning-windows
  (test set-window-position
    (set-window-position 20 30)
    (let ((rect (window-rect)))
      (assert (equal? 20 (rect-x rect)))
      (assert (equal? 30 (rect-y rect)))))
  (test set-window-size
    (set-window-size 640 480)
    (let ((rect (window-rect)))
      (assert (equal? 640 (rect-width rect)))
      (assert (equal? 480 (rect-height rect)))))
  (test set-window-rect-xywh
    (set-window-rect 30 40 800 600)
    (assert (equal? (make-rect 30 40 800 600) (window-rect))))
  (test set-window-rect-rect
    (set-window-rect (make-rect 40 50 888 666))
    (assert (equal? (make-rect 40 50 888 666) (window-rect))))
  (hook (restore))
  (test minimize
    ; do not how to test this,
    ; just check it does not throw
    (minimize))
  (test maximize
    (maximize)
    (let ((rect (window-rect)))
      (assert (equal? (0 (rect-x rect))))
      (assert (>= (rect-width rect) 1280))))
  (test full-screen
    (full-screen)
    (let ((rect (window-rect)))
      (assert (equal? (0 (rect-x rect))))
      (assert (equal? (0 (rect-y rect))))
      (assert (>= (rect-width rect) 1280))))
  (test restore
    (set-window-position 20 30)
    (maximize)
    (restore)
    (let ((rect (window-rect)))
      (assert (equal? 20 (rect-x rect)))
      (assert (equal? 30 (rect-y rect))))))

(test finding-elements
  (test element-by-css-selector
    (set-web-handler! (const-html "<html><body><div type='text'>content</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (element-by-css-selector "div[type='text']"))
    (assert (throws-exception (element-by-css-selector "div[type='image']"))))
  (test element-by-id
    (set-web-handler! (const-html "<html><body><div id='theid'>content</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (element-by-id "theid"))
    (assert (throws-exception (element-by-id "missing"))))
  (test element-by-class-name
    (set-web-handler! (const-html "<html><body><div class='clazz'>xxx</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (element-by-class-name "clazz"))
    (assert (throws-exception (element-by-class-name "missing"))))
  (test element-by-link-text
    (set-web-handler! (const-html "<html><body><a href='/'>link text</a></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (element-by-link-text "link text"))
    (assert (throws-exception (element-by-link-text "text")))
    (assert (element-by-partial-link-text "link"))
    (assert (throws-exception (element-by-partial-link-text "xxx"))))
  (test element-by-tag-name
    (set-web-handler! (const-html "<html><body><a href='/'>link text</a></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (element-by-tag-name "a"))
    (assert (throws-exception (element-by-tag-name "b"))))
  (test element-by-xpath
    (set-web-handler! (const-html "<html><body><div>content</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (element-by-xpath "//body/div"))
    (assert (throws-exception (element-by-xpath "//div/div"))))
  (test elements-by-class-name
    (set-web-handler! 
      (const-html 
        "<html><body><div class='clazz'>one</div><div class='clazz'>two</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (let ((divs (elements-by-class-name "clazz")))
      (assert (list? divs))
      (assert (equal? 2 (length divs)))
      (assert (equal? "one" (text (car divs))))
      (assert (equal? "two" (text (cadr divs))))))
  (test element-from 
    (set-web-handler!
      (const-html "<div class='empty'></div><div class='full'><p>in<p></div>"))
    (navigate-to "http://localhost:8080")
    (assert (element-by-tag-name "p" #:from (element-by-class-name "full")))
    (assert 
      (throws-exception (element-by-tag-name "p" #:from (element-by-class-name "empty"))))))

(test element-state
  (test selected?
    (set-web-handler! 
      (const-html 
        "<form>
          <input type='checkbox' name='checked' checked='true'/>
          <input type='checkbox' name='unchecked'/>
        </form>"))
    (navigate-to "http://localhost:8080")
    (assert (selected? (element-by-css-selector "input[name='checked']")))
    (assert (not (selected? (element-by-css-selector "input[name='unchecked'")))))
  (test property
    (set-web-handler!
      (const-html "<html><body>content</body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (equal? "BODY" (property (element-by-tag-name "body") "tagName"))))
  (test attribute
    (set-web-handler! (const-html "<div key='value'></div>"))
    (navigate-to "http://localhost:8080")
    (assert (equal? "value" (attribute (element-by-tag-name "div") "key")))
    (assert (not (attribute (element-by-tag-name "div") "xxx"))))
  (test css-value
    (set-web-handler! (const-html "<div style='font-size:11'>content</div>"))
    (navigate-to "http://localhost:8080")
    (assert (equal? "11px" (css-value (element-by-tag-name "div") "font-size"))))
  (test text
    (set-web-handler! (const-html "<html><body>outer <div id='theid'>text</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (equal? "text" (text (element-by-id "theid")))))
  (test tag
    (set-web-handler! (const-html "<div>content</div>"))
    (navigate-to "http://localhost:8080")
    (assert (equal? "div" (tag-name (element-by-tag-name "div")))))
  (test rect
    (set-web-handler! 
      (const-html 
        "<html>
           <body style='margin-left: 1px; margin-top: 2px;'>
             <div style='width: 3px; height: 4px; background: red;'>
             </div>
           </body>
         </html>"))
    (navigate-to "http://localhost:8080")
    (assert 
      (equal?
        (make-rect 1 2 3 4)
        (rect (element-by-tag-name "div")))))
  (test enabled?
    (set-web-handler!
      (const-html 
        "<form><input name=enabled /><input name=disabled disabled=true /></form>"))
    (navigate-to "http://localhost:8080")
    (assert (enabled? (element-by-css-selector "input[name='enabled']")))
    (assert (not (enabled? (element-by-css-selector "input[name='disabled']"))))))

(test element-interaction
  (test click
    (set-web-handler!
      (lambda (request body)
        (values
          '((content-type . (text/html)))
          (match (uri-path (request-uri request))
            ("/" "<html><body><a id='one' href='/one'>one</a></body></html>")
            ("/one/" "<html><body>one</body></html>")
            (else "")))))
    (navigate-to "http://localhost:8080")
    (click (element-by-id "one"))
    (assert (equal? "http://localhost:8080/one" (current-url))))
  (test clear
    (set-web-handler! (const-html "<form><input name=in value=filled /></form>"))
    (navigate-to "http://localhost:8080")
    (clear (element-by-tag-name "input"))
    (assert (equal? "" (property (element-by-tag-name "input") "value"))))
  (test send-keys
    (set-web-handler!
      (const-html
        "<html><body><form method='get' action='submit'>
           <input id='text' type='text' name='text' />
           <input id='submit' type='submit'/>
         </form></body></html>"))
    (navigate-to "http://localhost:8080")
    (send-keys (element-by-id "text") "keys")
    (click (element-by-id "submit"))
    (assert (equal? "http://localhost:8080/submit?text=keys" (current-url)))))

(test document
  (test page-source
    (set-web-handler! (const-html "<html><head></head><body>hello</body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (equal? "<html><head></head><body>hello</body></html>" (page-source))))
  (test execute-javascript
    (set-web-handler! (const-html "<div id='d'>content</div>"))
    (navigate-to "http://localhost:8080")
    (test execute
      (execute-javascript "window.location.href = 'http://localhost:1234/'")
      (assert (equal? "http://localhost:1234/" (current-url))))
    (test return-value
      (assert (equal? 7 (execute-javascript "return 3 + 4"))))
    (test pass-parameters
      (assert (equal? 7 (execute-javascript "return arguments[0] + arguments[1]" 3 4))))
    ; TODO pass list
    (test pass-element
      (let ((div (element-by-id "d")))
        (execute-javascript "arguments[0].innerHTML = 'updated'" div)
        (assert (equal? "updated" (text div)))))
    (test return-element
      (let ((div (execute-javascript "return document.getElementById('d')")))
        (assert (equal? "content" (text div)))))
    (test return-list
      (assert (equal? (list 1 2) (execute-javascript "return [1, 2]"))))
    (test return-object
      (let ((table (execute-javascript "var r = new Object(); r.key0 = 'value0'; return r;")))
        (assert (hash-table? table))
        (assert (equal? "value0") (hash-ref table "key0")))))
  (test callback
    (set-web-handler! (const-html "hello"))
    (navigate-to "http://localhost:8080/")
    (assert
      (equal?
        42
        (execute-javascript-async
          "callback = arguments[0];
           window.setTimeout(function () { callback(42); }, 1);")))))

(test cookies
  (set-web-handler!
    (lambda (request body)
      (values '((set-cookie . "name=value")) "ok")))
  (navigate-to "http://localhost:8080")
  (test get-all-cookies
    (let ((cookies (get-all-cookies)))
      (assert (equal? 1 (length cookies)))
      (assert (equal? "name" (cookie-name (first cookies))))
      (assert (equal? "value" (cookie-value (first cookies))))))
  (test get-named-cookie
    (let ((cookie (get-named-cookie "name")))
      (assert (equal? "name" (cookie-name cookie)))
      (assert (equal? "value" (cookie-value cookie)))))
  (test add-cookie
    (navigate-to "http://localhost:8080/path/component/test.html")
    (add-cookie #:name "session" #:value "77" #:path "/path")
    (let ((cookie (get-named-cookie "session")))
      (assert cookie)
      (assert (equal? "77" (cookie-value cookie)))
      (assert (equal? "/path" (cookie-path cookie)))))
  (test delete-named-cookie
    (add-cookie #:name "session" #:value "77")
    (delete-named-cookie "name")
    (let ((cookies (get-all-cookies)))
      (assert (equal? 1 (length cookies)))
      (assert (equal? "session" (cookie-name (first cookies))))))
  (test delete-all-cookies
    (delete-all-cookies)
    (assert (null? (get-all-cookies)))))

#!
(test seo
  (navigate-to "http://duckduckgo.com")
  (send-keys (element-by-id "search_form_input_homepage") "guile webdriver selenium")
  (click (element-by-id "search_button_homepage"))
  (assert (element-by-css-selector "a[href='https://github.com/her01n/guile-web-driver']")))
!#
    

