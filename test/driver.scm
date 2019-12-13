(define-module (test driver))

(use-modules
  (ice-9 iconv) (ice-9 match) (ice-9 threads)
  (hdt hdt)
  (web client) (web driver) (web request) (web response) (web server) (web uri))

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

(test elements
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
  (test text
    (set-web-handler! (const-html "<html><body>outer <div id='theid'>text</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (equal? "text" (text (element-by-id "theid")))))
  (test attribute
    (set-web-handler! (const-html "<html><body><div id='theid' class='cool'>text</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (equal? "cool" (attribute (element-by-id "theid") "class"))))
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

#!
(test seo
  (navigate-to "http://duckduckgo.com")
  (send-keys (element-by-id "search_form_input_homepage") "guile webdriver selenium")
  (click (element-by-id "search_button_homepage"))
  (assert (element-by-css-selector "a[href='https://github.com/her01n/guile-web-driver']")))
!#
    
; TODO test cookies

