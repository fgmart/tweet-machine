#lang racket

; tm-webserver.rkt
; Fred Martin, fred_martin@uml.edu
; December 16, 2017
;
; little web server which allows user to configure
; standing queries for the Tweet Machine

; Most of this code from Matt Might, http://matt.might.net/articles/low-level-web-in-racket/

(require web-server/servlet
         web-server/servlet-env)

(require net/uri-codec)
(require file/sha1)
(require net/base64)

(define *root* (path->string (current-directory)))

; path to the server certificate:
(define *cert-path* (string-append *root* "server-cert.pem"))

; path to the private key:
(define *key-path* (string-append *root* "private-key.pem"))

; path to the passwd file:
(define *passwd-file* (string-append *root* "passwd"))

(define *settings-file* (string-append *root* "settings.txt"))

(define (any? pred list)
  ; returns true iff any element matches pred:
  (match list
    ['()  #f]
     [(cons hd tl)
     (or (pred hd) (any? pred (cdr list)))]))

; reads into list where each item is a line
(define lines
  (call-with-input-file *settings-file* 
    (lambda (port) (port->lines port))))

; a password checking routine:
(define (htpasswd-credentials-valid?
         *passwd-file*
         username
         password)
  ; checks if the given credentials match those in the database
  ; it assumes all entries as SHA1-encoded as in `htpasswd -s`.

  ; read in the lines from the password file:
  (define lines (call-with-input-file *passwd-file* 
                  (lambda (port) (port->lines port))))
  
  ; convert the password to sha1:
  (define sha1-pass (sha1-bytes (open-input-bytes password)))
  
  ; then to base64 encoding:
  (define sha1-pass-b64 
    (bytes->string/utf-8 (base64-encode sha1-pass #"")))
  
  ; check if both the username and the password match:
  (define (password-matches? line)

      (define user:hash (string-split line ":"))
      
      (define user (car user:hash))
      (define hash (cadr user:hash))
      
      (match (string->list hash)
        ; check for SHA1 prefix
        [`(#\{ #\S #\H #\A #\} . ,hashpass-chars)
         (define hashpass (list->string hashpass-chars))
         (and (equal? username (string->bytes/utf-8 user)) 
              (equal? hashpass sha1-pass-b64))]))
  
  ; check to see if any line validates:
  (any? password-matches? lines))


(define (req->user req)
  ; extracts the user for this request
  (match (request->basic-credentials req)
    [(cons user pass)   user]
    [else               #f]))

(define (authenticated? req)
  ; checks if a request has valid credentials:
  (match (request->basic-credentials req)
    [(cons user pass)
     (htpasswd-credentials-valid? *passwd-file* user pass)]
    
    [else     #f]))

(define (hello-servlet req)
  
  (cond
    ; check for authentication:
    [(not (authenticated? req))
     (response
      401 #"Unauthorized" 
      (current-seconds) 
      TEXT/HTML-MIME-TYPE
      (list
       (make-basic-auth-header
        "Authentication required"
        ))
      void)]
    
    ; if authenticated, serve the page:
    [else
     (response/xexpr
      #:preamble #"<!DOCTYPE html>"
      `(html
        (head)
        (body
         (p "Hello, " ,(bytes->string/utf-8 (req->user req)) "!"))))]))

(define (form-servlet req)
 
  (define uri (request-uri req))
  (define path (map path/param-path (url-path uri)))    
  (define page (car path))
  
  (cond 

    ; check for authentication:
    [(not (authenticated? req))
     (response
      401 #"Unauthorized" 
      (current-seconds) 
      TEXT/HTML-MIME-TYPE
      (list
       (make-basic-auth-header
        "Authentication required"
        ))
      void)]
    
    ; /form
    [(equal? page "")     
     (response/xexpr
      `(html
        (title "The Tweet Machine")
        (body
         (h1 "The Tweet Machine")
         (p "Developed by Fred Martin during his sabbatical at Tufts CEEO, Fall 2017")
         (p "The entire application is implemented in Racket (a variant of Scheme/Lisp)," (br)
            "running on a Raspberry Pi 3.")
         (p "Thanks to:" (br)
             "Stephen Baldwin, OAuth 1.0 API in Racket, with Twitter example;" (br)
             "Adafruit, code for printer; Tanner Helland, image dithering tutorial;" (br)
             "Wikipedia, dithering pseudocode; Matt Might, code for web server;" (br)
             "libserialport team; Gordon Henderson, WiringPi.")
         (p "Configure the standing Twitter queries below." (br)
            "When you press the button, one of them will run and a result printed.")
         (p "Use " (em "from:twitterusername") " to get that person's tweets;" (br)
            (em "@twitterusername") " to include Twitter-mentions;" (br)
            (em "-RT") " to filter out retweets.")
         (form ([method "POST"] [action "/settings-updated"])
               (input ([type "text"]
                       [name "query1"]
                       [value ,(first lines)]))
               (br)
               (input ([type "text"]
                       [name "query2"]
                       [value ,(second lines)]))
               (br)
               (input ([type "text"]
                       [name "query3"]
                       [value ,(third lines)]))
               (br)
               (input ([type "text"]
                       [name "query4"]
                       [value ,(fourth lines)]))
               (br)
               (input ([type "text"]
                       [name "query5"]
                       [value ,(fifth lines)]))
               (br)
               (input ([type "submit"]))))))]
    
    ; /print-form-data
    [(equal? page "settings-updated")
  
     ; extract the form data:
     (define post-data (bytes->string/utf-8 (request-post-data/raw req)))
     
     ; convert to an alist:
     (define form-data (form-urlencoded->alist post-data))

     ; pull out the user and comment:
     (define query1 (cdr (assq 'query1 form-data)))
     (define query2 (cdr (assq 'query2 form-data)))
     (define query3 (cdr (assq 'query3 form-data)))
     (define query4 (cdr (assq 'query4 form-data)))
     (define query5 (cdr (assq 'query5 form-data)))

     ; now write the values out to disk
     (define out (open-output-file *settings-file* #:mode 'text #:exists 'replace))
     (displayln query1 out)
     (displayln query2 out)
     (displayln query3 out)
     (displayln query4 out)
     (displayln query5 out)
     (close-output-port out)
     
     ; send back the extracted data:
     (response/xexpr
      `(html
        (title "Settings updated")
        (body
         (h1 "The new queries are:")
         (p ,query1)
         (p ,query2)
         (p ,query3)
         (p ,query4)
         (p ,query5)
         )))
     ]
    
    ; another page?
    [else
     (response/xexpr
      `(html
        (title "404")
        (body
         (p "404 Page not found!")
         (p "Please start at the root"))))]))

(serve/servlet form-servlet
               #:launch-browser? #f
               #:servlet-regexp #rx""
               #:listen-ip #f
               #:servlet-path ""
               #:port 443
               #:ssl? #t
               #:ssl-cert *cert-path*
               #:ssl-key  *key-path*)
