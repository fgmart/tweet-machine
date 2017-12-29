#lang racket

;; main code for Tweet Machine
;; Fred Martin, fred_martin@uml.edu
;; December 29, 2017

(require
  net/url 
  json
  racket/draw
  hostname)

(require "oauth-single-user.rkt")
(require "dithering.rkt")
(require "wiringpi.rkt")
(require "goojprt.rkt")

; Twitter credentials and web server username/password
(include "secrets.rkt")

; Twitter search settings
(define *root* (path->string (current-directory)))
(define *settings-file* (string-append *root* "settings.txt"))
(define *max-results* 50)
(define *query-index* 0) ; do a search at this line of settings file

; reads into list where each item is a line
(define (settings)
  (call-with-input-file *settings-file* 
    (lambda (port) (port->lines port))))

(define twitter-oauth (new oauth-single-user%  
     [consumer-key *consumer-key*]
     [consumer-secret *consumer-secret*]
     [access-token *access-token*]
     [access-token-secret *access-token-secret*]))

(define (inc-query-index)
  (set! *query-index* (add1 *query-index*))
  (cond ((= *query-index* (length (settings))) (set! *query-index* 0))))

; will if settings file has all blank lines, will return default search query
(define (get-next-query)
  (define (iter n)
    (let ((result (list-ref (settings) *query-index*)))
      (if (= 0 n) "kittens puppies"
          (begin 
            (inc-query-index)
            (if (> (string-length result) 0)
                result
                (iter (sub1 n)))))))
  (iter (length (settings))))

(define (get-tweet-bytes)
  (let ((twitter-search (get-next-query)))
    (begin
      (displayln (string-append "search is " twitter-search))
      (car
       (send twitter-oauth get-request 
             "https://api.twitter.com/1.1/search/tweets.json"
             (list
              (cons 'q twitter-search)
              (cons 'count (number->string *max-results*))
              ))))))

(define (get-tweets)
  (hash-ref (bytes->jsexpr (get-tweet-bytes)) 'statuses))

(define (tweet->screen_name tweet)
  (hash-ref (hash-ref tweet 'user) 'screen_name))

; defaults to "normal" image size of 48x48
; provide this optional arg to get other sizes:
; 'bigger -> 73x73
; 'mini -> 24x24
; 'original -> arbitrarily large
(define (tweet->profile_image_url tweet . size)
  (let ((url-str (hash-ref (hash-ref tweet 'user) 'profile_image_url)))
    (if (null? size)
        url-str
        (let ((size (car size)))
          (cond ((eq? size 'bigger) (string-replace url-str "_normal." "_bigger."))
                ((eq? size 'mini) (string-replace url-str "_normal." "_mini."))
                ((eq? size 'original) (string-replace url-str "_normal." "."))
                (else url-str))))))

(define (tweet->text tweet)
  (hash-ref tweet 'text))

(define (tweet->created_at tweet)
  (hash-ref tweet 'created_at))

; re-wrote to scale on download!
; looks like images aren't necessary square. Ugh.
; we'll end up printing the first 255 lines of the 256 wide x randomly high image.
(define (make-scaled-bitmap tweet width type)    
    (read-bitmap
     (get-pure-port
      (string->url (tweet->profile_image_url tweet type)))
     #:backing-scale (/ width 256)))

(define (make-profile-bitmap tweet)
  (let* ((bitmap (read-bitmap
                 (get-pure-port
                  (string->url (tweet->profile_image_url tweet 'original)))))
         (width (send bitmap get-width)))
    ; awful patch since Twitter's API sometimes fails to produce the "original" image
    (if (> width 1)
        (make-scaled-bitmap tweet width 'original)
        (make-scaled-bitmap tweet 73 'bigger))))

(define (print-tweet tweet)
  (let ((bm (bitmap->burkes (make-profile-bitmap tweet)))
        (screen-name (string-append "@" (tweet->screen_name tweet)))
        (created-at (string-replace (tweet->created_at tweet) "+0000 " ""))
        (text (tweet->text tweet)))
    (begin
      (normal)(status-led-toggle)
      (print-bitmap bm)
      (double-width #t)
      (print-string screen-name)(print-newline)
      (normal)(bold #t)(print-string created-at)(print-newline)
      (normal)(print-string text)(print-newline)
      (print-newline)
      (print-newline)
      (print-newline)
      (print-newline))))

(define (print-random-tweet)
  (let* ((tweets (get-tweets))
         (num-tweets (length tweets)))
    (if (= 0 num-tweets)
        (begin
          (displayln "No tweets returned!")
          (bold #t)
          (print-string "No tweets returned! Try again.")
          (normal)
          (print-newline)(print-newline)(print-newline)(print-newline)(print-newline))
        (let ((tweet
               (if (= 1 num-tweets)
                   (car tweets)
                   (list-ref tweets (sub1 (random 1 num-tweets))))))
          (begin
            (displayln (string-append
                        "  from " (number->string num-tweets) " tweets: "
                        "@" (tweet->screen_name tweet)
                        " " (tweet->created_at tweet)))
            (print-tweet tweet))))))

(define (wait-for-button)
  (if (button?)
      #t
      (wait-for-button)))

(define (mainloop)
  (begin
    (status-led-on)
    (wait-for-button)
    (status-led-off)
    (print-random-tweet)
    (mainloop)))

(define (print-startup-msg)
  (normal)(double-height #t)(print-string "The Tweet Machine")(print-newline)
  (normal)(print-string "Developed by Fred Martin during his sabbatical at Tufts CEEO")(print-newline)
  (print-string "Fall 2017")(print-newline)
  (print-newline)
  (normal)(bold #t)(print-string "Twitter queries are:")(print-newline)
  (for ((i (in-range 0 (length (settings)))))
    (begin
      (normal)
      (let ((str (list-ref (settings) i)))
        (cond ((> (string-length str) 0)
               (begin
                 (print-string str)
                 (print-newline)))))))
  (print-newline)
  (let ((addrs (get-ipv4-addrs)))
    (if (pair? addrs)
        (begin
          (print-string "Configure queries at")(print-newline)
          (print-string (string-append "https://" (first addrs)))(print-newline)
          (print-string "with")(print-newline)
          (bold #t)(print-string "username: ")(bold #f)
          (print-string *username*)(print-newline)
          (bold #t)(print-string "password: ")(bold #f)
          (print-string *password*)(print-newline)
          (print-newline)
          (print-string "Press button to see")(print-newline)
          (print-string "your first tweet!")(print-newline)
          (print-newline)(print-newline)(print-newline))
        (begin
          (bold #t)(print-string "Uh-oh, no net!")
          (print-newline)(print-newline)(print-newline)))))

(print-startup-msg)
(mainloop)
