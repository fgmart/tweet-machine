#lang racket
(require libserialport)

(provide print-string)
(provide print-number)
(provide normal)
(provide bold)
(provide double-height)
(provide double-width)
(provide print-bitmap)
(provide print-newline)
 
(define-values (in out)
    (open-serial-port "/dev/ttyS0" #:baudrate 9600))

(define *resume-time* (current-inexact-milliseconds)) ; 1 ms per byte at 9600 baud

(define ASCII-DC2  18)  ; Device control 2
(define ASCII-ESC  27)  ; Escape
(define ASCII-FS   28)  ; Field separator
(define ASCII-GS   29)  ; Group separator

(define INVERSE-MASK       (expt 2 1)) ; doesnt work Not in 2.6.8 firmware (see inverseOn())
(define UPDOWN-MASK        (expt 2 2)) ; doesnt work
(define BOLD-MASK          (expt 2 3))
(define DOUBLE-HEIGHT-MASK (expt 2 4))
(define DOUBLE-WIDTH-MASK  (expt 2 5))
(define STRIKE-MASK        (expt 2 6)) ; doesnt work

(define *print-mode* 0)

(define (timeout-wait)
  (cond ((< (current-inexact-milliseconds) *resume-time*) (timeout-wait))))

(define (set-print-mode mask)
  (begin
    (set! *print-mode* (bitwise-ior *print-mode* mask))
    (write-print-mode)))

(define (unset-print-mode mask)
  (begin
    (set! *print-mode* (bitwise-and *print-mode* (bitwise-xor mask 255)))
    (write-print-mode)))

(define (write-print-mode)
  (write-bytes
   (bytes-append (bytes ASCII-ESC) #"!" (bytes *print-mode*))
   out))

(define (normal)
  (begin
    (set! *print-mode* 0)
    (write-print-mode)))

(define (bold mode?)
  (if mode?
      (set-print-mode BOLD-MASK)
      (unset-print-mode BOLD-MASK)))

(define (double-height mode?)
  (if mode?
      (set-print-mode DOUBLE-HEIGHT-MASK)
      (unset-print-mode DOUBLE-HEIGHT-MASK)))

(define (double-width mode?)
  (if mode?
      (set-print-mode DOUBLE-WIDTH-MASK)
      (unset-print-mode DOUBLE-WIDTH-MASK)))

(define (print-to start end)
  (if (> start end)
      '()
      (begin
        (if (even? start)
            (begin (double-width #f) (double-height #t))
            (begin (double-width #t) (double-height #f)))
        (write-bytes (string->bytes/utf-8 (number->string start)) out)
        (write-bytes #"\n" out)
        (print-to (+ start 1) end))))

(define (print-string str)
  (write-bytes (string->bytes/utf-8 str) out))

(define (print-number num)
  (print-string (number->string num)))

; one row; 48 columns (which is max and = 384 px)
; this is looking like one byte per "column" = 8 px per column
; looks like can output max of 255 rows at once
(define (enter-bitmap-mode rows cols)
  (write-bytes
   (bytes-append (bytes ASCII-DC2) #"*" (bytes rows) (bytes (ceiling (/ cols 8))))
   out))

(define (print-newline)
  (write-bytes #"\n" out))

; prints up to 384 wide, 255 high
; if the bitmap is bigger than that, prints upper corner
(define (print-bitmap bm)
  (let* ((width (min 384 (send bm get-width)))
         (height (min 255 (send bm get-height)))
         (pixels (make-bytes (* 4 width height))))
    (begin
      (send bm get-argb-pixels 0 0 width height pixels)
      (enter-bitmap-mode height width)   
      (for ((y (in-range 0 height)))
        (begin
          (set! *resume-time* (+ (current-inexact-milliseconds) (floor (/ width 8))))
          (for ((x (in-range 0 width 8)))
          ; gather the byte and send it
            (let ((byte 0))
              (begin
                (for ((i (in-range 0 8)))
                  (cond ((< (bytes-ref pixels (+ (* y width 4) (* x 4) (* i 4) 2)) ; get green px
                            128)
                         (set! byte (+ byte (arithmetic-shift 1 (- 7 i)))))))
                (write-bytes (bytes byte) out))))
          (timeout-wait)))
      (print-newline))))
