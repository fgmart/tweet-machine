#lang racket

; interface to Gordon Henderson's wiringPi library
; Fred Martin, fred_martin@uml.edu
; December 16, 2017

(require ffi/unsafe
         ffi/unsafe/define)

(provide status-led-on)
(provide status-led-off)
(provide status-led-toggle)     
(provide button?)

(define-ffi-definer define-wiringpi (ffi-lib "libwiringPi"))

(define INPUT 0)
(define OUTPUT 1)
(define PWM_OUTPUT 2)
(define SOFT_PWM_OUTPUT 4)
(define SOFT_TONE_OUTPUT 5)
(define PWM_TONE_OUTPUT 6)

(define	PUD_OFF			 0)
(define	PUD_DOWN		 1)
(define	PUD_UP			 2)

(define LOW 0)
(define HIGH 1)

(define-wiringpi wiringPiSetup   (_fun -> _int))
(define-wiringpi pinMode         (_fun _int _int -> _void))
(define-wiringpi digitalWrite    (_fun _int _int -> _void))
(define-wiringpi digitalRead     (_fun _int -> _int))
(define-wiringpi pullUpDnControl (_fun _int _int -> _void))

(define (update-status-led)
  (if *led-state*
      (digitalWrite 4 HIGH)
      (digitalWrite 4 LOW)))

(define (status-led-on)
  (begin
    (set! *led-state* #t)
    (update-status-led)))

(define (status-led-off)
  (begin
    (set! *led-state* #f)
    (update-status-led)))

(define (status-led-toggle)
  (begin
    (set! *led-state* (not *led-state*))
    (update-status-led)))

(define (button?)
  (= 0 (digitalRead 1)))

; initialize
(define *led-state* #f)
(void (wiringPiSetup))
(pinMode 4 OUTPUT)
(pinMode 1 INPUT)
(pullUpDnControl 1 PUD_UP)
(update-status-led)




(define *resume-time* (current-inexact-milliseconds))
(define (timeout-wait)
  (cond ((< (current-inexact-milliseconds) *resume-time*) (timeout-wait))))

(define (delay ms)
  (begin
    (set! *resume-time* (+ (current-inexact-milliseconds) ms))
    (timeout-wait)))

(define (blink)
  (define (iter)
    (digitalWrite 4 HIGH)
    (delay 500)
    (digitalWrite 4 LOW)
    (delay 500)
    (iter))
  (begin
    (pinMode 4 OUTPUT)
    (iter)))

(define (button-test)
  (define (iter)
    (begin
      (cond ((= 1 (digitalRead 1)) (digitalWrite 4 HIGH))
            (else (digitalWrite 4 LOW)))
      (iter)))
  (begin
    (pinMode 4 OUTPUT)
    (pinMode 1 INPUT)
    (pullUpDnControl 1 PUD_UP)
    (iter)))
  
