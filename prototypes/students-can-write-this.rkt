#lang racket/gui

(require 2htdp/image 2htdp/universe "we-must-deliver-this.rkt")

;; ===================================================================================================
;; a dumb server 

(define MP3-short "../short.mp3")
(define MP3-long  "../long.mp3")

(define (server-our-job)
  (define msg0 (file->bytes MP3-short))
  (define msg1 (file->bytes MP3-long))
  (universe (cons 1 '())
    ; [on-tick values 1 RATE]
    [on-msg  (match-lambda*
              [(list (cons h worlds) from msg)
               (make-bundle (cons (- 1 h) worlds)
                            (map (λ (w) (make-mail w (if (zero? h) msg0 msg1))) worlds)
                            '())])]
    [on-new  (match-lambda*
               [(list (cons h worlds) w)
                (make-bundle (cons h (cons w worlds)) (list (make-mail w msg0)) '())])]))
 
;; ===================================================================================================
;; a dumb student program 

(define (client-their-job)
  (big-bang #false
    [register   LOCALHOST]
    [to-draw    show]
    [on-receive (λ (w msg) (make-package (play-sound msg) 'next))]))

(define (show w)
  (if (boolean? w) (text "waiting... waiting... waiting..." 22 "red") (text w 22 "black")))

;; ===================================================================================================
;; running both 
(launch-many-worlds (server-our-job) (client-their-job))
