#lang racket/gui

(require 2htdp/image 2htdp/universe "we-must-deliver-this.rkt")

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
(client-their-job)
