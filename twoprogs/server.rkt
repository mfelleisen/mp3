#lang racket/gui

(require 2htdp/image
         2htdp/universe)
         
(define RATE 10)

(define MP3 "and.mp3")

(define (server)
  (define msg (file->bytes MP3))
  (universe '()
    [on-tick values 1 RATE]
    [on-msg  (λ (uni from msg) uni)]
    [on-new (λ (uni w)
              (make-bundle
               (cons w uni)
               (list (make-mail w msg))
               '()))]))

(launch-many-worlds (server))
