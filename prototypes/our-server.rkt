#lang racket/gui

;; PURPOSE a dumb server 

;; ===================================================================================================
;; SERVICES 

(provide
 ;; -> '()
 server-our-job)

;; ===================================================================================================
;; DEPENDENCIES

(require 2htdp/universe)
(require 2htdp/image)
(require "we-must-deliver-this.rkt")

(module+ test
  (require rackunit))

;; ===================================================================================================
;; IMPLEMENTATION 

(define MP3-short "../short.mp3")
(define MP3-long  "../long.mp3")

(define (server-our-job)
  (define msg0 (make-song-bytes "short" (file->bytes MP3-short)))
  (define msg1 (make-song-bytes "long"  (file->bytes MP3-long)))
  (void
   (universe (cons 1 '())
     [on-msg  (match-lambda*
                [(list (cons h worlds) from msg)
                 (make-bundle (cons (- 1 h) worlds)
                              (map (λ (w) (make-mail w (if (zero? h) msg0 msg1))) worlds)
                              '())])]
     [on-new  (match-lambda*
                [(list (cons h worlds) w)
                 (make-bundle (cons h (cons w worlds)) (list (make-mail w msg0)) '())])])))

;; ===================================================================================================
;; Run Program Run 
(module+ main (server-our-job))
