#! /bin/sh
#|
exec /Users/matthias/plt/racket/bin/racket -tm "$0" ${1+"$@"}
|#
#lang racket

(provide
  (rename-out
    ;; PathString ->* String String String (U Number False)
    ;; extract the title, artist, album and year from an MP3 file 
    [mp3-tags main]))

;; ---------------------------------------------------------------------------------------------------

(define (mp3-tags file:path)
  (define file:bytes     (file->bytes file:path))
  (define tags           (subbytes file:bytes (- (bytes-length file:bytes) 128)))
  (define-extract tags 3 [title 30] [artist 30] [album 30] [year:str 5])
  (define year           (string->number year:str))
  (values title artist album year))

;; SYNTAX defines 'name' ... by successively extracting bytes from 'tags' starting at 'base'
(define-syntax-rule
  (define-extract tags start [name step] ...)
  (define-values (name ...)
    (let ([i start])
      (let*-values ([(i name) (values (+ i step) (extract-string tags i (+ i step -1)))] ...)
	(values name ...)))))

;; Bytes N N -> String
;; extract specified bytes and trim UTF-8 string from right 
(define (extract-string tags low high)
  (define s       (bytes->string/utf-8 (subbytes tags low high)))
  (define s-nulls (string-trim s #rx"\0+" #:left? #f))
  (define s-space (string-trim s-nulls #rx" +" #:left? #f))
  s-space)
