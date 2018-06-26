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

(define (mp3-tags file:path)
  (define file:bytes (file->bytes file:path))
  (define tags       (subbytes file:bytes (- (bytes-length file:bytes) 128)))
  (define title      (extract-string tags 3 32))
  (define artist     (extract-string tags 33 62))
  (define album      (extract-string tags 63 92))
  (define year:str   (extract-string tags 93 97))
  (define year       (string->number year:str))
  (values title artist album year))

;; Bytes N N -> String
;; extract specified bytes and trim UTF-8 string from right 
(define (extract-string tags low high)
  (string-trim (bytes->string/utf-8 (subbytes tags low high))
               #rx"\0+"
	       #:left? #f))
