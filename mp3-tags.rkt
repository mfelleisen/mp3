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

(require (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class byte
    (pattern b:nat #:fail-unless (< (syntax-e #'b) 256) "not a byte")))
    
(define (mp3-tags file:path)
  (define file:bytes     (file->bytes file:path))
  (define tags           (subbytes file:bytes (- (bytes-length file:bytes) 128)))
  (define-extract tags 3 [title 30] [artist 30] [album 30] [year:str 5] 128)
  (define year           (string->number year:str))
  (values title artist album year))

;; SYNTAX defines 'name' ... by successively extracting bytes from 'tags' starting at 'base'
(define-syntax (define-extract stx)
  (syntax-parse stx
    [(_ tags start:integer [name step:byte] ... end:byte)
     ;; ---------------------------------
     ;; the static error checking 
     #:do [(define end-int  (syntax-e #'end))
           (define step-int (apply + (map syntax-e (syntax->list #'(step ...)))))]
     #:fail-unless (< step-int end-int) "index out of range"
     ;; ---------------------------------
     #`(define-values (name ...)
         (let ([i start])
           (let*-values ([(i name) (values (+ i step) (extract-string tags i (+ i step -1)))] ...)
             (values name ...))))]))

;; Bytes N N -> String
;; extract specified bytes and trim UTF-8 string from right 
(define (extract-string tags low high)
  (define s       (bytes->string/utf-8 (subbytes tags low high)))
  (define s-nulls (string-trim s #rx"\0+" #:left? #f))
  (define s-space (string-trim s-nulls #rx" +" #:left? #f))
  s-space)

;; ---------------------------------------------------------------------------------------------------
;; a Haskellian solution to extracting the tags from an "mp3" byte string 

;; Bytes N [Listof [List X N]] -> [Hashof X String]
(define (extract tags base end . assoc)
  (define-values (results _)
    (for/fold ([results '()] [base base]) ([a assoc])
      (match-define (list name step) a)
      ;; ---------------------------------
      ;; the dynamic error checking 
      (define up2 (+ base step -1))
      (unless (< step end)
        (error 'extract "index out of range"))
      ;; ---------------------------------
      (define r (extract-string tags base up2))
      (values (cons (cons name r) results) (+ base step))))
  (make-hash results))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  
  (define tags
    (bytes-append
     #"TAG"
     #"my title                      "
     #"my artist                     "
     #"my album                      "
     #"2018   "))

  (define-extract tags 3 [title 30] [artist 30] [album 30] [year 5] 128)
  (check-equal? title  "my title")
  (check-equal? artist "my artist")
  (check-equal? album  "my album")
  (check-equal? year   "2018")

  (define mapping (extract tags 3 128 '[title 30] '[artist 30] '[album 30] '[year 5]))
  (check-equal? (hash-ref mapping 'title)  "my title")
  (check-equal? (hash-ref mapping 'artist) "my artist")
  (check-equal? (hash-ref mapping 'album)  "my album")
  (check-equal? (hash-ref mapping 'year)   "2018"))