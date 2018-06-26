#! /bin/sh
#|
exec /Users/matthias/plt/racket/bin/racket -tm "$0" ${1+"$@"}
|#
#lang racket/gui

;; ---------------------------------------------------------------------------------------------------
;; the retriev-result function is kind of within reach of students after the first week
;; at the pace of the regular schedue; they should do it so they don't think this is black magic 

(provide
 ;; ByteString -> RESPONSE
 (contract-out
  [play-sound (-> bytes? response?)]
  [response?  (-> any/c boolean?)]
  [DONT       string?]
  [LIKE       string?]
  [DONE       string?]))

;; ---------------------------------------------------------------------------------------------------
;; dependencies 
(require 2htdp/image 2htdp/universe)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; implementation 

(define DONT "I don't like it")
(define LIKE "I like it")
(define DONE "song completed without feedback")
;; type Response = 
;;  | DONT     ;; feedback has been provided
;;  | LIKE     ;; feedback has been provided
;;  | DONE     ;; the song is over, no feedback 

(define (response? x)
  (memq x (list DONT LIKE DONE)))

(define (play-sound mp3)
  (define custodian (make-custodian))
  (parameterize ((current-custodian custodian))
    (parameterize ((current-eventspace (make-eventspace)))
      (define vps (mp3->vps mp3))
      (send vps play)
      (define stateN (retrieve-result vps))
      (send vps stop)
      (custodian-shutdown-all custodian)
      (cond
        [(false? stateN)  DONE]
        [(symbol? stateN) DONE]
        [else stateN]))))

(define PAUSED 'paused)
;; type State =
;;  | False    ;; the song is playing, no feedback
;;  | PAUSED   ;; the song is paused
;;  U Response

;; -> State 
(define (retrieve-result vps)
  (define gui
    (new gui%  ;; observe contractual obligations
         [paused?  #false]
         [cb-play  (λ (s) (unless (false? s)     (send vps play))  #false)]
         [cb-pause (λ (s) (unless (eq? PAUSED s) (send vps pause)) PAUSED)]
         [cb-like  (λ (_)                                          LIKE)]
         [cb-dont  (λ (_)                                          DONT)]))

  (big-bang #f
    [to-draw   (λ (s) (send gui show (eq? PAUSED s)))]
    [on-tick   (λ (s) (if (and (boolean? s) (send vps is-stopped?)) DONE s))]
    [on-mouse  (λ (s x y me) (if (mouse=? me "button-down") ((send gui geometry-manager x y) s) s))]
    [stop-when string?]))

;; (X) (class [cb-play (X -> X)] [cb-pause (X -> X)] [cb-like (X -> X)] [cb-dont (X -> X)]
;; produces 
;; (object [show (Boolean -> Image)] [geometry-manager (N N -> (X -> X))])
;; implement the primitive geometry management for buttons 
(define gui%
  (class object% (init paused? cb-play cb-pause cb-like cb-dont)
    
    ;; sizes and shapes 
    (define WIDTH   100)
    (define 2WIDTH  (* 2 WIDTH))
    (define HEIGHT  50)
    (define 2HEIGHT (* 2 HEIGHT))
  
    (define PLAY (scale .25 (bitmap "play.png")))
    (define PAUS (scale .25 (bitmap "pause.png")))
    (define LIKE (scale .25 (bitmap "like.png")))
    (define DONT (scale .25 (bitmap "dont.png")))
    
    ;; <Image , (X) (X -> X) >
    ;; generate images for buttons and callbacks 
    (define-values (play-button play-clicked) (button 0     2WIDTH 0      HEIGHT  PLAY cb-play))
    (define-values (paus-button paus-clicked) (button 0     2WIDTH 0      HEIGHT  PAUS cb-pause))
    (define-values (like-button like-clicked) (button 0     WIDTH  HEIGHT 2HEIGHT LIKE cb-like))
    (define-values (dont-button dont-clicked) (button WIDTH 2WIDTH HEIGHT 2HEIGHT DONT cb-dont))

    (field [xyz-clicked #false])

    (define/public (show paused?)
      (cond
        [paused?
         (set! xyz-clicked play-clicked)
         (above play-button
                (beside like-button dont-button))]
        [else
         (set! xyz-clicked paus-clicked)
         (above paus-button
                (beside like-button dont-button))]))

    ;; (N N -> (x -> X)
    ;; figure out which of the callbacks may fire, fire it and compute next X 
    (define/public (geometry-manager x y)
      (or (xyz-clicked x y)
          (like-clicked x y)
          (dont-clicked x y)))

    (super-new)
    (show paused?)))

;; (X Y) (N N (String U Image) X -> (values Image (Y N N -> X U False)))
;; generate "buttons" for simulated hierarchical GUI within big-bang 
(define (button w-start w-end h-start h-end label status)
  (define BACK (overlay
                (rectangle (- w-end w-start) (- h-end h-start) 'outline 'black)
                (rectangle (- w-end w-start) (- h-end h-start) 'solid 'gray)))
  (define BUTT (overlay (if (image? label) label (text label 16 'red)) BACK))
  (define (clicked? x y)
    (if (and (< w-start x w-end) (< h-start y h-end))
        status
        #f))
  (values BUTT clicked?))


;; ---------------------------------------------------------------------------------------------------
;; this second part is included only so that we get a single-file package 

(require (except-in video/base color) video/player)

(define (mp3->vps mp3)
  (define file-path (make-temporary-file))
  (with-output-to-file file-path #:exists 'replace (lambda () (write-bytes mp3)))
  (define file (path->string file-path))
  (define my-clip (clip file #:filters (list (mux-filter #:type 'a #:index 0))))
  (define frame   (new frame% [label ""]))
  (define vc      (new video-canvas% [parent frame] [width 640] [height 480]))
  (define vps     (new video-player-server% [video my-clip] #;[canvas vc])) ;; <-- would be nice
  (send vps set-canvas vc)
  (send vps render-video #f)
  vps)

;; ---------------------------------------------------------------------------------------------------
#;
(module+ test
  (require racket/runtime-path)
  (define-runtime-path MP3-long  "../long.mp3")
  (define-runtime-path MP3-short "../short.mp3")
  (play-sound (file->bytes MP3-short))
  (play-sound (file->bytes MP3-long)))

#;
(module+ test
  (define input-string (format "file:\n~s\nplay:\n" (path->string MP3-short)))
  (parameterize ([current-input-port (open-input-string input-string)])
    (main)))

;; ---------------------------------------------------------------------------------------------------

(provide

 ;; the following suggests that transmitted songs are structures 
 (contract-out
  [song?             (-> any/c boolean?)]
  [make-song-bytes   (-> string? bytes? song?)]
  [song-bytes-title  (-> song? string?)]
  [song-bytes-mp3    (-> song? bytes?)]))

(define SEPARATOR  #"|") ;; one byte that is unlikely to occur in song titles 
(define SONG+TITLE (byte-regexp (bytes-append #"(.*?)\\" SEPARATOR #"(.*)")))
(define GOOD-TITLE (byte-regexp (bytes-append #"\\" SEPARATOR)))

(define song? bytes?)

(module+ test (check-equal? (make-song-bytes "a" #"b") #"a|b"))
(module+ test (check-exn exn:fail? (lambda () (make-song-bytes "a|" #"b"))))
(define (make-song-bytes t:str s)
  (define t:byt (string->bytes/utf-8 t:str))
  (unless (good-title? t:byt)
    (error 'title+song "title contains ~e" SEPARATOR))
  (bytes-append t:byt SEPARATOR s))

(module+ test (check-equal? (song-bytes-title (make-song-bytes "a" #"b")) "a"))
(define (song-bytes-title s) (first (song-bytes->both 'song-bytes-title s)))

(module+ test (check-equal? (song-bytes-mp3 (make-song-bytes "a" #"b")) #"b"))
(define (song-bytes-mp3 s) (second (song-bytes->both 'song-bytes-mp3 s)))

;; Bytes -> Boolean
(module+ test
  (check-true  (good-title? #"a"))
  (check-false (good-title? #"b|c")))
(define (good-title? t)
  (not (regexp-match GOOD-TITLE t)))

;; Symbol Bytes -> [List String Bytes]
(module+ test (check-equal? (song-bytes->both 'name (make-song-bytes "a" #"b")) (list "a" #"b")))
(module+ test (check-equal? (song-bytes->both 'name (make-song-bytes "a" #"b|c")) (list "a" #"b|c")))
(define (song-bytes->both name t+s)
  (define m (regexp-match SONG+TITLE t+s))
  (unless m
    (error name "not a song-message"))
  (match-define (list _all title song) m)
  (list (bytes->string/utf-8 title) song))
