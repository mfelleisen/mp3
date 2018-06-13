#! /bin/sh
#|
exec /Users/matthias/plt/racket/bin/racket -tm "$0" ${1+"$@"}
|#
#lang racket/gui

;; ---------------------------------------------------------------------------------------------------
;; the retriev-result function is kind of within reach of students after the first week
;; at the pace of the regular schedue; they should do it so they don't think this is black magic 

(provide
 ;; ByteString -> State
 (contract-out
  [play-sound (-> bytes? response?)]))

(require 2htdp/image 2htdp/universe)

;; ---------------------------------------------------------------------------------------------------
(define DONT "I don't like it")
(define LIKE "I like it")
(define DONE "song completed")

;; type State = False U {DONT, LIKE, DONE}

;; Any -> Boolean
;; a response is a State 
(define (response? x)
  (or (false? x) (memq x (list DONT LIKE DONE))))

(define (play-sound mp3)
  (define custodian (make-custodian))
  (parameterize ((current-custodian custodian))
    (match-define (list in out _proc-id _err status) (process* play-server))
    (parameterize ([current-output-port out]
                   [current-input-port  in])
      (file mp3)
      (play)
      (begin0
        (parameterize ((current-eventspace (make-eventspace)))
          (retrieve-result))
        (stop)
        (custodian-shutdown-all custodian)))))

;; -> State 
(define (retrieve-result)
  (define-values (gui callback-for-simulated-gui) (make-gui))
  (big-bang #f
    [to-draw   (λ (s) gui)]
    [on-tick   (λ (s) (if (and (boolean? s) (stopped?)) DONE s))]
    [on-mouse  (λ (s x y me) (if (mouse=? me "button-down") (callback-for-simulated-gui s x y) s))]
    [on-key    (λ (s ke) (if (key=? ke "q") DONT s))]
    [stop-when string?]))

;; -> (values Image [(X) (X N N -> X)])
(define (make-gui)

  ;; sizes and shapes 
  (define WIDTH   100)
  (define 2WIDTH  (* 2 WIDTH))
  (define HEIGHT  50)
  (define 2HEIGHT (* 2 HEIGHT))
  (define PLAY    (rotate -90 (triangle 10 'solid 'green)))
  (define STOP    (square 10 'solid 'red))

  ;; (X) (X -> X)
  ;; compute next X from given X (and contextual knowledge of what triggers this callback) 
  ;; EFFECT may ask the MP3 player to stop or resume play 
  ;; actions 
  (define (do-play s) (unless (eq? 'playing s) (play)) 'playing)
  (define (do-stop s) (unless (eq? 'paused s) (stop))   DONT)
  (define (do-like s) LIKE)
  (define (do-dont s) DONT)

  ;; <Image , (X) (X -> X) >
  ;; generate images for buttons and callbacks 
  (define-values (play-button play-clicked) (button 0 WIDTH 0 HEIGHT PLAY do-play))
  (define-values (stop-button stop-clicked) (button 0 2WIDTH 0 HEIGHT STOP do-stop))
  (define-values (like-button like-clicked) (button 0 WIDTH HEIGHT 2HEIGHT LIKE do-like))
  (define-values (dont-button dont-clicked) (button WIDTH 2WIDTH HEIGHT 2HEIGHT DONT do-dont))
  (define BACK (above stop-button (beside like-button dont-button)))

  ;; (X) (X N N -> X)
  ;; figure out which of the callbacks may fire, fire it and compute next X 
  (define (callback-for-simulated-gui s x y)
    (or ; (play-clicked s x y)
     (stop-clicked s x y)
     (like-clicked s x y)
     (dont-clicked s x y)
     (error 'play-sound "can't happen ~e" (cons x y))))

  (values BACK callback-for-simulated-gui))

;; (X Y) (N N (String U Image) X -> (values Image (Y N N -> X U False)))
;; generate "buttons" for simulated hierarchical GUI within big-bang 
(define (button w-start w-end h-start h-end label status)
  (define BACK (overlay
                (rectangle (- w-end w-start) (- h-end h-start) 'outline 'black)
                (rectangle (- w-end w-start) (- h-end h-start) 'solid 'gray)))
  (define BUTT (overlay (if (image? label) label (text label 16 'red)) BACK))
  (define (clicked? s x y)
    (if (and (< w-start x w-end) (< h-start y h-end))
        (status s)
        #f))
  (values BUTT clicked?))


;; ---------------------------------------------------------------------------------------------------
;; this second part is included only so that we get a single-file package 

(provide main)

(require (except-in video/base color) video/player)
(require racket/runtime-path)

(define-runtime-path play-server (syntax-source #'here))
(define-runtime-path MP3-long  "../long.mp3")
(define-runtime-path MP3-short "../short.mp3")

;; -> Void
;; EFFECT manage an MP3 player as a separate process via STDIN/STDOUT 
;; ASSUME main is run in a separate process 
(define (main [current-vps #f])
  (define command (read))

  (unless (eof-object? command)
    (case command
      [(exit:) (void)]
      [(file:) (main (file-and-clip (read)))]
      [(play:) (send current-vps play) (main current-vps)]
      [(stop:) (send current-vps stop) (main current-vps)]
      [(done:) (writeln (send current-vps is-stopped?))
               (flush-output)
               (main current-vps)]
      [else (log-error (format "playserver: not a valid command: ~a" command))
            (main current-vps)])))

(define (file-and-clip file)
  (define my-clip (clip file #:filters (list (mux-filter #:type 'a #:index 0))))
  (define frame   (new frame% [label ""]))
  (define vc      (new video-canvas% [parent frame] [width 640] [height 480]))
  (define vps     (new video-player-server% [video my-clip] #;[canvas vc])) ;; <-- would be nice

  (send vps set-canvas vc)
  (send vps render-video #f)

  ; (send frame show #t)
  vps)

(define ((transmit token))
  (writeln token)
  (flush-output))

(define (receive _)
  (read))

(define (file mp3)
  (define file-path (make-temporary-file))
  (with-output-to-file file-path #:exists 'replace (lambda () (write-bytes mp3)))
  ((transmit 'file:))
  ((transmit (path->string file-path))))

(define play (transmit 'play:))
(define stop (transmit 'stop:))
(define kill (transmit 'kill:))
(define stopped? (compose receive (transmit 'done:)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (play-sound (file->bytes MP3-short))
  (play-sound (file->bytes MP3-long)))

#;
(module+ test
  (define input-string (format "file:\n~s\nplay:\n" (path->string MP3-short)))
  (parameterize ([current-input-port (open-input-string input-string)])
    (main)))