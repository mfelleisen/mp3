#lang racket/gui

(require 2htdp/image
         2htdp/universe
         racket/runtime-path
         racket/system
         compiler/find-exe)

(define *err (current-error-port))

(define-runtime-path play-server "playserver.rkt")

;; Sets up a subprocess to run the actual audio player.
;; Returns 4 values:
;; set-file : (-> (or/c path? path-string?) void?) -- Sets the mp3 file for the server to play.
;; play-sound : (-> void?) -- Starts playing the mp3 file set with set-file.
;; stop-sound : (-> void?) -- Stops the mp3 file from playig.
;; kill-server : (-> void?) -- Kill the subprocess thus closing the process and all ports.
(define (make-sound-server)
  (match-define (list in out proc-id err status) (process* play-server))
  (define (set-file snd)
    (writeln 'file: out)
    (if (path? snd)
        (writeln (path->string snd) out)
        (writeln snd out))
    (flush-output out))
  (define (play-sound)
    (writeln 'play: out)
    (flush-output out))
  (define (stop-sound)
    (writeln 'stop: out)
    (flush-output out))
  (define (kill-server)
    (write 'stop: out)
    (flush-output out)
    (close-input-port in)
    (close-output-port out)
    (close-input-port err)
    (status 'wait))
  (define proc (list in out proc-id err status))
  (values set-file play-sound stop-sound kill-server proc))

;; ===================================================================================================

(define RATE 10)

(define-runtime-path MP3 "and.mp3")
(define-runtime-path MP3-2 "mash.mp3")

(define-values (set-file play-sound stop-sound kill-server proc)
  (make-sound-server))

(define (server)
  (define msg (file->bytes MP3))
  (universe '()
    [on-tick values 1 RATE]
    [on-msg  (λ (uni from msg) uni)]
    [on-new (λ (uni w) (make-bundle (cons w uni) (list (make-mail w msg)) '()))]))

(define (client)
  (big-bang #false
    [register   LOCALHOST]
    [to-draw    show]
    [on-receive (λ (w msg)
                  (with-output-to-file MP3-2 (λ () (write-bytes msg)) #:exists 'replace)
                  (make-package 1 msg))]
    [on-tick    (λ (w) (and w 2)) 3]))

(define (show w)
    (cond
      [(boolean? w) (text "waiting..." 22 "red")]
      [(= w 1)      (text "received" 22 "black")]
      [else (set-file MP3-2)
            (play-sound)
            (text "playing" 22 "black")]))

(launch-many-worlds (server) (client))
