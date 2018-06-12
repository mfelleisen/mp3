#lang racket/gui

(require 2htdp/image
         2htdp/universe
         (prefix-in vid: video/base)
         (prefix-in vid: video/render)
         video/player)

(provide run-sound)

(define RATE 10)

(define MP3 "and.mp3")
(define MP3-2 "mash.mp3")

;; Runs the sound file. Returns a thunk to stop the sound if its still playing.
;; (-> path-string? (-> void?))
(define (run-sound file)
  (define vps
    (new video-player-server%
         [video (vid:clip file
                          #:filters (list (vid:mux-filter #:type 'a #:index 0)))]))
  (send vps set-canvas
        (new video-canvas% [parent (new frame% [label ""])]
             [width 640] [height 480]))
  (send vps render-video #f)
  (send vps play)
  (define stopped? #f)
  (λ ()
    (when stopped?
      (error 'run-sound "Sound already stopped"))
    (send vps stop)
    (set! stopped? #t)))

(define (client)
  (define (show w)
    (cond
      [(boolean? w) (text "waiting..." 22 "red")]
      [(= w 1) (text "received" 22 "black")]
      [else (run-sound MP3-2)
            (text "played" 22 "black")]))
  (big-bang #f
    [register LOCALHOST]
    [to-draw show]
    [on-receive (λ (w msg)
                  (with-output-to-file MP3-2 (λ () (write-bytes msg)) #:exists 'replace)
                  (make-package 1 msg))]
    [on-tick (λ (w) (and w 2)) 3]
    [stop-when (λ (w) (and w (> w 1))) show]))

(launch-many-worlds (client))
