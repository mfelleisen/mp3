#! /bin/sh
#|
exec /Users/matthias/plt/racket/bin/racket -tm "$0" ${1+"$@"}
|#
#lang racket/base

(require racket/class
         racket/gui/base
         video/base
         video/player)

(define (main [current-vps #f])
  (define command (read))

  (unless (eof-object? command)
    (case command
      [(file:)
       (define file    (read))
       (define my-clip (clip file #:filters (list (mux-filter #:type 'a #:index 0))))
       (define frame   (new frame% [label ""]))
       (define vc      (new video-canvas% [parent frame] [width 640] [height 480]))
       (define vps     (new video-player-server% [video my-clip] #;[canvas vc])) ;; <-- would be nice

       (send vps set-canvas vc)
       (send vps render-video #f)

       (send frame show #t)
       
       (main vps)]
      [(play:) (send current-vps play) (main current-vps)]
      [(stop:) (send current-vps stop) (main current-vps)]
      [(exit:) (void)]
      [else (log-error (format "playserver: not a valid command: ~a" command))
            (main current-vps)])))

#;
(parameterize ([current-input-port (open-input-string "file:\n\"and.mp3\"\nplay:\n")])
  (main))