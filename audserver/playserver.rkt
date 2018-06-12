#lang racket/base

(require racket/class
         racket/gui/base
         video/base
         video/player)

(define current-vps (make-parameter #f))

(let loop ()
  (define command (read))
  (unless (eof-object? command)
    (case command
      [(file:)
       (define file (read))
       (define vps
         (new video-player-server%
              [video (clip file
                           #:filters (list (mux-filter #:type 'a #:index 0)))]))
       (send vps set-canvas
             (new video-canvas% [parent (new frame% [label ""])]
                  [width 640] [height 480]))
       (send vps render-video #f)
       (current-vps vps)]
      [(play:) (send (current-vps) play)]
      [(stop:) (send (current-vps) stop)]
      [(exit:) (void)]
      [else (error 'playserver "Not a valid command: ~a" command)])
    (unless (equal? command 'exit:)
      (loop))))
