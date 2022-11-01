#lang racket

(require racket/gui)
(require "image-utils.rkt")
(require (only-in mrlib/image-core render-image))

(define frame (new frame%
                   [label "Example"]
                   [width 400]
                   [height 400]))

(define MSG "STARTING")
(define circle-radius 5)

(define (draw dc)
  (send dc set-scale 3 3)
  (send dc set-text-foreground "blue")
  (send dc draw-text MSG 0 0)
  (render-image (circ circle-radius) dc 50 50))


(define canvas (new canvas% [parent frame]
                    [paint-callback (lambda (canvas dc) (draw dc))]))


; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (begin
                           (set! MSG "button 1 was clicked")
                           (send canvas refresh-now)))])

; Make a button in the frame
(new button% [parent frame]
             [label "Make circle bigger"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (begin
                           (set! circle-radius 10)
                           (send canvas refresh-now)))])

; Make a button in the frame
(new button% [parent frame]
             [label "Save File"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (let
                             ([file-name (put-file)])
                           (with-output-to-file file-name (λ () (write MSG)))))])

                              

(send frame show #t)
