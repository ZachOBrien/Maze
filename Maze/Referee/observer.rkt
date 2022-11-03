#lang racket


(provide
 (contract-out
  [run-observer (-> (listof referee-state?) any)]))


(require racket/gui)
(require "../Common/state.rkt")
(require (submod "../Common/state.rkt" examples))
(require (submod "../Common/state.rkt" draw))
(require (only-in mrlib/image-core render-image))


(define (run-observer states)
  (define current-state 0)

  (define (draw dc)
    (render-image (referee-state->image (list-ref states current-state) 100) dc 0 0))

  (define frame (new frame%
                   [label "Example"]
                   [width 900]
                   [height 900]))

  (define canvas (new canvas% [parent frame]
                      [paint-callback (lambda (canvas dc) (draw dc))]))

  ; Make a static text message in the frame
  (define msg (new message% [parent frame]
                   [label (string-append "Viewing state " (number->string current-state) "/" (number->string (length states)))]))
  
  ; Make a button in the frame
  (new button% [parent frame]
       [label "Next"]
       [enabled #t]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (begin
                     (set! current-state (add1 current-state))
                     (send button enable (< -1 current-state (sub1 (length states))))
                     (send msg set-label (string-append "Viewing state " (add1 (number->string current-state)) "/" (number->string (length states))))
                     (send canvas refresh-now)))])

  ; Make a button in the frame
  (new button% [parent frame]
       [label "Save State"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (let
                       ([file-name (put-file)])
                     (with-output-to-file file-name (λ () (write "hi there")))))])
  
  (send frame show #t))
