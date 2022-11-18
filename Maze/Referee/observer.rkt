#lang racket


(provide
 (contract-out
  [run-observer (-> (listof referee-state?) any)]))

(require racket/gui/dynamic)
(require json)
(require "../Common/state.rkt")
(require (submod "../Common/state.rkt" examples))
(require (submod "../Common/state.rkt" draw))
(require (submod "../Common/state.rkt" serialize))
(require (only-in mrlib/image-core render-image))


;; [Listof RefereeState] -> Void
;; Runs a GUI which renders a game, captured as all intermediate states in that game
(define (run-observer states)
  (define current-state 0)

  (define (draw dc)
    (render-image (referee-state->image (list-ref states current-state) 100) dc 0 0))

  (define frame (new (gui-dynamic-require 'frame%)
                   [label "Example"]
                   [width 900]
                   [height 900]))

  (define canvas (new (gui-dynamic-require 'canvas%) [parent frame]
                      [paint-callback (lambda (canvas dc) (draw dc))]))
  
  ; Make a button in the frame
  (define next-btn (new (gui-dynamic-require 'button%) [parent frame]
                        [label "Next"]
                        [enabled #t]
                        ; Callback procedure for a button click:
                        [callback (lambda (button event)
                                    (begin
                                      (set! current-state (add1 current-state))
                                      (send button enable (< -1 current-state (sub1 (length states))))
                                      (send prev-btn enable (< 0 current-state (length states)))
                                      (send canvas refresh-now)))]))

    ; Make a button in the frame
  (define prev-btn (new (gui-dynamic-require 'button%) [parent frame]
                        [label "Prev"]
                        [enabled #f]
                        ; Callback procedure for a button click:
                        [callback (lambda (button event)
                                    (begin
                                      (set! current-state (sub1 current-state))
                                      (send next-btn enable (< -1 current-state (sub1 (length states))))
                                      (send button enable (< 0 current-state (length states)))
                                      (send canvas refresh-now)))]))

  ; Make a button in the frame
  (new (gui-dynamic-require 'button%) [parent frame]
       [label "Save State"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (let
                       ([file-name ((gui-dynamic-require 'put-file))])
                     (with-output-to-file file-name (λ () (write-json (ref-state->json-referee-state (list-ref states current-state)))))))])
  
  (send frame show #t))
