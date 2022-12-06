#lang racket/base

(require racket/contract)

(provide
 (contract-out
  ; Create an observer
  [observer-new (-> (is-a?/c observer%))]))

(require racket/gui)
(require json)
(require racket/class)
(require (only-in mrlib/image-core render-image))

(require "../Common/state.rkt")
(require (submod "../Common/state.rkt" examples))
(require (submod "../Common/state.rkt" draw))
(require (submod "../Common/state.rkt" serialize))


(define observer-interface (class/c
                            [add-state (->m referee-state? void?)]
                            [run (->m void?)]))

(define (observer-new)
  (new observer%))


(define/contract observer% observer-interface
  (class object%
    (init)

    (super-new)

    (define current-state 0)
    (define states empty)
    (define color:name #f)

    ;; RefereeState -> Void
    ;; Add a state to the list of states
    (define/public (add-state state)
      (set! states (append states (cons state empty))))

    ;; [HashTable AvatarColor : String] -> Void
    ;; Update the player names
    (define/public (update-names color-names)
      (set! color:name color-names))

    ;; Void -> Void
    ;; Run the observer with some states
    (define/public (run)
      (define (draw dc)
        (render-image (referee-state->image (list-ref states current-state) color:name 100) dc 0 0))

      (define frame (new frame%
                         [label "Maze"]
                         [width 900]
                         [height 900]))

      (define canvas (new canvas% [parent frame]
                          [paint-callback (lambda (canvas dc) (draw dc))]))
  
      ; Make a button in the frame
      (define next-btn (new button% [parent frame]
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
      (define prev-btn (new button% [parent frame]
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
      (new button% [parent frame]
           [label "Save State"]
           ; Callback procedure for a button click:
           [callback (lambda (button event)
                       (let
                           ([file-name (put-file)])
                         (with-output-to-file file-name (λ () (write-json (ref-state->json-referee-state (list-ref states current-state)))))))])
  
      (send frame show #t))))
