#lang racket/base

;;; This module implements a remote proxy player


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [proxy-player? contract?]
  ; Create a new proxy player
  [proxy-player-new (-> string? tcp-conn? proxy-player?)]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require json)
(require racket/list)
(require racket/class)
(require racket/bool)

(require "tcp-conn.rkt")
(require (only-in "../Players/player.rkt" player-interface))
(require (submod "../Common/state.rkt" serialize))
(require (submod "../Common/board.rkt" serialize))
(require (submod "../Players/strategy.rkt" serialize))


;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION


;; String TcpConn -> ProxyPlayer
;; Create a new proxy player
(define (proxy-player-new name tcp-conn)
  (new proxy-player% [init-name name] [init-tcp-conn tcp-conn]))


(define/contract proxy-player% player-interface
  (class object%
    (init init-name init-tcp-conn)

    (define plyr-name init-name)
    (define tcp-conn init-tcp-conn)

    (super-new)
    ;; TODO: write functions that build rpc messages and have contracts on those messages

    ;; -> String
    ;; Get the player's name
    (define/public (name) plyr-name)

    ;; Natural Natural -> Board
    ;; Get a starting board
    (define/public (propose-board min-num-rows min-num-cols)
      (send tcp-conn send-json (list "propose-board" (list min-num-rows min-num-cols)))
      (json-board->board (send tcp-conn receive-json)))
  
    ;; (U #f PlayerState) GridPosn -> Any
    ;; Sets initial state and treasure position
    (define/public (setup plyr-state new-goal)
      (send tcp-conn send-json (list "setup" (list (if plyr-state (player-state->json-public-state plyr-state) #f)
                                                   (gridposn->json-coordinate new-goal))))
      (define response (send tcp-conn receive-json))
      (or (equal? response "void") (error "invalid response to setup")))

    ;; PlayerState -> Action
    ;; Chooses either to make a move or pass
    (define/public (take-turn plyr-state)
      (send tcp-conn send-json (list "take-turn" (list (player-state->json-public-state plyr-state))))
      (json-choice->action (send tcp-conn receive-json)))
      

    ;; Boolean -> Any
    ;; Informs the player whether they won or lost
    (define/public (win status)
      (send tcp-conn send-json (list "win" (list status)))
      (define response (send tcp-conn receive-json))
      (or (equal? response "void") (error "invalid response to win")))

    ;; -> GridPosn
    (define/public (get-goal)
      (send tcp-conn send-json (list "get-goal" empty))
      (define response (send tcp-conn receive-json))
      (if (false? response)
          response
          (json-coordinate->gridposn response)))))

(define proxy-player? (is-a?/c proxy-player%))


;; --------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit)
  (require (submod "../Common/state.rkt" examples)))

;; test win
(module+ test
  (test-case
   "Test sending win"
   (define output (open-output-string))
   (define input (open-input-string "\"void\""))  ; Fake a return value
   (define conn (tcp-conn-new input output))
   (define proxy-player (proxy-player-new "borat" conn))
   (check-equal? (get-output-string output) "")
   (send proxy-player win #t)
   (check-equal? (get-output-string output) "[\"win\",[true]]")))


;; test setup
(module+ test
  (test-case
   "Test sending setup"
   (define output (open-output-string))
   (define input (open-input-string "\"void\""))
   (define conn (tcp-conn-new input output))
   (define proxy-player (proxy-player-new "borat" conn))
   (check-equal? (get-output-string output) "")
   (send proxy-player setup player-state0 (cons 1 1))
   (define json-repr (string->jsexpr (get-output-string output)))
   (check-true (= 2 (length json-repr)))
   (check-equal? (first json-repr) "setup")
   (check-equal? 2 (length (second json-repr)))))


;; test takeTurn
(module+ test
  (test-case
   "Test sending takeTurn"
   (define output (open-output-string))
   (define input (open-input-string "[2, \"LEFT\", 90, {\"row#\": 1, \"column#\": 1}]"))
   (define conn (tcp-conn-new input output))
   (define proxy-player (proxy-player-new "borat" conn))
   (check-equal? (get-output-string output) "")
   (send proxy-player take-turn player-state0)
   (define json-repr (string->jsexpr (get-output-string output)))
   (check-true (= 2 (length json-repr)))
   (check-equal? (first json-repr) "take-turn")
   (check-equal? 1 (length (second json-repr)))))
