#lang racket

;;; This module provides data definitions and logic for a player

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(provide
 (contract-out
  ; Run a game of Maze
  [run-game (-> (listof player?) referee-state? boolean? (values (listof avatar-color?) (listof avatar-color?) hash?))]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/sandbox)

(require "../Common/state.rkt")
(require "../Common/player-info.rkt")
(require "../Common/rulebook.rkt")
(require "../Common/math.rkt")
(require "../Players/player.rkt")
(require "../Remote/safety.rkt")
(require "observer.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(define DEFAULT-BOARD-SIZE 7)  ; Default number of tiles in a row and in a column
(define MAX-ROUNDS 1000)  ; Maximum number of rounds the game may be played for

(require json)

;; [Listof Player] RefereeState Boolean -> [Listof AvatarColor] [Listof AvatarColor]
;; Runs a game of Labrynth, finding winners and cheaters
(define (run-game init-players state0 observer?)
  (begin
    (define players (make-hash (for/list ([p init-players]
                                          [c (get-player-color-list state0)])
                                 (cons c p))))
    (define-values (state-after-getting-names color-names) (get-color-names players state0))
    (writeln (hash->list color-names))
    (define state-after-setup (setup-all-players players state-after-getting-names))
    (define intermediate-states (play-until-completion state-after-setup players MAX-ROUNDS))
    (define game-over-state (first intermediate-states))
    (define winners (determine-winners game-over-state))
    (define final-state (notify-winners-and-losers winners game-over-state players))
    (define winners-that-didnt-get-kicked
      (filter (λ (plyr) (member plyr (get-player-color-list final-state))) winners))
    (define criminals (filter (λ (plyr) (not (member plyr (get-player-color-list final-state))))
                              (get-player-color-list state0)))
    (if observer? (run-observer (reverse intermediate-states)) #f)
    (values winners-that-didnt-get-kicked criminals color-names)))


;; RefereeState HashTable Natural -> [Listof RefereeState]
;; Plays at most `rounds-remaining` rounds of Maze, and returns the
;; gamestate when the game has ended. Accumulates the states after each player move
;; in reverse order
(define (play-until-completion state players rounds-remaining)
  (play-until-completion-help (list state) players rounds-remaining))
         
;; [NonEmptyListof RefereeState] HashTable Natural -> [Listof RefereeState]
;; Plays at most `rounds-remaining` rounds of Maze, and returns a list of
;; all intermediate gamestates once the game is over
(define (play-until-completion-help prev-states players rounds-remaining)
  (cond
    [(<= rounds-remaining 0) prev-states]
    [else (let*-values ([(curr-state) (first prev-states)]
                        [(player-colors) (get-player-color-list curr-state)]
                        [(states-after-round plyrs-passed-turn) (run-round curr-state players player-colors)]
                        [(new-states) (append states-after-round prev-states)]
                        [(new-player-colors) (get-player-color-list (first new-states))]
                        [(all-players-passed) (equal? new-player-colors plyrs-passed-turn)])
            (cond
              [(or (game-over? (second new-states) (first new-states)) all-players-passed) new-states]
              [else (play-until-completion-help new-states players (sub1 rounds-remaining))]))]))


;; RefereeState HashTable [Listof AvatarColor] [Listof AvatarColor] [Listof RefereeState] -> (values [Listof RefereeState] [Listof AvatarColor])
;; Run a round of the game, end the round early if the game is over. Accumulates states after
;; each move in reverse order.
(define (run-round state players player-colors [passed-plyrs '()] [intermediate-states '()])
  (cond [(empty? player-colors) (values intermediate-states passed-plyrs)]
        [else (let*-values ([(passed-turn next-state)
                            (execute-turn state
                                          (hash-ref players (first player-colors))
                                          (first player-colors))]
                           [(new-passed-plyrs) (if passed-turn (cons (first player-colors) passed-plyrs) passed-plyrs)])
                (cond
                  [(game-over? state next-state) (values (cons next-state intermediate-states) passed-plyrs)]
                  [else (run-round next-state
                                   players
                                   (rest player-colors)
                                   new-passed-plyrs
                                   (cons next-state intermediate-states))]))]))


;; RefereeState [Hash Color:Player] AvatarColor -> Boolean RefereeState
;; Execute a turn for the player. The boolean flag is true if they chose to pass turn
(define (execute-turn state player color)
  (writeln (string-append color " taking their turn"))
  (define mv (safe-get-action player (referee-state->player-state state color)))
  (cond
    [(false? mv) (values #t (end-current-turn state))]
    [(or (equal? 'misbehaved mv) (not (valid-move? state mv))) (values #f (remove-player state))]
    [else (begin (define gamestate-after-move (gamestate-execute-move state mv))
                 (if (player-on-treasure? gamestate-after-move)
                     (values #f (send-setup-to-player gamestate-after-move player color))
                     #f)
                (values #f (end-current-turn gamestate-after-move)))]))

;; RefereeState -> [Listof AvatarColor]
;; Determine which players (if any) won the game
(define (determine-winners state)
  (define players-that-visited-treasure
    (filter (λ (plyr-info) (player-info-visited-treasure? plyr-info))
            (gamestate-players state)))
  (map player-info-color (if (empty? players-that-visited-treasure)
                             (all-min-distance (gamestate-players state))
                             (all-min-distance players-that-visited-treasure))))


;; [Listof Player] -> [Listof Player]
;; Get all players which are minimum distance from their objective
(define (all-min-distance players)
  (cond
    [(empty? players) empty]
    [(let* ([distances (map (curryr distance-from-objective euclidean-dist) players)]
            [min-dist (apply min distances)])
       (filter (λ (plyr) (= (distance-from-objective plyr euclidean-dist) min-dist)) players))]))


;; ===== SAFELY GETTING NAMES =====

;; [HashTable AvatarColor : Player] RefereeState -> (values RefereeState [HashTable AvatarColor : String])
;; Asks each player for their name to construct a hash table mapping colors to player names
(define (get-color-names players start-state)
  (define-values (final-state final-color-names)
    (for/fold ([state start-state]
               [color-names '()])
              ([color (hash-keys players)])
      (let-values ([(new-state name) (send-get-name-to-player state (hash-ref players color) color)])
        (values new-state (cons (cons color name) color-names)))))
  (values final-state (make-hash final-color-names)))

;; RefereeState Player AvatarColor -> Gamestate String
;; Get a player name
(define (send-get-name-to-player state plyr color)
  (define result (execute-safe (thunk (send plyr name))))
  (match result
    ['misbehaved (values (remove-player-by-color state color) "")]
    [_ (values state result)]))
  
;; ==================================


;; ===== SAFELEY SENDING SETUP =====

;; [HashTable AvatarColor : Player] Gamestate -> Gamestate
;; Update each player with the initial board and their treasure position. The gamestate returned
;; is the same as the original gamestate, but with any misbehaving players kicked
(define (setup-all-players players state0)
  (for/fold ([state state0])
            ([color (hash-keys players)])
    (send-setup-to-player state (hash-ref players color) color)))


;; Gamestate Player AvatarColor -> Gamestate
;; Sends a gamestate to the player, and returns the same gamestate either with that player
;; or, if they don't behave properly, without the player
(define (send-setup-to-player state plyr color)
  (writeln (string-append color " with setup"))
  (match (execute-safe (thunk (send plyr setup
                                    (referee-state->player-state state color)
                                    (get-goto-pos (gamestate-get-by-color state color)))))
    ['misbehaved (writeln (string-append color " misbehaved on setup")) (remove-player-by-color state color)]
    [_ state]))

;; ==================================

;; ===== SAFELY NOTIFYING WINNERS AND LOSERS =====

;; [Listof AvatarColor] RefereeState [Hash AvatarColor : Player] -> RefereeState
;; Notify players that they either won or lost
(define (notify-winners-and-losers winners final-state players)
  (define losers (filter (λ (color) (not (member color winners))) (get-player-color-list final-state)))
  (define state-after-notifying-winners (notify-outcome #t winners players final-state))
  (define state-after-notifying-losers (notify-outcome #f losers players state-after-notifying-winners))
  state-after-notifying-losers)

;; Boolean [Listof AvatarColor] [Hash Color:Player] RefereeState -> RefereeState
;; Notify a set of players whether they have won or lost the game
(define (notify-outcome win? colors players final-state)
  (for/fold ([state final-state])
            ([color colors])
    (safe-send-outcome state (hash-ref players color) color win?)))


;; Gamestate Player AvatarColor Boolean -> Gamestate
;; Notifies a player whether they won or lost, and returns the same gamestate either with that player
;; or, if they don't behave properly, without the player
(define (safe-send-outcome state plyr color win?)
  (match (execute-safe (thunk (send plyr win win?)))
    ['misbehaved (remove-player-by-color state color)]
    [_ state]))

;; ==================================


;; Player -> (U Action 'misbehaved)
;; Get a player's action. The Player may misbehave, and the following behaviors are handled:
;;    1. The call to the Player's take-turn method raises an exception
;;    2. The call to the Player's take-turn method exceeds a time limit
;; If the player misbehaves in any of these ways, 'misbehaved is returned.
(define (safe-get-action plyr plyr-state [time-limit-sec 4])
  (execute-safe (thunk (send plyr take-turn plyr-state)) time-limit-sec))


;; [Listof Any] [Listof Any] -> Boolean
;; Returns true if no elements from the first list are present in the second list,
;; and vice versa
(define (disjoint? l1 l2)
  (set-empty? (set-intersect (list->set l1) (list->set l2))))

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out)))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "../Common/board.rkt" examples))
  (require (submod "../Common/tile.rkt" examples))
  (require (submod "../Common/player-info.rkt" examples))
  (require (submod "../Players/player.rkt" examples))
  (require (submod "../Common/state.rkt" examples)))


(module+ examples
  (require (submod "../Common/state.rkt" examples))
  (require (submod "../Players/player.rkt" examples))
  (require (submod "../Common/player-info.rkt" examples)))

;; ==========================================
;; TOP LEVEL TESTS - FOR RUNNING ENTIRE GAMES

(module+ test
  (test-case
   "Run a game of Maze"
   (let-values
       ([(winners criminals color-names)
         (run-game (list player0 player1 player2) gamestate5 #f)])
     (check-equal? empty criminals)
     (check-equal? (list "red") winners)))
  (test-case
   "Run a game of Maze gs4"
   (let-values
       ([(winners criminals color-names)
         (run-game (list player0 player1 player2 player3) gamestate4 #f)])
     (check-equal? empty criminals)
     (check-equal? (list "green") winners)))
  (test-case
   "Run a game of Maze gs5"
   (let-values
       ([(winners criminals color-names)
         (run-game (list player0 player1) gamestate1 #f)])
     (check-equal? empty criminals)
     (check-equal? (list "yellow") winners))))


;; ==========================================
;; Tests for helpers

(module+ test
  (check-equal? (determine-winners gamestate5) '("red"))
  (check-equal? (determine-winners gamestate4) '("purple"))
  (check-equal? (determine-winners gamestate1) '("black")))


;; ==========================================
;; Tests for properly handling bad players

;; Test getting players' names
(module+ test
  (test-case
   "A well-behaved player gives their name"
   (let-values
       ([(state-after-getting-name name)
         (send-get-name-to-player gamestate0 player0 "blue")])
     (check-equal? state-after-getting-name gamestate0)
     (check-equal? name "bob")))
  (test-case
   "A misbehaving player fails to give their name"
      (let-values
       ([(state-after-getting-name name)
         (send-get-name-to-player gamestate0 player-bad-name "blue")])
     (check-equal? state-after-getting-name (remove-player gamestate0))
     (check-equal? name ""))))


;; Test sending players the initial state (calling `setup`)
(module+ test
  (test-case
   "A well-behaved player handles setup"
   (let ([state-after-setup (send-setup-to-player gamestate0 player0 "blue")])
     (check-equal? state-after-setup gamestate0)))
  (test-case
   "A misbehaved player fails to handle setup"
   (let ([state-after-setup (send-setup-to-player gamestate0 player-bad-setup "blue")])
     (check-equal? state-after-setup (remove-player gamestate0)))))

;; Test having players take their turn
(module+ test
  (test-case
   "A well-behaved player chooses an action"
   (let-values
       ([(passed-turn? state-after-turn)
         (execute-turn gamestate0 player0 "blue")])
     (check-equal? passed-turn? #f)))
  (test-case
   "A misbehaved player chooses an action"
   (let-values
       ([(passed-turn? state-after-turn)
         (execute-turn gamestate0 player-bad-taketurn "blue")])
     (check-equal? passed-turn? #f)
     (check-equal? state-after-turn (remove-player gamestate0)))))

;; Test informing players they won
(module+ test
  (test-case
   "A well-behaved player handles learning they won"
   (let ([state-after-inform-outcome (safe-send-outcome gamestate0 player0 "blue" #t)])
     (check-equal? state-after-inform-outcome gamestate0)))
  (test-case
   "A misbehaved player fails to handle setup"
   (let ([state-after-inform-outcome (safe-send-outcome gamestate0 player-bad-win "blue" #t)])
     (check-equal? state-after-inform-outcome (remove-player gamestate0)))))