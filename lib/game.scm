(define-module (lib game)
    #:use-module (srfi srfi-9 gnu))

(define-immutable-record-type
  <game>
  (game cursor)
  game?
  (cursor game-cursor set-game-cursor))

(export game game? game-cursor set-game-cursor)
