#lang racket

(require 2htdp/image)

;; A Game has:
;; - Board
;; - [List-of Tile]
;; - [List-of Player]
(struct game (board tile-pool players))

;; A Board is a:
;; - [List-of [List-of Cell]]

;; A Tile is a:
;; - Regexp in [a-z.]
;; - Number
(struct tile (char points))

;; A Cell is a:
;; - Integer
;; - Integer
;; - [Maybe Tile]
;; - [Tile -> Number]
;; - [Number -> Number]
;; - Boolean
(struct cell (row col tile been-scored? letter-score word-score))

;; A Player is a:
;; - String
;; - Number
;; - [List-of Tile]
(struct player (id score hand))


;; ============================================================================
;; Building the board
;; ============================================================================

(define (board-ref board row col)
  (list-ref (list-ref board row) col))

(define (board-update board row col updater)
  (list-update board row (λ(row) (list-update row col updater))))

(define (build-empty-board rows cols)
  (build-list rows
              (λ(row) (build-list cols
                                (λ(col) (make-cell row col))))))

(define TWS '((0 0)  (0 7)  (0 14)
              (7 0)         (7 14)
              (14 0) (14 7) (14 14)))

(define DWS '((1 1) (1 13)
              (2 2) (2 12)
              (3 3) (3 11)
              (4 4) (4 10)
                 (7 7)
              (10 4) (10 10)
              (11 3) (11 11)
              (12 2) (12 12)
              (13 1) (13 13)))

(define TLS '((1 5) (1 9)
        (5 1) (5 5) (5 9) (5 13)
        (9 1) (9 5) (9 9) (9 13)
              (13 5) (13 9)))

(define DLS '((0 3) (0 11)
              (2 6) (2 8)
              (3 0) (3 7) (3 14)
              (6 2) (6 6) (6 8) (6 12)
                    (7 3) (7 11)
              (8 2) (8 6) (8 8) (8 12)
              (11 0) (11 7) (11 14)
              (12 6) (12 8)
              (14 3) (14 11)))

;; modifiers:
(define (letter-score multiplier)
  (λ(t) (* multiplier (tile-points t))))

(define DOUBLE_LETTER_SCORE (letter-score 2))
(define TRIPLE_LETTER_SCORE (letter-score 3))

(define (word-score multiplier)
  (λ(points) (* points multiplier)))

(define DOUBLE_WORD_SCORE (word-score 2))
(define TRIPLE_WORD_SCORE (word-score 3))

(define (make-cell row col)
  (let ([pos (list row col)])
    (cell row col
          #f #f
          (cond [(member pos TLS) TRIPLE_LETTER_SCORE]
                [(member pos DLS) DOUBLE_LETTER_SCORE]
                [else values])
          (cond [(member pos TWS) TRIPLE_WORD_SCORE]
                [(member pos DWS) DOUBLE_WORD_SCORE]
                [else values]))))

(define STARTING_BOARD (build-empty-board 15 15))


(define (draw-cell c)
  (let ([pos (list (cell-row c) (cell-col c))])
    (frame (rectangle 20 20 "solid"
                      (cond [(member pos TLS) "blue"]
                            [(member pos DLS) "cyan"]
                            [(member pos TWS) "orange"]
                            [(member pos DWS) "dark pink"]
                            [else "light gray"])))))

(define (draw-board b)
  (define (draw-row row)
    (foldr (λ(cel acc) (beside (draw-cell cel) acc)) empty-image row))
  
  (foldr (λ(row acc) (above acc (draw-row row))) empty-image b))


;; ============================================================================
;; Playing the game
;; ============================================================================











