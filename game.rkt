#lang racket
; TDA Game
; Dom: player1 (player) X player2 (player) X board (board) X
; current-turn (int)
; Rec: game
; Constructor
; Nota: el último valor es el historial (stack)
(define (game p1 p2 brd c-turn)
	(list p1 p2 brd c-turn empty-stack)
)

; Selectores
(define (game-p1 gm)
	(list-ref 0 gm)
)


(define (game-p2 gm)
	(list-ref 1 gm)
)


(define (game-board gm)
	(list-ref 2 gm)
)


(define (game-current-turn gm)
	(list-ref 3 gm)
)


(define (game-history gm)
	(list-ref 4 gm)
)
; Modificadores

; Funciones

(all-defined-out)
