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
(define (game-p1-set gm pl)
	(list-set gm 0 pl)
)


(define (game-p2-set gm pl)
	(list-set gm 1 pl)
)


(define (game-board-set gm br)
	(list-set gm 2 br)
)


(define (game-turn-set gm turn)
	(list-set gm 3 turn)
)

(define (game-history-set gm st)
	(list-set gm 4 st)
)
; Funciones
(define (game-display-history gm)
	(display (game-history))
)

(define (game-add-history gm col color)
	(game-history-set gm 
		(stack-push (game-history gm) (cons col color))
	)
)

;FIXME: revisar casos borde o condiciones?
(define (game-is-draw? gm)
	(and (board-lleno? (game-board gm))
		(or (player-has-pieces? (game-p1 gm))
			(player-has-pieces? (game-p2 gm))
		)
	)
)


(all-defined-out)
