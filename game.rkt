#lang racket
(require "player.rkt")
(require "board.rkt")
(require "utils.rkt")
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

(define (game-get-board gm)
	(car (cons (game-board gm) 
			(display-board (game-board gm))
		)
	)
)

(define (game-current-turn gm)
	(list-ref 3 gm)
)


(define (game-history gm)
	(list-ref 4 gm)
)

;FIXME: cturn = 0
(define (game-get-current-player gm)
	(if (eqv? (game-current-turn gm) 0)
		(game-p1 gm)
		(game-p2 gm)
	)
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

;Special (MALAS IDEAS)
;Busca quién es red
;salidas: p1, sino p2
;Nota:FIXME: MALA IDEA
(define (game-get-red gm)
	(if (player-is-red? (game-p1 gm))
		(game-p1 gm)
		(game-p2 gm)
	)
)

(define (game-get-yellow gm)
	(if (player-is-yellow? (game-p1 gm))
		(game-p1 gm)
		(game-p2 gm)
	)
)


(define (game-who-is-red gm)
	(if (player-is-red? (game-p1 gm))
		1
		2
	)
)

(define (game-who-is-yellow gm)
	(if (player-is-yellow? (game-p1 gm))
		1
		2
	)
)

(define (game-pl_red-set gm pl_red)
	(if (eqv? (game-who-is-red gm) 1)
		(game-p1-set gm pl_red)
		(game-p2-set gm pl_red)
	)
)

(define (game-pl_yellow-set gm pl_yellow)
	(if (eqv? (game-who-is-yellow gm) 1)
		(game-p1-set gm pl_yellow)
		(game-p2-set gm pl_yellow)
	)
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

(define (game-red-win gm pl_red pl_yellow)
	(game-pl_yellow-set (game-pl_red-set gm (player-update-stats pl_red "win")
		(player-update-stats pl_yellow "loss")
	)
)

(define (game-yellow-win gm pl_red pl_yellow)
	(game-pl_yellow-set (game-pl_red-set gm (player-update-stats pl_red "loss")
		(player-update-stats pl_red "win")
	)
)

(define (game-draw-both gm pl_red pl_yellow)
	(game-pl_yellow-set (game-pl_red-set gm (player-update-stats pl_red "draw")
		(player-update-stats pl_yellow "draw")
	)
)

(define (game-stats-wrapper_wins gm win_num pl_red pl_yellow)
	(cond [(eqv? win_num 1)
		(game-red-win gm pl_red pl_yellow)]
		[(eqv? win_num 2)
		(game-yellow-win gm pl_red pl_yellow]
		[(eqv? win_num 0)
		(game-draw-both gm pl_red pl_yellow)]
	)
)

(define (game-stats-wrapper gm)
	(game-stats_wrapper_wins gm 
		(board-who-is-winner (game-board gm))
		(game-get-red gm)
		(game-get-yellow gm)
	)
)

; cturn=-1
(define (game-set-end gm)
	(game-stats-wrapper (game-turn-set gm -1))
)

(define (game-set-move gm col)
	(game-board-set 
		(board-set-play-piece (game-board gm) col 
				(piece 
					(player-color 
						(game-get-current-player gm)
					)
				)
		)
	)
)

(define (game-set-move-history gm col)
	(game-set-move
		(game-add-history gm col 
			(player-color 
				(game-get-current-player gm)
			)
		) 
		col
	)
)

(define (game-flip-turn gm)
	(game-turn-set gm 
		(remainder (+ (game-current-turn gm) 1) 2)
	)
)

(define (game-player-take-piece gm)
	(if (eqv? (game-current-turn gm) 0)
		(game-p1-set (player-rem_p-add (game-get-current-player gm) -1)
		)
		(game-p2-set (player-rem_p-add (game-get-current-player gm) -1)
		)
	)
)

(define (game-is-win? gm)
	(board-who-is-winner (game-board gm))
)

; ver si hay draw o win
(define (game-ended gm)
	(if	(or (game-is-draw? gm) 
			(board-is-win? (game-get-board gm))
		)
		(game-set-end gm)
		gm
	)
)

(define (game-do-action gm col))
(define (game-player-set-move gm pl col)
	(if (eqv? (game-get-current-player gm) pl)
		(game-ended (game-flip-turn 
			(game-player-take-piece (game-set-move-history gm col))
			)
		)
		(display "Error:Jugador incorrecto!\n")
	)
)

(all-defined-out)
