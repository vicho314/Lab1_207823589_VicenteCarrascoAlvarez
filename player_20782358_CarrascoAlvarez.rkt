#lang racket
; TDA Player aquí

; Constructor de TDA player
; Dom: id (int) X name (string) X color (string) X wins (int) X losses
;      (int) X draws (int) X remaining-pieces (int)
; Rec: (player)
(define (player id name color wins losses draws rem_pieces)
	(list id name color wins losses draws rem_pieces)
)

; Selectores

; Retorna el campo "id" de tda player.
; Dom: pl (player)
; Rec: id (int)
(define (player-id pl)
	(list-ref pl 0)
)

; Retorna el campo "name" de tda player.
; Dom: pl (player)
; Rec: name (string)
(define (player-name pl)
	(list-ref pl 1)
)

; Retorna el campo "color" de tda player.
; Dom: pl (player)
; Rec: color (string)
(define (player-color pl)
	(list-ref pl 2)
)

; Retorna el campo "wins" de tda player.
; Dom: pl (player)
; Rec: wins (int)
(define (player-wins pl)
	(list-ref pl 3)
)

; Retorna el campo "losses" de tda player.
; Dom: pl (player)
; Rec: losses (int)
(define (player-losses pl)
	(list-ref pl 4)
)

; Retorna el campo "draws" de tda player.
; Dom: pl (player)
; Rec: draws (int)
(define (player-draws pl)
	(list-ref pl 5)
)

; Retorna el campo "remaining_pieces" de tda player.
; Dom: pl (player)
; Rec: remaining_pieces (int)
(define (player-rem_p pl)
	(list-ref pl 6)
)

; Modificadores

; Modifica el campo "id" de tda player con un valor nuevo.
; Dom: pl (player) x id (int)
; Rec: (player)
(define (player-id-set pl id)
	(list-set pl 0 id)
)

; Modifica el campo "name" de tda player con un valor nuevo.
; Dom: pl (player) x name (string)
; Rec: (player)
(define (player-name-set pl name)
	(list-set pl 1 name)
)

; Modifica el campo "color" de tda player con un valor nuevo.
; Dom: pl (player) x color (string)
; Rec: (player)
(define (player-color-set pl color)
	(list-set pl 2 color)
)

; Modifica el campo "wins" de tda player con un valor nuevo.
; Dom: pl (player) x wins (int)
; Rec: (player)
(define (player-wins-set pl wins)
	(list-set pl 3 wins)
)

; Modifica el campo "wins" de tda player, agregando "num".
; Dom: pl (player) x num (int)
; Rec: (player)
(define (player-wins-add pl num)
	(player-wins-set pl (+ (player-wins pl) num))
)

; Modifica el campo "losses" de tda player con un valor nuevo.
; Dom: pl (player) x losses (int)
; Rec: (player)
(define (player-losses-set pl losses)
	(list-set pl 4 losses)
)


; Modifica el campo "losses" de tda player, agregando "num".
; Dom: pl (player) x num (int)
; Rec: (player)
(define (player-losses-add pl num)
	(player-losses-set pl (+ (player-losses pl) num))
)


; Modifica el campo "draws" de tda player con un valor nuevo.
; Dom: pl (player) x draws (int)
; Rec: (player)
(define (player-draws-set pl draws)
	(list-set pl 5 draws)
)

; Modifica el campo "draws" de tda player, agregando "num".
; Dom: pl (player) x num (int)
; Rec: (player)
(define (player-draws-add pl num)
	(player-draws-set pl (+ (player-draws pl) num))
)


; Modifica el campo "remaining_pieces" de tda player con un valor nuevo.
; Dom: pl (player) x rem_pieces (int)
; Rec: (player)
(define (player-rem_p-set pl rem_pieces)
	(list-set pl 6 rem_pieces)
)


; Modifica el campo "remaining_pieces" de tda player, agregando "num".
; Dom: pl (player) x num (int)
; Rec: (player)
(define (player-rem_p-add pl num)
	(player-rem_p-set pl (+ (player-rem_p pl) num))
)


;###### Funciones

; Crea un player con valores por defecto 
; (excepto id, name, y color)
; Dom: id (int) x name (string) x color (string)
; Rec: (player)
(define (new-player id name color)
	(player id name color 0 0 0 21)
)

; Retorna #t si el jugador tiene piezas, #f en caso contrario
; Dom: pl (player)
; Rec: (bool)
(define (player-has-pieces? pl)
	(> (player-rem_p pl) 0)
)

; Actualiza las estadísticas del jugador, dependiendo del resultado
; Dom: pl (player) x result (string)
; Rec: (player)
(define (player-update-stats pl result)
	(cond [(eqv? result "win") 
		(player-wins-add pl 1)]
		[(eqv? result "loss") 
		(player-losses-add pl 1)]
		[(eqv? result "draw") 
		(player-draws-add pl 1)]
	)
)

;(define (player-update-wrapper pl )
;)

; Retorna #t en caso de que el color del player sea "red".
; Dom: pl (player)
; Rec: (bool)
(define (player-is-red? pl)
	(eqv? (player-color pl) "red")
)

; Retorna #t en caso de que el color del player sea "yellow".
; Dom: pl (player)
; Rec: (bool)
(define (player-is-yellow? pl)
	(eqv? (player-color pl) "yellow")
)

(provide (all-defined-out))
