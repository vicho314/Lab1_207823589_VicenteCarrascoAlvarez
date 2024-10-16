#lang racket
; TDA Player aquí

; Dom: id (int) X name (string) X color (string) X wins (int) X losses
;      (int) X draws (int) X remaining-pieces (int)
(define (player id name color wins losses draws rem_pieces)
	(list id name color wins losses draws rem_pieces)
)

; Selectores
(define (player-id pl)
	(list-ref pl 1)
)

(define (player-name pl)
	(list-ref pl 2)
)

(define (player-color pl)
	(list-ref pl 3)
)

(define (player-wins pl)
	(list-ref pl 4)
)

(define (player-losses pl)
	(list-ref pl 5)
)

(define (player-draws pl)
	(list-ref pl 6)
)

(define (player-rem_p pl)
	(list-ref pl 7)
)

; Modificadores
(define (player-id-set pl id)
	(list-set! pl 1 id)
)

(define (player-name-set pl name)
	(list-set! pl 2 name)
)

(define (player-color-set pl color)
	(list-set! pl 3 color)
)

(define (player-wins-set pl wins)
	(list-set pl 4 wins)
)

(define (player-wins-add pl num)
	(player-wins-set pl (+ (player-wins pl) num))
)

(define (player-losses-set pl losses)
	(list-set pl 5 losses)
)

(define (player-losses-add pl num)
	(player-losses-set pl (+ (player-losses pl) num))
)

(define (player-draws-set pl draws)
	(list-set pl 6 draws)
)

(define (player-draws-add pl num)
	(player-draws-set pl (+ (player-draws pl) num))
)

(define (player-rem_p-set pl rem_pieces)
	(list-set pl 7 rem_pieces)
)

(define (player-rem_p-add pl num)
	(player-rem_p-set pl (+ (player-rem_p pl) num))
)


;###### Funciones
(define (new-player id name color)
	(player id name color 0 0 0 21)
)

(define (player-has-pieces? pl)
	(> (player-rem_p pl) 0)
)

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

(define (player_is_red? pl)
	(eqv? (player-color pl) "red")
)

(define (player_is_yellow? pl)
	(eqv? (player-color pl) "yellow")
)

(all-defined-out)
