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

(define (player-losses-set pl losses)
	(list-set! pl 5 losses)
)

(define (player-draws-set pl draws)
	(list-set! pl 6 draws)
)

(define (player-rem_p-set pl rem_pieces)
	(list-set! pl 7 rem_pieces)
)

