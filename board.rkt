#lang racket
; (require "player.rkt")
; TDA Board + TDA Piece

; TDA Piece

; Dom: color (string)
(define (piece color)
	(cons color null)
)

(define (piece-color p)
	(car p)
)

; ################### TDA Piece

; TDA Board
; Dom: void
; Matriz de 7x6 piezas vacías
(define (do-cols n)
	(make-list n null)
)

(define (do-filas n lst)
	(map (lambda (a) (do-cols n)) lst)
)
; Â[x][y]
(define (board)
; USAR LISTAS ENLAZADAS, NO MATRICES!!!
;	(do-filas 6 (do-cols 7))
	(do-cols 7)
)

(define empty-board (board))

; Selectores
(define (board-col br x)
	(list-ref br x)
)

;FIXME: Usar numeración desde 0!!!
(define (board-xy br x y)
	(if	(< (length (board-col br x)) y)
		null
		(list-ref (board-col br x) y)
	)
)

; Modificadores
; Modifica una columna
(define (col-set br x y pieza)
	(list-set (board-col br x) y pieza)
)

; Modifica y ASIGNA una fila para modificar UNA PIEZA
(define (board-set br x y pieza)
	(list-set br x (col-set br x y pieza))
)

(define (col-append br x pieza)
	(append (board-col br x) pieza)
)

; Modifica y ASIGNA una fila para agregar UNA PIEZA
(define (board-append br x pieza)
	(list-set br x (col-append br x pieza))
)

; Funciones
; Pretty-print del tablero
(define (display-board br)
	(display (reverse br))
)

(define (col-llena? col)
	(= 6 (length col))
)

(define (board-can-play br)
	(null? (filter col-llena? br))
)

; Dom: board (board) X column (int) X piece (piece)
; Nota: Si col está llena, mostrar error y no realizar
; jugada.
(define (board-set-play-piece br col pieza)
	(if (col-llena? (board-col br col))
		(car (cons br (display "Error: col llena\n")))
		(board-append br col pieza)
	)
)
; ################## TDA Board
( provide (all-defined-out))
