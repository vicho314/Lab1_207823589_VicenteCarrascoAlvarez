#lang racket
; (require "player.rkt")
(require "utils.rkt")
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
; A[x][y]
(define (board)
; USAR LISTAS ENLAZADAS, NO MATRICES!!!
	(do-filas 6 (do-cols 7))
;	(do-cols 7)
)

(define empty-board (board))

; Selectores
(define (get-board-col br x)
	(list-ref br x)
)

(define (get-board-fila br y)
	(foldr (lambda (l1 l2)(cons (list-ref l1 y) l2)) '() br)
)

;FIXME: Usar numeración desde 0!!!
(define (board-xy br x y)
	(if	(< (length (get-board-col br x)) y)
		null
		(list-ref (get-board-col br x) y)
	)
)

; Modificadores
; Modifica una columna
(define (col-set br x y pieza)
	(list-set (get-board-col br x) y pieza)
)

; Modifica y ASIGNA una fila para modificar UNA PIEZA
(define (board-set br x y pieza)
	(list-set br x (col-set br x y pieza))
)

(define (col-append br x pieza)
	(append-null (get-board-col br x) pieza)
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
	(= 6	(length (filter (lambda (a) (not (null? a))) col) 
		)
	)
)

(define (board-can-play? br)
	(null? (filter col-llena? br))
)

; Dom: board (board) X column (int) X piece (piece)
; Nota: Si col está llena, mostrar error y no realizar
; jugada.
(define (board-set-play-piece br col pieza)
	(if (col-llena? (get-board-col br col))
		(car (cons br (display "Error: col llena\n")))
		(board-append br col pieza)
	)
)

; Ve si 4 valores sucesivos son iguales. 
; Función auxiliar para checkeos
(define (col-check-win col count pieza)
	(if (or (and (null? col) (< count 4)) (eqv? pieza null))
		#f
		(if (= count 4) 
			#t
			(if (eqv? (car col) pieza)
				(col-check-win (cdr col) (+ 1 count) (car col))
				(col-check-win (cdr col) 1 (car col))
			)
		)
	)
)

; Nota: Recursividad Natural
(define (board-check-vertical-win br)
	(if (null? br)
		#f
		(or (col-check-win (car br) 0 1)
			(board-check-vertical-win (cdr br))
		)
	)
)

(define (fila-check-win br y)
	(col-check-win (get-board-fila br y) 0 1)
)

(define (fila-check-recurse br y)
	(if (>= y 6)
		#f
		(or (fila-check-win br y)
			(fila-check-win br (+ y 1))
		)
	)
)
; Nota: transformar fila a col, evaluar col.
(define (board-check-horizontal-win br)
	(if (null? br)
		#f
		(fila-check-recurse br 0)
	)
)
; ################## TDA Board
( provide (all-defined-out))
