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
(define (do-filas n)
	(make-list n null)
)

(define (do-cols n lst)
	(map (lambda (a) (do-filas n)) lst)
)
; Â[y][x] para tener rep. vertical
(define board
	(do-cols 7 (do-filas 6))
)

; Selectores
(define (board-fila br y)
	(list-ref br y)
)

;FIXME: Usar numeración desde 0!!!
(define (board-xy br x y)
	(list-ref (board-fila br y) x)
)

; Modificadores
; Modifica una fila
(define (fila-set! br x y pieza)
	(list-set! (board-fila br y) x pieza)
)

; Modifica y ASIGNA una fila para modificar UNA PIEZA
(define (board-set! br x y pieza)
	(list-set! br y (fila-set! br x y pieza))
)

; Funciones
; Pretty-print del tablero
(define (display-board br)
	(display (reverse br))
)

; ################## TDA Board
( provide (all-defined-out))
