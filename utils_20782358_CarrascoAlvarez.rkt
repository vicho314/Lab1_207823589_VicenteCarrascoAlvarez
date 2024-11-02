#lang racket
; Funciones auxiliares aquí
; Reemplaza el primer null encontrado con el valor de a.
; Dom: lst (list) x a (any)
; Rec: lst (list)
(define (append-null lst a)
	(if (null? lst)
		null
		(if (eqv? (car lst) null)
			(cons a (cdr lst))
			(cons (car lst) (append-null (cdr lst) a))
		)
	)
)

; implementar stack
; ###### stack: list con push, top y pull
; ## (stack) == (list)

; Retorna un stack vacío.
; Dom: void
; Rec: null (stack)
(define empty-stack null)

; Agrega un valor al stack.
; Dom: st (stack) x v (any)
; Rec: stack (list)
(define (stack-push st v)
	(cons v st)
)

; Muestra el tope del stack
; Dom : st (stack)
; Rec: tope del stack (any)
(define (stack-top st)
	(car st)
)

; Desapila el valor más reciente del stack (tope).
; Dom: st (stack)
; Rec: (stack)
(define (stack-pop st)
	(cdr st)
)
;###### TDA stack

(define (get-fila lst y)
	(foldr (lambda (l1 l2)(cons (list-ref l1 y) l2)) '() lst)
)

(define (transpose lst . y)
	(if	(null? y)
		(transpose lst 0)
		(if	(< (car y) (length lst))
			(cons (get-fila lst (car y)) 
				(transpose lst (+ (car y) 1))
			)
			null	
		)
	)
)

( provide (all-defined-out))
