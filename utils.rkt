#lang racket
; Funciones auxiliares aquí
; Reemplaza el primer null por a.
(define (append-null lst a)
	(if (null? lst)
		null
		(if (eqv? (car lst) null)
			(cons a (cdr lst))
			(cons (car lst) (append-null (cdr lst) a))
		)
	)
)

( provide (all-defined-out))
