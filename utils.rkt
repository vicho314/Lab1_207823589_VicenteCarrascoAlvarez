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

; implementar stack
; ###### stack
(define empty-stack null)

(define (stack-push st v)
	(cons v st)
)

(define (stack-top st)
	(car st)
)

(define (stack-pop st)
	(cdr st)
)

( provide (all-defined-out))
