#lang racket
; (require "player_207823589_CarrascoAlvarez.rkt")
(require "utils_207823589_CarrascoAlvarez.rkt")
; TDA Board + TDA Piece

; ###### TDA Piece

; Constructor de tda Piece.
; Dom: color (string)
; Rec: (piece)
(define (piece color)
	(cons color null)
)

; Selectores

; Retorna el "color" de una pieza.
; Dom: p (piece)
; Rec: (string)
(define (piece-color p)
	(car p)
)

;(define piece-p1 (piece "red"))
;(define piece-p2 (piece "yellow"))

; Funciones

; Genera un hash del color de la pieza, para posterior
; comparación. Asume "red"= 1, "yellow"=2, 0 en caso contrario.
; Dom: pieza (piece)
; Rec: (int)
(define (hash-piece pieza)
	(if (eqv? (piece-color pieza) "red")
		1
		(if (eqv? (piece-color pieza) "yellow")
			2
			0
		)
	)
)

; 'or' pseudológico de las piezas ya hasheadas, considerando 0 como #f.
; Dom: num1 (int) x num2 (int)
; Rec: (int)
(define (hash-or num1 num2)
	(if (eqv? num1 0)
		num2
		num1
	)
)

; Retorna el booleano correspondiente al resultado de 'hash-or'.
; Dom: num1 (int) x num2 (int)
(define (unhash-or num1 num2)
	(if (eqv? (hash-or num1 num2) 0)
		#f
		#t
	)
)

; ################### TDA Piece

; TDA Board

; Crea una lista de n términos de valor null.
; Útil para inicializar una "lista de n valores".
; Dom: n (int)
; Rec: (list)
(define (do-cols n)
	(make-list n null)
)

; Para cada valor encontrado en lst, hacer una lista de n nulos.
; Extensión de do-cols para hacer una matriz mediante listas
; enlazadas.
; Dom: n (int) x lst (list)
; Rec: (list) == (matriz n x len(lst))
(define (do-filas n lst)
	(map (lambda (a) (do-cols n)) lst)
)

; A[x][y]
; Matriz de 7x6 piezas vacías.
; Dom: void
; Rec: (board) == (list) == (matriz 7x6)
(define (board)
; USAR LISTAS ENLAZADAS, NO MATRICES!!!
	(do-filas 6 (do-cols 7))
;	(do-cols 7)
)

; Retorna la instancia de board, o sea, una matriz vacía.
; Dom: void
; Rec: (board)
(define empty-board (board))

; Selectores

; Retorna la columna (lista) x de board.
; Dom: br (board) X x (int)
; Rec: (list)
(define (get-board-col br x)
	(list-ref br x)
)

; Retorna la fila (lista) 'y' de board.
; Dom: br (board) X y (int)
; Rec: (list)
(define (get-board-fila br y)
	(foldr (lambda (l1 l2)(cons (list-ref l1 y) l2)) '() br)
)

; Retorna #t si la posición (x, y) está fuera de límites, #f en caso contrario.
; Dom: x (int) X y (int)
; Rec: (bool)
(define (fuera-del-tablero x y)
	(or (< x 0) (> x 6) (< y 0) (> y 5)
	)
)

;FIXME: Usar numeración desde 0!!!
; Retorna br[x][y], o sea, la pieza del tablero en dicha posición.
; Dom: br (board) X x (int) X y (int)
; Rec: (piece)
(define (get-board-xy br x y)
	(if	(fuera-del-tablero x y)
		null
		(list-ref (get-board-col br x) y)
	)
)

; Retorna la diagonal ascendente, desde la posición (x,y) en adelante.
; Dom: br (board) X x (int) X y (int)
; Rec: 'lista de (pieces)' == (list)
(define (get-board-diag-ascen br x y)
	(if (fuera-del-tablero x y)
		null
		(cons (get-board-xy br x y) 
			(get-board-diag-ascen br (+ x 1) (+ y 1))
		)
	)
)


; Retorna la diagonal descendente, desde la posición (x,y) en adelante.
; Dom: br (board) X x (int) X y (int)
; Rec: 'lista de (pieces)' == (list)
(define (get-board-diag-descen br x y)
	(if (fuera-del-tablero x y)
		null
		(cons (get-board-xy br x y) 
			(get-board-diag-descen br (+ x 1) (- y 1))
		)
	)
)

; Modificadores

; Modifica una columna en la posición (x,y), retornándola.
; Dom: br (board) X x (int) X y (int) X pieza (piece)
; Rec: columna de piezas (list)
(define (col-set br x y pieza)
	(list-set (get-board-col br x) y pieza)
)

; Modifica y ASIGNA una fila para modificar UNA PIEZA.
; Retorna el tablero MODIFICADO.
; Dom: br (board) X x (int) X y (int) X pieza (piece)
; Rec: br (board)
(define (board-set br x y pieza)
	(list-set br x (col-set br x y pieza))
)

; Agrega un valor a la columna x, reemplazando el primer null que encuentra.
; Esto es efectivamente un 'wrapper' de la operación append en listas, con
; la intención de 'asignar' valores no nulos , en vez de expandir la lista.
; Dom: br (board) X x (int) X pieza (piece)
; Rec: columna de piezas (list)
(define (col-append br x pieza)
	(append-null (get-board-col br x) pieza)
)

; Modifica y ASIGNA una fila para agregar UNA PIEZA
; Retorna el tablero MODIFICADO.
; Dom: br (board) X x (int) X pieza (piece)
(define (board-append br x pieza)
	(list-set br x (col-append br x pieza))
)

; Funciones

; Pretty-print del tablero
; Dom: br (board)
; Rec: display en salida estándar (void) 
(define (display-board br)
	;(display (reverse br))
	(car 
		(cons (display "\n") 
			(map writeln br)
		)
	)
)

; Retorna #t si una columna está llena (n=6), #f en caso contrario.
; No se consideran los null.
; Dom: col (list)
; Rec: (bool)
(define (col-llena? col)
	(= 6	(length (filter (lambda (a) (not (null? a))) col) 
		)
	)
)


; Retorna #f si un tablero está lleno, #t en caso contrario.
; No se consideran los null.
; Dom: br (board)
; Rec: (bool)
(define (board-can-play? br)
	(not (andmap col-llena? br))
)

; Mete una pieza en la columna x del tablero. Es decir, la pieza nueva se
; apila arriba de la anterior.
; Dom: br (board) X col (int) X pieza (piece)
; Rec: (board)
; Nota: Si col está llena, mostrar error y no realizar
; jugada y retornar tablero.
(define (board-set-play-piece br col pieza)
	(if (col-llena? (get-board-col br col))
		(car (cons br (display "Error: col llena\n")))
		(board-append br col pieza)
	)
)

; Ve si 4 valores sucesivos son iguales. 
; Función auxiliar para checkeos.
; Dom: col (list) x count (int) x pieza (piece)
; Rec: (int)
(define (col-check-win col count pieza)
	(if (or (and (null? col) (< count 4)) (eqv? pieza null))
		0	;#f
		(if (= count 4) 
			(hash-piece pieza)	;#t
			(if (eqv? (car col) pieza)
				(col-check-win (cdr col) (+ 1 count) (car col))
				(col-check-win (cdr col) 1 (car col))
			)
		)
	)
)

; Verifica si hay 4 piezas iguales en vertical en todas las columnas.
; Dom: br (board)
; Rec: (int)
; Nota: Recursividad Natural
(define (board-check-vertical-win br)
	(if (null? br)
		0	;#f
		(hash-or (col-check-win (car br) 0 1)
			(board-check-vertical-win (cdr br))
		)
	)
)

; Verifica si hay 4 piezas iguales en horizontal en la fila y.
; Dom: br (board) X y (int)
; Rec: (int)
(define (fila-check-win br y)
	(col-check-win (get-board-fila br y) 0 1)
)

; Verifica si hay 4 piezas iguales en horizontal en todas las filas,
; partiendo de y.
; Dom: br (board) X y (int)
; Rec: (int)
; Nota: Wrapper para check-horizontal, inicializar en 0.
(define (fila-check-recurse br y)
	(if (>= y 6)
		0	;#f
		(hash-or (fila-check-win br y)
			(fila-check-win br (+ y 1))
		)
	)
)

; Verifica si hay 4 piezas iguales en horizontal en todas las filas.
; Dom: br (board)
; Rec: (int)
; Nota: transformar fila a col, evaluar col.
(define (board-check-horizontal-win br)
	(if (null? br)
		0	;#f
		(fila-check-recurse br 0)
	)
)

; Entrega la lista de puntos a evaluar para las diagonales descendientes,
; ya que solo se necesita una cierta cantidad de puntos para evaluar
; todas las diagonales de tamaño >= 4.
; Dom: void
; Rec: lista de puntos (list)
; Nota: Solo se necesitan las diag. de (0,5),(1,5),(2,5),(3,5) 
; y (0,4),(0,3)
(define criterio-descen (list 
	(cons 0 5)
	(cons 1 5)
	(cons 2 5)
	(cons 3 5)
	(cons 0 4)
	(cons 0 3)
	)
)

; Función auxiliar para chequeo de diagonales descendientes.
; Revisa la diagonal desde (x,y) hacia adelante.
; Dom: br (board) X x (int) X y (int)
; Rec: (int)
(define (diag-check-descen-win br x y)
	(col-check-win (get-board-diag-descen br x y) 0 1)
)


; Función auxiliar para recursividad en chequeo de diagonales descendientes.
; Revisa las diagonales a parir de la lista de puntos entregada.
; Dom: br (board) X puntos (list)
; Rec: (int)
(define (recurse-descen-win br puntos)
	(if (null? puntos)
		0	;#f
		(hash-or (diag-check-descen-win br (caar puntos) (cdar puntos))
			(recurse-descen-win br (cdr puntos))
		)
	)
)


; Función para chequeo de diagonales descendientes.
; Revisa las diagonales descendientes en busca de un ganador.
; Dom: br (board)
; Rec: (int)
(define (board-check-descen-win br)
	(recurse-descen-win br criterio-descen)
)


; Entrega la lista de puntos a evaluar para las diagonales ascendientes,
; ya que solo se necesita una cierta cantidad de puntos para evaluar
; todas las diagonales de tamaño >= 4.
; Dom: void
; Rec: lista de puntos (list)
; Nota: Solo se necesitan las diag. de (0,0),(1,0),(2,0),(3,0) 
; y (0,1),(0,2)
(define criterio-ascen (list
	(cons 0 0)
	(cons 1 0)
	(cons 2 0)
	(cons 3 0)
	(cons 0 1)
	(cons 0 2)
	)
)

; Función auxiliar para chequeo de diagonales ascendientes.
; Revisa la diagonal desde (x,y) hacia adelante.
; Dom: br (board) X x (int) X y (int)
; Rec: (int)
(define (diag-check-ascen-win br x y)
	(col-check-win (get-board-diag-ascen br x y) 0 1)
)

; Función auxiliar para recursividad en chequeo de diagonales ascendientes.
; Revisa las diagonales a parir de la lista de puntos entregada.
; Dom: br (board) X puntos (list)
; Rec: (int)
(define (recurse-ascen-win br puntos)
	(if (null? puntos)
		0	;#f
		(hash-or (diag-check-ascen-win br (caar puntos) (cdar puntos))
			(recurse-ascen-win br (cdr puntos))
		)
	)
)

; Función para chequeo de diagonales ascendientes.
; Revisa las diagonales ascendientes en busca de un ganador.
; Dom: br (board)
; Rec: (int)
(define (board-check-ascen-win br)
	(recurse-ascen-win br criterio-ascen)
)

; Función para chequeo de diagonales.
; Revisa las diagonales ascendientes y descendientes en busca de un ganador.
; Dom: br (board)
; Rec: (int)
(define (board-check-diagonal-win br)
	(hash-or (board-check-descen-win br) 
		(board-check-ascen-win br)
	)
)

; Función para chequeo del tablero en busca de un ganador.
; Revisa las diagonales, filas y columnas.
; Dom: br (board)
; Rec: (int)
(define (board-who-is-winner br)
	(hash-or (hash-or (board-check-horizontal-win br)
			(board-check-vertical-win br)
		)
		(board-check-diagonal-win br)
	)
)

; Función para chequeo del tablero en busca de un ganador.
; Revisa las diagonales, filas y columnas.
; Dom: br (board)
; Rec: (bool)
; Nota: versión booleana
(define (board-is-win? br)
	(unhash-or (board-who-is-winner br) 0)
)

; ################## TDA Board
( provide (all-defined-out))
