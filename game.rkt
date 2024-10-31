#lang racket
(require "player.rkt")
(require "board.rkt")
(require "utils.rkt")

; ###### TDA Game

; Constructor de tda game
; Dom: player1 (player) X player2 (player) X board (board) X
; current-turn (int)
; Rec: (game)
; Nota: el último valor es el historial (stack)
(define (game p1 p2 brd c-turn)
	(list p1 p2 brd c-turn empty-stack)
)

; Selectores

; Retorna el campo "player1" del tda game.
; Dom: gm (game)
; Rec: (player)
(define (game-p1 gm)
	(list-ref 0 gm)
)

; Retorna el campo "player2" del tda game.
; Dom: gm (game)
; Rec: (player)
(define (game-p2 gm)
	(list-ref 1 gm)
)

; Retorna el campo "board" del tda game.
; Dom: gm (game)
; Rec: (player)
(define (game-board gm)
	(list-ref 2 gm)
)

; Retorna el campo "board" del tda game, y lo muestra en pantalla.
; Dom: gm (game)
; Rec: (player)
(define (game-get-board gm)
	(car (cons (game-board gm) 
			(display-board (game-board gm))
		)
	)
)

; Retorna el campo "current_turn" del tda game.
; Dom: gm (game)
; Rec: (player)
(define (game-current-turn gm)
	(list-ref 3 gm)
)

; Retorna el campo "history" del tda game.
; Dom: gm (game)
; Rec: (player)
(define (game-history gm)
	(list-ref 4 gm)
)

; FIXME: cturn = 0
; Retorna el actual jugador en base a current_turn.
; Dom: gm (game)
; Rec: (player) || null
(define (game-get-current-player gm)
	(if (eqv? (game-current-turn gm) 0)
		(game-p1 gm)
		(if (eqv? (game-current-turn gm) 1)
			(game-p2 gm)
			null
		)
	)
)

; Modificadores

; Modifica el campo "player1" del tda game.
; Dom: gm (game) x pl (player)
; Rec: (game)
(define (game-p1-set gm pl)
	(list-set gm 0 pl)
)

; Modifica el campo "player2" del tda game.
; Dom: gm (game) x pl (player)
; Rec: (game)
(define (game-p2-set gm pl)
	(list-set gm 1 pl)
)

; Modifica el campo "board" del tda game.
; Dom: gm (game) x br (board)
; Rec: (game)
(define (game-board-set gm br)
	(list-set gm 2 br)
)

; Modifica el campo "current_turn" del tda game.
; Dom: gm (game) x turn (int)
; Rec: (game)
(define (game-turn-set gm turn)
	(list-set gm 3 turn)
)

; Modifica el campo "history" del tda game.
; Dom: gm (game) x st (stack)
; Rec: (game)
(define (game-history-set gm st)
	(list-set gm 4 st)
)

; Special (MALAS IDEAS)

; salidas: p1, sino p2
; Nota:FIXME: MALA IDEA DEBIDO A MALOS REQ. FUN.!!
; Retorna el jugador correspondiente a "red".
; Dom: gm (game)
; Rec: (player)
(define (game-get-red gm)
	(if (player-is-red? (game-p1 gm))
		(game-p1 gm)
		(game-p2 gm)
	)
)

; Retorna el jugador correspondiente a "yellow".
; Dom: gm (game)
; Rec: (player)
(define (game-get-yellow gm)
	(if (player-is-yellow? (game-p1 gm))
		(game-p1 gm)
		(game-p2 gm)
	)
)

; Retorna el número del jugador correspondiente a "red".
; Dom: gm (game)
; Rec: (int)
(define (game-who-is-red gm)
	(if (player-is-red? (game-p1 gm))
		1
		2
	)
)

; Retorna el número del jugador correspondiente a "yellow".
; Dom: gm (game)
; Rec: (int)
(define (game-who-is-yellow gm)
	(if (player-is-yellow? (game-p1 gm))
		1
		2
	)
)

; Asigna un jugador en la casilla de jugadores correspondiente a "red".
; Dom: gm (game) x pl_red (player)
; Rec: (game)
(define (game-pl_red-set gm pl_red)
	(if (eqv? (game-who-is-red gm) 1)
		(game-p1-set gm pl_red)
		(game-p2-set gm pl_red)
	)
)

; Asigna un jugador en la casilla de jugadores correspondiente a "yellow".
; Dom: gm (game) x pl_yellow (player)
; Rec: (game)
(define (game-pl_yellow-set gm pl_yellow)
	(if (eqv? (game-who-is-yellow gm) 1)
		(game-p1-set gm pl_yellow)
		(game-p2-set gm pl_yellow)
	)
)

; Funciones

; Muestra el historial de acciones en pantalla
; Dom: gm (game)
; Rec: void
(define (game-display-history gm)
	(display (game-history))
)

; Añade una entrada (columna . color) en el historial.
; Dom: gm (game) x col (int) x color (string)
; Rec: (game)
(define (game-add-history gm col color)
	(game-history-set gm 
		(stack-push (game-history gm) (cons col color))
	)
)

;FIXME: revisar casos borde o condiciones?
; Revisa si el juego está en condición de empate: tablero lleno, o ya no hay
; fichas disponibles.
; Dom: gm (game)
; Rec: (bool)
(define (game-is-draw? gm)
	(and (board-lleno? (game-board gm))
		(or (player-has-pieces? (game-p1 gm))
			(player-has-pieces? (game-p2 gm))
		)
	)
)

; En caso de que el jugador rojo gane, actualizar las estadísticas de ambos
; jugadores de manera acorde al resultado (yellow = loss).
; Dom: gm (game) x pl_red (player) x pl_yellow (player)
; Rec: (game)
(define (game-red-win gm pl_red pl_yellow)
	(game-pl_yellow-set (game-pl_red-set gm (player-update-stats pl_red "win")
		(player-update-stats pl_yellow "loss")
	)
)

; En caso de que el jugador amarillo gane, actualizar las estadísticas de ambos
; jugadores de manera acorde al resultado (red = loss).
; Dom: gm (game) x pl_red (player) x pl_yellow (player)
; Rec: (game)
(define (game-yellow-win gm pl_red pl_yellow)
	(game-pl_yellow-set (game-pl_red-set gm (player-update-stats pl_red "loss")
		(player-update-stats pl_red "win")
	)
)

; En caso de que haya un empate, actualizar las estadísticas de ambos
; jugadores de manera acorde al resultado.
; Dom: gm (game) x pl_red (player) x pl_yellow (player)
; Rec: (game)
(define (game-draw-both gm pl_red pl_yellow)
	(game-pl_yellow-set (game-pl_red-set gm (player-update-stats pl_red "draw")
		(player-update-stats pl_yellow "draw")
	)
)

; Función auxiliar para actualizar estadísticas en base a quién ganó.
; Dom: gm (game) x win_num (int) x pl_red (player) x pl_yellow (player)
; Rec: (game)
; Nota: FIXME: Condición 0 equivale a draw???
(define (game-stats-wrapper_wins gm win_num pl_red pl_yellow)
	(cond [(eqv? win_num 1)
		(game-red-win gm pl_red pl_yellow)]
		[(eqv? win_num 2)
		(game-yellow-win gm pl_red pl_yellow]
		[(and (eqv? win_num 0) (not (game-is-draw? gm)))
		gm]
		[(and (eqv? win_num 0) (game-is-draw? gm))
		(game-draw-both gm pl_red pl_yellow)]
	)
)

; Función para actualizar estadísticas en base a quién ganó.
; Dom: gm (game)
; Rec: (game)
(define (game-stats-wrapper gm)
	(game-stats_wrapper_wins gm 
		(board-who-is-winner (game-board gm))
		(game-get-red gm)
		(game-get-yellow gm)
	)
)

; cturn=-1
; Función para declarar un juego como terminado.
; Dom: gm (game)
; Rec: (game)
(define (game-set-end gm)
	(game-stats-wrapper (game-turn-set gm -1))
)

; Mala idea?.
; Función que ejecuta un movimiento en el tablero en base al turno actual.
: Dom: gm (game) x col (int)
; Rec: (game)
(define (game-set-move gm col)
	(game-board-set 
		(board-set-play-piece (game-board gm) col 
				(piece 
					(player-color 
						(game-get-current-player gm)
					)
				)
		)
	)
)

; Función que ejecuta un movimiento en el tablero en base al turno actual,
; y añade el movimiento al historial.
: Dom: gm (game) x col (int)
; Rec: (game)
(define (game-set-move-history gm col)
	(game-set-move
		(game-add-history gm col 
			(player-color 
				(game-get-current-player gm)
			)
		) 
		col
	)
)

; Función que alterna el turno actual (0 o 1).
: Dom: gm (game)
; Rec: (game)
(define (game-flip-turn gm)
	(game-turn-set gm 
		(remainder (+ (game-current-turn gm) 1) 2)
	)
)

; Función que quita una pieza al jugador del turno actual. Útil para 
; ejecutar movimientos.
: Dom: gm (game)
; Rec: (game)
(define (game-player-take-piece gm)
	(if (eqv? (game-current-turn gm) 0)
		(game-p1-set (player-rem_p-add (game-get-current-player gm) -1)
		)
		(game-p2-set (player-rem_p-add (game-get-current-player gm) -1)
		)
	)
)

; Muestra al ganador del juego actual.
; Dom: gm (game)
; Rec: (int)
(define (game-is-win? gm)
	(board-who-is-winner (game-board gm))
)

; Ver si hay draw o win. Si los hay, termina el juego, si no, retorna el juego
; sin cambios.
; Dom: gm (game)
; Rec: (game)
(define (game-ended gm)
	(if	(or (game-is-draw? gm) 
			(board-is-win? (game-get-board gm))
		)
		(game-set-end gm)
		gm
	)
)

;(define (game-do-action gm col))

; Función que ejecuta un movimiento, actualiza el turno actual,
; verifica si hay un ganador, quita una pieza del jugador, 
; y actualiza el historial.
; Dom: gm (board) x pl (player) x col (int)
; Rec: (game) 
(define (game-player-set-move gm pl col)
	(if (eqv? (game-get-current-player gm) pl)
		(game-ended (game-flip-turn 
			(game-player-take-piece (game-set-move-history gm col))
			)
		)
		(car '(gm . (display "Error:Jugador incorrecto!\n")))
	)
)

(all-defined-out)
