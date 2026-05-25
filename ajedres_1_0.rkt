#lang racket
#|
- Diego Alexander Neva Pati;o Ing(c)
- Racket
- 8.16
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- El programa que permite jugar ajedrez, a dos(2) jugadores
jugar con las reglas del ajedrez de movimiento y ataque, solo tiene en cuenta el jaque, jaque mate y coronación de peones
Las piezas se codifican como:

Letras: a (peón), t (torre), c (caballo), d (alfil), e (reina), f (rey), g (casilla vacía).
Colores: n (negro), w (blanco).

/-initialBoard: Define el tablero inicial como una cadena de texto donde cada pieza está representada por un código (ej. "00tn" = posición 0, torre negra).

/-getPiece y getColor: Obtienen el tipo y color de una pieza en una posición dada.

/-isEmpty?: Verifica si una casilla está vacía.

/-las reglas para cada pieza:

Peones (isValidBlackPawn? y isValidWhitePawn?): Movimientos hacia adelante, capturas en diagonal y avance inicial de dos casillas.

Torres (isValidRook?): Movimientos lineales horizontales/verticales.

Alfiles (isValidBishop?): Movimientos diagonales.

Reina (isValidQueen?): Combina movimientos de torre y alfil.

Rey (isValidKing?): Un paso en cualquier dirección.

Caballos (isValidKnight?): Movimiento en "L".

4. Validación de movimientos

isValidMove?: Verifica si un movimiento es válido según el tipo y color de la pieza.

5. Actualización del tablero

updateBoard y changeString: Modifican el estado del tablero después de un movimiento válido.

6. Interfaz gráfica

Dibujo del tablero:

drawPieces: Renderiza imágenes de piezas (ej. "peon-negro.png") en posiciones calculadas.

PrintFullBoard: Dibuja todas las piezas iterando sobre el tablero.

|#
( require graphics/graphics )

; ============================================= ============================================= =============================================
; Constantes y configuraciones
; ============================================= ============================================= =============================================
( define initialBoard 
  "00tn01cn02dn03en04fn05dn06cn07tn08an09an10an11an12an13an14an15an16gg17gg18gg19gg20gg21gg22gg23gg24gg25gg26gg27gg28gg29gg30gg31gg32gg33gg34gg35gg36gg37gg38gg39gg40gg41gg42gg43gg44gg45gg46gg47gg48aw49aw50aw51aw52aw53aw54aw55aw56tw57cw58dw59ew60fw61dw62cw63tw"
)

; ============================================= ============================================= =============================================
; Funciones auxiliares para manipular el tablero
; ============================================= ============================================= =============================================
( define ( getPiece board position )
   ( string-ref board ( + ( * position 4 ) 2 ) ) )

( define ( getColor board position )
   ( string-ref board ( + ( * position 4 ) 3 ) ) )

( define ( isEmpty? board position )
   ( equal? ( getPiece board position ) #\g )
   );end isEmpty?

; ============================================= ============================================= =============================================
; Funciones de movimiento para cada pieza
; ============================================= ============================================= =============================================
; ------------------------------------------------------- Funcion del peon negro -------------------------------------------------------
( define ( ValidateBlackPawn? board initialPosition finalPosition )
   ( cond
      [ ( = finalPosition ( + initialPosition 8 ) )
        ( isEmpty? board finalPosition ) ]
      [ ( and ( >= initialPosition 8 ) ( <= initialPosition 15 )
              ( = finalPosition ( + initialPosition 16 ) ) )
        ( and ( isEmpty? board ( + initialPosition 8 ) )
              ( isEmpty? board finalPosition ) ) ]
      [ ( and
          ( not ( = ( remainder initialPosition 8 ) 7 ) )
          ( = finalPosition ( + initialPosition 9 ) )
        )  
        ( equal? ( getColor board finalPosition ) #\w )  ]
      [ ( and
          ( not ( = ( remainder initialPosition 8 ) 0 ) )
          ( = finalPosition ( + initialPosition 7 ) )
        )  
        ( equal? ( getColor board finalPosition ) #\w )  ]
      [ else #f ]
   );   end cond
);end IsValidBlackPawn

; ------------------------------------------------------- Funcion del peon blanco -------------------------------------------------------
( define ( ValidateWhitePawn? board initialPosition finalPosition )
     ( cond
      [ ( = finalPosition ( - initialPosition 8 ) )
        ( isEmpty? board finalPosition ) ]
      [ ( and ( >= initialPosition 48 ) ( <= initialPosition 55 )
              ( = finalPosition ( - initialPosition 16 ) ) )
        ( and ( isEmpty? board ( - initialPosition 8 ) )
              ( isEmpty? board finalPosition ) ) ]
      [ ( and ( not ( = ( remainder initialPosition 8 ) 7 ) )
          ( = finalPosition ( - initialPosition 7 ) )
        )  
        ( equal? ( getColor board finalPosition ) #\n ) ]
      [ ( and ( not ( = ( remainder initialPosition 8 ) 0 ) )
              ( = finalPosition ( - initialPosition 9 ) )
        )
        ( equal? ( getColor board finalPosition ) #\n ) ]
      [ else #f ] )
   ); IsValidWhitePawn?
#|----------------------------------------------------------------------------------------------------------------|#
#|--------------------------------------------CORONACIÓN DE PEONES------------------------------------------------|#
#|----------------------------------------------------------------------------------------------------------------|#

  ;Esta funcion abre un viewport, dependiendo del click del usuario el peón blanco coronado se cambia por:
  ;Reina, alfil, caballo o torre
  ;En cada caso se llaman las mismas funciones, el cambio está en el caracter que cambiamos por el peón,("M" o "A" o "T" o "C")
( define ( CoronationWhite stringTablero contador turno viewBoard )
   ( define viewCoronation ( open-viewport "Coronacion" 300 600 ) );se abre un viewport en el identificador ventanaCoronacion
   ( ( ( draw-pixmap-posn "imagenes/coronacionblancas.png" ) viewCoronation ) ( make-posn 0 0 ) )
   ( define clickWhite ( mouse-click-posn ( get-mouse-click viewCoronation ) ) );se guarda el click del usuario en clickBlancas
   ( close-viewport viewCoronation );despues de escojida la ficha se cierra el viewport
   
   ( cond
      ;si se escoje Torre
      [ ( <= ( posn-y clickWhite ) 160 )
        ( DrawTwoRectangle contador viewBoard )
       ( ( draw-solid-rectangle viewCoronation ) ( make-posn 600 400 ) 300 100 "black" );se esconde la jugada invalida
       ( define newBoard ( string-append (substring stringTablero 0 ( + ( * contador 4 ) 2 ) ) "t" ( substring stringTablero ( + ( *  contador 4 ) 3 ) ) ) )
       ( drawPieces newBoard ( + ( * contador 4 ) 3 ) ( + ( * contador 4 ) 2 ) viewBoard )
       newBoard
      ]
      ;si se escoje Caballo
      [ ( <= ( posn-y clickWhite ) 300 )
        ( DrawTwoRectangle contador viewBoard )
       ( ( draw-solid-rectangle viewCoronation ) ( make-posn 600 400) 300 100 "black" )
       ( define newBoard ( string-append ( substring stringTablero 0 ( + ( * contador 4 ) 2 ) ) "c" ( substring stringTablero ( + ( * contador 4 ) 3 ) ) ) )
       ( drawPieces newBoard ( + ( * contador 4 ) 3 ) ( + ( * contador 4 ) 2 ) viewBoard )
       newBoard
      ]
      ;si se escoje Alfil
      [ ( <= ( posn-y clickWhite ) 420 )
        ( DrawTwoRectangle contador viewBoard )
        ( ( draw-solid-rectangle viewCoronation ) ( make-posn 600 400 ) 300 100 "black" )
        ( define newBoard ( string-append ( substring stringTablero 0 ( + ( * contador 4 ) 2 ) ) "d" ( substring stringTablero ( + ( * contador 4 ) 3 ) ) ) )
        ( drawPieces newBoard ( + ( * contador 4 ) 3 ) ( + ( * contador 4 ) 2 ) viewBoard )
       newBoard
      ]
      ;si se escoje Reina
      [ ( <= ( posn-y clickWhite ) 600 )
        ( DrawTwoRectangle contador viewBoard )
       ( ( draw-solid-rectangle viewCoronation ) ( make-posn 600 400 ) 300 100 "black" )
       ( define newBoard ( string-append ( substring stringTablero 0 ( + ( * contador 4 ) 2 ) ) "e" ( substring stringTablero ( + ( * contador 4 ) 3 ) ) ) )
       ( drawPieces newBoard ( + ( * contador 4 ) 3 ) ( + ( * contador 4 ) 2 ) viewBoard )
       newBoard
      ]
    );end cond
  );end LiberarBlancas

  ;Esta funcion abre un viewport, dependiendo del click del usuario el peón negro coronado se cambia por:
  ;Reina, alfil, caballo o torre
  ;En cada caso se llaman las mismas funciones, el cambio está en el caracter que cambiamos por el peón,("m" o "s" o "t" o "c")
  ( define ( CoronationBlack stringTablero contador turno viewBoard )
    ( define viewCoronation ( open-viewport "Coronacion" 300 600 ) );se abre un viewport con el identificador ventanaCoronacion
    ( ( ( draw-pixmap-posn "imagenes/coronacionnegras.png" ) viewCoronation ) ( make-posn 0 0 ) )
    ( define clickBlack ( mouse-click-posn ( get-mouse-click viewCoronation ) ) );Se almacena el click del usuario en el identificador clickNegras
    ( close-viewport viewCoronation );des pues de la selección del usuario se cierra el viewport
   
    ( cond
      ;si se escoje torre
      [ ( <= ( posn-y clickBlack ) 160 )
        ( DrawTwoRectangle contador viewBoard )
       ( ( draw-solid-rectangle viewCoronation ) ( make-posn 600 400 ) 300 100 "black");se esconde el aviso de jugada invalida
       ( define newBoard ( string-append ( substring stringTablero 0 ( + ( * contador 4 ) 2 ) ) "t" ( substring stringTablero ( + ( * contador 4 ) 3 ) ) ) )
       ( drawPieces newBoard ( + ( * contador 4 ) 3 ) ( + ( * contador 4 ) 2 ) viewBoard )
       newBoard
      ]
      ;si se escoje caballo
      [ ( <= ( posn-y clickBlack ) 300 )
        ( DrawTwoRectangle contador viewBoard )
       ( ( draw-solid-rectangle viewCoronation ) ( make-posn 600 400 ) 300 100 "black" )
       ( define newBoard ( string-append ( substring stringTablero 0 ( + ( * contador 4 ) 2 ) ) "c" ( substring stringTablero ( + ( * contador 4 ) 3 ) ) ) )
       ( drawPieces newBoard ( + ( * contador 4 ) 3 ) ( + ( * contador 4 ) 2 ) viewBoard )
       newBoard
      ]
      ;si se escoje alfil
      [ ( <= ( posn-y clickBlack ) 420 )
        ( DrawTwoRectangle contador viewBoard )
        ( ( draw-solid-rectangle viewCoronation ) ( make-posn 600 400 ) 300 100 "black" )
        ( define newBoard ( string-append ( substring stringTablero 0 ( + ( * contador 4 ) 2 ) ) "d" ( substring stringTablero ( + ( * contador 4 ) 3 ) ) )  )
        ( drawPieces newBoard ( + ( * contador 4 ) 3 ) ( + ( * contador 4 ) 2 ) viewBoard )
       newBoard
      ]
      ;si se escoje Reina
      [ ( <= ( posn-y clickBlack ) 600 )
        ( DrawTwoRectangle contador viewBoard )
       ( ( draw-solid-rectangle viewCoronation ) ( make-posn 600 400 ) 300 100 "black" )
       ( define newBoard ( string-append (substring stringTablero 0 ( + ( * contador 4 ) 2 ) ) "e" ( substring stringTablero ( + ( * contador 4 ) 3 ) ) )  )
       ( drawPieces newBoard ( + ( * contador 4 ) 3 ) ( + ( * contador 4 ) 2 ) viewBoard )
       newBoard
      ]
    );end cond
  );end LiberarNegras

  ;En caso de que haya un peón que pueda hacer la coronacion se llama la funcion LiberarNegras o LiberarBlancas
  ;Depende si un peón ha coronado en la fila correspondiente
  ( define ( Coronation counter stringTablero turn viewBoard )
    ( if ( < counter 8)
        ( if ( and
             ( equal? ( getPiece stringTablero counter ) #\a )
             ( equal? ( getColor stringTablero counter ) #\w )
            )
            ( CoronationWhite stringTablero counter turn viewBoard )
        ;else
            ( Coronation ( + counter 1 ) stringTablero turn viewBoard )
        );end if
    ;else

        (if ( < ( + counter 48 ) 64 )
            ( if ( and
             ( equal? ( getPiece stringTablero ( + counter 48 ) ) #\a )
             ( equal? ( getColor stringTablero ( + counter 48 ) ) #\n )
            )
                ( CoronationBlack stringTablero ( + counter 48 ) turn viewBoard )
                ;else
                ( Coronation ( + counter 1 ) stringTablero turn viewBoard )
            );end if
        ;else
            stringTablero
        );end if
    );end if
  );end Coronacion
 
#|--------------------------------------------CORONACIÓN DE PEONES------------------------------------------------|#
#|----------------------------------------------------------------------------------------------------------------|#
; ------------------------------------------------------- Funcion de la torre negra  -------------------------------------------------------
( define ( ValidateTowerBlack? board initialPosition finalPosition )
   ( cond
      [ ( and ( < initialPosition finalPosition )
              ( = ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition ) 
                  ( equal?  ( getColor board finalPosition ) #\w )
             )
             ( isValidLinearMoveOne? board initialPosition finalPosition 1 )
             ;else
             #f
        )
      ]
      [ ( and ( > initialPosition finalPosition )
              ( = ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition ) #\w )
             )
             ( isValidLinearMoveTwo? board initialPosition finalPosition 1 )
             ;else
             #f
        );end if
      ]
      [ ( and ( < initialPosition finalPosition )
              ( = ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition ) #\w )
             )
                  ( isValidLinearMoveThree? board initialPosition finalPosition 8 )
                  ;else
                  #f      
        )
      ] 
      [ ( and ( > initialPosition finalPosition )
              ( = ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition )  #\w ) 
             )
             ( isValidLinearMoveFour? board initialPosition finalPosition 8 )
             ;else
             #f
        )          
      ]
      [ else #f ]
   );end cond
);end isValidRook?
; ------------------------------------------------------- Funciones Auxiliares para las torres  -------------------------------------------------------
( define ( isValidLinearMoveOne? board initialPosition finalPosition counter )
   ( if ( = initialPosition ( - finalPosition counter ) )
        #t
        ;else
        ( if ( isEmpty? board ( - finalPosition counter ) )
             ( isValidLinearMoveOne? board initialPosition finalPosition ( + counter 1 ) )
             ;else
             #f
        );end if
   );end if
 )
( define ( isValidLinearMoveTwo? board initialPosition finalPosition counter )
   ( if ( = initialPosition ( + finalPosition counter ) )
        #t
        ;else
        ( if ( isEmpty? board ( + finalPosition counter ) )
             ( isValidLinearMoveTwo? board initialPosition finalPosition ( + counter 1 ) )
             ;else
             #f
        );end if
   );end if
 )
( define ( isValidLinearMoveThree? board initialPosition finalPosition counter )
   ( if ( = initialPosition ( - finalPosition counter ) )
        #t
        ;else
        ( if ( isEmpty? board ( - finalPosition counter ) )
             ( isValidLinearMoveThree? board initialPosition finalPosition ( + counter 8 ) )
             ;else
             #f
        );end if
   );end if
 )
( define ( isValidLinearMoveFour? board initialPosition finalPosition counter )
   ( if ( = initialPosition ( + finalPosition counter ) )
        #t
        ;else
        ( if ( isEmpty? board ( + finalPosition counter ) )
             ( isValidLinearMoveFour? board initialPosition finalPosition ( + counter 8 ) )
             ;else
             #f
        );end if
   ); end if
)
; ------------------------------------------------------- Funcion de la torre blanca  -------------------------------------------------------

( define ( ValidateTowerWhite? board initialPosition finalPosition )
   ( cond
      [ ( and ( < initialPosition finalPosition )
              ( = ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition ) 
                  ( equal?  ( getColor board finalPosition ) #\n )
             )
             ( isValidLinearMoveOne? board initialPosition finalPosition 1 )
             ;else
             #f
        )
      ]
      [ ( and ( > initialPosition finalPosition )
              ( = ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition ) #\n )
             )
             ( isValidLinearMoveTwo? board initialPosition finalPosition 1 )
             ;else
             #f
        );end if
      ]
      [ ( and ( < initialPosition finalPosition )
              ( = ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition ) #\n )
             )
                  ( isValidLinearMoveThree? board initialPosition finalPosition 8 )
                  ;else
                  #f      
        )
      ] 
      [ ( and ( > initialPosition finalPosition )
              ( = ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition )  #\n ) 
             )
             ( isValidLinearMoveFour? board initialPosition finalPosition 8 )
             ;else
             #f
        )          
      ]
      [ else #f ]
   );end cond
);end isValidRook?
; ------------------------------------------------------- Funcion del alfil negro -------------------------------------------------------
(define ( ValidateBishopBlack? board initialPosition finalPosition)
   ( cond
      [ ( and ( < ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
              ( < ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition ) 
                  ( equal?  ( getColor board finalPosition ) #\w )
             )
             ( ValiteDiagonalMoveOne? board initialPosition finalPosition 9 )
             ;else
             #f
        )
      ]
      [ ( and ( > ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
              ( < ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition ) #\w )
             )
             ( ValiteDiagonalMoveTwo? board initialPosition finalPosition 7 )
             ;else
             #f
        );end if
      ]
      [ ( and ( < ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
              ( > ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition ) #\w )
             )
                  ( ValiteDiagonalMoveThree? board initialPosition finalPosition 7 )
                  ;else
                  #f      
        )
      ] 
      [ ( and ( > ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
              ( > ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition )  #\w ) 
             )
             ( ValiteDiagonalMoveFour? board initialPosition finalPosition 9 )
             ;else
             #f
        )          
      ]
      [ else #f ]
   );end cond
);end ValidateBishop
; ------------------------------------------------------- Funciones Auxiliares para los alfiles  -------------------------------------------------------
( define ( ValiteDiagonalMoveOne? board initialPosition finalPosition counter )
   ( if ( = initialPosition ( - finalPosition counter ) )
        #t
        ;else
        ( if ( and
               ( < counter finalPosition )
               ( isEmpty? board ( - finalPosition counter ) )
             )
             ( ValiteDiagonalMoveOne? board initialPosition finalPosition ( + counter 9 ) )
             ;else
             #f
        );end if
   );end if
 )
( define ( ValiteDiagonalMoveTwo? board initialPosition finalPosition counter )
   ( if ( = initialPosition ( - finalPosition counter ) )
        #t
        ;else
        ( if ( and
               ( < counter finalPosition )
               ( isEmpty? board ( - finalPosition counter ) )
               )
             ( ValiteDiagonalMoveTwo? board initialPosition finalPosition ( + counter 7 ) )
             ;else
             #f
        );end if
   );end if
 )
( define ( ValiteDiagonalMoveThree? board initialPosition finalPosition counter )
   ( if ( = initialPosition ( + finalPosition counter ) )
        #t
        ;else
        ( if ( and
               ( <=  counter initialPosition )
               ( isEmpty? board ( + finalPosition counter ) )               
              )
             ( ValiteDiagonalMoveThree? board initialPosition finalPosition ( + counter 7 ) )
             ;else
             #f
        );end if
   );end if
 )
( define ( ValiteDiagonalMoveFour? board initialPosition finalPosition counter )
   ( if ( = initialPosition ( + finalPosition counter ) )
        #t
        ;else
        ( if ( and
               ( <= counter initialPosition )
               ( isEmpty? board ( + finalPosition counter ) )
               )
             ( ValiteDiagonalMoveFour? board initialPosition finalPosition ( + counter 9 ) )
             ;else
             #f
        );end if
   ); end if
)
; ------------------------------------------------------- Funcion del alfil blanco -------------------------------------------------------
(define ( ValidateBishopWhite? board initialPosition finalPosition)
   ( cond
      [ ( and ( < ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
              ( < ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition ) 
                  ( equal?  ( getColor board finalPosition ) #\n )
             )
             ( ValiteDiagonalMoveOne? board initialPosition finalPosition 9 )
             ;else
             #f
        )
      ]
      [ ( and ( > ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
              ( < ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition ) #\n )
             )
             ( ValiteDiagonalMoveTwo? board initialPosition finalPosition 7 )
             ;else
             #f
        );end if
      ]
      [ ( and ( < ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
              ( > ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition ) #\n )
             )
                  ( ValiteDiagonalMoveThree? board initialPosition finalPosition 7 )
                  ;else
                  #f      
        )
      ] 
      [ ( and ( > ( remainder initialPosition 8 ) ( remainder finalPosition 8 ) )
              ( > ( quotient initialPosition 8 ) ( quotient finalPosition 8 ) )
        )
        ( if ( or ( isEmpty? board finalPosition )
                  ( equal?  ( getColor board finalPosition )  #\n ) 
             )
             ( ValiteDiagonalMoveFour? board initialPosition finalPosition 9 )
             ;else
             #f
        )          
      ]
      [ else #f ]
   );end cond
);end ValidateBishop
; ------------------------------------------------------- Funcion del reina negra -------------------------------------------------------
(define ( ValiteQueenBlack? board initialPosition finalPosition)
  (or ( ValidateTowerBlack? board initialPosition finalPosition)
      ( ValidateBishopBlack? board initialPosition finalPosition)
  )
);end ValiteQueenBlack?
; ------------------------------------------------------- Funcion del reina blanca -------------------------------------------------------
(define ( ValiteQueenWhite? board initialPosition finalPosition)
  (or
   ( ValidateTowerWhite? board initialPosition finalPosition)
   ( ValidateBishopWhite? board initialPosition finalPosition )
  )
);end ValiteQueenWhite?
; ------------------------------------------------------- Funcion del rey negro -------------------------------------------------------
( define ( ValiteKingBlack? board initialPosition finalPosition )
   ( if ( or ( isEmpty? board finalPosition )
             ( equal?  ( getColor board finalPosition )  #\w )
        )
        (and (<= ( abs ( - ( quotient finalPosition 8 ) ( quotient initialPosition 8 ) ) )  1 )
             (<= ( abs ( - ( remainder finalPosition 8 ) ( remainder initialPosition 8 ) ) ) 1 )
        )
        ;else
        #f
   )
)
; ------------------------------------------------------- Funcion del rey blanco -------------------------------------------------------
( define ( ValiteKingWhite? board initialPosition finalPosition )
   ( if ( or ( isEmpty? board finalPosition )
             ( equal?  ( getColor board finalPosition )  #\n )
        )
        (and (<= ( abs ( - ( quotient finalPosition 8 ) ( quotient initialPosition 8 ) ) )  1 )
             (<= ( abs ( - ( remainder finalPosition 8 ) ( remainder initialPosition 8 ) ) ) 1 )
        )
        ;else
        #f
   )
)
; =============================================
; Funciones para detectar jaque y jaque mate
; =============================================

;; Función para buscar la posición del rey
( define ( SearchKingPosition board color )
   ( define ( SearchKingPositionAux board color position )
      ( cond
         [ ( >= position 64 ) #f ] ; No se encontró el rey (no debería pasar en un juego válido)
         [ ( and
             ( equal? ( getPiece board position ) #\f )
             ( equal? ( getColor board position ) color )
           )
           position
         ]
         [ else ( SearchKingPositionAux board color ( + position 1 ) ) ]
      )
   )
   ( SearchKingPositionAux board color 0 )
)

;; Función para verificar si el rey está en jaque
( define ( ValiteCheck? board kingColor )
   ( define kingPosition ( SearchKingPosition board kingColor ) )
   ( define ( CheckAttacker attackerPosition kingPosition board kingColor )
      ( cond
         [ ( >= attackerPosition 64 ) #f ] ; Ninguna pieza enemiga puede atacar al rey
         [ ( and 
             ( not ( isEmpty? board attackerPosition ) )
             ( not ( equal? ( getColor board attackerPosition ) kingColor ) )
             ( IsValidMoveAux? board attackerPosition kingPosition 
                ( if ( equal? kingColor #\w ) 1 0 ) )
           )
           #t ; El rey está en jaque
         ]
         [ else ( CheckAttacker ( + attackerPosition 1 ) kingPosition board kingColor ) ]
      )
   )
   ( CheckAttacker 0 kingPosition board kingColor )
)

;; Función para simular un movimiento y verificar si sigue en jaque
( define ( SimulateMove board initialPosition finalPosition kingColor )
   ( define temporaryBoard ( ChangeStringTemporary board initialPosition finalPosition ) )
   ( define inCheck ( ValiteCheck? temporaryBoard kingColor ) )
   ( not inCheck )
)

;; Función para crear un tablero temporal sin modificar el original
( define ( ChangeStringTemporary board positionOne positionTwo )
   ( if ( < positionOne positionTwo )
        ( ChangeStringTemporaryAuxOne board positionOne positionTwo )
        ( ChangeStringTemporaryAuxTwo board positionOne positionTwo )
   )
)

( define ( ChangeStringTemporaryAuxOne board positionOne positionTwo )
   ( define partOne ( substring board 0 ( + ( * positionOne 4 ) 2 ) ) )
   ( define partTwo ( substring board ( + ( * positionOne 4 ) 2 ) ( + ( * positionOne 4 ) 4 ) ) )
   ( define emptyCell ( make-string 2 #\g ) )
   ( define partThree ( substring board ( + ( * positionOne 4 ) 4 ) ( + ( * positionTwo 4 ) 2 ) ) )
   ( define partFour ( substring board ( + ( * positionTwo 4 ) 4 ) ( string-length board ) ) )
   ( string-append partOne emptyCell partThree partTwo partFour )
)

( define ( ChangeStringTemporaryAuxTwo board positionOne positionTwo )
   ( define partOne ( substring board 0 ( + ( * positionTwo 4 ) 2 ) ) )
   ( define partTwo ( substring board ( + ( * positionOne 4 ) 2 ) ( + ( * positionOne 4 ) 4 ) ) )
   ( define emptyCell ( make-string 2 #\g ) )
   ( define partThree ( substring board ( + ( * positionTwo 4 ) 4 ) ( + ( * positionOne 4 ) 2 ) ) )
   ( define partFour ( substring board ( + ( * positionOne 4 ) 4 ) ( string-length board ) ) )
   ( string-append partOne partTwo partThree emptyCell partFour )
)


;; Función para verificar si hay movimientos legales que salven al rey
( define ( HaveLegalMoves? board kingColor )
   ( define ( CheckPieceMovements position destPosition )
      ( cond
         [ ( >= destPosition 64 ) 
           ( CheckNextPiece ( + position 1 ) ) ]
         [ ( and 
             ( not (equal? position destPosition) )
             ( IsValidMoveAux? board position destPosition 
                ( if ( equal? kingColor #\w ) 0 1 ) )
             ( SimulateMove board position destPosition kingColor ) )
           #t ]
         [ else ( CheckPieceMovements position ( + destPosition 1 ) ) ]
      )
   )
   
   ( define ( CheckNextPiece position )
      ( cond
         [ ( >= position 64 ) #f ]
         [ ( and 
             ( not ( isEmpty? board position ) )
             ( equal? ( getColor board position ) kingColor ) )
           ( CheckPieceMovements position 0 ) ]
         [ else ( CheckNextPiece ( + position 1 ) ) ]
      )
   )
   
   ( CheckNextPiece 0 )
)

;; Función principal para detectar jaque mate
( define ( ValiteCheckMate? board kingColor )
   ( and 
     ( ValiteCheck? board kingColor )
     ( not ( HaveLegalMoves? board kingColor ) )
   )
)

;; Versión auxiliar de IsValidMove para verificación de jaque
( define ( IsValidMoveAux? board initialPosition finalPosition turn )
   ( define piece ( getPiece board initialPosition ) )
   ( define color ( getColor board initialPosition ) )
   
   ( cond
      [ ( and
          ( equal? color #\n )
          ( = ( remainder turn 2 ) 1 )
        )
        ( cond
           [ ( equal? piece #\a ) ( ValidateBlackPawn? board initialPosition finalPosition ) ]
           [ ( equal? piece #\t ) ( ValidateTowerBlack? board initialPosition finalPosition ) ]
           [ ( equal? piece #\c ) ( ValiteHorseBlack? board initialPosition finalPosition ) ]
           [ ( equal? piece #\d ) ( ValidateBishopBlack? board initialPosition finalPosition ) ]
           [ ( equal? piece #\e ) ( ValiteQueenBlack? board initialPosition finalPosition ) ]
           [ ( equal? piece #\f ) ( ValiteKingBlack? board initialPosition finalPosition ) ]
           [ else #f ]
        )
      ]
      [ ( and
          ( equal? color #\w )
          ( = ( remainder turn 2 ) 0 )
        )
        ( cond
           [ ( equal? piece #\a ) ( ValidateWhitePawn? board initialPosition finalPosition ) ]
           [ ( equal? piece #\t ) ( ValidateTowerWhite? board initialPosition finalPosition ) ]
           [ ( equal? piece #\c ) ( ValiteHorseWhite? board initialPosition finalPosition ) ]
           [ ( equal? piece #\d ) ( ValidateBishopWhite? board initialPosition finalPosition ) ]
           [ ( equal? piece #\e ) ( ValiteQueenWhite? board initialPosition finalPosition ) ]
           [ ( equal? piece #\f ) ( ValiteKingWhite? board initialPosition finalPosition ) ]
           [ else #f ]
        )
      ]
      [ else #f ]
   )
)

;; Modificación de la función IsValidMove para considerar el jaque
( define ( IsValidMove? board initialPosition finalPosition turn )
   ( define piece ( getPiece board initialPosition ) )
   ( define color ( getColor board initialPosition ) )
   
   ( if ( not ( or 
                ( ValiteCheckMate? board #\n )
                ( ValiteCheckMate? board #\w )
              ) )
        ( cond
           [ ( and
               ( equal? color #\n )
               ( = ( remainder turn 2 ) 1 )
             )
             ( and
                ( cond
                   [ ( equal? piece #\a ) ( ValidateBlackPawn? board initialPosition finalPosition ) ]
                   [ ( equal? piece #\t ) ( ValidateTowerBlack? board initialPosition finalPosition ) ]
                   [ ( equal? piece #\c ) ( ValiteHorseBlack? board initialPosition finalPosition ) ]
                   [ ( equal? piece #\d ) ( ValidateBishopBlack? board initialPosition finalPosition ) ]
                   [ ( equal? piece #\e ) ( ValiteQueenBlack? board initialPosition finalPosition ) ]
                   [ ( equal? piece #\f ) ( ValiteKingBlack? board initialPosition finalPosition ) ]
                   [ else #f ]
                )
                ( SimulateMove board initialPosition finalPosition color )
             )
           ]
           [ ( and
               ( equal? color #\w )
               ( = ( remainder turn 2 ) 0 )
             )
             ( and
                ( cond
                   [ ( equal? piece #\a ) ( ValidateWhitePawn? board initialPosition finalPosition ) ]
                   [ ( equal? piece #\t ) ( ValidateTowerWhite? board initialPosition finalPosition ) ]
                   [ ( equal? piece #\c ) ( ValiteHorseWhite? board initialPosition finalPosition ) ]
                   [ ( equal? piece #\d ) ( ValidateBishopWhite? board initialPosition finalPosition ) ]
                   [ ( equal? piece #\e ) ( ValiteQueenWhite? board initialPosition finalPosition ) ]
                   [ ( equal? piece #\f ) ( ValiteKingWhite? board initialPosition finalPosition ) ]
                   [ else #f ]
                )
                ( SimulateMove board initialPosition finalPosition color )
             )
           ]
           [ else #f ]
        )
        #f  ;; No permitir movimientos si hay jaque mate
   )
)
; =============================================
; Función para mostrar jaque y jaque mate gráficamente
; =============================================

;; Mejora de la función ShowCheckStatus para mostrar mejor el jaque mate
( define ( ShowCheckStatus board viewBoard )
   ( define ( HighlightKing position color )
      ( define x-pos ( * ( remainder position 8 ) 60 ) )
      ( define y-pos ( * ( quotient position 8 ) 60 ) )
      ;; Resaltar el rey en jaque con un color más visible
      ( ( draw-solid-rectangle viewBoard ) ( make-posn x-pos y-pos ) 60 60 
         ( if ( equal? color #\w ) "pink" "red" ) )
      ( drawPieces board ( + ( * position 4 ) 3 ) ( + ( * position 4 ) 2 ) viewBoard )
   )
   ( cond
      [ ( ValiteCheckMate? board #\w )
        ( HighlightKing ( SearchKingPosition board #\w ) #\w )
        ( ( draw-solid-rectangle viewBoard ) ( make-posn 500 400 ) 160 50 "khaki"  )
        ( ( draw-string viewBoard ) ( make-posn 518 420 ) "¡JAQUE MATE!" "red" )
        ( ( draw-string viewBoard ) ( make-posn 510 440 ) "Ganan las negras" "red" )
        ( sleep 2 )
        ( close-viewport viewBoard )
        ( sleep 2 )
        ( Main )
        #t ]
      [ ( ValiteCheckMate? board #\n )
        ( HighlightKing ( SearchKingPosition board #\n ) #\n )
        ( ( draw-solid-rectangle viewBoard ) ( make-posn 500 400 ) 160 50 "khaki"  )
        ( ( draw-string viewBoard ) ( make-posn 518 420 ) "¡JAQUE MATE!" "red" )
        ( ( draw-string viewBoard ) ( make-posn 510 440 ) "Ganan las Blancas" "red" )
        ( sleep 2 )
        ( close-viewport viewBoard )
        ( sleep 2 )
        ( Main )
        #t ]
      [ ( ValiteCheck? board #\w )
        ( HighlightKing ( SearchKingPosition board #\w ) #\w )
        ( ( draw-solid-rectangle viewBoard ) ( make-posn 500 400 ) 160 50 "khaki"  )
        ( ( draw-string viewBoard ) ( make-posn 520 420 ) "Rey blanco en " "red" )
        ( ( draw-string viewBoard ) ( make-posn 548 440 ) "JAQUE" "red" )
        #f ]
      [ ( ValiteCheck? board #\n )
        ( ( draw-solid-rectangle viewBoard ) ( make-posn 500 400 ) 160 50 "khaki"  )
        ( HighlightKing ( SearchKingPosition board #\n ) #\n )
        ( ( draw-string viewBoard ) ( make-posn 520 420 ) "Rey negro en" "red" )
        ( ( draw-string viewBoard ) ( make-posn 548 440 ) "JAQUE" "red" )
        #f ]
      [ else #f ]
   )
)
; ------------------------------------------------------- Funcion del caballo negro -------------------------------------------------------
( define ( ValiteHorseBlack? board initialPosition finalPosition )
   ( if ( or ( isEmpty? board finalPosition )
             ( equal?  ( getColor board finalPosition )  #\w )
        )
        ( or ( and ( = ( abs ( - ( quotient finalPosition 8 ) ( quotient initialPosition 8 ) ) ) 2 )
                   ( = ( abs ( - ( remainder finalPosition 8 ) ( remainder initialPosition 8 ) ) ) 1 )
              )
             ( and ( = ( abs ( - ( quotient finalPosition 8 ) ( quotient initialPosition 8 ) ) ) 1 )
                   ( = ( abs ( - ( remainder finalPosition 8 ) ( remainder initialPosition 8 ) ) ) 2 )
             )
        )
        ;else
        #f
   )
)
; ------------------------------------------------------- Funcion del caballo blanco -------------------------------------------------------
( define ( ValiteHorseWhite? board initialPosition finalPosition )
   ( if ( or ( isEmpty? board finalPosition )
             ( equal?  ( getColor board finalPosition )  #\n )
        )
        ( or ( and ( = ( abs ( - ( quotient finalPosition 8 ) ( quotient initialPosition 8 ) ) ) 2 )
                   ( = ( abs ( - ( remainder finalPosition 8 ) ( remainder initialPosition 8 ) ) ) 1 )
              )
             ( and ( = ( abs ( - ( quotient finalPosition 8 ) ( quotient initialPosition 8 ) ) ) 1 )
                   ( = ( abs ( - ( remainder finalPosition 8 ) ( remainder initialPosition 8 ) ) ) 2 )
             )
        )
        ;else
        #f
   )
)

; =============================================
; Funciones para actualizar el tablero
; =============================================

( define ( UpdateBoard board initialPosition finalPosition viewBoard turn )
   ( if ( IsValidMove? board initialPosition finalPosition turn )
        ( begin
           ( ( clear-string viewBoard ) ( make-posn 520 420 ) "Rey blanco en " )
           ( ( clear-string viewBoard ) ( make-posn 520 420 ) "Rey negro en " )
           ( ( clear-string viewBoard ) ( make-posn 548 440 ) "JAQUE"  )
           ( ( clear-rectangle viewBoard ) ( make-posn ( * ( remainder initialPosition 8 ) 60 ) ( * ( quotient initialPosition 8 ) 60 ) ) 60 60 )
           ( ( draw-solid-rectangle viewBoard ) ( make-posn 500 400 ) 160 50 "olive" )
           ( ChangeString board initialPosition finalPosition viewBoard ) 
        );end begin
        ;else
        board
        );end if
   );end updateBoard
( define ( ChangeString board positionOne positionTwo viewBoard )
   ( DrawTwoRectangle positionOne viewBoard )
   ( DrawTwoRectangle positionTwo viewBoard )
   ( if ( < positionOne positionTwo )
        ( ChangeStringAuxOne board positionOne positionTwo viewBoard )
        ;else
        ( ChangeStringAuxTwo board positionOne positionTwo viewBoard )
   );end if ( < positionOne positionTwo )
   
);Changestring
( define ( ChangeStringAuxOne board positionOne positionTwo viewBoard )
   ( define partOne ( substring board 0 ( + ( * positionOne 4 ) 2 ) ) )
   ( define partTwo ( substring board ( + ( * positionOne 4 ) 2 ) ( + ( * positionOne 4 ) 4 ) ) )
   ( define emptyCell ( make-string 2 #\g ) )
   ( define partThree ( substring board ( + ( * positionOne 4 ) 4 ) ( + ( * positionTwo 4 ) 2 ) ) )
   ( define partFour ( substring board ( + ( * positionTwo 4 ) 4 ) ( string-length board ) ) )
   ( define newBoard ( string-append partOne emptyCell partThree partTwo partFour ) )
   ( drawPieces newBoard ( + ( * positionTwo 4 ) 3 ) ( + ( * positionTwo 4 ) 2 ) viewBoard )
   newBoard
   ); changeStringAuxOne

( define ( ChangeStringAuxTwo board positionOne positionTwo viewBoard )
   ( define partOne ( substring board 0 ( + ( * positionTwo 4 ) 2 ) ) )
   ( define partTwo ( substring board ( + ( * positionOne 4 ) 2 ) ( + ( * positionOne 4 ) 4 ) ) )
   ( define emptyCell ( make-string 2 #\g ) )
   ( define partThree ( substring board ( + ( * positionTwo 4 ) 4 ) ( + ( * positionOne 4 ) 2 ) ) )
   ( define partFour ( substring board ( + ( * positionOne 4 ) 4 ) ( string-length board ) ) )
   ( define newBoard ( string-append partOne partTwo partThree emptyCell partFour ) )
   ( drawPieces newBoard ( + ( * positionTwo 4 ) 3 ) ( + ( * positionTwo 4 ) 2 ) viewBoard )
   newBoard
   ); changeStringAuxTwo

; =============================================
; Funciones para dibujar el tablero
; =============================================
( define ( DrawTwoRectangle position viewBoard )
   ( cond
      [ 
       ( = ( remainder ( + ( remainder position 8) ( quotient position 8 ) ) 2 ) 0 )
       ( ( draw-solid-rectangle viewBoard ) ( make-posn ( * ( remainder position 8 ) 60 ) ( * ( quotient position 8 ) 60 ) )  60 60 "khaki"  )
      ]
      [ ( = ( remainder ( + ( remainder position 8 ) ( quotient position 8 ) ) 2 ) 1 )
        ( ( draw-solid-rectangle viewBoard ) ( make-posn ( * ( remainder position 8 ) 60 ) ( * ( quotient position 8 ) 60 ) )  60 60 "olive"  )
      ]
   )
); end DrawTwoRectangle
                 
( define ( drawPieces board colorCounter pieceCounter viewBoard )
   ( cond
      [ ( equal? ( string-ref board colorCounter ) #\n )
        ( cond
           [ (equal? ( string-ref board pieceCounter ) #\a )
             ( ( ( draw-pixmap-posn "imagenes/peon-negro.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) ) ]
           [ ( equal? (string-ref board pieceCounter ) #\t )
             ( ( ( draw-pixmap-posn "imagenes/torre-negra.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) )
           ]
           [ ( equal? (string-ref board pieceCounter) #\c )
             ( ( ( draw-pixmap-posn "imagenes/caballo-negro.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) )
           ]
           [ ( equal? (string-ref board pieceCounter) #\d)
             ( ( ( draw-pixmap-posn "imagenes/alfil-negro.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                          ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) )
           ]
           [ ( equal? (string-ref board pieceCounter) #\e)
             ( ( ( draw-pixmap-posn "imagenes/reina-negra.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) )
           ]
           [ ( equal? (string-ref board pieceCounter) #\f)
             ( ( ( draw-pixmap-posn "imagenes/rey-negro.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) )
           ]
        )
      ]
      [ ( equal? ( string-ref board colorCounter ) #\w )
        ( cond
           [ ( equal? ( string-ref board pieceCounter ) #\a )
             ( ( ( draw-pixmap-posn "imagenes/peon-blanco.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) )
             ]
           [ ( equal? ( string-ref board pieceCounter ) #\t )
             ( ( ( draw-pixmap-posn "imagenes/torre-blanca.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) ) ]
           [ ( equal? ( string-ref board pieceCounter) #\c )
             ( ( ( draw-pixmap-posn "imagenes/caballo-blanco.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) )
             ]
           [ ( equal? ( string-ref board pieceCounter) #\d)
             ( ( ( draw-pixmap-posn "imagenes/alfil-blanco.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) )
             ]
           [ ( equal? ( string-ref board pieceCounter) #\e)
             ( ( ( draw-pixmap-posn "imagenes/reina-blanca.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) )
             ]
           [ ( equal? ( string-ref board pieceCounter ) #\f )
             ( ( ( draw-pixmap-posn "imagenes/rey-blanco.png" ) viewBoard )
               ( make-posn ( * 60 ( remainder ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) )
                           ( * 60 ( quotient ( string->number ( substring board ( - pieceCounter 2 ) ( - pieceCounter 0 ) ) ) 8 ) ) ) )
             ]
           )
        ]
      [ else ( void ) ] )
   )
( define ( PrintFullBoard board counter colorCounter pieceCounter end viewBoard ) 
   ( if ( = end counter )
        ( void )
        ;else
        ( begin ( drawPieces board colorCounter pieceCounter viewBoard )
                ( PrintFullBoard board ( + counter 1 ) ( + colorCounter 4 ) ( + pieceCounter 4 ) end viewBoard )
                );end begin
        )
   )
( define ( DrawTotalBoard counterOne counterTwo CounterThree end viewBoard )
   ( if ( < counterOne 8  )
        ( cond
           [ ( = end CounterThree ) ( void ) ]
           [ ( and ( = ( remainder counterOne 2 ) 0 )
                   ( = ( remainder counterTwo 2 ) 1 )
                   )
             ( ( draw-solid-rectangle viewBoard ) ( make-posn ( * counterOne 60 ) ( * counterTwo 60 ) )  60 60 "olive"  )
             ( DrawTotalBoard  ( + counterOne 1 ) counterTwo ( + CounterThree 1 ) end viewBoard )
             ]  
           [ ( and ( = ( remainder counterOne 2 ) 1 )
                     ( = ( remainder counterTwo 2 ) 1 )
                     )
             ( ( draw-solid-rectangle viewBoard ) ( make-posn ( * counterOne 60 ) ( * counterTwo 60 ) )  60 60 "khaki"  )
             ( DrawTotalBoard  ( + counterOne 1 ) counterTwo ( + CounterThree 1 ) end viewBoard )
             ]
           [ ( and ( = ( remainder counterOne 2 ) 0 )
                   ( = ( remainder counterTwo 2 ) 0 )
                   )
             ( ( draw-solid-rectangle viewBoard ) ( make-posn ( * counterOne 60 ) ( * counterTwo 60 ) )  60 60 "khaki"  )
             ( DrawTotalBoard  ( + counterOne 1 ) counterTwo  ( + CounterThree 1 ) end viewBoard )
             ]
           [ ( and ( = ( remainder counterOne 2 ) 1 )
                   ( = ( remainder counterTwo 2 ) 0 )
             )
             ( ( draw-solid-rectangle viewBoard ) ( make-posn ( * counterOne 60 ) ( * counterTwo 60 ) )  60 60 "olive"  )
             ( DrawTotalBoard  ( + counterOne 1 ) counterTwo ( + CounterThree 1 ) end viewBoard )
           ]
        )
        ;else
        ( DrawTotalBoard ( - counterOne 8 ) ( + 1 counterTwo ) CounterThree end viewBoard )
        )
);end DrawTotalBoard
; =============================================
; Función principal del juego
; =============================================
   
( define ( Main )
   ( open-graphics )
   ( define viewBoard ( open-viewport "Ajedrez" 680 480 ) )
   ( ( ( draw-pixmap-posn "imagenes/barra.png" ) viewBoard )
     ( make-posn  480 0 ) )
   ( DrawTotalBoard 0 0 0 64 viewBoard )
   ( PrintFullBoard initialBoard 0 3 2 64 viewBoard )
   ( ( draw-rectangle viewBoard ) ( make-posn 483 0 ) 194 478 "khaki")
   ( ( draw-rectangle viewBoard ) ( make-posn 506 329 ) 149 57 "khaki")
   ( define ( gameLoopAux board viewBoard turn )
      ( if ( = ( remainder turn 2 ) 0 )
           ( begin
              ( ( draw-solid-rectangle viewBoard ) ( make-posn 535 245 ) 80 20 "olive" ) 
              ( ( draw-string viewBoard ) ( make-posn 545 260 ) "blancas" )
           )
           ;else
           ( begin
              ( ( draw-solid-rectangle viewBoard ) ( make-posn 535 245 ) 80 20 "olive" ) 
              ( ( draw-string viewBoard ) ( make-posn 545 260 ) "negras" )
           )
      )
      ( define initialClick ( get-mouse-click viewBoard ) )
      ( define initialPosition ( + ( quotient ( posn-x ( mouse-click-posn initialClick ) ) 60 ) 
                                   ( * ( quotient ( posn-y ( mouse-click-posn initialClick ) ) 60 ) 8 ) ) )
      ( sleep 0.2 )
      ( define endClick ( get-mouse-click viewBoard ) )
      ( define finalPosition ( + ( quotient ( posn-x ( mouse-click-posn endClick ) ) 60 ) 
                                 ( * ( quotient ( posn-y ( mouse-click-posn endClick ) ) 60 ) 8 ) ) )
      ( if ( or
             ( and
               ( >= ( posn-y ( mouse-click-posn initialClick ) ) 330 )
               ( <= ( posn-y ( mouse-click-posn initialClick ) ) 385 )
               ( >= ( posn-x ( mouse-click-posn initialClick ) ) 508 )
               ( <= ( posn-x ( mouse-click-posn initialClick ) ) 653 )
             )
             ( and
               ( >= ( posn-y ( mouse-click-posn endClick ) ) 330 )
               ( <= ( posn-y ( mouse-click-posn endClick ) ) 385 )
               ( >= ( posn-x ( mouse-click-posn endClick ) ) 508 )
               ( <= ( posn-x ( mouse-click-posn endClick ) ) 653 )
             )
           )
           ( begin
              ( close-viewport viewBoard )
              ( sleep 2 )
              ( Main )
           );end begin
           ;else
           ( void )
      );end if
      ( define newBoard ( UpdateBoard board initialPosition finalPosition viewBoard turn ) )
              ( define newTurn ( if ( equal? board newBoard ) turn ( + turn 1 ) ) )
              ( define newRealBoard ( Coronation 0 newBoard turn viewBoard ) )
              ;; Continuar con el siguiente turno
              ( gameLoop newRealBoard viewBoard newTurn )
   )   
   ( define ( gameLoop board viewBoard turn )
      ;; Verificar jaque/jaque mate al inicio de cada turno
      ( define gameOver ( ShowCheckStatus board viewBoard ) )
      ( printf "~a" turn )
      ( if gameOver
          (void)  ;; Terminar el juego si hay jaque mate
          ( gameLoopAux board viewBoard turn )
      )
      
   )
   
   ( gameLoop initialBoard viewBoard 0 )
)
( Main )
; Iniciar el juego
