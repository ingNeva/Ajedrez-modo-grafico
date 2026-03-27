# ♟️ Ajedrez en Racket

<div align="center">

![Racket](https://img.shields.io/badge/Lenguaje-Racket%208.16-9B4F96?style=for-the-badge&logo=racket&logoColor=white)
![Status](https://img.shields.io/badge/Estado-Funcional-4CAF50?style=for-the-badge)
![Players](https://img.shields.io/badge/Jugadores-2%20Locales-blue?style=for-the-badge)
![License](https://img.shields.io/badge/Licencia-Académica-orange?style=for-the-badge)

**Juego de ajedrez completo para dos jugadores, desarrollado en paradigma funcional con Racket.**

*Proyecto académico — Universidad Tecnológica de Pereira*  
*Ingeniería de Sistemas y Computación*

---

</div>

## 👤 Autor

| Campo | Detalle |
|-------|---------|
| **Nombre** | Diego Alexander Neva Patiño |
| **Título** | Ingeniería de Sistemas y Computación (en curso) |
| **Universidad** | Universidad Tecnológica de Pereira |
| **Docente** | Doctor Ricardo Moreno Laverde |
| **Versión Racket** | 8.16 |

---

## 📋 Descripción General

Este proyecto implementa un **juego de ajedrez completo** para dos jugadores en Racket, usando programación funcional pura. El tablero se representa como una **cadena de texto codificada**, y todas las operaciones —movimientos, validaciones, jaque, jaque mate y coronación— se realizan mediante funciones recursivas sin mutación de estado.

### ✅ Reglas implementadas

- ♟ Movimientos válidos para **todas las piezas**
- 👑 Detección de **jaque** con resaltado visual
- ☠️ Detección de **jaque mate** con mensaje en pantalla
- 🎖️ **Coronación de peones** (blancas y negras) con selección interactiva de pieza
- 🔄 Turnos alternados entre jugador blanco y jugador negro
- 🔁 Reinicio automático al hacer clic en "Terminar"

---

## 🗂️ Estructura del Proyecto

```
ajedrez/
│
├── ajedres_1_0.rkt          # Código fuente principal
│
└── imagenes/
    ├── peon-blanco.png
    ├── peon-negro.png
    ├── torre-blanca.png
    ├── torre-negra.png
    ├── caballo-blanco.png
    ├── caballo-negro.png
    ├── alfil-blanco.png
    ├── alfil-negro.png
    ├── reina-blanca.png
    ├── reina-negra.png
    ├── rey-blanco.png
    ├── rey-negro.png
    ├── barra.png
    ├── coronacionblancas.png
    └── coronacionnegras.png
```

> ⚠️ **Importante:** La carpeta `imagenes/` debe estar en el mismo directorio que el archivo `.rkt`.

---

## 🧠 Arquitectura del Sistema

```
┌─────────────────────────────────────────────────────────────┐
│                      FUNCIÓN Main()                         │
│  Abre ventana gráfica → Dibuja tablero → Inicia gameLoop    │
└──────────────────────────┬──────────────────────────────────┘
                           │
              ┌────────────▼────────────┐
              │      gameLoop()         │
              │  Verifica jaque/mate    │
              │  → gameLoopAux()        │
              └────────────┬────────────┘
                           │
         ┌─────────────────▼──────────────────────┐
         │           gameLoopAux()                 │
         │  Lee 2 clics → posición inicial/final   │
         │  → UpdateBoard() → Coronation()         │
         └─────────────────┬──────────────────────┘
                           │
    ┌──────────────────────▼──────────────────────────┐
    │                 UpdateBoard()                    │
    │  IsValidMove?() → ChangeString() → drawPieces() │
    └──────────────────────┬──────────────────────────┘
                           │
         ┌─────────────────▼──────────────────┐
         │           IsValidMove?()            │
         │  Valida pieza + turno + SimulateMove│
         └─────────────────────────────────────┘
```

---

## 🔢 Codificación del Tablero

El tablero es una **cadena de 256 caracteres** (64 casillas × 4 caracteres por casilla):

```
Formato por casilla:  [PP][T][C]
                       ^^  ^  ^
                       │   │  └── Color:  n = negro | w = blanco | g = vacío
                       │   └───── Tipo:   a=peón  t=torre  c=caballo
                       │                  d=alfil  e=reina  f=rey  g=vacío
                       └───────── Posición: 00–63 (dos dígitos)
```

### Ejemplo — Tablero Inicial (primeras 4 casillas):

```
"00tn 01cn 02dn 03en ..."
  ^^  ^^  ^^  ^^
  │    │   │   └── Reina negra en posición 03
  │    │   └────── Alfil negro en posición 02
  │    └────────── Caballo negro en posición 01
  └─────────────── Torre negra en posición 00
```

### Mapa de posiciones del tablero (0–63):

```
  Col: 0   1   2   3   4   5   6   7
Fil 0: 00  01  02  03  04  05  06  07   ← Negras (fila trasera)
Fil 1: 08  09  10  11  12  13  14  15   ← Peones negros
Fil 2: 16  17  18  19  20  21  22  23
Fil 3: 24  25  26  27  28  29  30  31   ← Casillas vacías
Fil 4: 32  33  34  35  36  37  38  39
Fil 5: 40  41  42  43  44  45  46  47
Fil 6: 48  49  50  51  52  53  54  55   ← Peones blancos
Fil 7: 56  57  58  59  60  61  62  63   ← Blancas (fila trasera)
```

---

## ⚙️ Funciones Principales

### 🔧 Auxiliares del Tablero

| Función | Descripción |
|---------|-------------|
| `getPiece board pos` | Retorna el carácter de tipo de pieza en la posición dada |
| `getColor board pos` | Retorna el carácter de color de la pieza en esa posición |
| `isEmpty? board pos` | Verifica si una casilla está vacía (`#\g`) |

```racket
; Extrae el tipo de pieza: índice = posición × 4 + 2
(define (getPiece board position)
  (string-ref board (+ (* position 4) 2)))

; Extrae el color: índice = posición × 4 + 3
(define (getColor board position)
  (string-ref board (+ (* position 4) 3)))
```

---

### ♟ Validación de Movimientos por Pieza

#### Peón Negro (`ValidateBlackPawn?`)

```
Avanza hacia abajo (+8 por fila)

  [Inicial]
     ↓  ← avance simple (+8)
     ↓↓ ← avance doble desde fila 1 (+16)
   ↙   ↘  ← captura diagonal (+7 ó +9)
```

| Caso | Condición |
|------|-----------|
| Avance simple | `finalPos = initialPos + 8` y casilla vacía |
| Avance doble | Solo desde fila 1 (pos 8–15), casillas intermedias vacías |
| Captura diagonal izq | `finalPos = initialPos + 7`, hay pieza blanca |
| Captura diagonal der | `finalPos = initialPos + 9`, hay pieza blanca |

#### Peón Blanco (`ValidateWhitePawn?`)

Lógica simétrica, avanza hacia arriba (−8 por fila). Avance doble desde fila 6 (pos 48–55).

---

#### Torre (`ValidateTowerBlack?` / `ValidateTowerWhite?`)

```
   ↑
   │
←──♜──→   Movimiento horizontal y vertical
   │       Sin saltar piezas intermedias
   ↓
```

Implementado con 4 funciones recursivas de recorrido lineal:
- `isValidLinearMoveOne?` — hacia la derecha
- `isValidLinearMoveTwo?` — hacia la izquierda  
- `isValidLinearMoveThree?` — hacia abajo
- `isValidLinearMoveFour?` — hacia arriba

---

#### Alfil (`ValidateBishopBlack?` / `ValidateBishopWhite?`)

```
↖       ↗
  ╲   ╱
    ♗       Movimiento diagonal
  ╱   ╲    Sin saltar piezas intermedias
↙       ↘
```

4 funciones de recorrido diagonal (`ValiteDiagonalMoveOne?` … `Four?`), con pasos de ±7 y ±9.

---

#### Reina (`ValiteQueenBlack?` / `ValiteQueenWhite?`)

```racket
; Combina torre + alfil (unión de sus movimientos)
(define (ValiteQueenBlack? board initialPosition finalPosition)
  (or (ValidateTowerBlack? board initialPosition finalPosition)
      (ValidateBishopBlack? board initialPosition finalPosition)))
```

---

#### Rey (`ValiteKingBlack?` / `ValiteKingWhite?`)

```
Un paso en cualquier dirección:
   ↖ ↑ ↗
   ← ♔ →
   ↙ ↓ ↘

Condición: |Δfila| ≤ 1  y  |Δcol| ≤ 1
```

---

#### Caballo (`ValiteHorseBlack?` / `ValiteHorseWhite?`)

```
Movimiento en "L":

  . X . X .
  X . . . X
  . . ♞ . .
  X . . . X
  . X . X .

Condición: (|Δfila|=2 y |Δcol|=1) ó (|Δfila|=1 y |Δcol|=2)
```

---

### 🔍 Sistema de Jaque y Jaque Mate

```
┌─────────────────────────────────────────┐
│          SearchKingPosition()           │
│  Recorre el tablero buscando #\f        │
│  del color indicado → retorna posición  │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│             ValiteCheck?()              │
│  Para cada pieza enemiga:               │
│  ¿Puede atacar la posición del rey?     │
│  Usa IsValidMoveAux?() sin restricción  │
│  de turno                               │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│           SimulateMove()                │
│  Crea tablero temporal con             │
│  ChangeStringTemporary()               │
│  Verifica si el rey sigue en jaque     │
│  después del movimiento                │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│          HaveLegalMoves?()              │
│  Para cada pieza aliada:               │
│  ¿Existe algún movimiento válido que   │
│  saque al rey del jaque?               │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│          ValiteCheckMate?()             │
│  = ValiteCheck? AND NOT HaveLegalMoves? │
└─────────────────────────────────────────┘
```

---

### 🎖️ Coronación de Peones

Cuando un peón llega al extremo opuesto del tablero, se abre una ventana secundaria:

```
┌──────────────────────┐
│  TORRE    [y ≤ 160]  │
├──────────────────────┤
│  CABALLO  [y ≤ 300]  │
├──────────────────────┤
│  ALFIL    [y ≤ 420]  │
├──────────────────────┤
│  REINA    [y ≤ 600]  │
└──────────────────────┘
```

- **Blancas**: peón llega a fila 0 (posiciones 0–7)  
- **Negras**: peón llega a fila 7 (posiciones 56–63)

La función `Coronation` recorre el tablero buscando estos casos y llama a `CoronationWhite` o `CoronationBlack` según corresponda.

---

### 🎨 Sistema Gráfico

Construido sobre la biblioteca `graphics/graphics` de Racket.

| Componente | Descripción |
|-----------|-------------|
| `DrawTotalBoard` | Dibuja las 64 casillas con colores `"olive"` / `"khaki"` alternados |
| `PrintFullBoard` | Itera sobre el tablero y llama `drawPieces` para cada casilla |
| `drawPieces` | Carga el `.png` correspondiente y lo posiciona en `(col×60, fila×60)` |
| `DrawTwoRectangle` | Redibuja una casilla con su color correcto (limpieza visual) |
| `HighlightKing` | Resalta la casilla del rey cuando está en jaque |

#### Cálculo de posición en pantalla:
```
x_pixel = (posición % 8) × 60   ← columna
y_pixel = (posición ÷ 8) × 60   ← fila
```

---

## 🖥️ Interfaz de Juego

```
┌──────────────────────────────────────────────┐
│  Tablero 480×480px         │  Panel 200px    │
│                            │                 │
│  ┌──┬──┬──┬──┬──┬──┬──┬──┐│  ┌───────────┐ │
│  │♜ │♞ │♝ │♛ │♚ │♝ │♞ │♜ ││  │  Ajedrez  │ │
│  ├──┼──┼──┼──┼──┼──┼──┼──┤│  │           │ │
│  │♟ │♟ │♟ │♟ │♟ │♟ │♟ │♟ ││  │ Turno de: │ │
│  ├──┼──┼──┼──┼──┼──┼──┼──┤│  │  blancas  │ │
│  │  │  │  │  │  │  │  │  ││  │           │ │
│  │  ... (casillas vacías) ││  │           │ │
│  │  │  │  │  │  │  │  │  ││  │ Terminar  │ │
│  ├──┼──┼──┼──┼──┼──┼──┼──┤│  └───────────┘ │
│  │♙ │♙ │♙ │♙ │♙ │♙ │♙ │♙ ││                │
│  ├──┼──┼──┼──┼──┼──┼──┼──┤│                │
│  │♖ │♘ │♗ │♕ │♔ │♗ │♘ │♖ ││                │
│  └──┴──┴──┴──┴──┴──┴──┴──┘│                │
└──────────────────────────────────────────────┘
```

**Cómo jugar:**
1. Haz clic en la pieza que deseas mover
2. Haz clic en la casilla destino
3. Si el movimiento es válido, la pieza se mueve
4. Si el movimiento deja al propio rey en jaque, se ignora
5. El turno pasa automáticamente al otro jugador
6. Clic en "Terminar" reinicia la partida

---

## 🚀 Instalación y Ejecución

### Requisitos

- [Racket 8.x](https://racket-lang.org/) instalado
- Biblioteca `graphics/graphics` (incluida en Racket estándar)

### Pasos

```bash
# 1. Clonar el repositorio
git clone https://github.com/tu-usuario/ajedrez-racket.git
cd ajedrez-racket

# 2. Verificar estructura de carpetas
ls imagenes/   # Debe mostrar los 14 archivos .png

# 3. Ejecutar desde DrRacket
#    Abrir ajedres_1_0.rkt → Ejecutar (Ctrl+R)

# O desde terminal:
racket ajedres_1_0.rkt
```

> ⚠️ La librería `graphics/graphics` requiere un entorno gráfico activo. No funciona en servidores headless sin configuración adicional.

---

## 📊 Resumen de Funciones por Módulo

```
MÓDULO 1 — Representación del tablero
  ├── initialBoard        : Cadena de estado inicial
  ├── getPiece            : Extrae tipo de pieza
  ├── getColor            : Extrae color de pieza
  └── isEmpty?            : Verifica casilla vacía

MÓDULO 2 — Validación de movimientos
  ├── ValidateBlackPawn?  : Reglas peón negro
  ├── ValidateWhitePawn?  : Reglas peón blanco
  ├── ValidateTowerBlack? : Reglas torre negra
  ├── ValidateTowerWhite? : Reglas torre blanca
  ├── ValidateBishopBlack?: Reglas alfil negro
  ├── ValidateBishopWhite?: Reglas alfil blanco
  ├── ValiteQueenBlack?   : Reglas reina negra
  ├── ValiteQueenWhite?   : Reglas reina blanca
  ├── ValiteKingBlack?    : Reglas rey negro
  ├── ValiteKingWhite?    : Reglas rey blanco
  ├── ValiteHorseBlack?   : Reglas caballo negro
  └── ValiteHorseWhite?   : Reglas caballo blanco

MÓDULO 3 — Movimientos lineales/diagonales
  ├── isValidLinearMoveOne?   … Four?
  └── ValiteDiagonalMoveOne?  … Four?

MÓDULO 4 — Jaque y jaque mate
  ├── SearchKingPosition  : Localiza el rey
  ├── ValiteCheck?        : Detecta jaque
  ├── SimulateMove        : Simula movimiento temporal
  ├── HaveLegalMoves?     : Verifica si hay salidas al jaque
  ├── ValiteCheckMate?    : Detecta jaque mate
  └── IsValidMoveAux?     : Validación sin restricción de turno

MÓDULO 5 — Actualización del tablero
  ├── IsValidMove?            : Validación completa con turno
  ├── UpdateBoard             : Aplica movimiento si es válido
  ├── ChangeString            : Modifica la cadena del tablero
  ├── ChangeStringAuxOne/Two  : Auxiliares de reordenamiento
  └── ChangeStringTemporary   : Crea copia para simulación

MÓDULO 6 — Coronación
  ├── Coronation          : Detecta peones elegibles
  ├── CoronationWhite     : Selección interactiva piezas blancas
  └── CoronationBlack     : Selección interactiva piezas negras

MÓDULO 7 — Gráficos
  ├── DrawTotalBoard      : Dibuja las 64 casillas
  ├── PrintFullBoard      : Renderiza todas las piezas
  ├── drawPieces          : Dibuja una pieza individual
  ├── DrawTwoRectangle    : Limpia/redibuja una casilla
  ├── HighlightKing       : Resalta rey en jaque
  └── ShowCheckStatus     : Muestra estado de jaque/mate

MÓDULO 8 — Control del juego
  ├── Main                : Punto de entrada
  ├── gameLoop            : Bucle principal
  └── gameLoopAux         : Manejo de turnos y clics
```

---

## 🐛 Limitaciones Conocidas

| Limitación | Descripción |
|-----------|-------------|
| Enroque | No implementado |
| Captura al paso | No implementada |
| Ahogado (stalemate) | No detectado; el juego continúa |
| IA | Solo modo 2 jugadores locales |
| Historial de movimientos | No se registra |
| Validación de clic fuera del tablero | Puede generar posiciones inválidas |

---

## 📐 Decisiones de Diseño

### ¿Por qué cadena de texto para el tablero?

En Racket funcional se evita la mutación. Una cadena inmutable que se reconstruye en cada movimiento es consistente con el paradigma funcional. Las funciones de actualización devuelven una **nueva cadena**, no modifican la existente.

### ¿Por qué recursión en lugar de bucles?

Racket favorece la recursión de cola. Funciones como `HaveLegalMoves?`, `SearchKingPosition` y `PrintFullBoard` usan recursión acumulativa, equivalente en eficiencia a un bucle.

---

## 📄 Licencia

Proyecto académico desarrollado para la **Universidad Tecnológica de Pereira**.  
Uso educativo. No distribuir con fines comerciales sin autorización del autor.

---

<div align="center">

Hecho con ♟️ y mucho `cond` por **Diego Alexander Neva Patiño**  
Universidad Tecnológica de Pereira — 2024/2025

</div>
