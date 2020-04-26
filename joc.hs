--000000000000000000000000000000000000000000000000000000000
--00000000000000000  CONNECT 4 GAME  0000000000000000000000
--000000000000000000000000000000000000000000000000000000000
--0000000000000000  BY MARCOS GÓMEZ  0000000000000000000000
--000000000000000000000000000000000000000000000000000000000
--000000000  FIB,  LENGUAJES DE PROGRAMACION  0000000000000
--000000000000000000000000000000000000000000000000000000000

import System.Random

{-
Tree a: árbol genérico de elementos de tipo a
Un Tree consta de:
- Un nodo con su respectivo elemento de tipo a
- Una lista de árboles que representan los hijos del nodo
-}
data Tree a = Node a [Tree a] deriving (Show)

{-
Board: Un tablero es un sinónimo de [Col], es decir, es una lista de columnas
Cada posición del tablero puede ser:
  0 (empty) => casilla vacía
  1 (human) => casilla con ficha del jugador humano
  2 (npc)   => casilla con ficha del npc (non-playable character)
-}
type Board = [Col]

{-
Col: Una columna es un sinónimo de [Int], es decir, es una lista de enteros
Los enteros de la lista son las casillas de la columna, ya explicadas en la definición de Board
-}
type Col = [Int]

{-
Strategy: Una estrategia es un sinónimo de (Board -> IO Int)
Es una función que, dado un tablero, devuelve la columna donde el npc debe colocar una ficha según el algoritmo que encapsula
La columna se devuelve encapsulada en una mónada IO
-}
type Strategy = Board -> IO Int

{-
human: función sinónimo del entero 1
Representa el jugador humano y es el valor que se asigna a las posiciones del tablero que el humano ha ocupado
-}
human :: Int
human = 1

{-
npc: función sinónimo del entero 2
Representa el jugador computador (npc) y es el valor que se asigna a las posiciones del tablero que el npc ha ocupado
-}
npc :: Int
npc = 2

{-
empty: función sinónimo de 0
Representa el valor que se asigna a las posiciones del tablero vacías
-}
empty :: Int
empty = 0

{-
switchP:
Dado un jugador, devuelve a su enemigo en el juego
-}
switchP :: Int -> Int
switchP p
  | (p == human) = npc
  | (p == npc)   = human
{-
createBoard:
Dados dos enteros (n, m) que representan filas y columnas respectivamente
Devuelve un tablero de tamaño n x m vacío (todas sus casillas son empty)
-}
createBoard :: Int -> Int -> Board
createBoard n m = take m $ repeat $ take n $ repeat empty

{-
nullBoard: función sinónimo del tablero con 0 filas y 0 columnas
(Útil para la estrategia smart)
-}
nullBoard :: Board
nullBoard = createBoard 0 0

--00000000000000000000000000000000000000000000000000000000000000000000000
--00000000000000000000000000000  MAIN  0000000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

{-
main: función principal del programa
El usuario inicializa el tablero, escoge estrategia de npc y elige quién inicia la partida
Se hace la primera llamada a la función 'play', con el tablero vacío, el jugador que empieza y la estrategia elegida como parámetros
Cuando la partida finaliza se escribe por la salida estándar el ganador de la partida o si ha acabado en empate
-}
main :: IO ()
main = do
  putStrLn "Welcome to the Connect4 game"
  b <- initBoard
  s <- chooseStrategy
  p <- whoStarts
  showBoard b
  putStrLn "Initial Board"
  winner <- play b p s
  case winner of
    0 -> putStrLn "Draw"
    1 -> putStrLn "You win!"
    2 -> putStrLn "CPU wins!"
    otherwise -> putStrLn "Error. Something were wrong"
  return ()

--00000000000000000000000000000000000000000000000000000000000000000000000
--0000000000000000000000  INITIALIZE GAME  000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

{-
initBoard:
Devuelve un tablero vacío de n filas y m columnas, en una mónada IO
n y m se introducen por la entrada estándar
Tanto n como m deben ser mayores o iguales que 4 para que pueda haber un ganador
En caso de introducir un valor menor que 4 la función se reinicia
-}
initBoard :: IO Board
initBoard = do
  putStrLn "Please introduce the number of rows of the board (min. 4)"
  rows <- getLine
  let n = read rows :: Int
  if (n < 4) then do
    putStrLn "\nERROR: You must introduce minimum 4 rows\n"
    initBoard
  else do
    putStrLn "Please introduce the number of columns of the board (min. 4)"
    cols <- getLine
    let m = read cols :: Int
    if (m < 4) then do
      putStrLn "\nERROR: You must introduce minimum 4 columns\n"
      initBoard
    else
      return $ createBoard n m

{-
chooseStrategy:
Devuelve una estrategia, en una mónada IO
La estrategia se introduce por la entrada estándar. Cada una de las siguientes posibles entradas tiene asociada una estrategia:
  0 => estrategia Random
  1 => estrategia Greedy
  2 => estrategia Smart
En caso de no introducir uno de estos valores (0,1,2) la función se reinicia
-}
chooseStrategy :: IO (Board -> IO Int)
chooseStrategy = do
  putStrLn "Please introduce the strategy of the CPU:"
  putStrLn "[0] Random (Easy)"
  putStrLn "[1] Greedy (Medium)"
  putStrLn "[2] Smart  (Hard)"
  str <- getLine
  let s = read str :: Int
  if (s > 2 || s < 0) then do
    putStrLn "\nERROR: You must introduce a number between [0..2]\n"
    chooseStrategy
  else do
    case s of
      0 -> return randomStrat
      1 -> return greedyStrat
      2 -> return smartStrat

{-
whoStarts:
Devuelve un jugador (entero) en una mónada IO
El jugador se introduce por la entrada estándar. Cada una de las siguientes posibles entradas tiene asociado un jugador:
  0 => human
  1 => npc
El jugador elegido es el que inicia la partida
En caso de no introducir uno de estos valores (0,1) la función se reinicia
-}
whoStarts :: IO Int
whoStarts = do
  putStrLn "Who starts the game?"
  putStrLn "[0] Human"
  putStrLn "[1] CPU"
  str <- getLine
  let p = read str :: Int
  if (p /= 0 && p /= 1) then do
    putStrLn "\nERROR: You must introduce minimum 0 or 1\n"
    whoStarts
  else return (p+1)

--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000000  PLAY  000000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

{-
play:
Dado un tablero, un jugador y una estrategia
Se actualiza el tablero colocando una ficha en una de sus columnas por parte del jugador indicado
Si el jugador es human el usuario introduce por la entrada estándar la columna donde desea tirar ficha
Si el usuario es npc e calcula la columna donde tirar ficha basándose en la estrategia indicada
Se comprueba si el nuevo tablero contiene victoria, empate o nada
Si hay victoria la función devuelve el jugador que ha ganado la partida (en una mónada IO)
Si hay empate la función devuelve el entero 0 (en una mónada IO)
Si no hay ni victoria ni empate se llama de nuevo a la función 'play' con el nuevo tablero, el jugador enemigo del actual y la misma estrategia como parámetros
Cada vez que npc actualiza el tablero éste se escribe por la salida estándar, mostrando así la última tirada de human y npc
-}
play :: Board -> Int -> Strategy -> IO Int
play b player strat
  | (player == human) = do
    putStrLn "Select a column"
    col <- getLine
    let c = read col :: Int
    let new_b = updateBoard b c player
    let w = checkWin new_b player
    if (w /= -1) then do
      showBoard new_b
      return w
    else play new_b (switchP player) strat

  | (player == npc) = do
      c <- strat b
      let new_b = updateBoard b c player
      showBoard new_b
      putStrLn $ "CPU has choosen the row "++(show c)
      let w = checkWin new_b player
      if (w /= -1) then
        return w
      else play new_b (switchP player) strat

--00000000000000000000000000000000000000000000000000000000000000000000000
--0000000000000000000000  PRINT THE BOARD  000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

{-
showBoard:
Dado un tablero, escribe por la salida estándar una representación del mismo
Devuelve una mónada IO con el tipo ()
Las casillas con fichas del humano se representan con el carácter 'o'
Las casillas con fichas del npc se representan con el carácter 'x'
Las casillas vacías se representan con el carácter '·'
La última fila del tablero indica el índice de cada columna (mod 10, por motivos de estética)
-}
showBoard :: Board -> IO()
showBoard (c:cs) = showBoardRec (c:cs) ((length c)-1)

{-
showBoardRec: función auxiliar de showBoard
Dado un tablero y un índice de fila n, escribe por la salida estándar las filas de n a 0
La primera fila en ser escrita es la n, y la última la 0
Cuando ha escrito todas las filas, escribe el índice de cada columna
Devuelve una mónada IO con el tipo ()
-}
showBoardRec :: Board -> Int -> IO ()
showBoardRec b (-1) = do
  printIndex 0 ((length b)-1)
  return ()
showBoardRec b n = do
  putStrLn $ printRow b n
  showBoardRec b (n-1)

{-
printRow: función auxiliar de showBoardRec
Dado un tablero y un índice de fila n
Devuelve un string que representa la fila n del tablero
-}
printRow :: Board -> Int -> String
printRow [] _ = ""
printRow (c:cs) n
  | ((c !! n) == empty) = "· " ++ (printRow cs n)
  | ((c !! n) == human) = "o " ++ (printRow cs n)
  | ((c !! n) == npc)   = "x " ++ (printRow cs n)

{-
printIndex:
Dados dos enteros (i, m)
Escribe por la salida estándar los números de i hasta m (mod 10) separados por espacios
Devuelve una mónada IO con el tipo ()
-}
printIndex :: Int -> Int -> IO ()
printIndex i m = do
  if (i <= m) then do
    putStr $ (show $ mod i 10)++" "
    printIndex (i+1) m
  else do
    putStrLn ""
    return ()

--000000000000000000000000000000000000000000000000000000000000000000000000
--0000000000000000000000  UPDATE THE BOARD  000000000000000000000000000000
--000000000000000000000000000000000000000000000000000000000000000000000000

{-
updateBoard:
Dado un tablero, una columna y un jugador
Devuelve el mismo tablero pero con una nueva ficha del jugador indicado en la columna indicada
Si la columna no existe o ya está llena devuelve el mismo tablero
Las fichas se añaden en la primera fila disponible, empezando por la fila 0
-}
updateBoard :: Board -> Int -> Int -> Board
updateBoard (c:cs) i p
  | (i > ((length cs))) = (c:cs)
  | (i < 0)             = (c:cs)
  | (i == 0)            = (u : cs)
  | otherwise           = (c : (updateBoard (cs) (i-1) p))
  where
    u = updateCol c p

{-
updateCol: función auxiliar de updateBoardRec
Dada una columna y un jugador
Devuelve la misma columna pero con una ficha del jugador indicado añadida en la columna en la primera fila disponible
-}
updateCol :: Col -> Int -> Col
updateCol [] _ = []
updateCol (x:xs) p
  | (x == empty)  = (p:xs)
  | otherwise     = (x:(updateCol xs p))

--00000000000000000000000000000000000000000000000000000000000000000000000
--0000000000000000000000000000 CHECK WIN  0000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

{-
checkWin:
Dado un tablaro y un jugador 'p'
Devuelve el estado del tablero. Los posibles estados son:
  0  => empate
  p  => el tablero contiene una victoria por parte del jugador indicado
  -1 => ninguno de los anteriores (no hay ni victoria ni empate)
-}
checkWin :: Board -> Int -> Int
checkWin b player
  | (checkWinVert b player /= -1)                                  = player
  | (checkWinHori b 0 player /= -1)                                = player
  | (checkWinDia1 b 0 3 (length b) player /= -1)                   = player
  | (checkWinDia2 b 0 (length (b !! 0)-4) (length b) player /= -1) = player
  | (checkDraw b)                                                  = 0
  | otherwise                                                      = -1

{-
checkDraw: función auxiliar de checkWin
Dado un tablero
Devuelve un booleano que indica si el estado del tablero es un empate
  True  => Hay un empate (No hay ninguna casilla empty)
  False => No hay empate (Hay alguna casilla empty)
-}
checkDraw :: Board -> Bool
checkDraw [] = True
checkDraw (c:cs)
  | (elem empty c) = False
  | otherwise = checkDraw cs

{-
checkWinDia1: función auxiliar de checkWin
Dado un tablero, un índice de columna, un índice de fila, el número de columnas del tablero y un jugador 'p'
Devuelve el estado del tablero, que puede ser:
  p  => el tablero contiene una victoria por parte del jugador indicado
  -1 => no hay victoria
Sólo se comprueban posibles victorias en las diagonales con dirección descendente
No se comprueban las esquinas del tablero donde no se pueden obtener 4 casillas contiguas
Los índices de columna y fila sirven para calcular recursivamente el valor en sub-tableros del tablero inicial
-}
checkWinDia1 :: Board -> Int -> Int -> Int -> Int -> Int
checkWinDia1 (c:cs) col row n p
  | (col == n - 3)                         = -1
  | (checkWinOneDia1 (c:cs) row p 0 /= -1) = p
  | (row == (length c)-1)                  = checkWinDia1 cs    (col+1) row    n p
  | (col == 0)                             = checkWinDia1 (c:cs) col   (row+1) n p

{-
checkWinOneDia1: función auxiliar de checkWinDia1
Dado un tablero, un índice de fila, un jugador 'p' y un entero 'count'
Devuelve el estado del tablero, que puede ser:
  p  => el tablero contiene una victoria por parte del jugador indicado (count = 4)
  -1 => no hay victoria (count < 4)
El valor inicial de count es 0
Sólo se comprueba la diagonal del tablero de sentido descendiente que comienza en la primera columna del tablero dado y la fila indicada
En las llamadas recursivas count se incrementa en 1 si la casilla consultada pertenece a 'p', si no, se reestablece a 0
-}
checkWinOneDia1 :: Board -> Int -> Int -> Int -> Int
checkWinOneDia1 _ _ p 4  = p
checkWinOneDia1 [] _ _ _ = -1
checkWinOneDia1 (c:cs) row p count
  | (row == -1)     = -1
  | (c !! row == p) = checkWinOneDia1 cs (row-1) p (count+1)
  | otherwise       = checkWinOneDia1 cs (row-1) p 0

{-
checkWinDia2: función auxiliar de checkWin
Dado un tablero, un índice de columna, un índice de fila, el número de columnas del tablero y un jugador 'p'
Devuelve el estado del tablero, que puede ser:
  p  => el tablero contiene una victoria por parte del jugador indicado
  -1 => no hay victoria
Sólo se comprueban posibles victorias en las diagonales con dirección ascendente
No se comprueban las esquinas del tablero donde no se pueden obtener 4 casillas contiguas
Los índices de columna y fila sirven para calcular recursivamente el valor en sub-tableros del tablero inicial
-}
checkWinDia2 :: Board -> Int -> Int -> Int -> Int -> Int
checkWinDia2 (c:cs) col row n p
  | (col == n - 3)                         = -1
  | (checkWinOneDia2 (c:cs) row p 0 /= -1) = p
  | (row == 0)                             = checkWinDia2  cs   (col+1) row    n p
  | (col == 0)                             = checkWinDia2 (c:cs) col   (row-1) n p

{-
checkWinOneDia2: función auxiliar de checkWinDia2
Dado un tablero, un índice de fila, un jugador 'p' y un entero 'count'
Devuelve el estado del tablero, que puede ser:
  p  => el tablero contiene una victoria por parte del jugador indicado (count = 4)
  -1 => no hay victoria (count < 4)
Sólo se comprueba la diagonal del tablero de sentido ascendiente que comienza en la primera columna del tablero dado y la fila indicada
El valor inicial de count es 0
En las llamadas recursivas count se incrementa en 1 si la casilla consultada pertenece a 'p', si no, se reestablece a 0
-}
checkWinOneDia2 :: Board -> Int -> Int -> Int -> Int
checkWinOneDia2 _ _ p 4   = p
checkWinOneDia2 [] _ _ _  = -1
checkWinOneDia2 (c:cs) row p count
  | (row == length c)     = -1
  | (c !! row == p)       = checkWinOneDia2 cs (row+1) p (count+1)
  | otherwise             = checkWinOneDia2 cs (row+1) p 0

{-
checkWinHori: función auxiliar de checkWin
Dado un tablero, un índice de fila y un jugador 'p'
Devuelve el estado del tablero, que puede ser:
  p  => el tablero contiene una victoria por parte del jugador indicado
  -1 => no hay victoria
Sólo se comprueban posibles victorias en sentido horizontal
El índice de fila sirve para la llamada a la función auxiliar checkWinRow
El índice de fila inicial es 0
-}
checkWinHori :: Board -> Int -> Int -> Int
checkWinHori (c:cs) row p
  | (row == length c)                  = -1
  | (checkWinRow (c:cs) row p 0 /= -1) = p
  | otherwise                        = checkWinHori (c:cs) (row+1) p

{-
checkWinRow: función auxiliar de checkWinHori
Dado un tablero, un índice de fila, un jugador y un entero 'count'
Devuelve el estado del tablero, que puede ser:
  p  => el tablero contiene una victoria por parte del jugador indicado (count = 4)
  -1 => no hay victoria (count < 4)
Sólo se comprueba la fila del tablero del índice indicado
El valor inicial de count es 0
En las llamadas recursivas count se incrementa en 1 si la casilla consultada pertenece a 'p', si no, se reestablece a 0
-}
checkWinRow :: Board -> Int -> Int -> Int -> Int
checkWinRow _ _ p 4 = p
checkWinRow [] _ _ _ = -1
checkWinRow (c:cs) row p count
  | ((c !! row) == p) = checkWinRow cs row p (count+1)
  | otherwise       = checkWinRow cs row p 0

{-
checkWinVert: función auxiliar de checkWin
Dado un tablero y un jugador 'p'
Devuelve el estado del tablero, que puede ser:
  p  => el tablero contiene una victoria por parte del jugador indicado
  -1 => no hay victoria
Sólo se comprueba la columna inicial del tablero
En las llamadas recursivas se va comprobando la columna inicial de sub-tableros (es decir, se acaban comprobando todas)
-}
checkWinVert :: Board -> Int -> Int
checkWinVert [] _ = -1
checkWinVert (c:cs) p
  | (checkWinCol c p 0 /= -1) = p
  | otherwise                 = checkWinVert cs p

{-
checkWinCol: función auxiliar de checkWinVert
Dada una columna, un jugador 'p' y un entero 'count'
Devuelve el estado del tablero, que puede ser:
  p  => la columna contiene una victoria por parte del jugador indicado (count = 4)
  -1 => no hay victoria (count < 4)
Si se encuentra una casilla empty y no se ha alcanzado count=4, se deja de buscar (el resto de la columna está vacía)
El valor inicial de count es 0
En las llamadas recursivas count se incrementa en 1 si la casilla consultada pertenece a 'p', si no, se reestablece a 0
-}
checkWinCol :: Col -> Int -> Int -> Int
checkWinCol _ p 4 = p
checkWinCol [] _ _ = -1
checkWinCol (x:xs) p count
  | (x == 0)  = -1
  | (x == p)  = checkWinCol xs p (count+1)
  | otherwise = checkWinCol xs p 0


--00000000000000000000000000000000000000000000000000000000000000000000000
--00000000000000000000 CHECK NEXT OBJECTIVE  0000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

{-
checkNextObj:
Dado un tablero, un jugador 'p' y un objetivo 'obj'
Devuelve un entero que indica la columna donde, si 'p' coloca ficha, consigue 'obj' fichas seguidas
Si no es posible conseguir el objetivo de fichas en la siguiente tirada, devuelve (-1)
'obj' debe ser <= 4
-}
checkNextObj :: Board -> Int -> Int -> Int
checkNextObj b player obj
  | (v  /= -1) = v
  | (d1 /= -1) = d1
  | (d2 /= -1) = d2
  | (h  /= -1) = h
  | otherwise = -1
  where
    v  = checkNextObjVert b 0 player obj
    d1 = checkNextObjDia1 b 0 3 (length b) player obj
    d2 = checkNextObjDia2 b 0 (length (b !! 0) - 4) (length b) player obj
    h  = checkNextObjHori b 0 player obj

{-
checkNextObjVert: función auxiliar de checkNextObj
Dado un tablero, un índice de columna, un jugador 'p' y un objetivo 'obj'
Devuelve un entero que indica la columna donde, si 'p' coloca ficha, consigue 'obj' fichas seguidas en sentido vertical
El índice de columna sirve para guardar la posición de la columna en las llamadas recursivas de checkNextObjVert
-}
checkNextObjVert :: Board -> Int -> Int -> Int -> Int
checkNextObjVert [] _ _ _ = -1
checkNextObjVert (c:cs) col player obj
  | (checkNextObjCol c 0 player obj) = col
  | otherwise                         = checkNextObjVert cs (col+1) player obj

{-
checkNextObjCol: función auxiliar de checkNextObjVert
Dada una columna, un entero 'count', un jugador 'p' y un objetivo 'obj'
Devuelve un booleano que indica:
  True  => 'p' puede lograr 'obj' fichas seguidas en vertical en la columna indicada
  False => 'p' no puede lograr 'obj' fichas seguidas en vertical en la columna indicada
Se logra el objetivo cuando se encuentra una casilla vacía y count = obj-1
Si se encuentra una casilla vacía y count < obj-1, no se podrá alcanzar el objetivo (devuelve falso)
'count' se incrementa en 1 en cada llamada recursiva si la casilla consultada es de 'p', sinó, se reestablece a 0
-}
checkNextObjCol :: Col -> Int -> Int -> Int -> Bool
checkNextObjCol [] _ _ _ = False
checkNextObjCol (x:xs) count player obj
  | (x == empty && count == obj-1) = True
  | (x == empty)  = False
  | (x == player) = checkNextObjCol xs (count+1) player obj
  | otherwise     = checkNextObjCol xs 0 player obj

{-
checkNextObjHori: función auxiliar de checkNextObj
Dado un tablero, un índice de fila, un jugador 'p' y un objetivo 'obj'
Devuelve un entero que indica la columna donde, si 'p' coloca ficha, consigue 'obj' fichas seguidas en sentido horizontal
El índice de fila sirve para guardar la posición de la fila en las llamadas recursivas de checkNextObjHori
-}
checkNextObjHori :: Board -> Int -> Int -> Int -> Int
checkNextObjHori cs i player obj
  | (i == length (cs !! 0)) = -1
  | (res /= -1)             = res
  | otherwise               = checkNextObjHori cs (i+1) player obj
  where
    res = checkNextObjRow cs 0 i 0 (-1) (switchP player) (switchP player) player obj

{-
Dado un tablero, un índice de columna y uno de fila, un entero 'count', índice de columna 'gap', dos índices de columna 'ant1' y 'ant2',
un jugador 'p' y un objetivo 'obj'
Devuelve 'gap', que indica el hueco donde, si 'p' coloca ficha, consigue 'obj' fichas seguidas en la fila indicada (cuando count=obj)
Si no es posible, devuelve (-1)
'ant2' y 'ant1' sirven para recordar el contenido de las dos casillas anteriores de la fila indicada en las llamadas recursivas
El hueco 'gap' se actualiza en cada llamada recursiva si se encuentra una casilla vacía válida, con el índice de su columna
(si se encuentra una casilla vacía pero sin suelo, es decir, con una casilla vacía debajo suya, se reestablece 'count' a 0)
Cuando se alcanza 'obj', se devuelve el último hueco encontrado, que permitirá al jugador lograr el objetivo de fichas seguidas
-}
checkNextObjRow :: Board -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkNextObjRow [] _ _ count gap _ _ _ obj
  | (count == obj) = gap
  | otherwise      = -1
checkNextObjRow (c:cs) col row count gap ant2 ant1 player obj
  | (count == obj)                                          = gap
  | (c !! row == player)                                    = checkNextObjRow cs (col+1) row (count+1) gap  ant1 (c !! row) player obj
  | (c !! row == empty && row > 0 && c !! (row-1) == empty) = checkNextObjRow cs (col+1) row 0         (-1) ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player && ant2 == empty)  = checkNextObjRow cs (col+1) row 2         col  ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player)                   = checkNextObjRow cs (col+1) row (count+1) col  ant1 (c !! row) player obj
  | (c !! row == empty)                                     = checkNextObjRow cs (col+1) row 1         col  ant1 (c !! row) player obj
  | (c !! row /= player)                                    = checkNextObjRow cs (col+1) row 0         (-1) ant1 (c !! row) player obj

{-
checkNextObjDia1: función auxiliar de checkNextObj
Dado un tablero, un índice de columna y uno de fila, el número de columnas del tablero 'n', un jugador 'p' y un objetivo 'obj'
Devuelve un entero que indica la columna donde, si 'p' coloca ficha, consigue 'obj' fichas seguidas en sentido diagoal descendente
No se comprueban las esquinas del tablero donde no se pueden obtener 4 casillas contiguas
Los índices de columna y fila sirven para calcular recursivamente el valor en sub-tableros del tablero inicial
-}
checkNextObjDia1 :: Board -> Int -> Int -> Int -> Int -> Int -> Int
checkNextObjDia1 (c:cs) col row n player obj
  | (col == n - 3)                  = -1
  | (row == length c && res1 /= -1) = res1
  | (row == length c)               = checkNextObjDia1 cs    (col+1) row     n player obj
  | (col == 0 && res2 /= -1)        = res2
  | (col == 0)                      = checkNextObjDia1 (c:cs) col    (row+1) n player obj
  where
    res1 = checkNextObjOneDia1 (c:cs) col (row-1) 0 (-1) (switchP player) (switchP player) player obj
    res2 = checkNextObjOneDia1 (c:cs) col  row    0 (-1) (switchP player) (switchP player) player obj

{-
checkNextObjOneDia1: función auxiliar de checkNextObj
Dado un tablero, un índice de columna y uno de fila, un entero 'count', índice de columna 'gap', dos índices de columna 'ant1' y 'ant2',
un jugador 'p' y un objetivo 'obj'
Devuelve 'gap', que indica el hueco donde, si 'p' coloca ficha, consigue 'obj' fichas seguidas en la diagonal descendente indicada (cuando count=obj)
Si no es posible, devuelve (-1)
'ant2' y 'ant1' sirven para recordar el contenido de las dos casillas anteriores de la diagonal ascendente indicada en las llamadas recursivas
El hueco 'gap' se actualiza en cada llamada recursiva si se encuentra una casilla vacía válida, con el índice de su columna
(si se encuentra una casilla vacía pero sin suelo, es decir, con una casilla vacía debajo suya, se reestablece 'count' a 0)
Cuando se alcanza 'obj', se devuelve el último hueco encontrado, que permitirá al jugador lograr el objetivo de fichas seguidas
-}

checkNextObjOneDia1 :: Board -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkNextObjOneDia1 [] _ _ count gap _ _ _ obj
  | (count == obj) = gap
  | otherwise      = -1
checkNextObjOneDia1 (c:cs) col row count gap ant2 ant1 player obj
  | (count == obj)                                          = gap
  | (row == -1)                                             = (-1)
  | (c !! row == player)                                    = checkNextObjOneDia1 cs (col+1) (row-1) (count+1) gap ant1 (c !! row) player obj
  | (c !! row == empty && row > 0 && c !! (row-1) == empty) = checkNextObjOneDia1 cs (col+1) (row-1) 0        (-1) ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player && ant2 == empty)  = checkNextObjOneDia1 cs (col+1) (row-1) 2         col ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player)                   = checkNextObjOneDia1 cs (col+1) (row-1) (count+1) col ant1 (c !! row) player obj
  | (c !! row == empty)                                     = checkNextObjOneDia1 cs (col+1) (row-1) 1         col ant1 (c !! row) player obj
  | (c !! row /= player)                                    = checkNextObjOneDia1 cs (col+1) (row-1) 0        (-1) ant1 (c !! row) player obj

{-
checkNextObjDia2: función auxiliar de checkNextObj
Dado un tablero, un índice de columna y uno de fila, el número de columnas del tablero 'n', un jugador 'p' y un objetivo 'obj'
Devuelve un entero que indica la columna donde, si 'p' coloca ficha, consigue 'obj' fichas seguidas en sentido diagoal ascendente
No se comprueban las esquinas del tablero donde no se pueden obtener 4 casillas contiguas
Los índices de columna y fila sirven para calcular recursivamente el valor en sub-tableros del tablero inicial
-}
checkNextObjDia2 :: Board -> Int -> Int -> Int -> Int -> Int -> Int
checkNextObjDia2 (c:cs) col (-1) n player obj
  | (col == n - 3) = -1
  | (res /= -1)    = res
  | otherwise      = checkNextObjDia2 cs (col+1) (-1) n player obj
  where
    res = checkNextObjOneDia2 (c:cs) col 0 0 (-1) (switchP player) (switchP player) player obj
checkNextObjDia2 cs 0 row n player obj
  | (res /= -1) = res
  | otherwise   = checkNextObjDia2 cs 0 (row-1) n player obj
  where
    res = checkNextObjOneDia2 cs 0 row 0 (-1) (switchP player) (switchP player) player obj

{-
checkNextObjOneDia2: función auxiliar de checkNextObj
Dado un tablero, un índice de columna y uno de fila, un entero 'count', índice de columna 'gap', dos índices de columna 'ant1' y 'ant2',
un jugador 'p' y un objetivo 'obj'
Devuelve 'gap', que indica el hueco donde, si 'p' coloca ficha, consigue 'obj' fichas seguidas en la diagonal ascendente indicada (cuando count=obj)
Si no es posible, devuelve (-1)
'ant2' y 'ant1' sirven para recordar el contenido de las dos casillas anteriores de la diagonal ascendente indicada en las llamadas recursivas
El hueco 'gap' se actualiza en cada llamada recursiva si se encuentra una casilla vacía válida, con el índice de su columna
(si se encuentra una casilla vacía pero sin suelo, es decir, con una casilla vacía debajo suya, se reestablece 'count' a 0)
Cuando se alcanza 'obj', se devuelve el último hueco encontrado, que permitirá al jugador lograr el objetivo de fichas seguidas
-}
checkNextObjOneDia2 :: Board -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkNextObjOneDia2 [] _ _ count gap _ _ _ obj
  | (count == obj) = gap
  | otherwise      = -1
checkNextObjOneDia2 (c:cs) col row count gap ant2 ant1 player obj
  | (count == obj)                                          = gap
  | (row == length c)                                       = (-1)
  | (c !! row == player)                                    = checkNextObjOneDia2 cs (col+1) (row+1) (count+1) gap ant1 (c !! row) player obj
  | (c !! row == empty && row > 0 && c !! (row-1) == empty) = checkNextObjOneDia2 cs (col+1) (row+1) 0        (-1) ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player && ant2 == empty)  = checkNextObjOneDia2 cs (col+1) (row+1) 2         col ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player)                   = checkNextObjOneDia2 cs (col+1) (row+1) (count+1) col ant1 (c !! row) player obj
  | (c !! row == empty)                                     = checkNextObjOneDia2 cs (col+1) (row+1) 1         col ant1 (c !! row) player obj
  | (c !! row /= player)                                    = checkNextObjOneDia2 cs (col+1) (row+1) 0        (-1) ant1 (c !! row) player obj


--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000  RANDOM STRATEGY  0000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

{-
randomStrat:
Dado un tablero devuelve (en una mónada IO) el índice de la columna donde npc debe colocar ficha basándose en la estrategia random
La columna seleccionada es elegida aleatoriamente. Si está llena, se reinicia la función hasta encontrar una válida
-}
randomStrat :: Board -> IO Int
randomStrat b = do
  res <- randInt 0 cols
  let new_b = updateBoard b res npc
  if (new_b == b) then do -- if the selected row is full then retry
    randomStrat b
  else return res
  where
    cols = (length b) - 1

{-
randInt: función auxiliar de randomStrat
Dados dos enteros (min, max) devuelve un número aleatorio entre min y max, incluidos
-}
randInt :: Int -> Int -> IO Int
randInt low high = do
  random <- randomIO :: IO Int
  let result = low + random `mod` (high - low + 1)
  return result

--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000  GREEDY STRATEGY  0000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

{-
greedyStrat:
Dado un tablero devuelve (en una mónada IO) el índice de la columna donde npc debe colocar ficha basándose en la estrategia greedy
Si el jugador npc tiene oportunidad de ganar en la siguiente tirada, selecciona la columna que lo permite
Sinó, si el jugador human tiene oportunidad de ganar en la siguiente tirada, selecciona la columna que lo evita
Sinó, se selecciona la columna que permite a npc juntar 3 fichas seguidas en la próxima tirada. Sinó, la que le permite juntar 2
Sinó, se selecciona la columna que permite a human juntar 3 fichas seguidas en la próxima tirada. Sinó, la que le permite juntar 2
-}

greedyStrat :: Board -> IO Int
greedyStrat b = do
  let c1 = checkNextObj b npc 4 --check if npc can win
  if (c1 /= -1) then
    return c1
  else do
    let c2 = checkNextObj b human 4 -- check if human can win
    if (c2 /= -1) then
      return c2
    else do
      c3 <- greedyStratAux b npc 3
      if (c3/= -1) then
        return c3
      else do
        c4 <- greedyStratAux b human 3
        if (c4/= -1) then
          return c4
        else randomStrat b

{-
greedyStratAux: función auxiliar de greedyStrat
Dado un tablero, un jugador y un objetivo 'n'
Devuelve (en una mónada IO) el índice de la columna donde se tiene que tirar ficha
para que el jugador indicado logre juntar 'n' fichas seguidas (n debe ser <= 4)
Si el objetivo no se cumple se llama recursivamente a sí misma hasta llegar a un objetivo de 1, donde la función devuelve (-1)
-}
greedyStratAux :: Board -> Int -> Int -> IO Int
greedyStratAux _ _ 1 = do return (-1)
greedyStratAux b player n = do
  let c1 = checkNextObj b player n
  if (c1 /= -1) then
    return c1
  else greedyStratAux b player (n-1)

--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000  SMART STRATEGY  00000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

{-
smartStrat:
Dado un tablero devuelve (en una mónada IO) el índice dela columna donde colocar ficha basándose en la estrategia smart
Esta estrategia está basada en el algoritmo minimax.
Se genera un arbol de decisiones con todas las posibles combinaciones de tableros hasta una profundidad definida por la función getDepth
Se puntua cada uno de los nodos hoja y se envía la mejor puntuación al nodo raíz.
La puntuación de los nodos de profundidad par es el máximo de las puntuaciones de sus hijos
Los nodos de profundidad par pertenecen a turnos de npc, y queremos escoger la jugada que da una puntuación lo más alta posible
La puntuación de los nodos de profundidad impar es el mínimo de las puntaciones de sus hijos
Los nodos de profundidad impar pertenecen a turnos de human, y por eso escogemos la puntuación más baja posible
(Debemos suponer que human elegirá siempre la jugada que más perjudique a npc)
En caso de llenarse una columna, o llegar a un estado final (victoria o empate) en el tablero de un nodo, ese nodo no tendrá hijos
Los hijos del nodo raíz son una excepción. Aunque no se pueda generar un hijo de la raíz se crea uno con un tablero 'nullBoard',
el cual recibe la puntuación más baja posible. Así, a la hora de elegir una columna no se pierde el índice real de dicha columna
En caso de empate de puntuaciones en los hijos del nodo raíz se selecciona la columna más cercana al centro del tablero
-}
smartStrat :: Board -> IO Int
smartStrat b = do
  let (Node _ childs) = createTree b npc (getDepth b)
  let scores = getScoreChilds childs human
  let maxi = maximum scores
  putStrLn $ "scores: "++(show scores)
  putStrLn $ "depth: "++(show $ getDepth b)
  let results = filter (>0) $ indexMax maxi 1 scores
  let mid = div ((length results)-1) 2
  return $ (results !! mid)-1

{-
createTree: función auxiliar de smartStrat
Dado un tablero, un jugador y una profundidad 'depth':
  Si se trata de un tablero 'nullBoard', devuelve un arbol cuyo nodo es un tablero 'nullBoard y sin hijos'
  Si depth = 0 => devuelve un árbol cuyo nodo es el tablero dado y sin hijos (depth 0 se obtiene en el último nivel de profundidad)
  Sino         => devuelve un árbol cuyo nodo es el tablero dado y sus hijos son árboles cuyos nodos son
                  todas las posibles combinaciones de tableros que puede generar una tirada del jugador dado
-}
createTree :: Board -> Int -> Int -> Tree Board
createTree b _ 0 = (Node b [])
createTree b p depth
  | (b == nullBoard)               = (Node b [])
  | (checkWin b (switchP p) == -1) = (Node b (createChildTrees childs (switchP p) (depth-1)))
  | otherwise                      = (Node b [])
  where
    childs = initChildTrees b p 0 depth

{-
createChildTrees: función auxiliar de createTree
Dada una lista de tableros, un jugador y una profundidad
Devuelve una lista de árboles de tableros, cuyos nodos son los de la lista de tableros y con sus correspondientes hijos.
La lista de árboles se asigna como hijos de un nodo en la función createTree
-}
createChildTrees :: [Board] -> Int -> Int -> [Tree Board]
createChildTrees [] _ _ = []
createChildTrees (b:childs) p depth = ((createTree b p depth):(createChildTrees childs p depth))

{-
initChildTrees: función auxiliar de createTree
Dado un tablero, un jugador, un índice de columna y una profundidad
Devuelve una lista con todos los tableros que se pueden generar con una tirada del jugador dado
Si el tablero corresponde a la raíz del árbol principal, se genera un nullBoard para los tableros que no se pueden generar
Sino, si después de actualizar el tablero éste no cambia (por una tirada en una columna llena), el tablero actualizado no se añade a la lista
La lista de tableros resultante debe ser transformada en lista de árboles de tableros en la función createTree
-}
initChildTrees :: Board -> Int -> Int -> Int -> [Board]
initChildTrees b p m depth
  | (m == (length b))                   = []
  | (new_b == b && depth == getDepth b) = (nullBoard:(initChildTrees b p (m+1) depth))
  | (new_b == b)                        = initChildTrees b p (m+1) depth
  | otherwise                           = (new_b:(initChildTrees b p (m+1) depth))
  where
    new_b = updateBoard b m p

{-
getScoreChilds: función auxiliar de smartStrat
Dada una lista de árboles de tableros y un jugador
Devuelve una lista de enteros que representan la puntuación correspondiente a cada uno de los árboles respecto al jugador dado
-}
getScoreChilds :: [Tree Board] -> Int -> [Int]
getScoreChilds [] _ = []
getScoreChilds (t:ts) p = ((getScore t p):(getScoreChilds ts p))

{-
getScore: función auxiliar de getScoreChilds
Dado un árbol de tableros y un jugador 'p'
Devuelve la puntuación correspondiente al nodo de dicho árbol
Si el árbol no tiene hijos (nodo hoja) se calcula su puntuación con la función calcScore y el enemigo de 'p' (el que generó el tablero del nodo actual)
Si el árbol tiene hijos:
  p = human (nodo impar) => la puntuación es el mínimo de las puntuaciones de sus árboles hijos
  p = npc   (nodo par)   => la puntuación es el máximo de las puntuaciones de sus árboles hijos
-}
getScore :: Tree Board -> Int -> Int
getScore (Node b []) p = calcScore b (switchP p)
getScore (Node b childs) p
  | (p == human) = minimum (getScoreChilds childs (switchP p))
  | (p == npc)   = maximum (getScoreChilds childs (switchP p))

{-
calcScore: función auxiliar de getScore
Dado un tablero 'b' y un jugador 'p'
Devuelve la puntuación del tablero dado respecto a 'p'
  b = nullBoard           => la puntuación más baja posible (imposible de obtener una puntiación menor, así nunca se elegirá este tablero)
  b con empate            => la puntuación es 0 (no beneficia a ningún jugador)
  b con victoria de human => la puntuación es el número de fichas colocadas hasta el momento por human - total de fichas del tablero
                             (cuantas menos fichas haya usado peor puntuación se obtiene, ya que es más perjudicial para npc)
  b con victoria de npc   => la puntuación es el total de fichas del tablero - número de fichas colocadas hasta el momento por human
                             (cuantas menos fichas haya usado mayor puntuación se obtiene, ya que es más beneficioso para npc)
  Si en el tablero no hay victoria (tablaro no pertenece a un estado final):
  La puntuación se obtiene comprobando si en la próxima tirada el rival puede juntar 4 fichas seguidas, sinó 3 y sinó 2,
  con puntuaciones asociadas 3, 2 y 1 respectivamente (el signo se invierte para los tableros donde el siguiente en tirar es human)
  En caso de no cumplirse ninguna de las condiciones anteriores se asigna una puntuación de 0
-}
calcScore :: Board -> Int -> Int
calcScore b p
  | (b == nullBoard)            = -99999999999
  | (result == 0)               = 0
  | (result == human)           = num - s --negative score
  | (result == npc  )           = s - num --positive score
  | (p == human && next4 /= -1) =  3
  | (p == npc   && next4 /= -1) = -3
  | (p == human && next3 /= -1) =  2
  | (p == npc   && next3 /= -1) = -2
  | (p == human && next2 /= -1) =  1
  | (p == npc   && next2 /= -1) = -1
  | otherwise = 0
  where
    result = checkWin b p
    s = maxScore b
    num = numTokensBoard b p
    next4 = checkNextObj b (switchP p) 4
    next3 = checkNextObj b (switchP p) 3
    next2 = checkNextObj b (switchP p) 2

{-
getDepth: función sinónimo de la profundidad escogida para el árbol de decisiones de smartStrat
Dado un tablero
Devuelve un entero que representa dicha profundidad. La profundidad inicial es 5, y se incrementa en 1 por cada columna del tablero llena
Cuando una columna se llena el factor de ramificación se decrementa en 1 unidad, por lo que podemos incrementar la profundidad
sin sufrir un gran incremento en el tiempo de cálculo de la columna a escoger
-}
getDepth :: Board -> Int
getDepth b = 5 + numColsFull b

{-
numColsFull: función auxiliar de getDepth
Dado un tablero
Devuelve el número de columnas llenas del tablero indicado
Se sabe que una columna está llena si su última casilla no está vacía
-}
numColsFull :: Board -> Int
numColsFull [] = 0
numColsFull (c:cs)
  | (last c /= empty) = 1 + (numColsFull cs)
  | otherwise         = numColsFull cs

{-
indexMax: función auxiliar de smartStrat
Dado un elemento 'e' de tipo 'a' (cuyo tipo es instancia de la clase Eq), un índice y una lista de elementos de tipo 'a'
Devuelve una lista de enteros del tamaño de la lista dada, cuyos elementos son los índices de cada elemento en la lista original,
pero a los elementos diferentes a 'e' se les asigna el índice con símbolo negativo.
Esto servirá en la función smartStrat para filtar los elementos >0 y escoger el elemento central de la lista original
(de entre los que son iguales al máximo)
-}
indexMax :: Eq a => a -> Int -> [a] -> [Int]
indexMax _ _ [] = []
indexMax e i (x:xs)
  | (e == x)  =   (i:(indexMax e (i+1) xs))
  | otherwise = ((-i):(indexMax e (i+1) xs))

{-
maxScore: función auxiliar de calcScore
Dado un tablero devuelve la máxima puntuación posible para un tablero cualquiera de tamaño n x m
La máxima puntuación es (n*m/2)+1, ya que corresponde a la mitad de las casillas del tablero (+1) que son las que un jugador puede colocar
-}
maxScore :: Board -> Int
maxScore b = 1 + (div ((length (b !! 0))*(length b)) 2)

{-
numTokensBoard: función auxiliar de calcScore
Dado un tablero y un jugador
Devuelve el número de fichas que el jugador ha colocado en el tablero hasta el momento
-}
numTokensBoard :: Board -> Int -> Int
numTokensBoard [] _ = 0
numTokensBoard (c:cs) p = (numTokensCol c p) + (numTokensBoard cs p)

{-
numTokensBoard: función auxiliar de numTokens
Dada una columna y un jugador
Devuelve el número de fichas que el jugador ha colocado en la columna hasta el momento
-}
numTokensCol :: Col -> Int -> Int
numTokensCol [] _ = 0
numTokensCol (x:xs) p
  | (x == empty) = 0
  | (x == p)     = 1 + (numTokensCol xs p)
  | otherwise    = numTokensCol xs p
