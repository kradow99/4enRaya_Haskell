import System.Random

{-
Board: representa el tablero del juego
Consta de una lista de columnas, que son listas de enteros
Cada posición del tablero puede ser:
  0 (empty) => casilla vacía
  1 (human) => casilla con ficha del jugador humano
  2 (npc)   => casilla con ficha del npc (non-playable character)
-}
data Board = Board [[Int]]

{-
Tree a: árbol genérico de elementos de tipo a
Un Tree consta de:
- Un nodo con su respectivo elemento de tipo a
- Una lista de árboles que representan los hijos del nodo
-}
data Tree a = Node a [Tree a] deriving (Show)

{-
Strategy: Una estrategia es un sinónimo de (Board -> IO Int)
Es una función que, dado un tablero, devuelve la columna donde el npc debe colocar una ficha según el algoritmo que encapsula
La columna se devuelve encapsulada en una mónada IO
-}
type Strategy = Board -> IO Int
type Xi = [[Int]]
{-
El tipo Board es instancia de la clase Eq.
Dos Board son iguales si sus listas de columnas son iguales
-}
instance Eq Board where
  (Board cs1) == (Board cs2) = (cs1 == cs2)

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
createBoard n m = Board (take m $ repeat $ (take n $ repeat empty))

{-
nullBoard: función sinónimo del tablero con 0 filas y 0 columnas
(Útil para la estrategia smart)
-}
nullBoard :: Board
nullBoard = createBoard 0 0
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
showBoard (Board (c:cs)) = showBoardRec (Board (c:cs)) ((length c)-1)

{-
showBoardRec: función auxiliar de showBoard
Dado un tablero y un índice de fila n, escribe por la salida estándar las filas de n a 0
La primera fila en ser escrita es la n, y la última la 0
Cuando ha escrito todas las filas, escribe el índice de cada columna
Devuelve una mónada IO con el tipo ()
-}
showBoardRec :: Board -> Int -> IO ()
showBoardRec (Board cs) (-1) = do
  printIndex 0 ((length cs)-1)
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
printRow (Board []) _ = ""
printRow (Board (c:cs)) n
  | ((c !! n) == empty) = "· " ++ (printRow (Board cs) n)
  | ((c !! n) == human) = "o " ++ (printRow (Board cs) n)
  | ((c !! n) == npc)   = "x " ++ (printRow (Board cs) n)

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
updateBoard (Board cs) i p = (Board new)
  where
    new = updateBoardRec cs i p

{-
updateBoardRec: función auxiliar de updateBoard
Dada una lista con las columnas de un tablero, un índice de columna y un jugador
Devuelve la misma lista de columnas pero con una ficha del jugador indicado añadida en la columna indicada
-}
updateBoardRec :: [[Int]] -> Int -> Int -> [[Int]]
updateBoardRec (c:cs) i p
  | (i > ((length cs))) = (c:cs)
  | (i < 0)             = (c:cs)
  | (i == 0)            = (u : cs)
  | otherwise           = (c : (updateBoardRec (cs) (i-1) p))
  where
    --n = (length c)-1
    u = updateCol c p
{-
updateCol: función auxiliar de updateBoardRec
Dada una columna y un jugador
Devuelve la misma columna pero con una ficha del jugador indicado añadida en la columna en la primera fila disponible
-}
updateCol :: [Int] -> Int -> [Int]
updateCol [] _ = []
updateCol (x:xs) p
  | (x == empty)  = (p:xs)
  | otherwise     = (x:(updateCol xs p))

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
  putStrLn "[0] Random"
  putStrLn "[1] Greedy"
  putStrLn "[2] Smart"
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
--0000000000000000000000000000 CHECK WIN  0000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

{-
checkWin:
Dado un tablaro y un jugador
Devuelve el estado del tablero. Los posibles estados son:
  0     => empate
  human => el tablero contiene una victoria por parte del jugador humano
  npc   => el tablero contiene una victoria por parte del jugador npc
  -1    => ninguno de los anteriores (no hay ni victorias ni empate)
-}
checkWin :: Board -> Int -> Int
checkWin (Board cs) player
  | (checkWinVert cs player /= -1)                                    = player
  | (checkWinHori cs 0 player /= -1)                                  = player
  | (checkWinDia1 cs 0 3 (length cs) player /= -1)                    = player
  | (checkWinDia2 cs 0 (length (cs !! 0)-4) (length cs) player /= -1) = player
  | (checkDraw cs)                                                    = 0
  | otherwise                                                         = -1

{-
checkDraw: función auxiliar de checkWin
Dada una lista con las columnas de un tablero
Devuelve un booleano que indica si en el tablero con esas columnas hay un empate
  True => Hay un empate (todas las columnas llenas, sin casillas empty)
-}
checkDraw :: [[Int]] -> Bool
checkDraw [] = True
checkDraw (c:cs)
  | (elem empty c) = False
  | otherwise = checkDraw cs

checkWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int
checkWinDia1 (c:cs) col row n p
  | (col == n - 3)                         = -1
  | (checkWinOneDia1 (c:cs) row p 0 /= -1) = p
  | (row == (length c)-1)                  = checkWinDia1 cs    (col+1) row    n p
  | (col == 0)                             = checkWinDia1 (c:cs) col   (row+1) n p

checkWinOneDia1 :: [[Int]] -> Int -> Int -> Int -> Int
checkWinOneDia1 _ _ p 4  = p
checkWinOneDia1 [] _ _ _ = -1
checkWinOneDia1 (c:cs) row p count
  | (row == -1)     = -1
  | (c !! row == p) = checkWinOneDia1 cs (row-1) p (count+1)
  | otherwise       = checkWinOneDia1 cs (row-1) p 0

checkWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int
checkWinDia2 (c:cs) col row n p
  | (col == n - 3)                         = -1
  | (checkWinOneDia2 (c:cs) row p 0 /= -1) = p
  | (row == 0)                             = checkWinDia2  cs   (col+1) row    n p
  | (col == 0)                             = checkWinDia2 (c:cs) col   (row-1) n p

checkWinOneDia2 :: [[Int]] -> Int -> Int -> Int -> Int
checkWinOneDia2 _ _ p 4   = p
checkWinOneDia2 [] _ _ _  = -1
checkWinOneDia2 (c:cs) row p count
  | (row == length c)     = -1
  | (c !! row == p)       = checkWinOneDia2 cs (row+1) p (count+1)
  | otherwise             = checkWinOneDia2 cs (row+1) p 0


checkWinHori :: [[Int]] -> Int -> Int -> Int
checkWinHori (c:cs) r p
  | (r == length c)                  = -1
  | (checkWinRow (c:cs) r p 0 /= -1) = p
  | otherwise                        = checkWinHori (c:cs) (r+1) p

checkWinRow :: [[Int]] -> Int -> Int -> Int -> Int
checkWinRow _ _ p 4 = p
checkWinRow [] _ _ _ = -1
checkWinRow (c:cs) r p count
  | ((c !! r) == p) = checkWinRow cs r p (count+1)
  | otherwise       = checkWinRow cs r p 0

checkWinVert :: [[Int]] -> Int -> Int
checkWinVert [] _ = -1
checkWinVert (c:cs) p
  | (checkWinCol c p 0 /= -1) = p
  | otherwise                 = checkWinVert cs p

checkWinCol :: [Int] -> Int -> Int -> Int
checkWinCol _ p 4 = p
checkWinCol [] _ _ = -1
checkWinCol (x:xs) p count
  | (x == p)  = checkWinCol xs p (count+1)
  | otherwise = checkWinCol xs p 0


--00000000000000000000000000000000000000000000000000000000000000000000000
--00000000000000000000000 CHECK NEXT WIN  0000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

checkNextWin :: [[Int]] -> Int -> Int -> Int
checkNextWin cs player obj
  | (v  /= -1) = v
  | (d1 /= -1) = d1
  | (d2 /= -1) = d2
  | (h  /= -1) = h
  | otherwise = -1
  where
    v  = checkNextWinVert cs 0 player obj
    d1 = checkNextWinDia1 cs 0 3 (length cs) player obj
    d2 = checkNextWinDia2 cs 0 (length (cs !! 0) - 4) (length cs) player obj
    h  = checkNextWinHori cs 0 player obj

checkNextWinVert :: [[Int]] -> Int -> Int -> Int -> Int
checkNextWinVert [] _ _ _ = -1
checkNextWinVert (c:cs) col player obj
  | (checkCouldWinCol c 0 player obj) = col
  | otherwise                         = checkNextWinVert cs (col+1) player obj

checkCouldWinCol :: [Int] -> Int -> Int -> Int -> Bool
checkCouldWinCol [] _ _ _ = False
checkCouldWinCol (x:xs) count player obj
  | (x == empty && count == obj-1 && ((length xs)+1 >= 4-count)) = True
  | (x == empty)  = False
  | (x == player) = checkCouldWinCol xs (count+1) player obj
  | (x == 0)      = False
  | otherwise     = checkCouldWinCol xs 0 player obj

checkNextWinHori :: [[Int]] -> Int -> Int -> Int -> Int
checkNextWinHori cs i player obj
  | (i == length (cs !! 0)) = -1
  | (res /= -1)             = res
  | otherwise               = checkNextWinHori cs (i+1) player obj
  where
    res = checkCouldWinRow cs 0 i 0 (-1) (switchP player) (switchP player) player obj

checkCouldWinRow :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkCouldWinRow [] _ _ count gap _ _ _ obj
  | (count == obj) = gap
  | otherwise      = -1
checkCouldWinRow (c:cs) col row count gap ant2 ant1 player obj
  | (count == obj && ((length cs)+1+count) < 4)             = (-1)
  | (count == obj)                                          = gap
  | (c !! row == player)                                    = checkCouldWinRow cs (col+1) row (count+1) gap  ant1 (c !! row) player obj
  | (c !! row == empty && row > 0 && c !! (row-1) == empty) = checkCouldWinRow cs (col+1) row 0         (-1) ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player && ant2 == empty)  = checkCouldWinRow cs (col+1) row 3         col  ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player)                   = checkCouldWinRow cs (col+1) row (count+1) col  ant1 (c !! row) player obj
  | (c !! row == empty)                                     = checkCouldWinRow cs (col+1) row 1         col  ant1 (c !! row) player obj
  | (c !! row /= player)                                    = checkCouldWinRow cs (col+1) row 0         (-1) ant1 (c !! row) player obj

checkNextWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int
checkNextWinDia1 (c:cs) col row n player obj
  | (col == n - 3)                  = -1
  | (row == length c && res1 /= -1) = res1
  | (row == length c)               = checkNextWinDia1 cs    (col+1) row     n player obj
  | (col == 0 && res2 /= -1)        = res2
  | (col == 0)                      = checkNextWinDia1 (c:cs) col    (row+1) n player obj
  where
    res1 = checkCouldWinDia1 (c:cs) col (row-1) 0 (-1) (switchP player) (switchP player) player obj
    res2 = checkCouldWinDia1 (c:cs) col  row    0 (-1) (switchP player) (switchP player) player obj

checkCouldWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkCouldWinDia1 [] _ _ count gap _ _ _ obj
  | (count == obj) = gap
  | otherwise      = -1
checkCouldWinDia1 (c:cs) col row count gap ant2 ant1 player obj
  | (count == obj)                                          = gap
  | (row == -1)                                             = (-1)
  | (c !! row == player)                                    = checkCouldWinDia1 cs (col+1) (row-1) (count+1) gap ant1 (c !! row) player obj
  | (c !! row == empty && row > 0 && c !! (row-1) == empty) = checkCouldWinDia1 cs (col+1) (row-1) 0        (-1) ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player && ant2 == empty)  = checkCouldWinDia1 cs (col+1) (row-1) 2         col ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player)                   = checkCouldWinDia1 cs (col+1) (row-1) (count+1) col ant1 (c !! row) player obj
  | (c !! row == empty)                                     = checkCouldWinDia1 cs (col+1) (row-1) 1         col ant1 (c !! row) player obj
  | (c !! row /= player)                                    = checkCouldWinDia1 cs (col+1) (row-1) 0        (-1) ant1 (c !! row) player obj

checkNextWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int
checkNextWinDia2 (c:cs) col (-1) n player obj
  | (col == n - 3) = -1
  | (res /= -1)    = res
  | otherwise      = checkNextWinDia2 cs (col+1) (-1) n player obj
  where
    res = checkCouldWinDia2 (c:cs) col 0 0 (-1) (switchP player) (switchP player) player obj
checkNextWinDia2 cs 0 row n player obj
  | (res /= -1) = res
  | otherwise   = checkNextWinDia2 cs 0 (row-1) n player obj
  where
    res = checkCouldWinDia2 cs 0 row 0 (-1) (switchP player) (switchP player) player obj


checkCouldWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkCouldWinDia2 [] _ _ count gap _ _ _ obj
  | (count == obj) = gap
  | otherwise      = -1
checkCouldWinDia2 (c:cs) col row count gap ant2 ant1 player obj
  | (count == obj)                                          = gap
  | (row == length c)                                       = (-1)
  | (c !! row == player)                                    = checkCouldWinDia2 cs (col+1) (row+1) (count+1) gap ant1 (c !! row) player obj
  | (c !! row == empty && row > 0 && c !! (row-1) == empty) = checkCouldWinDia2 cs (col+1) (row+1) 0        (-1) ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player && ant2 == empty)  = checkCouldWinDia2 cs (col+1) (row+1) 2         col ant1 (c !! row) player obj
  | (c !! row == empty && ant1 == player)                   = checkCouldWinDia2 cs (col+1) (row+1) (count+1) col ant1 (c !! row) player obj
  | (c !! row == empty)                                     = checkCouldWinDia2 cs (col+1) (row+1) 1         col ant1 (c !! row) player obj
  | (c !! row /= player)                                    = checkCouldWinDia2 cs (col+1) (row+1) 0        (-1) ant1 (c !! row) player obj

--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000000  PLAY  000000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

play :: Board -> Int -> Strategy -> IO Int--winner
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
--00000000000000000000000000000  MAIN  0000000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

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
--000000000000000000000000000  RANDOM STRATEGY  0000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

randomStrat :: Board -> IO Int
randomStrat (Board cs) = do
  res <- randInt 0 cols
  let new_b = updateBoard (Board cs) res npc
  if (new_b == (Board cs)) then do -- if the selected row is full then retry
    randomStrat (Board cs)
  else return res
  where
    cols = (length cs) - 1

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
  random <- randomIO :: IO Int
  let result = low + random `mod` (high - low + 1)
  return result

--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000  GREEDY STRATEGY  0000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

greedyStrat :: Board -> IO Int
greedyStrat (Board cs) = do
  let c1 = checkNextWin cs npc 4 --check if npc can win
  if (c1 /= -1) then
    return c1
  else do
    let c2 = checkNextWin cs human 4 -- check if human can win
    if (c2 /= -1) then
      return c2
    else do
      c3 <- greedyStratAux (Board cs) npc 3
      if (c3/= -1) then
        return c3
      else do
        c4 <- greedyStratAux (Board cs) human 3
        if (c4/= -1) then
          return c4
        else randomStrat (Board cs)

greedyStratAux :: Board -> Int -> Int -> IO Int
greedyStratAux _ _ 1 = do return (-1)
greedyStratAux (Board cs) player n = do
  let c1 = checkNextWin cs player n
  if (c1 /= -1) then
    return c1
  else greedyStratAux (Board cs) player (n-1)

--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000  SMART STRATEGY  00000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

smartStrat :: Board -> IO Int
smartStrat (Board cs) = do
  let (Node _ childs) = createTree (Board cs) npc getDepth
  let scores = getScoreChilds childs human
  let maxi = maximum scores
  putStrLn $ "scores: "++(show scores)
  let results = filter (>0) $ indexMax maxi 1 scores
  let mid = div ((length results)-1) 2
  return $ (results !! mid)-1

createTree :: Board -> Int -> Int -> Tree Board
createTree b _ 0 = (Node b [])
createTree (Board cs) p depth
  | (length cs == 0)                        = (Node (Board cs) [])
  | (checkWin (Board cs) (switchP p) == -1) = (Node (Board cs) (createChildTrees childs (switchP p) (depth-1)))
  | otherwise                               = (Node (Board cs) [])
  where
    childs = initChildTrees (Board cs) p 0 depth

createChildTrees :: [Board] -> Int -> Int -> [Tree Board]
createChildTrees [] _ _ = []
createChildTrees (b:childs) p depth = ((createTree b p depth):(createChildTrees childs p depth))

initChildTrees :: Board -> Int -> Int -> Int -> [Board]
initChildTrees (Board cs) p m depth
  | (m == (length cs)) = []
  | (new_b == (Board cs) && depth == getDepth) = (nullBoard:(initChildTrees (Board cs) p (m+1) depth))
  | (new_b == (Board cs)) = initChildTrees (Board cs) p (m+1) depth
  | otherwise =     (new_b:(initChildTrees (Board cs) p (m+1) depth))
  where
    new_b = updateBoard (Board cs) m p

getScoreChilds :: [Tree Board] -> Int -> [Int]
getScoreChilds [] _ = []
getScoreChilds (t:ts) p = ((getScore t p):(getScoreChilds ts p))

getScore :: Tree Board -> Int -> Int
getScore (Node b []) p = calcScore b (switchP p)
getScore (Node b childs) p
  | (p == human) = minimum (getScoreChilds childs (switchP p))
  | (p == npc)   = maximum (getScoreChilds childs (switchP p))

calcScore :: Board -> Int -> Int
calcScore (Board cs) p
  | ((Board cs) == nullBoard) = -9999
  | (result == 0)    = 0
  | (result == human)    = num - s --negative score
  | (result == npc  )    = s - num --positive score
  | (p == human && next4 /= -1) =  3
  | (p == npc   && next4 /= -1) = -3
  | (p == human && next3 /= -1) =  2
  | (p == npc   && next3 /= -1) = -2
  | (p == human && next2 /= -1) =  1
  | (p == npc   && next2 /= -1) = -1
  | otherwise = 0
  where
    result = checkWin (Board cs) p
    s = maxScore (Board cs)
    num = numTokensBoard cs p
    next4 = checkNextWin cs (switchP p) 4
    next3 = checkNextWin cs (switchP p) 3
    next2 = checkNextWin cs (switchP p) 2

getDepth :: Int
getDepth = 6

indexMax :: Eq a => a -> Int -> [a] -> [Int]
indexMax _ _ [] = []
indexMax e i (x:xs)
  | (e == x)  =   (i:(indexMax e (i+1) xs))
  | otherwise = ((-i):(indexMax e (i+1) xs))

maxScore :: Board -> Int
maxScore (Board cs) = 1 + (div ((length (cs !! 0))*(length cs)) 2)

numTokensBoard :: [[Int]] -> Int -> Int
numTokensBoard [] _ = 0
numTokensBoard (c:cs) p = (numTokensCol c p) + (numTokensBoard cs p)

numTokensCol :: [Int] -> Int -> Int
numTokensCol [] _ = 0
numTokensCol (x:xs) p
  | (x == p) = 1 + (numTokensCol xs p)
  | otherwise = numTokensCol xs p
