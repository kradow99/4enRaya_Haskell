import System.Random

data Tree a = Node a [Tree a] deriving (Show)
data Board = Board [[Int]]
type Strategy = Board -> IO Int

instance Show Board where
    show (Board cs) = (show cs)++"\n"

createBoard :: Int -> Int -> Board
createBoard n m = Board (take m $ repeat $ (take n $ repeat 0))

--00000000000000000000000000000000000000000000000000000000000000000000000
--0000000000000000000000  PRINT THE BOARD  000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

showBoard :: Board -> IO()
showBoard (Board (c:cs)) = showBoardRec (Board (c:cs)) ((length c)-1)

showBoardRec :: Board -> Int -> IO ()
showBoardRec (Board cs) (-1) = do
  printIndex 0 ((length cs)-1)
  return ()
showBoardRec b n = do
  putStrLn $ printRow b n
  showBoardRec b (n-1)

printRow :: Board -> Int -> String
printRow (Board []) _ = ""
printRow (Board (c:cs)) n
  | ((c !! n) == 0) = "· " ++ (printRow (Board cs) n)
  | ((c !! n) == 1) = "o " ++ (printRow (Board cs) n)
  | ((c !! n) == 2) = "x " ++ (printRow (Board cs) n)

printIndex :: Int -> Int -> IO ()
printIndex 0 m = do
  putStr $ (show 0)++" "
  printIndex 1 m
printIndex i m = do
  if (i == m) then do
    putStrLn $ show i
  else do
    putStr "  "
    printIndex (i+1) m

--000000000000000000000000000000000000000000000000000000000000000000000000
--0000000000000000000000  UPDATE THE BOARD  000000000000000000000000000000
--000000000000000000000000000000000000000000000000000000000000000000000000

             --Board -> Col -> Jug -> (Board, height)
updateBoard :: Board -> Int -> Int -> (Board, Int)
updateBoard (Board cs) i p = (Board new, pos)
  where
    u     = updateBoardRec cs i p
    [pos] = last u
    new   = removeLast u

updateBoardRec :: [[Int]] -> Int -> Int -> [[Int]]
updateBoardRec (c:cs) i p
  | (i > ((length cs))) = (c:cs)++[[-1]]
  | (i < 0)             = (c:cs)++[[-1]]
  | (i == 0)            = (u2 : cs) ++ [[pos]]
  | otherwise           = (c : (updateBoardRec (cs) (i-1) p))
  where
    n   = (length c)-1
    u1  = updateCol c n n p
    pos = last u1
    u2  = removeLast u1

updateCol :: [Int] -> Int -> Int -> Int -> [Int]
updateCol c (-1) _ _ = c ++ [-1]
updateCol (x:xs) i n p
  | (x == 0)  = (p:xs) ++ [n-i]
  | otherwise = (x:(updateCol xs (i-1) n p))

removeLast :: [a] -> [a]
removeLast (x:[]) = []
removeLast (x:xs) = (x:(removeLast xs))

--00000000000000000000000000000000000000000000000000000000000000000000000
--0000000000000000000000  INITIALIZE GAME  000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

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
      return (createBoard n m)

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

--00000000000000000000000000000000000000000000000000000000000000000000000
--0000000000000000000000000000 CHECK WIN  0000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

checkWin :: Board -> Int -> Int -> Int -> Int
checkWin _ _ (-1) _ = -1
checkWin (Board cs) col row player
  | (checkDraw cs)                                     = 0
  | ((checkWinVert (cs !! col) player 0) /= -1)        = player
  | ((checkWinHori cs row player 0) /= -1)             = player
  | ((checkWinDia1 cs col row col row player 0) /= -1) = player
  | ((checkWinDia2 cs col row col row player 0) /= -1) = player
  | otherwise = -1

checkDraw :: [[Int]] -> Bool
checkDraw [] = True
checkDraw (c:cs)
  | (elem 0 c) = False
  | otherwise = checkDraw cs

checkWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkWinDia1 _ _ _ _ _ p 4 = p
checkWinDia1 cs cini rini col row p count
  | (row == -1)        = -1
  | (col == length cs) = -1

  | (col == cini)                               = checkWinDia1 cs cini rini (cini-1) (rini+1) p 1

  | (col == -1)                                 = checkWinDia1 cs cini rini (cini+1) (rini-1) p count
  | (row == length (cs !! 0))                   = checkWinDia1 cs cini rini (cini+1) (rini-1) p count

  | (col < cini && ((cs !! col) !! row) == p)   = checkWinDia1 cs cini rini (col-1) (row+1) p (count+1)
  | (col < cini)                                = checkWinDia1 cs cini rini (cini+1) (rini-1) p count
  | (col > cini && (((cs !! col) !! row) == p)) = checkWinDia1 cs cini rini (col+1) (row-1) p (count+1)
  | otherwise = -1

checkWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkWinDia2 _ _ _ _ _ p 4 = p
checkWinDia2 cs cini rini col row p count
  | (row == length (cs !! 0)) = -1
  | (col == length cs)        = -1

  | (col == cini)                               = checkWinDia2 cs cini rini (cini-1) (rini-1) p 1

  | (col == -1)                                 = checkWinDia2 cs cini rini (cini+1) (rini+1) p count
  | (row == -1)                                 = checkWinDia2 cs cini rini (cini+1) (rini+1) p count

  | (col < cini && (((cs !! col) !! row) == p)) = checkWinDia2 cs cini rini (col-1) (row-1) p (count+1)
  | (col < cini)                                = checkWinDia2 cs cini rini (cini+1) (rini+1) p count
  | (col > cini && (((cs !! col) !! row) == p)) = checkWinDia2 cs cini rini (col+1) (row+1) p (count+1)
  | otherwise = -1

checkWinHori :: [[Int]] -> Int -> Int -> Int -> Int
checkWinHori _ _ p 4 = p
checkWinHori [] _ _ _ = -1
checkWinHori (c:cs) r p count
  | ((c !! r) == p) = checkWinHori cs r p (count+1)
  | otherwise       = checkWinHori cs r p 0

checkWinVert :: [Int] -> Int -> Int -> Int
checkWinVert _ p 4 = p
checkWinVert [] _ _ = -1
checkWinVert (x:xs) p count
  | (x == p)  = checkWinVert xs p (count+1)
  | otherwise = checkWinVert xs p 0

--00000000000000000000000000000000000000000000000000000000000000000000000
--00000000000000000000000 CHECK NEXT WIN  0000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

checkNextWin :: [[Int]] -> Int -> Int -> [Int] -> IO Int
checkNextWin cs player obj forbid = do
  v <- checkNextWinVert cs 0 player obj forbid
  if (v /= -1) then
    return v
  else do
    d1 <- checkNextWinDia1 cs 0 3 (length cs) player obj forbid
    if (d1 /= -1) then
      return d1
    else do
      d2 <- checkNextWinDia2 cs 0 (length (cs !! 0) - 4) (length cs) player obj forbid
      if (d2 /= -1) then
        return d2
      else do
        h <- checkNextWinHori cs 0 player obj forbid
        if (h /= -1) then
          return h
        else return (-1) -- esto sobra en realidad

checkNextWinVert :: [[Int]] -> Int -> Int -> Int -> [Int] -> IO Int
checkNextWinVert [] _ _ _ _ = do return (-1)
checkNextWinVert (c:cs) col player obj forbid = do
  if (elem col forbid) then
    checkNextWinVert cs (col+1) player obj forbid
  else do
    if (checkCouldWinCol c 0 player obj) then
      return col
    else checkNextWinVert cs (col+1) player obj forbid

checkCouldWinCol :: [Int] -> Int -> Int -> Int -> Bool
checkCouldWinCol [] _ _ _ = False
checkCouldWinCol (0:xs) count player obj
  | (count == obj - 1 && ((length xs)+1 >= 4-count)) = True
  | otherwise = False
checkCouldWinCol (x:xs) count player obj
  | (x == player) = checkCouldWinCol xs (count+1) player obj
  | (x == 0)      = False
  | otherwise     = checkCouldWinCol xs 0 player obj

checkNextWinHori :: [[Int]] -> Int -> Int -> Int -> [Int] -> IO Int
checkNextWinHori cs i player obj forbid
  | (i == length (cs !! 0)) = do return (-1)
  | otherwise = do
    let res = checkCouldWinRow cs i 0 0 (-1) 2 2 player obj forbid
    if (res /= -1) then
      return res
    else checkNextWinHori cs (i+1) player obj forbid

checkCouldWinRow :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> Int
checkCouldWinRow [] _ _ count gap _ _ _ obj _
  | (count == obj) = gap
  | otherwise      = -1
checkCouldWinRow (c:cs) i col count gap ant2 ant1 player obj forbid
  | (elem col forbid)                                 = checkCouldWinRow cs     i (col+1) 0         (-1) ant1 2        player obj forbid
  | (count == obj && ((length cs)+1+count) < 4)       = (-1)
  -- FALTA CHECK ESTO: oox·xo, ahi pondria con obj=3 pero no podria llegar a 4
  -- | (count == obj && ((c:cs) !! (4-count)) !! i == 0) = checkCouldWinRow (c:cs) i (col+1) 0         (-1) ant1 (c !! i) player obj
  | (count == obj)                                    = gap
  | (c !! i == player)                                = checkCouldWinRow cs     i (col+1) (count+1) gap  ant1 (c !! i) player obj forbid
  | (c !! i == 0 && i > 0 && c !! (i-1) == 0)         = checkCouldWinRow cs     i (col+1) 0         (-1) ant1 (c !! i) player obj forbid
  | (c !! i == 0 && ant1 == player && ant2 == 0)      = checkCouldWinRow cs     i (col+1) 3         col  ant1 (c !! i) player obj forbid
  | (c !! i == 0 && ant1 == player)                   = checkCouldWinRow cs     i (col+1) (count+1) col  ant1 (c !! i) player obj forbid
  | (c !! i == 0)                                     = checkCouldWinRow cs     i (col+1) 1         col  ant1 (c !! i) player obj forbid
  | (c !! i /= player)                                = checkCouldWinRow cs     i (col+1) 0         (-1) ant1 (c !! i) player obj forbid

checkNextWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> [Int] -> IO Int
checkNextWinDia1 (c:cs) col row n player obj forbid
  | (col == n - 3) = do return (-1)
  | (row == length c) = do
    let res = checkCouldWinDia1 (c:cs) col (row-1) 0 (-1) 2 2 player obj forbid
    if (res /= -1) then
      return res
    else checkNextWinDia1 cs (col+1) row n player obj forbid
  | (col == 0) = do
    let res = checkCouldWinDia1 (c:cs) col row 0 (-1) 2 2 player obj forbid
    if (res /= -1) then
      return res
    else checkNextWinDia1 (c:cs) col (row+1) n player obj forbid

checkCouldWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> Int
checkCouldWinDia1 [] _ _ count gap _ _ _ obj _
  | (count == obj) = gap
  | otherwise      = -1
checkCouldWinDia1 (c:cs) col row count gap ant2 ant1 player obj forbid
  | (count == obj)                                       = gap
  | (row == -1)                                          = (-1)
  | (elem col forbid)                                    = checkCouldWinDia1 cs (col+1) (row-1) 0 (-1) ant1 2 player obj forbid
  | (c !! row == player)                                 = checkCouldWinDia1 cs (col+1) (row-1) (count+1) gap ant1 (c !! row) player obj forbid
  | (c !! row == 0 && row > 0 && c !! (row-1) == 0)      = checkCouldWinDia1 cs (col+1) (row-1) 0 (-1) ant1 (c !! row) player obj forbid
  | (c !! row == 0 && ant1 == player && ant2 == 0)       = checkCouldWinDia1 cs (col+1) (row-1) 2 col ant1 (c !! row) player obj forbid
  | (c !! row == 0 && ant1 == player)                    = checkCouldWinDia1 cs (col+1) (row-1) (count+1) col ant1 (c !! row) player obj forbid
  | (c !! row == 0)                                      = checkCouldWinDia1 cs (col+1) (row-1) 1 col ant1 (c !! row) player obj forbid
  | (c !! row /= player)                                 = checkCouldWinDia1 cs (col+1) (row-1) 0 (-1) ant1 (c !! row) player obj forbid

checkNextWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> [Int] -> IO Int
checkNextWinDia2 (c:cs) col (-1) n player obj forbid
  | (col == n - 3) = do return (-1)
  | otherwise = do
    let res = checkCouldWinDia2 (c:cs) col 0 0 (-1) 2 2 player obj forbid
    if (res /= -1) then
      return res
    else checkNextWinDia2 cs (col+1) (-1) n player obj forbid
checkNextWinDia2 cs 0 row n player obj forbid = do
  let res = checkCouldWinDia2 cs 0 row 0 (-1) 2 2 player obj forbid
  if (res /= -1) then
    return res
  else checkNextWinDia2 cs 0 (row-1) n player obj forbid

checkCouldWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> Int
checkCouldWinDia2 [] _ _ count gap _ _ _ obj _
  | (count == obj) = gap
  | otherwise      = -1
checkCouldWinDia2 (c:cs) col row count gap ant2 ant1 player obj forbid
  | (count == obj)                                       = gap
  | (row == length c)                                    = (-1)
  | (elem col forbid)                                    = checkCouldWinDia2 cs (col+1) (row+1) 0 (-1) ant1 2 player obj forbid
  | (c !! row == player)                                 = checkCouldWinDia2 cs (col+1) (row+1) (count+1) gap ant1 (c !! row) player obj forbid
  | (c !! row == 0 && row > 0 && c !! (row-1) == 0)      = checkCouldWinDia2 cs (col+1) (row+1) 0 (-1) ant1 (c !! row) player obj forbid
  | (c !! row == 0 && ant1 == player && ant2 == 0)       = checkCouldWinDia2 cs (col+1) (row+1) 2 col ant1 (c !! row) player obj forbid
  | (c !! row == 0 && ant1 == player)                    = checkCouldWinDia2 cs (col+1) (row+1) (count+1) col ant1 (c !! row) player obj forbid
  | (c !! row == 0)                                      = checkCouldWinDia2 cs (col+1) (row+1) 1 col ant1 (c !! row) player obj forbid
  | (c !! row /= player)                                 = checkCouldWinDia2 cs (col+1) (row+1) 0 (-1) ant1 (c !! row) player obj forbid

--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000000  PLAY  000000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

play :: Board -> Int -> Strategy -> IO Int--winner
play b 1 strat = do
  putStrLn "Select a column"
  col <- getLine
  let c = read col :: Int
  let (new_b, i) = updateBoard b c 1
  --putStrLn $ "Index : "++(show i)
  let w = checkWin new_b c i 1
  if (w /= -1) then do
    showBoard new_b
    return w
  else play new_b 2 strat
--play new_b ((mod player 2)+1)
play b 2 strat = do
  c <- strat b
  let (new_b, i) = updateBoard b c 2
  showBoard new_b
  putStrLn $ "CPU has choosen the row "++(show c)
  let w = checkWin new_b c i 2
  if (w /= -1) then
    return w
  else play new_b 1 strat

--00000000000000000000000000000000000000000000000000000000000000000000000
--00000000000000000000000000000  MAIN  0000000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

main :: IO ()
main = do
  putStrLn "Welcome to the Connect4 game"
  b <- initBoard
  s <- chooseStrategy
  showBoard b
  winner <- play b 1 s
  case winner of
    0 -> putStrLn "Draw"
    1 -> putStrLn "You win!"
    2 -> putStrLn "CPU wins!"
    otherwise -> putStrLn "Error. Something were wrong"
  return ()

--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000  STRATEGIES  000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

randomStrat :: Board -> IO Int
randomStrat (Board cs) = do
  res <- randInt 0 cols
  let (new_b, i) = updateBoard (Board cs) res 1
  if (i == -1) then do -- if the row selected is full then retry
    randomStrat (Board cs)
  else return res
  where
    cols = (length cs) - 1

greedyStrat :: Board -> IO Int
greedyStrat (Board cs) = do
  c1 <- checkNextWin cs 2 4 [] --check if CPU can win => wins
  if (c1 /= -1) then
    return c1
  else do
    c2 <- checkNextWin cs 1 4 [] -- check if human can win => avoid it
    if (c2 /= -1) then
      return c2
    else do
      c3 <- greedyStratAux (Board cs) 2 3
      if (c3/= -1) then
        return c3
      else do
        c4 <- greedyStratAux (Board cs) 1 3
        if (c4/= -1) then
          return c4
        else randomStrat (Board cs)

greedyStratAux :: Board -> Int -> Int -> IO Int
greedyStratAux _ _ 1 = do return (-1)
greedyStratAux (Board cs) player n = do
  c1 <- checkNextWin cs player n []
  if (c1 /= -1) then
    return c1
  else greedyStratAux (Board cs) player (n-1)

-----------------------------------------------------------
smartStrat :: Board -> IO Int
smartStrat (Board cs) = do
  c1 <- checkNextWin cs 2 4 [] --check if CPU can win => wins
  if (c1 /= -1) then
    return c1
  else do
    c2 <- checkNextWin cs 1 4 [] -- check if human can win => avoid it
    if (c2 /= -1) then
      return c2
    else do
      c3 <- smartStratAux (Board cs) 2 3 []
      if (c3/= -1) then
        return c3
      else do
        c4 <- smartStratAux (Board cs) 1 3 []
        if (c4/= -1) then
          return c4
        else randomStrat (Board cs)

smartStratAux :: Board -> Int -> Int -> [Int] -> IO Int
smartStratAux _ _ 1 _ = do return (-1)
smartStratAux (Board cs) player n x = do
  c1 <- checkNextWin cs player n x
  if (c1 /= -1) then do
    let ((Board new_cs), i) = updateBoard (Board cs) c1 player
    future <- checkNextWin new_cs 1 4 x
    if (future /= -1) then
      smartStratAux (Board cs) player 3 (future:x)
    else return c1
  else smartStratAux (Board cs) player (n-1) x

--PROBLEMAS:
-- PORQUE SACA ENCIMA MIA?

nullBoard :: Board
nullBoard = createBoard 0 0

treeStrat :: Board -> IO Int
treeStrat b = do
  let (Node _ childs) = createTree b 2 4
  let scores = getScoreChilds childs 1
  return (maximum scores)

--calcScore :: Board -> Int -> Int


getScore :: Tree Board -> Int -> Int
getScore (Node b []) p = 0--calcScore b p
getScore (Node b childs) p
  | (p == 1) = minimum (getScoreChilds childs (changeP p))
  | (p == 2) = maximum (getScoreChilds childs (changeP p))

getScoreChilds :: [Tree Board] -> Int -> [Int]
getScoreChilds [] _ = []
getScoreChilds (t:ts) p = ((getScore t p):(getScoreChilds ts p))

createTree :: Board -> Int -> Int -> Tree Board
createTree b _ 0 = (Node b [])
createTree b p depth = (Node b (createChildTrees childs (changeP p) (depth-1)))
  where
    childs = createChilds b p 0 depth

createChildTrees :: [Board] -> Int -> Int -> [Tree Board]
createChildTrees [] _ _ = []
createChildTrees (b:childs) p depth = ((createTree b p depth):(createChildTrees childs p depth))

createChilds :: Board -> Int -> Int -> Int -> [Board]
createChilds (Board cs) p m depth
  | (m == (length cs)) = []
  | (i == -1 && depth == 4) = (nullBoard:(createChilds (Board cs) p (m+1) depth))
  | (i == -1) = createChilds (Board cs) p (m+1) depth
  | otherwise = (new_b:(createChilds (Board cs) p (m+1) depth))
  where
    (new_b, i) = updateBoard (Board cs) m p

changeP :: Int -> Int
changeP 1 = 2
changeP 2 = 1

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
  random <- randomIO :: IO Int
  let result = low + random `mod` (high - low + 1)
  return result
