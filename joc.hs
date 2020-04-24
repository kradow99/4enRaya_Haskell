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
      2 -> return treeStrat

--00000000000000000000000000000000000000000000000000000000000000000000000
--0000000000000000000000000000 CHECK WIN  0000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

checkWin :: Board -> Int -> Int
checkWin (Board cs) player
  | (checkWinVert cs player /= -1)                                    = player
  | (checkWinHori cs 0 player /= -1)                                  = player
  | (checkWinDia1 cs 0 3 (length cs) player /= -1)                    = player
  | (checkWinDia2 cs 0 (length (cs !! 0)-4) (length cs) player /= -1) = player
  | (checkDraw cs)                                                    = 0
  | otherwise                                                         = -1

checkDraw :: [[Int]] -> Bool
checkDraw [] = True
checkDraw (c:cs)
  | (elem 0 c) = False
  | otherwise = checkDraw cs

checkWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int
checkWinDia1 (c:cs) col row n p
  | (col == n - 3) = -1
  | (checkWinOneDia1 (c:cs) row p 0 /= -1) = p
  | (row == (length c)-1) = checkWinDia1 cs (col+1) row n p
  | (col == 0)            = checkWinDia1 (c:cs) col (row+1) n p

checkWinOneDia1 :: [[Int]] -> Int -> Int -> Int -> Int
checkWinOneDia1 _ _ p 4 = p
checkWinOneDia1 [] _ _ _ = -1
checkWinOneDia1 (c:cs) row p count
  | (row == -1)     = -1
  | (c !! row == p) = checkWinOneDia1 cs (row-1) p (count+1)
  | otherwise       = checkWinOneDia1 cs (row-1) p 0

checkWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int
checkWinDia2 (c:cs) col row n p
  | (col == n - 3) = -1
  | (checkWinOneDia2 (c:cs) row p 0 /= -1) = p
  | (row == 0)            = checkWinDia2 cs (col+1) row n p
  | (col == 0)            = checkWinDia2 (c:cs) col (row-1) n p

checkWinOneDia2 :: [[Int]] -> Int -> Int -> Int -> Int
checkWinOneDia2 _ _ p 4 = p
checkWinOneDia2 [] _ _ _ = -1
checkWinOneDia2 (c:cs) row p count
  | (row == length c)     = -1
  | (c !! row == p) = checkWinOneDia2 cs (row+1) p (count+1)
  | otherwise       = checkWinOneDia2 cs (row+1) p 0


checkWinHori :: [[Int]] -> Int -> Int -> Int
checkWinHori (c:cs) r p
  | (r == height)                    = -1
  | (checkWinRow (c:cs) r p 0 /= -1) = p
  | otherwise                        = checkWinHori (c:cs) (r+1) p
  where
    height = length c

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
  | otherwise = checkWinVert cs p

checkWinCol :: [Int] -> Int -> Int -> Int
checkWinCol _ p 4 = p
checkWinCol [] _ _ = -1
checkWinCol (x:xs) p count
  | (x == p)  = checkWinCol xs p (count+1)
  | otherwise = checkWinCol xs p 0


--00000000000000000000000000000000000000000000000000000000000000000000000
--00000000000000000000000 CHECK NEXT WIN  0000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

checkNextWin :: [[Int]] -> Int -> Int -> [Int] -> Int
checkNextWin cs player obj forbid
  | (v  /= -1) = v
  | (d1 /= -1) = d1
  | (d2 /= -1) = d2
  | (h  /= -1) = h
  | otherwise = -1
  where
    v  = checkNextWinVert cs 0 player obj forbid
    d1 = checkNextWinDia1 cs 0 3 (length cs) player obj forbid
    d2 = checkNextWinDia2 cs 0 (length (cs !! 0) - 4) (length cs) player obj forbid
    h  = checkNextWinHori cs 0 player obj forbid

checkNextWinVert :: [[Int]] -> Int -> Int -> Int -> [Int] -> Int
checkNextWinVert [] _ _ _ _ = -1
checkNextWinVert (c:cs) col player obj forbid
  | (elem col forbid)                 = checkNextWinVert cs (col+1) player obj forbid
  | (checkCouldWinCol c 0 player obj) = col
  | otherwise                         = checkNextWinVert cs (col+1) player obj forbid

checkCouldWinCol :: [Int] -> Int -> Int -> Int -> Bool
checkCouldWinCol [] _ _ _ = False
checkCouldWinCol (0:xs) count player obj
  | (count == obj - 1 && ((length xs)+1 >= 4-count)) = True
  | otherwise = False
checkCouldWinCol (x:xs) count player obj
  | (x == player) = checkCouldWinCol xs (count+1) player obj
  | (x == 0)      = False
  | otherwise     = checkCouldWinCol xs 0 player obj

checkNextWinHori :: [[Int]] -> Int -> Int -> Int -> [Int] -> Int
checkNextWinHori cs i player obj forbid
  | (i == length (cs !! 0)) = -1
  | (res /= -1)             = res
  | otherwise               = checkNextWinHori cs (i+1) player obj forbid
  where
    res = checkCouldWinRow cs i 0 0 (-1) 2 2 player obj forbid

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

checkNextWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> [Int] -> Int
checkNextWinDia1 (c:cs) col row n player obj forbid
  | (col == n - 3)                  = -1
  | (row == length c && res1 /= -1) = res1
  | (row == length c)               = checkNextWinDia1 cs    (col+1) row     n player obj forbid
  | (col == 0 && res2 /= -1)        = res2
  | (col == 0)                      = checkNextWinDia1 (c:cs) col    (row+1) n player obj forbid
  where
    res1 = checkCouldWinDia1 (c:cs) col (row-1) 0 (-1) 2 2 player obj forbid
    res2 = checkCouldWinDia1 (c:cs) col  row    0 (-1) 2 2 player obj forbid

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

checkNextWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> [Int] -> Int
checkNextWinDia2 (c:cs) col (-1) n player obj forbid
  | (col == n - 3) = -1
  | (res /= -1)    = res
  | otherwise      = checkNextWinDia2 cs (col+1) (-1) n player obj forbid
  where
    res = checkCouldWinDia2 (c:cs) col 0 0 (-1) 2 2 player obj forbid
checkNextWinDia2 cs 0 row n player obj forbid
  | (res /= -1) = res
  | otherwise   = checkNextWinDia2 cs 0 (row-1) n player obj forbid
  where
    res = checkCouldWinDia2 cs 0 row 0 (-1) 2 2 player obj forbid


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
  let w = checkWin new_b 1
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
  let w = checkWin new_b 2

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
  let c1 = checkNextWin cs 2 4 [] --check if CPU can win => wins
  if (c1 /= -1) then
    return c1
  else do
    let c2 = checkNextWin cs 1 4 [] -- check if human can win => avoid it
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
  let c1 = checkNextWin cs player n []
  if (c1 /= -1) then
    return c1
  else greedyStratAux (Board cs) player (n-1)

-----------------------------------------------------------
smartStrat :: Board -> IO Int
smartStrat (Board cs) = do
  let c1 = checkNextWin cs 2 4 [] --check if CPU can win => wins
  if (c1 /= -1) then
    return c1
  else do
    let c2 = checkNextWin cs 1 4 [] -- check if human can win => avoid it
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
  let c1 = checkNextWin cs player n x
  if (c1 /= -1) then do
    let ((Board new_cs), i) = updateBoard (Board cs) c1 player
    let future = checkNextWin new_cs 1 4 x
    if (future /= -1) then
      smartStratAux (Board cs) player 3 (future:x)
    else return c1
  else smartStratAux (Board cs) player (n-1) x

--PROBLEMAS:
-- PORQUE SACA ENCIMA MIA?

treeStrat :: Board -> IO Int
treeStrat (Board cs) = do
  let (Node _ childs) = createTree (Board cs) 2 5
  --putStrLn $ "childs: "++(show childs)
  let scores = getScoreChilds childs 1
  let maxi = maximum scores
  putStrLn $ "scores: "++(show scores)
  let results = filter (>0) $ indexMax maxi 1 scores
  putStrLn $ "results: "++(show results)
  let mid = div ((length results)-1) 2
  return $ (results !! mid)-1

calcScore :: Board -> Int -> Int
calcScore (Board cs) p
  | (length cs == 0) = -9999
  | (result == 0)    = 0
  | (result == 1)    = num - s
  | (result == 2)    = s - num
  | (p == 1 && next /= -1) = num - s
  | (p == 2 && next /= -1) = s - num
  | otherwise = -69
  where
    result = checkWin (Board cs) p
    s = maxScore (Board cs)
    num = numTokens cs p
    next = checkNextWin cs p 3 []

--heuristic :: [[Int]] -> Int -> Int
--heuristic cs 1
--  | (next /= -1) = num - s
--  | otherwise    =
getScore :: Tree Board -> Int -> Int
getScore (Node b []) p = calcScore b (changeP p)
getScore (Node b childs) p
  | (p == 1) = minimum (getScoreChilds childs (changeP p))
  | (p == 2) = maximum (getScoreChilds childs (changeP p))

getScoreChilds :: [Tree Board] -> Int -> [Int]
getScoreChilds [] _ = []
getScoreChilds (t:ts) p = ((getScore t p):(getScoreChilds ts p))

createTree :: Board -> Int -> Int -> Tree Board
createTree b _ 0 = (Node b [])
createTree (Board cs) p depth
  | (length cs == 0)                        = (Node (Board cs) [])
  | (checkWin (Board cs) (changeP p) == -1) = (Node (Board cs) (createChildTrees childs (changeP p) (depth-1)))
  | otherwise                               = (Node (Board cs) [])
  where
    childs = createChilds (Board cs) p 0 depth

createChildTrees :: [Board] -> Int -> Int -> [Tree Board]
createChildTrees [] _ _ = []
createChildTrees (b:childs) p depth = ((createTree b p depth):(createChildTrees childs p depth))

createChilds :: Board -> Int -> Int -> Int -> [Board]
createChilds (Board cs) p m depth
  | (m == (length cs)) = []
  | (i == -1 && depth == 5) = (nullBoard:(createChilds (Board cs) p (m+1) depth))
  | (i == -1) = createChilds (Board cs) p (m+1) depth
  | otherwise = (new_b:(createChilds (Board cs) p (m+1) depth))
  where
    (new_b, i) = updateBoard (Board cs) m p

changeP :: Int -> Int
changeP 1 = 2
changeP 2 = 1

indexMax :: Eq a => a -> Int -> [a] -> [Int]
indexMax _ _ [] = []
indexMax e i (x:xs)
  | (e == x)  =   (i:(indexMax e (i+1) xs))
  | otherwise = ((-i):(indexMax e (i+1) xs))

nullBoard :: Board
nullBoard = createBoard 0 0

maxScore :: Board -> Int
maxScore (Board cs) = 1 + (div ((length (cs !! 0))*(length cs)) 2)

numTokens :: [[Int]] -> Int -> Int
numTokens [] _ = 0
numTokens (c:cs) p = (numTokensCol c p) + (numTokens cs p)

numTokensCol :: [Int] -> Int -> Int
numTokensCol [] _ = 0
numTokensCol (x:xs) p
  | (x == p) = 1 + (numTokensCol xs p)
  | otherwise = numTokensCol xs p

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
  random <- randomIO :: IO Int
  let result = low + random `mod` (high - low + 1)
  return result
