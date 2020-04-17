import System.Random

data Board = Board [[Int]]
type Strategy = Board -> IO Int

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
  | ((c !! n) == 0) = "·" ++ (printRow (Board cs) n)
  | ((c !! n) == 1) = "o" ++ (printRow (Board cs) n)
  | ((c !! n) == 2) = "x" ++ (printRow (Board cs) n)

printIndex :: Int -> Int -> IO ()
printIndex 0 m = do
  putStr $ show 0
  printIndex (1) m
printIndex i m = do
  if (i == m) then do
    putStrLn $ show i
  else do
    putStr " "
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

checkWin :: Board -> Int -> Int -> Int -> IO Int
checkWin _ _ (-1) _ = do
  return (-1)
checkWin (Board cs) col row player = do
  if (checkDraw cs) then
    return 0
  else do
    r1 <- checkWinVert (cs !! col) player 0
    if (r1 /= -1) then
      return r1
    else do
      r2 <- checkWinHori cs row player 0
      if (r2 /= -1) then
        return r2
      else do
        r3 <- checkWinDia1 cs col row col row player 0
        if (r3 /= -1) then
          return r3
        else do
          r4 <- checkWinDia2 cs col row col row player 0
          if (r4 /= -1) then
            return r4
          else
            return (-1)

checkDraw :: [[Int]] -> Bool
checkDraw [] = True
checkDraw (c:cs)
  | (fullCol c) = checkDraw cs
  | otherwise = False

fullCol :: [Int] -> Bool
fullCol [] = True
fullCol (0:xs) = False
fullCol (x:xs) = fullCol xs

checkWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int
checkWinDia1 _ _ _ _ _ p 4 = do return p
checkWinDia1 cs cini rini col row p count
  | (row == -1)        = do return (-1)
  | (col == length cs) = do return (-1)

  | (col == cini) = checkWinDia1 cs cini rini (cini-1) (rini+1) p 1

  | (col == -1)               = checkWinDia1 cs cini rini (cini+1) (rini-1) p count
  | (row == length (cs !! 0)) = checkWinDia1 cs cini rini (cini+1) (rini-1) p count

  | (col < cini) = do
    if (((cs !! col) !! row) == p) then
      checkWinDia1 cs cini rini (col-1) (row+1) p (count+1)
    else checkWinDia1 cs cini rini (cini+1) (rini-1) p count
  | (col > cini) = do
    if (((cs !! col) !! row) == p) then
      checkWinDia1 cs cini rini (col+1) (row-1) p (count+1)
    else return (-1)

checkWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int
checkWinDia2 _ _ _ _ _ p 4 = do return p
checkWinDia2 cs cini rini col row p count
  | (row == length (cs !! 0)) = do return (-1)
  | (col == length cs)        = do return (-1)

  | (col == cini) = checkWinDia2 cs cini rini (cini-1) (rini-1) p 1

  | (col == -1) = checkWinDia2 cs cini rini (cini+1) (rini+1) p count
  | (row == -1) = checkWinDia2 cs cini rini (cini+1) (rini+1) p count

  | (col < cini) = do
    if (((cs !! col) !! row) == p) then
      checkWinDia2 cs cini rini (col-1) (row-1) p (count+1)
    else checkWinDia2 cs cini rini (cini+1) (rini+1) p count
  | (col > cini) = do
    if (((cs !! col) !! row) == p) then
      checkWinDia2 cs cini rini (col+1) (row+1) p (count+1)
    else return (-1)

checkWinHori :: [[Int]] -> Int -> Int -> Int -> IO Int
checkWinHori _ _ p 4 = do
  return p
checkWinHori [] _ _ _ = do
  return (-1)
checkWinHori (c:cs) r p count = do
  if ((c !! r) == p) then
    checkWinHori cs r p (count+1)
  else checkWinHori cs r p 0

checkWinVert :: [Int] -> Int -> Int -> IO Int
checkWinVert _ p 4 = do
  return p
checkWinVert [] _ _ = do
  return (-1)
checkWinVert (x:xs) p count = do
  if (x == p) then
    checkWinVert xs p (count+1)
  else checkWinVert xs p 0

--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000000  PLAY  000000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

play :: Board -> Int -> Strategy -> IO Int--winner
play b 1 strat = do
  putStrLn "Select a column"
  col <- getLine
  let c = read col :: Int
  let (new_b, i) = updateBoard b c 1
  putStrLn $ "Index : "++(show i)
  w <- checkWin new_b c i 1
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
  w <- checkWin new_b c i 2
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
  c1 <- checkNextWin cs 2 --check if CPU can win => wins
  if (c1 /= -1) then
    return c1
  else do
    c2 <- checkNextWin cs 1 -- check if human can win => avoid it
    if (c2 /= -1) then
      return c2
    else randomStrat (Board cs)

checkNextWin :: [[Int]] -> Int -> IO Int
checkNextWin cs player = do
  h <- checkNextWinHori cs 0 player
  if (h /= -1) then
    return h
  else do
    v <- checkNextWinVert cs 0 player
    if (v /= -1) then
      return v
    else do
      d1 <- checkNextWinDia1 cs 0 3 (length cs) player
      if (d1 /= -1) then
        return d1
      else do
        d2 <- checkNextWinDia2 cs 0 (length (cs !! 0) - 4) (length cs) player
        if (d2 /= -1) then
          return d2
        else return (-1)

checkNextWinVert :: [[Int]] -> Int -> Int -> IO Int
checkNextWinVert [] _ _ = do return (-1)
checkNextWinVert (c:cs) i player = do
  if (checkCouldWinCol c 0 player) then
    return i
  else checkNextWinVert cs (i+1) player

checkCouldWinCol :: [Int] -> Int -> Int -> Bool
checkCouldWinCol [] _ _ = False
checkCouldWinCol (0:xs) 3 _ = True
checkCouldWinCol (x:xs) count player
  | (x == player) = checkCouldWinCol xs (count+1) player
  | (x == 0)      = False
  | otherwise     = checkCouldWinCol xs 0 player

checkNextWinHori :: [[Int]] -> Int -> Int -> IO Int
checkNextWinHori cs i player
  | (i == length (cs !! 0)) = do return (-1)
  | otherwise = do
    let res = checkCouldWinRow cs i 0 0 (-1) 2 2 player
    if (res /= -1) then
      return res
    else checkNextWinHori cs (i+1) player

checkCouldWinRow :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkCouldWinRow _ _ _ 4 gap _ _ _ = gap
checkCouldWinRow [] _ _ _ _ _ _ _  = (-1)
checkCouldWinRow (c:cs) i col count gap ant2 ant1 player
  | (c !! i == player)                             = checkCouldWinRow cs i (col+1) (count+1) gap ant1 (c !! i) player
  | (c !! i == 0 && i > 0 && c !! (i-1) == 0)      = checkCouldWinRow cs i (col+1) 0 (-1) ant1 (c !! i) player
  | (c !! i == 0 && ant1 == player && ant2 == 0)   = checkCouldWinRow cs i (col+1) 3 col ant1 (c !! i) player
  | (c !! i == 0 && ant1 == player)                = checkCouldWinRow cs i (col+1) (count+1) col ant1 (c !! i) player
  | (c !! i == 0)                                  = checkCouldWinRow cs i (col+1) 1 col ant1 (c !! i) player
  | (c !! i /= player)                             = checkCouldWinRow cs i (col+1) 0 (-1) ant1 (c !! i) player

checkNextWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> IO Int
checkNextWinDia1 (c:cs) col row n player
  | (col == n - 3) = do return (-1)
  | (row == length c) = do
    let res = checkCouldWinDia1 (c:cs) col (row-1) 0 (-1) 2 2 player
    if (res /= -1) then
      return res
    else checkNextWinDia1 cs (col+1) row n player
  | (col == 0) = do
    let res = checkCouldWinDia1 (c:cs) col row 0 (-1) 2 2 player
    if (res /= -1) then
      return res
    else checkNextWinDia1 (c:cs) col (row+1) n player


checkCouldWinDia1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkCouldWinDia1 _ _ _ 4 gap _ _ _ = gap
checkCouldWinDia1 [] _ _ _ _ _ _ _  = (-1)
checkCouldWinDia1 (c:cs) col row count gap ant2 ant1 player
  | (row == -1)                                          = (-1)
  | (c !! row == player)                                 = checkCouldWinDia1 cs (col+1) (row-1) (count+1) gap ant1 (c !! row) player
  | (c !! row == 0 && row > 0 && c !! (row-1) == 0)      = checkCouldWinDia1 cs (col+1) (row-1) 0 (-1) ant1 (c !! row) player
  | (c !! row == 0 && ant1 == player && ant2 == 0)       = checkCouldWinDia1 cs (col+1) (row-1) 2 col ant1 (c !! row) player
  | (c !! row == 0 && ant1 == player)                    = checkCouldWinDia1 cs (col+1) (row-1) (count+1) col ant1 (c !! row) player
  | (c !! row == 0)                                      = checkCouldWinDia1 cs (col+1) (row-1) 1 col ant1 (c !! row) player
  | (c !! row /= player)                                 = checkCouldWinDia1 cs (col+1) (row-1) 0 (-1) ant1 (c !! row) player


checkNextWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> IO Int
checkNextWinDia2 (c:cs) col (-1) n player
  | (col == n - 3) = do return (-1)
  | otherwise = do
    let res = checkCouldWinDia2 (c:cs) col 0 0 (-1) 2 2 player
    if (res /= -1) then
      return res
    else checkNextWinDia2 cs (col+1) (-1) n player
checkNextWinDia2 cs 0 row n player = do
  let res = checkCouldWinDia2 cs 0 row 0 (-1) 2 2 player
  if (res /= -1) then
    return res
  else checkNextWinDia2 cs 0 (row-1) n player


checkCouldWinDia2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
checkCouldWinDia2 _ _ _ 4 gap _ _ _ = gap
checkCouldWinDia2 [] _ _ _ _ _ _ _  = (-1)
checkCouldWinDia2 (c:cs) col row count gap ant2 ant1 player
  | (row == length c)                                    = (-1)
  | (c !! row == player)                                 = checkCouldWinDia2 cs (col+1) (row+1) (count+1) gap ant1 (c !! row) player
  | (c !! row == 0 && row > 0 && c !! (row-1) == 0)      = checkCouldWinDia2 cs (col+1) (row+1) 0 (-1) ant1 (c !! row) player
  | (c !! row == 0 && ant1 == player && ant2 == 0)       = checkCouldWinDia2 cs (col+1) (row+1) 2 col ant1 (c !! row) player
  | (c !! row == 0 && ant1 == player)                    = checkCouldWinDia2 cs (col+1) (row+1) (count+1) col ant1 (c !! row) player
  | (c !! row == 0)                                      = checkCouldWinDia2 cs (col+1) (row+1) 1 col ant1 (c !! row) player
  | (c !! row /= player)                                 = checkCouldWinDia2 cs (col+1) (row+1) 0 (-1) ant1 (c !! row) player





smartStrat :: Board -> IO Int
smartStrat (Board cs) = randInt 0 cols
  where
    cols = (length cs) - 1

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
  random <- randomIO :: IO Int
  let result = low + random `mod` (high - low + 1)
  return result
