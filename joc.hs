import System.Random

data Board = Board [[Int]]

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

--00000000000000000000000000000000000000000000000000000000000000000000000
--000000000000000000000000000000  PLAY  000000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

checkWin :: Board -> Int -> Int -> Int -> IO Int
checkWin _ _ (-1) _ = do
  return (-1)
checkWin b c i p = do
  r1 <- checkWinVert b c i p
  if (r1 /= -1) then
    return r1
  else do
    r2 <- checkWinVert b c i p
    if (r2 /= -1) then
      return r2
    else do
      r3 <- checkWinVert b c i p
      if (r3 /= -1) then
        return r3
      else do
        r4 <- checkWinVert b c i p
        if (r4 /= -1) then
          return r4
        else
          return (-1)

checkWinVert :: Board -> Int -> Int -> Int -> IO Int
checkWinVert b c i p = do
  return (-1)

play :: Board -> Int -> IO Int--winner
play b player = do
  putStrLn "Select a column"
  col <- getLine
  let c = read col :: Int
  let (new_b, i) = updateBoard b c player
  showBoard new_b
  putStrLn $ "Index : "++(show i)
  w <- checkWin new_b c i player
  if (w /= -1) then
    return w
  else
    play new_b ((mod player 2)+1)

--00000000000000000000000000000000000000000000000000000000000000000000000
--00000000000000000000000000000  MAIN  0000000000000000000000000000000000
--00000000000000000000000000000000000000000000000000000000000000000000000

main :: IO ()
main = do
  putStrLn "Welcome to the Connect4 game"
  b <- initBoard
  showBoard b
  winner <- play b 1
  case winner of
    0 -> putStrLn "Draw"
    1 -> putStrLn "You win!"
    2 -> putStrLn "CPU wins!"
    otherwise -> putStrLn "Error. Something were wrong"
  return ()

b  = createBoard 5 10
