import System.Random

data Col = Col [Int] deriving(Show)

data Board = Board [Col] deriving(Show)

createBoard :: Int -> Int -> Board
createBoard n m = Board (take m $ repeat $ Col (take n $ repeat 0))


----------------PRINT THE BOARD------------------------------------------
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

printRow :: Board -> Int -> String
printRow (Board []) _ = ""
printRow (Board ((Col c):cs)) n
  | ((c !! n) == 0) = "·" ++ (printRow (Board cs) n)
  | ((c !! n) == 1) = "o" ++ (printRow (Board cs) n)
  | ((c !! n) == 2) = "x" ++ (printRow (Board cs) n)

showBoardRec :: Board -> Int -> IO ()
showBoardRec (Board cs) (-1) = do
  printIndex 0 ((length cs)-1)
  return ()
showBoardRec b n = do
  putStrLn $ printRow b n
  showBoardRec b (n-1)

showBoard :: Board -> IO()
showBoard (Board ((Col c):cs)) = showBoardRec (Board ((Col c):cs)) ((length c)-1)


---------------------------UPDATE THE BOARD--------------------------------------
updateCol :: [Int] -> Int -> Int -> [Int]
updateCol c (-1) _ = c
updateCol (x:xs) n p
  | (x == 0) = (p:xs)
  | otherwise = (x:(updateCol xs (n-1) p))

findCol :: [Col] -> Int -> Int -> [Col]
findCol [] _ _ = [] -- para cuando m > num_cols
findCol ((Col c):cs) m p
  | (m == 0)  = ((Col(updateCol c ((length c)-1) p)):cs)
  | otherwise = (Col c):(findCol cs (m-1) p)

             --Board -> Col -> Jug -> Result
updateBoard :: Board -> Int -> Int -> Board
updateBoard (Board cs) col p
  | (col > ((length cs)-1)) = Board cs
  | (col < 0)               = Board cs
  | otherwise               = (Board (findCol cs col p))


b = createBoard 5 15

createIntro :: IO Board
createIntro = do
  putStrLn "Welcome to the Connect4 game"
  putStrLn "Please introduce the number of rows of the board"
  rows <- getLine
  let n = read rows :: Int
  putStrLn "Please introduce the number of columns of the board"
  cols <- getLine
  let m = read cols :: Int
  return (createBoard n m)

play :: Board -> Int -> IO Int--winner
play b player = do
  putStrLn "Select a row"
  row <- getLine
  let r = read row :: Int
  let new_b = updateBoard b r player
  showBoard new_b
  if (player == 1) then play new_b 2
  else play new_b 1

main :: IO ()
main = do
  b <- createIntro
  showBoard b
  winner <- play b 1
  return ()
