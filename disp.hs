

main = do
  x <- getLine
  let n = read x :: Int
  xx <- updatee b n
  return ()

updatee :: Int -> Int -> IO Int
updatee x y = do return y

b = 5
