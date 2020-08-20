sumSqDiff :: Int -> Int
sumSqDiff n = ((n * (n + 1) `div` 2) ^ 2) - ((((2 * n) + 1) * (n + 1) * n) `div` 6)

euler006 :: IO()
euler006 = do
  inp <- getContents
  let lst = map read (words inp)
  mapM_ (putStrLn . show) $ map sumSqDiff lst

main = do
  t <- getLine
  euler006