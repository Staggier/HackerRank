fibonacci:: Int -> Int -> Int -> Int
fibonacci n k max
  | (k > max)      = 0
  | (mod k 2 == 0) = k + (fibonacci k (n + k) max)
  | otherwise      = fibonacci k (n + k) max

euler002 :: IO ()
euler002 = do
  inp <- getContents
  let lst = map read (words inp)
  mapM_ (putStrLn . show) $ map (fibonacci 0 1) lst

main = do
  t <- getLine
  euler002