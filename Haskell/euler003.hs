maxPrime :: Int -> Int -> Int
maxPrime n k
  | (n `mod` 2) == 0 && n > 2 = maxPrime (n `div` 2) k
  | (n `mod` k) == 0 && n > k = maxPrime (n `div` k) k
  | otherwise                 =  if (floor $ sqrt $ fromIntegral n)^2 <= k^2 then n else maxPrime n (k + 1)
                          
euler003 :: IO()
euler003 = do
  inp <- getContents
  let lst = map read (words inp)
  mapM_ (putStrLn . show) $ map f lst where f lst = maxPrime lst 3

main = do
  t <- getLine
  euler003
  