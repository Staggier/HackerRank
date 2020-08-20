primes :: [Int]
primes = sieve [2..] where 
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

euler007 :: IO()
euler007 = do
  inp <- getContents
  let lst = map read (words inp)
  mapM_ (putStrLn . show) $ map (\n -> primes!!(n-1)) lst
  
main = do
  t <- getLine
  euler007