import Data.List(nub)

sumDiv35 :: Int -> Int
sumDiv35 n = (a + b) - c where f n k = let a = n `div` k in
                                (a * (a + 1) * k) `div` 2
                               a = f (n - 1) 3
                               b = f(n - 1) 5
                               c = f (n - 1) 15                         
  
euler001 :: IO()
euler001 = do
  inp <- getContents
  let lst = map read (words inp)
  mapM_ (putStrLn . show) $ map sumDiv35 lst
  
main = do
  t <- getLine
  euler001