digit :: Integral x => x -> [x]
digit 0 = []
digit x = let (q, r) = quotRem x 10 in digit q ++ [r]

lps :: [Integer] -> Int -> Int -> Integer -> Integer
lps d k i m
  | (length (drop i d) - k + 1 < k) = m
  | (otherwise)             = let t = product $ take k $ drop i d in 
                                    lps d k (i + 1) (if m > t then m else t)

euler008 :: Int -> IO ()
euler008 t = do
  l1 <- getLine
  l2 <- getLine
  
  let n = read l2
  let k = read $ (words l1) !! 1
  
  if t == 1 then do  
    putStrLn $ show $ lps (digit n) k 0 0
  else do
    putStrLn $ show $ lps (digit n) k 0 0
    euler008 (t - 1)
   
  
main = do
  t <- getLine
  euler008 (read t)
  
