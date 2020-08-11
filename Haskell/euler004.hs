isPalindrome :: [Char] -> Bool
isPalindrome s = (s == reverse s)

loopB :: Int -> Int -> Int -> Int -> Int -> Int
loopB n a b db pal
  | (a * b > pal)                    = pal
  | (a > b)                          = pal
  | (isPalindrome $ show $ a * b)    = loopB n a (b - db) db (if n > a * b then a * b else pal)
  | (otherwise)                      = loopB n a (b - db) db pal
  
loopA :: Int -> Int -> Int -> Int
loopA a pal n
  | (a == 99) = pal
  | otherwise =  let db  = if (a `mod` 11 == 0) then   1 else 11
                     b   = if (a `mod` 11 == 0) then 999 else 990
                     p   = loopB n a b db pal in
                 loopA (a - 1) (if p < n then p else pal) n

euler004 :: IO()
euler004 = do
  inp <- getContents
  let lst = map read (words inp)
  mapM_ (putStrLn . show) $ map (loopA 999 0) lst

main = do
  t <- getLine
  euler004