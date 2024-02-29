
-----------------------------------------------------------------------------------------

-- ASKHSH 1
nearest :: [Int]->Int->Int

nearest s n = nearest2 s n 1 (abs (n - head s)) 1
  where
    nearest2 [] _ _ _ best = best
    nearest2 (h:t) n p d best
      | abs (n - h) < d = nearest2 t n (p+1) (abs (n - h)) p
      | otherwise = nearest2 t n (p+1) d best

  


-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

replace :: String->String->String->String

replace a b w = go w
  where
    go [] = []
    go str@(c:cs)
      | matches a str = b ++ go (dropChars (length a) str)
      | otherwise = c : go cs

matches :: String -> String -> Bool
matches [] _ = True
matches _ [] = False
matches (a:as) (b:bs) = a == b && matches as bs

dropChars :: Int -> String -> String
dropChars 0 xs = xs
dropChars _ [] = []
dropChars n (x:xs) = dropChars (n - 1) xs