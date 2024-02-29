
-----------------------------------------------------------------------------------------

-- ASKHSH 1

join :: Integer -> Integer -> Integer
join a b
  | a < 0 && b >= 0 = -1 * join2 (-a) b 
  | a >= 0 && b < 0 = -1 * join2 a (-b)
  | a >= 0 && b >= 0 = join2 a b
  | otherwise = join2 (-a) (-b)

join2 :: Integer -> Integer -> Integer
join2 i j
  | i `div` 10 == 0 = (13*(m + 5) + 19*(n + 3)) `mod` 10
  | otherwise = ((13*(m + 5) + 19*(n + 3)) `mod` 10) + 10 * (join2 (i `div` 10) (j `div` 10))
  where 
    (m, n) = twoLastDigits i j

twoLastDigits :: Integer -> Integer -> (Integer, Integer)
twoLastDigits i j = (i `mod` 10, j `mod` 10)



-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

kgcd :: Int -> Int -> Int -> Int
kgcd m n k
  | m <= n = kgcd1 m n k m
  | otherwise = kgcd1 m n k n

kgcd1 :: Int -> Int -> Int -> Int -> Int
kgcd1 m n k j
  | j == 0 = 0
  | ( k > 1 && commonDiv m n j) = kgcd1 m n (k-1) (j-1)
  | ( k == 1 && commonDiv m n j) = j
  | otherwise = kgcd1 m n k (j-1)

commonDiv :: Int -> Int -> Int -> Bool
commonDiv m n j = m `mod` j == 0 && n `mod` j == 0

