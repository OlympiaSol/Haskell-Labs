
-----------------------------------------------------------------------------------------

-- ASKHSH 1

generating :: (Int->Double)->Int->(Double->Double)

generating f k = gk
  where
    gk z = sum 0
      where
        sum i
          | i > k     = 0
          | i <= k    = f i * z ^ i + sum (i + 1)


-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

mapi :: [u] -> (u -> Int -> v) -> [v]
mapi s f = mapi1 s f 1
  where
    mapi1 :: [u] -> (u -> Int -> v) -> Int -> [v]
    mapi1 [] _ _ = []
    mapi1 (h:t) f p = (f h p) : mapi1 t f (p+1)