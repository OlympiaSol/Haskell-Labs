
-----------------------------------------------------------------------------------------

-- ASKHSH 1


area :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
area (x1, y1) (x2, y2) (x3, y3) = sqrt(t*(t-a)*(t-b)*(t-c))
  where
    a = sqrt((x2 - x1)^2 + (y2 - y1)^2)
    b = sqrt((x3 - x2)^2 + (y3 - y2)^2)
    c = sqrt((x1 - x3)^2 + (y1 - y3)^2)
    t = (a + b + c) / 2                                  




-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

parking :: (Int,Int) -> (Int,Int) -> Int
parking (h1, m1) (h2, m2)
  | hours <= 3 = 8
  | hours <= 6 = 8 + (hours - 3) * 2
  | otherwise = 14 + (hours - 6)
  where
    hours = ceiling (min / 60)
    min = fromIntegral (hDiff * 60 + minDiff)
    hDiff = h2 - h1
    minDiff = m2 - m1
                 




