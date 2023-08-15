
factorial :: Integer -> Integer
factorial n = product[1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

checkNumbers :: (Eq a) => a -> a -> Bool
checkNumbers x y = x == y

luckyNumber :: (Integral a) => a -> String
luckyNumber 8 = "Your Luck Number"
luckyNumber x = "Not your luck number"

-- factorial using recursion
factorial2 :: Integer -> Integer
factorial2 0 = 1
factorial2 n = n * factorial2(n - 1)

-- => symbol used for pattern matching
addCordinates :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addCordinates (x1, y1) (x2, y2) =  (x1 + x2, y1 + y2)


addLists :: 



