module PolymorphismTypeClasses where

import          Prelude hiding (fst)

fst :: (a, b) -> a 
fst (x, y) = x


restrictedFst :: (Int, Int) -> Int
restrictedFst = fst

foo1 :: (Int, Int) -> (Int, Int)
foo1 (x, y) = (x + 1, y + 1)

-- type classes
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool


add3 :: int -> (int -> (Int -> Int))

add3' :: (Int, Int, Int) -> Int
add3; (x, y, z) = x + y + z

-- sum adds 
sum :: [Int] -> Int
sum[] = 0
sum(x : xs) = x + sum xs

flatten :: Tree a-> [a]
flatten (Leaf a) = [a]
example :: [Int]
example = flatten (Node (Node (Leaf 1)) (Leaf 2( (Leaf7)))

add 5 = (\x -> (\y -> (\z -> x + y + z)))