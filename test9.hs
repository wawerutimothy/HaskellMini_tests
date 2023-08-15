{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE BangPatterns#-}
module HigherOrderFunctions where

import Prelude hiding(all, sum, reverse, any, length, foldr, filter, (.), (++))

  
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f.g) x = f(g x)

example :: [Int]
example = take 100 . filter odd . map(\x -> x * x) $ [1..]


foreach :: [a] -> ( a -> b) -> [b]

foreach = flip map

example2 :: [Int]
example2 = foreach [1..10] $ \x -> 2 * x


-- eta reduction

foldr :: (a -> r -> r) -> r -> [a] ->r
foldr cons nil  = go
    where
        go[] = nil
        go (y : ys) = cons y $ go ys

length:: [a] -> Int
length = foldr (const(+ 1)) 0

filter :: (a -> Bool) -> [a] -> [a]

filter p = 
    foldr (\x ys -> if p x then x : ys else ys) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

all, any :: [Bool] -> Bool
all = foldr (&&) True
any = foldr (||) False


--idList :: [a] -> [a]
--idList = foldr (:) 

reverse :: [a] -> [a]
reverse = foldr (\x ys -> ys ++ [x]) [] 
{-
reverse1 :: [a] -> [a]
    reverse1 = go []
    where
        go :: [a] -> [a] -> [a]
        go acc [] = acc
        go acc (x : xs) = go (x : acc) xs

sum' :: Num a => [a] -> a
sum' = go 0
    where
        go acc [] = acc
        go acc (x : xs) = (x : acc)xs-}

{-
foldl :: (r -> a ->)
foldl op e = go e
    where
        go acc [] = acc
        go acc (x : xs) = let !acc 
-}



chain1 = Block (Block ())

data Tree a = Leaf a | Node (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f(Leaf a) = Leaf $ f a
mapTree f (node l r) = Node (mapTree f l) (mapTree f r)

