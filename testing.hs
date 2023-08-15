module Testing where

sort :: [Int] -> [Int]
sorty [] =  []
sort (x : xs) = insert x xs

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : ys)
    | x <= y = x : y : ys
    | otherwise = y : insert x ys

--sortPreserveLength :: [Int] -> Bool
--sortPreserveLength  = sort `preserves` length
--ifPreserveLength = id `preserves` length

--preserves :: Eq a => (t -> t) -> (t -> a) -> t -> Bool
--(f `preserves`) p xs  = p (f xs) == p xs

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : zs) = x < y && isSorted (y : zs)

sortEnsuresSorted :: [Int] -> Bool
sortEnsuresSorted xs = isSorted $ sort xs

evileNoSort :: [Int] -> [Int]
evileNoSort xs = replicate (length xs )42
{-
permutes :: Eq a => [a] -> [a] -> [a] -> Bool
f `permutes` xs = f xs `elem` permutations xs
sortPermutes xs  = sort `permutes` xs
-}

appendLength ::  [a] -> [a] -> Bool
appendLength xs ys = length (xs ++ ys) == length xs + length ys

pluscummnulative :: Int -> Int -> Bool
pluscummnulative x y = x + y == y + x

takeDrop :: Int -> [Int] -> Bool
takeDrop n xs  =  take n xs ++ drop n xs == xs

droptwice :: Int -> Int -> [Int] -> Bool
droptwice m n xs = drop m ( drop n xs ) drop (m +n) xs

lengthEmpty :: Bool
lengthEmpty = length [] == 0



{-data Tree a = Leaf a | Node (Tree a) (Tree a)
    dereiving (show)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = oneOf [genLeaf, genNode]
        where
            genLeaf = Leaf <$> arbitrary
            genNode = liftM2 Node arbitrary arbitrary-}

mksorted :: [Int] -> Int
mksorted [] = []
mksorted[x] = [x]
mksorted (x : y : ys) = x : mksorted (x + abs y :ys)