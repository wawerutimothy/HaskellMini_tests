
module Main where
-- defines main module, a way to structure code
import Data.List (foldl')
-- we import fold' to enable us process lists

getMinAndMax :: Ord a => [a] -> Maybe (a, a)

{- above we create function
the :: symbol  gives a type signature
> ord a => means a which is alist  must be ordable
for us to compare elements of the type
to find minimum and max-}

getMinAndMax [] = Nothing -- if list is empty return nothing

getMinAndMax (x:xs) = Just (foldl' min x xs, foldl' max x xs)
-- x deals with list with one element and xs deals with the rest of the list
main :: IO ()
-- tells our program we're dealing with Input/Output
main = do
    putStrLn "Enter a list of number with space separated: "
    -- asks for input
    input <- getLine
    -- reads the input of the user
    let numbers = map (read :: String -> Integer) (words input)
    -- we split user input into individual words and convert each word 
    -- to a number and put them in a list called numbers
    case getMinAndMax (numbers :: [Integer]) of
        Nothing -> putStrLn "The List is empty"
        Just (minVal, maxVal) -> putStrLn $ "Minimum: " ++ show (minVal :: Integer) ++ ", Maximum: " ++ show (maxVal :: Integer)
-- show is used to convert the output to string to be printed on screen