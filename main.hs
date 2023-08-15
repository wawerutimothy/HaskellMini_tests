module Main where

main :: IO ()
main - putStrLn "Test suite"


sortPreserveLength :: [Int] -> Bool
sortPreserveLength xs  = length (sort xs) == length xs

