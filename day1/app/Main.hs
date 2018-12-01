module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let _frequencyChanges = frequencyChanges input
    print $ firstTwice 0 [] _frequencyChanges

removePlus :: String -> String
removePlus xs = [ x | x <- xs, x /= '+']

frequencyChanges :: String -> [Int]
frequencyChanges input = map (read :: String -> Int) inputLines
                        where
                            inputLines = lines (removePlus input)

firstTwice :: Int -> [Int] -> [Int] -> Int
firstTwice currentFrequency pastFrequencies (x : xs)
            | currentFrequency `elem` pastFrequencies = currentFrequency
            | otherwise = firstTwice (currentFrequency + x) (currentFrequency : pastFrequencies) (xs ++ [x])