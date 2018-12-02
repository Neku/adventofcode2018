module Lib
    ( hasTwoRepeat
    , hasThreeRepeat
    , checkSum
    , numberOfDifferentChars
    , findId
    ) where

import Data.List
hasTwoRepeat :: String -> Bool
hasTwoRepeat = hasNRepeat 2

hasThreeRepeat :: String -> Bool
hasThreeRepeat = hasNRepeat 3

hasNRepeat :: Int -> String -> Bool
hasNRepeat n str = any (\x -> countOccurences x str == n) str

countOccurences :: Char -> String -> Int
countOccurences c str = length $ filter (== c) str

checkSum :: [String] -> Int
checkSum xs = twoRepeats * threeRepeats
        where
            twoRepeats = length $ filter hasTwoRepeat xs
            threeRepeats = length $ filter hasThreeRepeat xs

findId :: [String] -> (String, String)
findId (x : xs) = case find (isOneDifferent x) xs of
                        Just str -> (x, str)
                        Nothing -> findId xs

isOneDifferent :: String -> String -> Bool
isOneDifferent x y = numberOfDifferentChars x y == 1

numberOfDifferentChars :: String -> String -> Int
numberOfDifferentChars [] [] = 0
numberOfDifferentChars (x:xs) (y:ys) | x == y = numberOfDifferentChars xs ys
                                     | otherwise = 1 + numberOfDifferentChars xs ys