module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let boxIds = lines input
    print $ findId boxIds

