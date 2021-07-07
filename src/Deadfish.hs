-- https://www.codewars.com/kata/51e0007c1f9378fa810002a9/train/haskell
module Deadfish (parse) where

doer :: String -> Int -> [Int]
doer [] _ = []
doer (x:xs) n = case x of
    'i' -> doer xs (n+1)
    'd' -> doer xs (n-1)
    's' -> doer xs (n^2)
    'o' -> n : doer xs n
    _ -> doer xs n

parse :: String -> [Int]
parse s = doer s 0