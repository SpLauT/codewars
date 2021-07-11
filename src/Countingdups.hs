module Countingdups (duplicateCount, frequency) where

import Data.Map (fromListWith, toList)
import Data.Char (toLower)

frequency :: [Char] -> [(Char, Int)]
frequency xs = toList $ fromListWith (+) [(toLower x, 1) | x <- xs]

duplicateCount :: String -> Int 
duplicateCount s = length . filter (\x -> snd x > 1) $ frequency s