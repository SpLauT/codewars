--https://www.codewars.com/kata/52fba66badcd10859f00097e/haskell

module Disemvoweltrolls (disemvowel) where

vowels :: String
vowels = "aeiouAEIOU"

disemvowel :: String -> String
disemvowel = foldl (\b a -> if a `elem` vowels then b else b ++ [a]) ""