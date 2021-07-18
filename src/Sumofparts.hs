module Sumofparts () where

--https://www.codewars.com/kata/5ce399e0047a45001c853c2b/train/haskell

sumize :: [Integer] -> Integer -> [Integer]
sumize [] _ = [0]
sumize (x:xs) n = n : sumize xs (n-x)

partsSum :: [Integer] -> [Integer]
partsSum xs = sumize xs initSum
    where initSum = sum xs


--better solution would have been
-- scanr (+) 0 xs