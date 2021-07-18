module Narcessistic () where

--https://www.codewars.com/kata/5287e858c6b5a9678200083c/


getParts :: Integral a => a -> [a]
getParts 0 = []
getParts n = getParts a ++ [b] 
    where (a, b) = n `divMod` 10

mapToPower :: Integral a => [a] -> [a]
mapToPower xs = map (^thePower) xs
    where thePower = length xs 

narcissistic :: Integral n => n -> Bool
narcissistic n = n == (sum . mapToPower $ getParts n)