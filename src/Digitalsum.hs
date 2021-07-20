module Digitalsum () where
--https://www.codewars.com/kata/541c8630095125aba6000c00/
convertToList :: Integral a => a -> [a]
convertToList 0 = []
convertToList x = convertToList a ++ [b]
    where (a,b) = x `divMod` 10

digitalRoot :: Integral a => a -> a
digitalRoot x = if val < 10 then val else digitalRoot val
    where val = sum $ convertToList x
    