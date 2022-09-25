module MergeSort where

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge a@(x:xs) b@(y:ys)
    | x < y = x : merge xs b
    | otherwise = y : merge a ys
