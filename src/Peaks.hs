module Peaks (pickPeaks) where

--https://www.codewars.com/kata/5279f6fe5ab7f447890006a7/

data PickedPeaks = PickedPeaks {
    pos :: [Int],
    peaks :: [Int]
} deriving (Eq, Show)

test1 :: [Int] -> PickedPeaks
test1 = foldl (\(PickedPeaks a b) x  -> PickedPeaks (a ++ [x]) (b ++ [x])) (PickedPeaks [] [])

test2 :: [Int] -> PickedPeaks
test2 xs = foldl (\(PickedPeaks a b) x  -> if isPeak (drop (x-1) $ take 3 xs) then PickedPeaks (a ++ [x]) (b ++ [xs !! x]) else PickedPeaks a b) (PickedPeaks [] []) [1 .. length xs]

endFinder :: Int -> [Int] -> Bool
endFinder y (x:xs) 
    | y > x = True
    | y == x = endFinder y xs
    | otherwise = False
endFinder _ [] = False

isPeak :: [Int] -> Bool 
isPeak (x:y:xs) = (y > x) && endFinder y xs
isPeak [_] = False
isPeak _ = False

getPos :: [Int]-> Int -> PickedPeaks
getPos [] _ = PickedPeaks [] []
getPos xs idx = if isPeak xs then PickedPeaks (idx : a) (xs !! 1 : b) else PickedPeaks a b
    where PickedPeaks a b = getPos (drop 1 xs) (idx +1)

pickPeaks :: [Int] -> PickedPeaks
pickPeaks xs = getPos xs 1
    -- where go xs idx = if isPeak $ take 3 xs then ([idx], [xs !! 1]) ++ go (drop 1 xs) idx + 1 else () ++ go (drop 1 xs) idx + 1
            