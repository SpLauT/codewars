module Oddint() where
--https://www.codewars.com/kata/54da5a58ea159efa38000836
import Data.List (group, groupBy, sort, partition)
import Debug.Trace

test1 :: [Int] -> [[Int]]
test1 = groupBy (==)

test2 :: [Int] -> [Int]
test2 xs = head $ filter (odd . length) $ test1 xs

test3 xs = group xs

findOdd :: [Int] -> Int
findOdd = head . head . filter (odd . length) . group . sort 