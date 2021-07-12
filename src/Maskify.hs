-- https://www.codewars.com/kata/5412509bd436bd33920011bc/train/haskell
module Maskify (maskify) where

maskify :: String -> String
maskify str = map (const '#') $ take grabAmount str ++ drop grabAmount str
    where grabAmount = length str -4
