module Titlecase (titleCase) where
-- https://www.codewars.com/kata/5202ef17a402dd033c000009/train/haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize [] = ""
capitalize (x:xs) = toUpper x : map toLower xs

capitalizeFirst :: String -> String
capitalizeFirst [] = ""
capitalizeFirst (x:xs) = toUpper x : xs

isInArray :: String -> [String] -> Bool
isInArray x ys = makeLowercase x `elem` ys

makeLowercase :: String -> String
makeLowercase = map toLower

titleCase :: String -> String -> String
titleCase minor title = capitalizeFirst . unwords . map testFunction $ words title
    where minorArray = map makeLowercase $ words minor
          testFunction x = if not (isInArray x minorArray) then capitalize x else makeLowercase x