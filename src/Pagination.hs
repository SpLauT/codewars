module Pagination () where

import Data.List.Split
import Data.Maybe
import Data.List

--https://www.codewars.com/kata/515bb423de843ea99400000a/

type Collection a = [a]
type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length

getPages :: Collection a -> ItemsPerPage -> [[a]]
getPages = flip chunksOf

pageCount :: Collection a -> ItemsPerPage -> Int
pageCount xs n = length $ getPages xs n

pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount xs n page 
    | page < 0 = Nothing
    | page > (length pages) -1 = Nothing
    | otherwise = Just $ length (pages !! page)
    where pages = getPages xs n


pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex [] _ _ = Nothing 
pageIndex xs n item 
    | item < 0 = Just 0
    | item >= itemCount xs = Nothing
    | otherwise = Just $ item `div` n


          
          