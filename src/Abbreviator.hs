module Abbreviator () where
-- I have to abbandon this for now
import Data.List.Split
--https://www.codewars.com/kata/5375f921003bf62192000746/train/haskell
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

testRegex :: String
testRegex = "\\d+"

test2 :: String -> [String]
test2 str = split (oneOf "a") str
    

test1 :: String -> (String, String, String)
test1 str = str =~ testRegex :: (String, String, String) 


regex :: String
regex = "[ /\n//\DEL/0-9!?-]"

shorten :: String -> String
shorten str 
    | length str >=4 = head str : show ((length str) -2) ++ [last str]
    | otherwise = str

abbreviate :: String -> String
abbreviate "" = ""
abbreviate str = shorten a ++ b ++ abbreviate c 
   where (a,b,c) = str =~ regex :: (String, String, String)