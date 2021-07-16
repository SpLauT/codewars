module Phonenumber (validPhoneNumber) where
--https://www.codewars.com/kata/525f47c79f2f25a4db000025
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

regex :: String
regex = "\\([0-9]{3}\\)"

test1 :: String
test1 = ("hello (123)" :: String)    =~ (regex :: String) :: String

regex2 :: String
regex2 = "\\`\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}\\'"

test2 :: String
test2 = ("(123) 123-4567" :: String) =~ (regex2 :: String) :: String


main = putStrLn "Hello"


phoneNumberRegex = "\\`\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}\\'" :: String

validPhoneNumber :: String -> Bool
validPhoneNumber str = str =~ (phoneNumberRegex :: String) 
