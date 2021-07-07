module Main where

import Lib
import qualified Deadfish

main :: IO ()
main = print (Deadfish.parse "iiisdoso")
