module Main where

import           Test.Hspec
import           Test.NoName.Nat

main :: IO ()
main = hspec $ natModuleSpec
