
module Main (main) where

import Test.Tasty (defaultMain)
import Test.Compat (TestTree, testGroup)
import Test.SrcLoc qualified 

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ Test.SrcLoc.testTree
    ]