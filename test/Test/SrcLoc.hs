
module Test.SrcLoc (testTree) where

import Test.Compat (TestTree, testGroup)
import Test.SrcLoc.Ord qualified

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "SrcLoc"
    [ Test.SrcLoc.Ord.testTree
    ]