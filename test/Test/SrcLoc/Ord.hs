module Test.SrcLoc.Ord (testTree) where

import Data.SrcLoc (SrcLoc (SrcLoc))

import Hedgehog ((===))

import Test.Core
  ( TestTree,
    forAll,
    order,
    property,
    strict'order,
    testGroup,
    testProp,
  )
import Test.SrcLoc.Gen qualified as Gen

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Ord"
    [ testProp "(==)" $ property do
        loc0@(SrcLoc x0 y0 z0) <- forAll Gen.srcloc
        loc1@(SrcLoc x1 y1 z1) <- forAll Gen.srcloc
        (loc0 == loc1) === ((x0, y0, z0) == (x1, y1, z1))
    , testProp "(/=)" $ property do
        loc0@(SrcLoc x0 y0 z0) <- forAll Gen.srcloc
        loc1@(SrcLoc x1 y1 z1) <- forAll Gen.srcloc
        (loc0 /= loc1) === ((x0, y0, z0) /= (x1, y1, z1))
    , strict'order "(>)" (>) Gen.srcloc
    , order "(>=)" (>=) Gen.srcloc
    , strict'order "(<)" (<) Gen.srcloc
    , order "(<=)" (<=) Gen.srcloc
    ]