module Test.SrcLoc.Gen 
  ( Gen,
    srcloc,
  ) 
where

import Data.SrcLoc (SrcLoc (SrcLoc))

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

srcloc :: Gen SrcLoc
srcloc = 
  SrcLoc
    <$> Gen.int Range.constantBounded
    <*> Gen.int Range.constantBounded
    <*> Gen.int Range.constantBounded