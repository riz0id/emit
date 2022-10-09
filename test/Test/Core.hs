{-# LANGUAGE NumericUnderscores #-}

module Test.Core
  ( TestTree,
    forAll,
    property,
    testGroup,
    testProp,

    -- * Properties
    order,
    strict'order,
    reflexive,
    antisymmetric,
    irreflexive,
    asymmetry,
    transitive,
  )
where

import Hedgehog (Gen, forAll, property, withTests, (===))
import Test.Tasty (TestName, TestTree, testGroup)

import Control.Monad (when)
import Test.Compat (testProp)

--------------------------------------------------------------------------------

order ::
  (Eq a, Show a) =>
  TestName ->
  (a -> a -> Bool) ->
  Gen a ->
  TestTree
order name op gen =
  testGroup
    name
    [ reflexive op gen
    , antisymmetric op gen
    , transitive op gen
    ]

strict'order ::
  (Eq a, Show a) =>
  TestName ->
  (a -> a -> Bool) ->
  Gen a ->
  TestTree
strict'order name op gen =
  testGroup
    name
    [ irreflexive op gen
    , asymmetry op gen
    , transitive op gen
    ]

reflexive ::
  (Eq a, Show a) =>
  (a -> a -> Bool) ->
  Gen a ->
  TestTree
reflexive op gen =
  testProp "reflexive" $
    withTests 1_000 $ property do
      x <- forAll gen
      op x x === True

antisymmetric ::
  (Eq a, Show a) =>
  (a -> a -> Bool) ->
  Gen a ->
  TestTree
antisymmetric op gen =
  testProp "antisymmetric" $
    withTests 1_000 $ property do
      x <- forAll gen
      y <- forAll gen
      when (op x y && op y x) (x === y)

irreflexive ::
  (Eq a, Show a) =>
  (a -> a -> Bool) ->
  Gen a ->
  TestTree
irreflexive op gen =
  testProp "irreflexive" $
    withTests 1_000 $ property do
      x <- forAll gen
      op x x === False

asymmetry ::
  (Eq a, Show a) =>
  (a -> a -> Bool) ->
  Gen a ->
  TestTree
asymmetry op gen =
  testProp "asymmetry" $
    withTests 1_000 $ property do
      x <- forAll gen
      y <- forAll gen
      op x y === not (op y x)

transitive ::
  (Eq a, Show a) =>
  (a -> a -> Bool) ->
  Gen a ->
  TestTree
transitive op gen =
  testProp "transitive" $
    withTests 1_000 $ property do
      x <- forAll gen
      y <- forAll gen
      z <- forAll gen
      when (op x y && op y z) (op x z === True)