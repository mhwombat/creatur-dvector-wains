------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.DoubleQC
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ALife.Creatur.Wain.DVector.DoubleQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.Test as GT
import ALife.Creatur.Wain.DVector.Double
import ALife.Creatur.Gene.Numeric.UnitInterval (UIDouble)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck


prop_diff_can_be_0 :: Double -> Property
prop_diff_can_be_0 x = property $ diff x x == 0

prop_diff_btw_0_and_1 :: Double -> Double -> Property
prop_diff_btw_0_and_1 x y = property $ 0 <= z && z <= 1
  where z = diff x y

prop_diff_symmetric :: Double -> Double -> Property
prop_diff_symmetric x y = property $ diff x y == diff y x

prop_zero_adjustment_is_no_adjustment ::  Double -> Double -> Property
prop_zero_adjustment_is_no_adjustment a b =
  property $ diff b b' == 0
  where b' = makeSimilar a 0 b

prop_full_adjustment_gives_perfect_match ::  Double -> Double -> Property
prop_full_adjustment_gives_perfect_match a b = property $ diff b' a < aTad
  where b' = makeSimilar a 1 b
        aTad = 1e-10

prop_makeSimilar_improves_similarity ::
  Double -> Double -> UIDouble -> Property
prop_makeSimilar_improves_similarity a b r
  = a /= b ==> d2 < d1
      where d1 = diff a b
            d2 = diff a b'
            b' = makeSimilar a r b

test :: Test
test = testGroup "ALife.Creatur.Wain.DVector.DoubleQC"
  [
    testProperty "prop_serialize_round_trippable - Double"
      (GT.prop_serialize_round_trippable :: Double -> Property),
    testProperty "prop_genetic_round_trippable - Double"
      (GT.prop_genetic_round_trippable (==) :: Double -> Property),
    testProperty "prop_diploid_identity - Double"
      (GT.prop_diploid_identity (==) :: Double -> Property),
    testProperty "prop_diff_can_be_0"
      prop_diff_can_be_0,
    testProperty "prop_diff_btw_0_and_1"
      prop_diff_btw_0_and_1,
    testProperty "prop_diff_symmetric"
      prop_diff_symmetric,
    testProperty "prop_zero_adjustment_is_no_adjustment"
      prop_zero_adjustment_is_no_adjustment,
    testProperty "prop_full_adjustment_gives_perfect_match"
      prop_full_adjustment_gives_perfect_match,
    testProperty "prop_makeSimilar_improves_similarity"
      prop_makeSimilar_improves_similarity
  ]
