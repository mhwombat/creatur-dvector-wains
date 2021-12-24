------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.PatternQC
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.DVector.PatternQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Gene.Numeric.Weights      (Weights, makeWeights)
import qualified ALife.Creatur.Gene.Test                 as GT
import           ALife.Creatur.Wain.DVector.Pattern
import           Data.Datamining.Pattern.Numeric         (maxDouble, minDouble)
import qualified Numeric.ApproxEq                        as Q
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck

-- instance Arbitrary UI.UIDouble where
--   arbitrary = doubleToUI <$> choose unitInterval

sizedArbPattern :: Int -> Gen Pattern
sizedArbPattern n = vectorOf n arbitrary

-- instance Arbitrary Pattern where
--   arbitrary = sized sizedArbPattern

prop_diff_can_be_0 :: Weights -> Pattern -> Bool
prop_diff_can_be_0 ws xs = weightedDiff ws xs xs == 0

data MaxDiffTestData = MaxDiffTestData Weights Pattern Pattern
  deriving Show

sizedArbMaxDiffTestData :: Int -> Gen MaxDiffTestData
sizedArbMaxDiffTestData n = do
  xs <- vectorOf (n+1) $ elements [minDouble, maxDouble]
  let f x = if x == minDouble then maxDouble else minDouble
  let ys = map f xs
  let ws = makeWeights . replicate (n+1) $ 1
  return $ MaxDiffTestData ws xs ys

instance Arbitrary MaxDiffTestData where
  arbitrary = sized sizedArbMaxDiffTestData

prop_diff_can_be_1 :: MaxDiffTestData -> Bool
prop_diff_can_be_1 (MaxDiffTestData ws xs ys) = Q.within 20 d 1
  where d = UI.wide $ weightedDiff ws xs ys

prop_diff_btw_0_and_1 :: Weights -> Pattern -> Pattern -> Bool
prop_diff_btw_0_and_1 ws xs ys = 0 <= z && z <= 1
  where z = weightedDiff ws xs ys

prop_diff_symmetric :: Weights -> Pattern -> Pattern -> Bool
prop_diff_symmetric ws xs ys
  = weightedDiff ws xs ys == weightedDiff ws ys xs

data TwoPatternsSameLength = TwoPatternsSameLength Pattern Pattern
  deriving Show

sizedTwoPatternsSameLength :: Int -> Gen TwoPatternsSameLength
sizedTwoPatternsSameLength n =
  TwoPatternsSameLength <$> sizedArbPattern n <*> sizedArbPattern n

instance Arbitrary TwoPatternsSameLength where
  arbitrary = sized sizedTwoPatternsSameLength

prop_zero_adjustment_is_no_adjustment ::
  Weights -> TwoPatternsSameLength -> Bool
prop_zero_adjustment_is_no_adjustment ws (TwoPatternsSameLength a b) =
  weightedDiff ws b b' < aTad
  where b' = makeSimilar a 0 b
        aTad = 1e-10

prop_full_adjustment_gives_perfect_match ::
  Weights -> TwoPatternsSameLength -> Bool
prop_full_adjustment_gives_perfect_match
  ws (TwoPatternsSameLength a b) = weightedDiff ws b' a < aTad
  where b' = makeSimilar a 1 b
        aTad = 1e-10

prop_makeSimilar_improves_similarity ::
  Weights -> TwoPatternsSameLength -> UI.UIDouble -> Property
prop_makeSimilar_improves_similarity ws (TwoPatternsSameLength a b) r
  = not (null a) && a /= b ==> d2 < d1
      where d1 = weightedDiff ws a b
            d2 = weightedDiff ws a b'
            b' = makeSimilar a r b

equiv :: Pattern -> Pattern -> Bool
equiv a b = diff a b <= aTad -- use unweighted diff
  where aTad = 0.00001

test :: Test
test = testGroup "ALife.Creatur.Wain.DVector.PatternQC"
  [
    testProperty "prop_serialize_round_trippable - Pattern"
      (GT.prop_serialize_round_trippable :: Pattern -> Bool),
    testProperty "prop_genetic_round_trippable - Pattern"
      (GT.prop_genetic_round_trippable equiv :: Pattern -> Bool),
    testProperty "prop_diploid_identity - Pattern"
      (GT.prop_diploid_identity (==) :: Pattern -> Bool),
    testProperty "prop_diff_can_be_0"
      prop_diff_can_be_0,
    testProperty "prop_diff_can_be_1"
      prop_diff_can_be_1,
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
