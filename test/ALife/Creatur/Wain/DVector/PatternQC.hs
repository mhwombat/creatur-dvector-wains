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

import qualified ALife.Creatur.Gene.AdjusterTest         as AT
import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Gene.Numeric.Weights      (makeWeights)
import qualified ALife.Creatur.Gene.Test                 as GT
import           ALife.Creatur.Wain.DVector.Pattern      (DVectorAdjuster (..),
                                                          Pattern)
import qualified Data.Datamining.Clustering.SGM4         as SOM
import qualified Data.Datamining.Pattern.List            as L
import           Data.Datamining.Pattern.Numeric         (maxDouble, minDouble)
import qualified Data.Datamining.Pattern.Numeric         as N
import qualified Numeric.ApproxEq                        as Q
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck.Counterexamples

data MaxDiffTestData = MaxDiffTestData DVectorAdjuster Pattern Pattern
  deriving Show

sizedArbMaxDiffTestData :: Int -> Gen MaxDiffTestData
sizedArbMaxDiffTestData n = do
  xs <- vectorOf (n+1) $ elements [minDouble, maxDouble]
  let f x = if x == minDouble then maxDouble else minDouble
  let ys = map f xs
  params <- arbitrary
  let ws = makeWeights . replicate (n+1) $ 1
  let a = DVectorAdjuster params ws
  return $ MaxDiffTestData a xs ys

instance Arbitrary MaxDiffTestData where
  arbitrary = sized sizedArbMaxDiffTestData

prop_diff_can_be_1 :: MaxDiffTestData -> Bool
prop_diff_can_be_1 (MaxDiffTestData a xs ys) = Q.within 20 d 1
  where d = UI.wide $ SOM.difference a xs ys

data TwoPatternsSameLength = TwoPatternsSameLength Pattern Pattern
  deriving Show

sizedTwoPatternsSameLength :: Int -> Gen TwoPatternsSameLength
sizedTwoPatternsSameLength n =
  TwoPatternsSameLength <$> vector n <*> vector n

instance Arbitrary TwoPatternsSameLength where
  arbitrary = sized sizedTwoPatternsSameLength

equiv :: Pattern -> Pattern -> Bool
equiv a b = L.diff N.realFloatDiff a b <= 1e-10
  -- use unweighted diff here

prop_makeSimilar_improves_similarity :: DVectorAdjuster -> TwoPatternsSameLength -> UI.Double -> Bool
prop_makeSimilar_improves_similarity a (TwoPatternsSameLength x y) r
  = AT.prop_makeSimilar_improves_similarity a x r y

prop_zero_adjustment_makes_no_change :: DVectorAdjuster -> TwoPatternsSameLength -> Bool
prop_zero_adjustment_makes_no_change a (TwoPatternsSameLength x y)
  = AT.prop_zero_adjustment_makes_no_change equiv a x y

prop_full_adjustment_gives_perfect_match :: DVectorAdjuster -> TwoPatternsSameLength -> Bool
prop_full_adjustment_gives_perfect_match a (TwoPatternsSameLength x y)
  = AT.prop_full_adjustment_gives_perfect_match equiv a x y

test :: Test
test = testGroup "ALife.Creatur.Wain.DVector.PatternQC"
  [
    testProperty "prop_serialize_round_trippable - Pattern"
      (GT.prop_serialize_round_trippable :: Pattern -> Bool),
    testProperty "prop_genetic_round_trippable - Pattern"
      (GT.prop_genetic_round_trippable equiv :: Pattern -> Bool),
    testProperty "prop_diploid_identity - Pattern"
      (GT.prop_diploid_identity (==) :: Pattern -> Bool),
    testProperty "prop_diploid_expressable - Pattern"
      (GT.prop_diploid_expressable :: Pattern -> Pattern -> Bool),
    testProperty "prop_diploid_readable - Pattern"
      (GT.prop_diploid_readable :: Pattern -> Pattern -> Bool),
    testProperty "prop_show_read_round_trippable - Pattern"
      (GT.prop_show_read_round_trippable (==) :: Pattern -> Bool),

    testProperty "prop_diff_can_be_0 - DVectorAdjuster"
      (AT.prop_diff_can_be_0 :: DVectorAdjuster -> Pattern -> Bool),
    testProperty "prop_diff_can_be_1 - DVectorAdjuster"
    --   (AT.prop_diff_can_be_1 :: DVectorAdjuster -> Pattern -> Bool),
      prop_diff_can_be_1,
    testProperty "prop_diff_is_symmetric - DVectorAdjuster"
      (AT.prop_diff_is_symmetric :: DVectorAdjuster -> Pattern -> Pattern -> Bool),
    testProperty "prop_makeSimilar_improves_similarity - DVectorAdjuster"
      prop_makeSimilar_improves_similarity,
    --   (AT.prop_makeSimilar_improves_similarity :: DVectorAdjuster -> Pattern -> UI.Double -> Pattern -> Bool),
    testProperty "prop_zero_adjustment_makes_no_change - DVectorAdjuster"
      prop_zero_adjustment_makes_no_change,
      -- (AT.prop_zero_adjustment_makes_no_change equiv :: DVectorAdjuster -> Pattern -> Pattern -> Bool),
    testProperty "prop_full_adjustment_gives_perfect_match - DVectorAdjuster"
      prop_full_adjustment_gives_perfect_match
    --   (AT.prop_full_adjustment_gives_perfect_match equiv :: DVectorAdjuster -> Pattern -> Pattern -> Bool)
  ]
