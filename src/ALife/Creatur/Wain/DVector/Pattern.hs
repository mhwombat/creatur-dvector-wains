------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Pattern
-- Copyright   :  (c) 2017-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with patterns.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.DVector.Pattern
  (
    Pattern,
    DVectorAdjuster(..)
  ) where

import ALife.Creatur.Gene.Numeric.UnitInterval qualified as UI
import ALife.Creatur.Gene.Numeric.Weights      (Weights, extractWeights)
import ALife.Creatur.Genetics.BRGCWord8        qualified as G
import ALife.Creatur.Genetics.Diploid          (Diploid)
import ALife.Creatur.Wain.LearningParams       (LearningParams,
                                                toLearningFunction)
import ALife.Creatur.Wain.Pretty               (Pretty (..))
import ALife.Creatur.Wain.Report               (Report, report)
import ALife.Creatur.Wain.Statistics           (Statistical (..))
import Data.Datamining.Clustering.SGM4         qualified as SOM
import Data.Datamining.Pattern.List            qualified as L
import Data.Datamining.Pattern.Numeric         qualified as N
import Data.Serialize                          (Serialize)
import Data.Word                               (Word32)
import GHC.Generics                            (Generic)
import Test.QuickCheck                         (Arbitrary, arbitrary)

type Pattern = [Double]

-- instance Statistical Pattern where
--   stats = dStats ""

data DVectorAdjuster = DVectorAdjuster LearningParams (Weights Double)
  deriving (Eq, Show, Read, Pretty, Generic, Serialize, G.Genetic, Diploid)

instance SOM.Adjuster DVectorAdjuster where
  type TimeType DVectorAdjuster = Word32
  type MetricType DVectorAdjuster = UI.Double
  type PatternType DVectorAdjuster = Pattern
  learningRate (DVectorAdjuster l _) = toLearningFunction l
  difference (DVectorAdjuster _ w) xs ys = UI.narrow $ L.weightedDiff ws N.doubleDiff xs ys
    where ws = extractWeights w
  makeSimilar _ target r x
    =  L.makeSimilar N.makeOrdFractionalSimilar target (UI.wide r) x

instance Statistical DVectorAdjuster where
  stats (DVectorAdjuster l _) = stats l

instance Report DVectorAdjuster where
  report (DVectorAdjuster l _) = report l

-- TODO: Include weights in stats and reports

instance Arbitrary DVectorAdjuster where
  arbitrary = DVectorAdjuster <$> arbitrary <*> arbitrary
