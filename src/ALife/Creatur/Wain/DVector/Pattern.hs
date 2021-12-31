------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Pattern
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
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

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Gene.Numeric.Weights      (Weights, toUIDoubles)
import qualified ALife.Creatur.Genetics.BRGCWord8        as G
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           ALife.Creatur.Wain.LearningParams       (LearningParams,
                                                          toLearningFunction)
import           ALife.Creatur.Wain.Pretty               (Pretty (..))
import           ALife.Creatur.Wain.Report               (Report, report)
import           ALife.Creatur.Wain.Statistics           (Statistical (..))
import qualified Data.Datamining.Clustering.SGM4         as SOM
import qualified Data.Datamining.Pattern.List            as L
import qualified Data.Datamining.Pattern.Numeric         as N
import           Data.Serialize                          (Serialize)
import           Data.Word                               (Word32)
import           GHC.Generics                            (Generic)
import           Test.QuickCheck                         (Arbitrary, arbitrary)

type Pattern = [Double]

-- instance Statistical Pattern where
--   stats = dStats ""

data DVectorAdjuster = DVectorAdjuster LearningParams Weights
  deriving (Eq, Show, Read, Pretty, Generic, Serialize, G.Genetic, Diploid)

instance SOM.Adjuster DVectorAdjuster where
  type TimeType DVectorAdjuster = Word32
  type MetricType DVectorAdjuster = UI.Double
  type PatternType DVectorAdjuster = Pattern
  learningRate (DVectorAdjuster l _) = toLearningFunction l
  difference (DVectorAdjuster _ ws) xs ys = UI.narrow $ L.weightedDiff ws' N.realFloatDiff xs ys
    where ws' = map UI.wide $ toUIDoubles ws
  makeSimilar _ target r x
    =  L.makeSimilar N.makeOrdFractionalSimilar target (UI.wide r) x

instance Statistical DVectorAdjuster where
  stats (DVectorAdjuster l _) = stats l

instance Report DVectorAdjuster where
  report (DVectorAdjuster l _) = report l

-- TODO: Include weights in stats and reports

instance Arbitrary DVectorAdjuster where
  arbitrary = DVectorAdjuster <$> arbitrary <*> arbitrary
