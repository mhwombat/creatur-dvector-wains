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
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ALife.Creatur.Wain.DVector.Pattern
  (
    Pattern,
    vectorDiff,
    weightedDVectorDiff,
    makeSimilar
  ) where

import qualified ALife.Creatur.Wain.DVector.Double as D
import  ALife.Creatur.Wain.Statistics (Statistical(..), dStats)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble,
  doubleToUI, interval)
import ALife.Creatur.Wain.Util (inRange)
import ALife.Creatur.Wain.Weights (Weights, toUIDoubles)
import Data.Datamining.Pattern (adjustVector)

type Pattern = [Double]

makeSimilar :: Pattern -> UIDouble -> Pattern -> Pattern
makeSimilar t a v = adjustVector t (uiToDouble a) v

instance Statistical Pattern where
  stats = dStats ""

-- | Returns a number between 0 and 1 which indicates how different
--   the two input vectors are.  A result of 0 indicates that the
--   inputs are identical.
vectorDiff :: [Double] -> [Double] -> UIDouble
vectorDiff xs ys
  | null xs && null ys     = doubleToUI 0
  | null xs || null ys     = doubleToUI 1
  | inRange interval x    = doubleToUI x
  | otherwise             = error $ "uiVectorDiff: out of bounds"
                               ++ " xs=" ++ show xs
                               ++ " ys=" ++ show ys
  where x = d / fromIntegral (length deltas)
        deltas = zipWith D.diff xs ys
        d = sum deltas

-- | Calculates the weighted difference between two sequences of
--   numbers.
--   Returns a number between 0 and 1.
--   A result of 0 indicates that the inputs are identical.
weightedDVectorDiff
  :: Weights -> [Double] -> [Double] -> UIDouble
weightedDVectorDiff ws xs ys
  = doubleToUI . sum . zipWith (*) ws' $ zipWith D.diff xs ys
  where ws' = map uiToDouble $ toUIDoubles ws
