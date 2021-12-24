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
module ALife.Creatur.Wain.DVector.Pattern
  (
    Pattern,
    diff,
    weightedDiff,
    makeSimilar
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Gene.Numeric.Weights      (Weights, toUIDoubles)
import           ALife.Creatur.Wain.Statistics           (Statistical (..),
                                                          dStats)
import qualified Data.Datamining.Pattern.List            as L
import qualified Data.Datamining.Pattern.Numeric         as N

type Pattern = [Double]

-- | @'makeSimilar' f ts r xs@ uses the function @f@ to adjust
--   each element of @x@ to move it closer to the corresponding element
--   of @ts@.
--   The result will be the length of @ts@ or @xs@, whichever is
--   shorter.
--   The amount of adjustment is controlled by @r@,
--   which should normally be between 0 and 1 (enforcement of that
--   constraint is left up to the function @f@).
--   Larger values of @r@ permit more adjustment.
--   If @r@=1, the result will be identical to the @ts@.
--   If @r@=0, the result will be identical to @xs@
--   (apart from the truncation that occurs when @ts@
--   is shorter than @xs@).
makeSimilar :: Pattern -> UI.UIDouble -> Pattern -> Pattern
makeSimilar target r = L.makeSimilar N.makeSimilar target (UI.wide r)

instance Statistical Pattern where
  stats = dStats ""

-- | @'diff' f xs ys@ returns a number between 0 and 1 which indicates
--   how different the input lists @xs@ and @ys@ are.
--   If @xs@ and @ys@ have the same length, the result is the mean of
--   the difference between corresponding pairs of list elements,
--   as calculated by @f@.
--   If the lists are of different lengths, @1@ is returned.
diff :: [Double] -> [Double] -> UI.UIDouble
diff xs ys = UI.narrow $ L.diff N.diff xs ys

-- | @'weightedDiff' ws f xs ys@ returns a number between 0 and 1
--   which indicates how different the input lists @xs@ and @ys@ are.
--   If @xs@ and @ys@ have the same length, the result
--   is the weighted sum of differences between corresponding pairs
--   of list elements, as calculated by @f@.
--   If the lists are of different lengths, @1@ is returned.
weightedDiff :: Weights -> [Double] -> [Double] -> UI.UIDouble
weightedDiff ws xs ys = UI.narrow $ L.weightedDiff ws' N.diff xs ys
  where ws' = map UI.wide $ toUIDoubles ws
