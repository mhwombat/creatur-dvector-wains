------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Double
-- Copyright   :  (c) Amy de Buitléir 2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with patterns.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.DVector.Double
  (
    sanitise,
    diff,
    makeSimilar
  ) where

import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble,
  doubleToUI)
import Data.Datamining.Pattern (adjustNum)

-- | @'makeSimilar' target amount x@ adjusts @x@ to move it closer to
--   @target@.
--   The amount of adjustment is controlled by amount@,
--   which is a number between 0 and 1.
--   Larger values of @amount@ permit more adjustment.
--   If @amount@=1, the result will be identical to the @target@.
--   If @amount@=0, the result will be the unmodified @pattern@.
makeSimilar :: Double -> UIDouble -> Double -> Double
makeSimilar target r x = adjustNum target (uiToDouble r) x

-- | Returns a number between 0 and 1 which indicates how different
--   the two inputs are. A result of 0 indicates that the
--   inputs are identical.
diff :: Double -> Double -> UIDouble
diff x y = doubleToUI ((abs (x/2 - y/2)) / (halfMaxDiff))
  -- divide by two so we don't overflow or underflow

halfMaxDiff :: Double
halfMaxDiff = maxDouble/2 - minDouble/2

maxDouble :: Double
maxDouble = maxNonInfiniteFloat 0

minDouble :: Double
minDouble = minNonInfiniteFloat 0

maxNonInfiniteFloat :: RealFloat a => a -> a
maxNonInfiniteFloat a = encodeFloat m n
  where b = floatRadix a
        e = floatDigits a
        (_, e') = floatRange a
        m = b ^ e - 1
        n = e' - e

minNonInfiniteFloat :: RealFloat a => a -> a
minNonInfiniteFloat a = - (maxNonInfiniteFloat a)

sanitise :: Double -> Double
sanitise = max minDouble . min maxDouble
