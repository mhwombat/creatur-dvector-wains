------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Wain
-- Copyright   :  (c) 2017-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with wains.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.DVector.Wain
  (
    PatternWain,
    -- adjustEnergy,
    metabCost,
    packageVersion
  ) where

import ALife.Creatur.Wain                 qualified as W
import ALife.Creatur.Wain.Brain           (classifier)
import ALife.Creatur.Wain.DVector.Pattern (DVectorAdjuster, Pattern)
import Data.Datamining.Clustering.SGM4    (size)
import Data.Version                       (showVersion)
import Paths_creatur_dvector_wains        (version)

-- | Returns the current version number of this library.
packageVersion :: String
packageVersion = "creatur-dvector-wains-" ++ showVersion version

type PatternWain rt a m = W.Wain DVectorAdjuster rt Pattern a m

metabCost :: Double -> Double -> Double -> PatternWain rt a m -> Double
metabCost bmc cpcm scale w = scale * (bmc + cpcm * fromIntegral n)
  where n = size . classifier . W.brain $ w
