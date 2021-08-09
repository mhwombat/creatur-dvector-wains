------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Wain
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with wains.
--
------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.DVector.Wain
  (
    PatternWain,
    -- adjustEnergy,
    metabCost,
    packageVersion
  ) where

import qualified ALife.Creatur.Wain                 as W
import           ALife.Creatur.Wain.Brain           (classifier)
import           ALife.Creatur.Wain.DVector.Pattern (Pattern)
import           ALife.Creatur.Wain.DVector.Tweaker (PatternTweaker (..))
import           ALife.Creatur.Wain.GeneticSOM      (numModels)
import           Control.Lens                       hiding (universe)
import           Data.Version                       (showVersion)
import           Paths_creatur_dvector_wains        (version)

-- | Returns the current version number of this library.
packageVersion :: String
packageVersion = "creatur-dvector-wains-" ++ showVersion version

type PatternWain a rt m = W.Wain Pattern PatternTweaker rt m a

metabCost :: Double -> Double -> Double -> PatternWain a rt m -> Double
metabCost bmc cpcm scale w = scale * (bmc + cpcm * fromIntegral n)
  where n = numModels . view (W.brain . classifier) $ w
