------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Wain
-- Copyright   :  (c) Amy de Buitléir 2017
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
    describeClassifierModels,
    describePredictorModels,
    adjustEnergy,
    metabCost,
    packageVersion
  ) where

import ALife.Creatur (agentId)
import qualified ALife.Creatur.Wain as W
import ALife.Creatur.Wain.Brain (classifier, predictor)
import ALife.Creatur.Wain.GeneticSOM (modelMap, numModels)
import ALife.Creatur.Wain.DVector.Pattern (Pattern)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.DVector.Tweaker (PatternTweaker(..))
import ALife.Creatur.Wain.UnitInterval (uiToDouble)
import Control.Lens hiding (universe)
import Control.Monad.State.Lazy (StateT)
import qualified Data.Map.Strict as M
import Data.Version (showVersion)
import Paths_creatur_dvector_wains (version)
import Text.Printf (printf)

-- | Returns the current version number of this library.
packageVersion :: String
packageVersion = "creatur-dvector-vector-wains-" ++ showVersion version

type PatternWain a rt m = W.Wain Pattern PatternTweaker rt m a

describeClassifierModels :: PatternWain a rt m -> [String]
describeClassifierModels w = map f ms
  where ms = M.toList . modelMap . view (W.brain . classifier) $ w
        f (l, r) = agentId w ++ "'s classifier model "
                     ++ show l ++ " " ++ show r

describePredictorModels :: Show a => PatternWain a rt m -> [String]
describePredictorModels w = map f ms
  where ms = M.toList . modelMap . view (W.brain . predictor) $ w
        f (l, r) = agentId w ++ "'s predictor model "
                     ++ show l ++ ": " ++ pretty r

adjustEnergy
  :: Simple Lens e (PatternWain a rt m) -> Double
    -> Simple Lens s Double -> String -> Simple Lens e s
      -> (String -> StateT e IO ()) -> StateT e IO ()
adjustEnergy
    wainLens deltaE statLens reason summary report = do
  w <- use wainLens
  let (w', used) = W.adjustEnergy deltaE w
  report $ "Adjusting energy of " ++ agentId w
    ++ " because " ++ reason ++ ": "
    ++ printf "%.3f" (uiToDouble . W._energy $ w)
    ++ " + " ++ printf "%.3f" deltaE
    ++ " -> " ++ printf "%.3f" (uiToDouble . W._energy $ w')
    ++ " used=" ++ printf "%.3f" used ++ " leftover="
    ++ printf "%.3f" (deltaE - used)
  (summary . statLens) += used
  assign wainLens w'

metabCost :: Double -> Double -> Double -> PatternWain a rt m -> Double
metabCost bmc cpcm scale w = scale * (bmc + cpcm * fromIntegral n)
  where n = numModels . view (W.brain . classifier) $ w