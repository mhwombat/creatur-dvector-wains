------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Tweaker
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for comparing and adjusting vectors.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.DVector.Tweaker
  (
    PatternTweaker(..)
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Tweaker(..))
import qualified ALife.Creatur.Wain.DVector.Pattern as P
import  ALife.Creatur.Wain.Pretty (Pretty)
import  ALife.Creatur.Wain.Statistics (Statistical(..), prefix)
import ALife.Creatur.Gene.Numeric.Weights (Weights)

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

newtype PatternTweaker = PatternTweaker Weights
  deriving (Eq, Show, Pretty, Generic)

instance Tweaker PatternTweaker where
  type Pattern PatternTweaker = P.Pattern
  diff (PatternTweaker ws) = P.weightedDVectorDiff ws
  adjust _ = P.makeSimilar

instance Serialize PatternTweaker
instance W8.Genetic PatternTweaker
instance Diploid PatternTweaker

instance Statistical PatternTweaker where
  stats (PatternTweaker ws) = map (prefix "tweaker weight") . stats $ ws
