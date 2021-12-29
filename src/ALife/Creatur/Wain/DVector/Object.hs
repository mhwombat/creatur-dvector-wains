------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Object
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with objects that could be either wains or
-- records.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeFamilies  #-}
module ALife.Creatur.Wain.DVector.Object
  (
    Object(..),
    isPattern,
    objectId,
    objectNum,
    objectAppearance,
    objectEnergy,
    objectChildEnergy,
    addIfWain,
    objectToWain
  ) where

import           ALife.Creatur                           (agentId)
import           ALife.Creatur.Gene.Numeric.UnitInterval (UIDouble)
import qualified ALife.Creatur.Wain                      as W
import           ALife.Creatur.Wain.DVector.Pattern      (Pattern)
import           ALife.Creatur.Wain.DVector.Wain         (PatternWain)
import           Data.Serialize
import           GHC.Generics                            (Generic)

data Object rt a m = PObject Pattern String
              | AObject (PatternWain rt a m)
              deriving (Eq, Show, Generic)

instance (Ord a, Serialize a, Serialize rt, Serialize m)
    => Serialize (Object rt a m)

isPattern :: Object rt a m -> Bool
isPattern (PObject _ _) = True
isPattern (AObject _)   = False

objectId :: Object rt a m -> String
objectId (PObject _ s) = "DVector " ++ s
objectId (AObject a)   = agentId a

objectNum :: Object rt a m -> Int
objectNum (PObject _ s) = read . take 1 . drop 1 $ s
objectNum (AObject _)   = 10

objectAppearance :: Object rt a m -> Pattern
objectAppearance (PObject img _) = img
objectAppearance (AObject a)     = W.appearance a

objectEnergy :: Object rt a m -> UIDouble
objectEnergy (PObject _ _) = 0
objectEnergy (AObject a)   = W.energy a

objectChildEnergy :: Object rt a m -> Double
objectChildEnergy (PObject _ _) = 0
objectChildEnergy (AObject a)   = W.childEnergy a

addIfWain
  :: Object rt a m -> [PatternWain rt a m]
    -> [PatternWain rt a m]
addIfWain (PObject _ _) xs = xs
addIfWain (AObject a) xs   = a:xs

objectToWain :: Object rt a m -> PatternWain rt a m
objectToWain (PObject _ _) = error "record, not wain"
objectToWain (AObject a)   = a
