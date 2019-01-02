------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Object
-- Copyright   :  (c) Amy de Buitléir 2017-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with objects that could be either wains or
-- records.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
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

import ALife.Creatur (agentId)
import qualified ALife.Creatur.Wain as W
import qualified ALife.Creatur.Wain.GeneticSOM as S
import ALife.Creatur.Wain.Response (Response)
import ALife.Creatur.Wain.DVector.Pattern (Pattern)
import ALife.Creatur.Wain.DVector.Wain (PatternWain)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Lens
import GHC.Generics (Generic)
import Data.Serialize

data Object a rt m = PObject Pattern String
              | AObject (PatternWain a rt m)
              deriving (Eq, Show, Generic)

instance (Ord a, Serialize a, Serialize rt, Serialize m, S.Tweaker rt,
  Response a ~ S.Pattern rt)
    => Serialize (Object a rt m)

isPattern :: Object a rt m -> Bool
isPattern (PObject _ _) = True
isPattern (AObject _) = False

objectId :: Object a rt m -> String
objectId (PObject _ s) = "DVector " ++ s
objectId (AObject a) = agentId a

objectNum :: Object a rt m -> Int
objectNum (PObject _ s) = read . take 1 . drop 1 $ s
objectNum (AObject _) = 10

objectAppearance :: Object a rt m -> Pattern
objectAppearance (PObject img _) = img
objectAppearance (AObject a) = view W.appearance a

objectEnergy :: Object a rt m -> UIDouble
objectEnergy (PObject _ _) = 0
objectEnergy (AObject a) = view W.energy a

objectChildEnergy :: Object a rt m -> Double
objectChildEnergy (PObject _ _) = 0
objectChildEnergy (AObject a) = W.childEnergy a

addIfWain
  :: Object a rt m -> [PatternWain a rt m]
    -> [PatternWain a rt m]
addIfWain (PObject _ _) xs = xs
addIfWain (AObject a) xs = a:xs

objectToWain :: Object a rt m -> PatternWain a rt m
objectToWain (PObject _ _) = error "record, not wain"
objectToWain (AObject a) = a
