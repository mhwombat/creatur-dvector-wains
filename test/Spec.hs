------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the QuickCheck tests.
--
------------------------------------------------------------------------
module Main where

import           ALife.Creatur.Wain.DVector.PatternQC (test)

import           Test.Framework                       as TF (Test, defaultMain)

tests :: [TF.Test]
tests =
  [
    -- In increasing order of complexity
    ALife.Creatur.Wain.DVector.PatternQC.test
  ]

main :: IO ()
main = defaultMain tests
