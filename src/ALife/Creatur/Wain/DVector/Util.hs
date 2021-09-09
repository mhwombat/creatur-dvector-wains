------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Util
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.DVector.Util
  (
    formatVector
  ) where

import Text.Printf (printf)

formatVector :: String -> [Double] -> String
formatVector fmt = unwords . map (printf fmt)
