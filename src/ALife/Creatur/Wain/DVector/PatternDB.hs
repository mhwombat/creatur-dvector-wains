------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.PatternDB
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A "flat file" database
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.DVector.PatternDB
  (
    PatternDB,
    mkPatternDB,
    anyPattern
  ) where

import ALife.Creatur.Wain.DVector.Pattern (Pattern)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO, uniform)
import Control.Monad.State (StateT, get, gets, put)
-- import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Read (readEither)

-- | A "flat file" database.
data PatternDB = PatternDB
  {
    initialised :: Bool,
    file :: FilePath,
    records :: [(String, Pattern)]
  } deriving (Eq)

instance Show PatternDB where
  show = file

-- | @'mkPatternDB' f@ (re)creates the PatternDB in the
--   file @f@.
mkPatternDB :: FilePath -> PatternDB
mkPatternDB f = PatternDB False f []

initIfNeeded :: StateT PatternDB IO ()
initIfNeeded = do
  isInitialised <- gets initialised
  unless isInitialised $ do
    db <- get
    db' <- liftIO $ initialise db
    put db'

initialise :: PatternDB -> IO PatternDB
initialise db = do
  (_,xss) <- fmap fromCSV $ readFile (file db)
  return db { initialised=True, records = xss }

fromCSV :: String -> ([String], [(String, Pattern)])
fromCSV xss = extractValues . tokenise $ xss

-- toCSVLine :: Show a => (String,[a]) -> String
-- toCSVLine (k,vs) = k ++ ',':(intercalate "," . map show $ vs)

tokenise :: String -> [[String]]
tokenise = map (splitOn ",") . lines

extractValues :: [[String]] -> ([String], [(String, Pattern)])
extractValues xss =
  if null xss
    then error "no data"
    else (headings, values)
  where (headings:xs) = xss
        values = map parseLine xs

parseLine :: [String] -> (String, Pattern)
parseLine (x:xs) = (x, map safeRead xs)
parseLine [] = ("???",[])

safeRead :: Read a => String -> a
safeRead s = case readEither s of
               Left msg -> error $
                            "Unable to parse '" ++ s ++ "': " ++ msg
               Right x  -> x

anyPattern :: StateT PatternDB IO (String, Pattern)
anyPattern = do
  initIfNeeded
  db <- get
  liftIO $ evalRandIO (uniform . records $ db)
