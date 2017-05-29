{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Annie                      as A
import qualified HAnon.Lib                  as H
import           Control.Monad
import qualified Options                    as O


main :: IO ()
main = O.runCommand $ \opts args ->
  case AnnieBE of
    HAnonBE -> do
      when (optScan opts) $ H.scanFiles   H.byteStringHAnon "nonah_mapping" args
      when (optMap opts)  $ H.mapFiles    H.byteStringHAnon "nonah_hanon_mapping" args
      when (optList opts) $ H.listMapping                   "nonah_hanon_mapping"
    AnnieBE -> do
      when (optScan opts) $ A.scanFiles                     "annie_mapping" args
      when (optMap opts)  $ A.mapFiles                      "annie_mapping" args
      when (optList opts) $ A.listMapping                   "annie_mapping"
    TrivialBE -> do
      when (optScan opts) $ A.scanFilesT                    "annie_mapping" args
      when (optMap opts)  $ A.scanFilesT                    "annie_mapping" args
      when (optList opts) $ A.listMapping                   "annie_mapping"

data BackEnd = AnnieBE | HAnonBE | TrivialBE

data MainOptions = MainOptions
    { optScan :: Bool
    , optList :: Bool
    , optMap  :: Bool
    }

instance O.Options MainOptions where
    defineOptions = pure MainOptions
        <*> O.simpleOption "scan" False
              "Scan the given files to create a anonimizing translation mappings"
        <*> O.simpleOption "list" False
              "List all mappings"
        <*> O.simpleOption "map" False
              "Apply all possible mappings to the given files and output .anon versions"
