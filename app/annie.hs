{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Annie                      as A
import qualified HAnon.Lib                  as H
import           Control.Monad
import qualified Options                    as O

main :: IO ()
main = O.runCommand $ \opts args ->
  case True of
    False -> do
      when (optScan opts) $ H.scanFiles   H.byteStringHAnon "hanon_mapping" args
      when (optMap opts)  $ H.mapFiles    H.byteStringHAnon "hanon_mapping" args
      when (optList opts) $ H.listMapping                   "hanon_mapping"
    True  -> do
      when (optScan opts) $ A.scanFiles                     "annie_mapping" args
      when (optMap opts)  $ A.mapFiles                      "annie_mapping" args
      when (optList opts) $ A.listMapping                   "annie_mapping"

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
