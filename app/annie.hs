{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Annie
import           Control.Monad
import           Database.LevelDB.Higher
import qualified Options                      as O

main :: IO ()
main = O.runCommand $ \opts args ->
    runCreateLevelDB "hanon_mapping" "hanon" $ do
        when (optScan opts) $ scanFiles   byteStringAnnie args
        when (optList opts) $ listMapping byteStringAnnie
        when (optMap opts)  $ mapFiles    byteStringAnnie args

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
