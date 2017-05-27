{-# LANGUAGE OverloadedStrings          #-}

module Annie
    ( Annie
    , stringAnnie
    , byteStringAnnie
    , scanFiles
    , mapFiles
    , listMapping
    ) where

import           Annie.Mapper
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.UTF8       as B
import           Data.Function
import           Data.List
import           Data.Maybe
import           Database.LevelDB.Higher
import           System.IO


-- | Open each of the files and create translations for each candidate
scanFiles :: Annie t -> [FilePath] -> LevelDB ()
scanFiles at = mapM_ $ scanFile at

-- | Map each of the argument files
mapFiles :: Annie t -> [FilePath] -> LevelDB ()
mapFiles at = mapM_ $ mapFile at

-- | Show all mappings available in the database
listMapping :: Annie t -> LevelDB ()
listMapping _ = do
    liftIO $ putStrLn "Listing dictionary"
    lns <- scan (B.fromString "")
      queryList
        { scanMap = \(key, value) -> BS.intercalate ":" [key, value]
        }
    liftIO $ mapM_ BS.putStrLn lns


-----------------------------------------------------------------------
-- scanFile
-----------------------------------------------------------------------

scanFile :: Annie t -> FilePath -> LevelDB ()
scanFile at file = do
    liftIO $ putStrLn $ "Scanning " ++ file
    fh <- liftIO $ openFile file ReadMode
    runOnLinesFromHandle at fh
    liftIO $ hClose fh

runOnLinesFromHandle :: Annie t -> Handle -> LevelDB ()
runOnLinesFromHandle at h = do
    isoef <- liftIO $ hIsEOF h
    unless isoef $ do
      ln <- liftIO $ hGetLineAT at h
      mapM_ (runInputPath at ln) $ inputPaths at
      runOnLinesFromHandle at h

runInputPath :: Annie t -> t -> InputPath t -> LevelDB ()
runInputPath at ln (hl, mpr) = do
    mks <- liftIO $ mapM mpr kys
    writeMapping at $ zip kys mks
  where
    kys = searchAT at hl ln

writeMapping :: Annie t -> [(t, t)] -> LevelDB ()
writeMapping at = mapM_ $ \(k,v) -> putAT at k v


-----------------------------------------------------------------------
-- mapFile
-----------------------------------------------------------------------

mapFile :: Annie t -> FilePath -> LevelDB ()
mapFile at inputPath = do
    liftIO $ putStrLn $ "Mapping " ++ inputPath
    let outputPath = inputPath ++ ".anon"
    ifh <- liftIO $ openFile inputPath ReadMode
    ofh <- liftIO $ openFile outputPath WriteMode
    mapLinesFromTo at combinedHighlighter ifh ofh
    liftIO $ hClose ifh
    liftIO $ hClose ofh

mapLinesFromTo :: Annie t -> Highlighter -> Handle -> Handle -> LevelDB ()
mapLinesFromTo at hl ifh ofh = do
    iseof <- liftIO $ hIsEOF ifh
    unless iseof $ do
        ln <- liftIO $ hGetLineAT at ifh
        let kys = searchAT at hl ln
        mpg <- readMapping at kys
        let ml = foldr (uncurry $ replaceAT at) ln $
                        sortBy (compare `on` (lengthAT at . fst)) mpg
        liftIO $ hPutStrLnAT at ofh ml
        mapLinesFromTo at hl ifh ofh

-- | Read all given keys from the database and return their mappings
readMapping :: Annie t -> [t] -> LevelDB [(t, t)]
readMapping at = mapM $ \k -> do
    mb_v <- getAT at k
    return (k, fromJust mb_v)
      -- The key must exists, or scanning failed and we would crash here
