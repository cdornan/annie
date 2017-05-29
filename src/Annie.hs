{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE QuasiQuotes                #-}

module Annie
    ( scanFiles
    , mapFiles
    , listMapping
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Array
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as B
import           Data.Default
import qualified Data.HashMap.Strict        as HM
import           Data.IORef
import           Data.Maybe
import           Data.Random
import           Data.Random.Extras
import           Data.Random.Source.DevRandom
import qualified Data.Text                  as T
import           Database.LevelDB.Iterator
import           Database.LevelDB.MonadResource
import           System.IO
import           Text.RE.Replace
import           Text.RE.TDFA


-- | Open each of the files and create translations for each candidate
scanFiles :: FilePath -> [FilePath] -> IO ()
scanFiles db_fp fps = runAnnie db_fp $ \a -> mapM_ (scanFile a) fps

-- | Map each of the argument files
mapFiles :: FilePath -> [FilePath] -> IO ()
mapFiles db_fp fps = runAnnie db_fp $ \a -> mapM_ (mapFile a) fps

-- | Show all mappings available in the database
listMapping :: FilePath -> IO ()
listMapping db_fp = do
    putStrLn "Listing dictionary"
    runResourceT $ do
      db   <- open db_fp dbOptions
      liftIO $ do
        iter <- createIter db dbReadOptions
        iterFirst iter
        loop      iter
  where
    loop iter = do
      mb_k <- iterKey   iter
      mb_v <- iterValue iter
      case (mb_k,mb_v) of
        (Just k,Just v) -> do
          B.putStrLn $ B.intercalate ":" [k, v]
          iterNext iter
          loop iter
        _ -> releaseIter iter

dbOptions :: Options
dbOptions = def { createIfMissing = True }

dbReadOptions :: ReadOptions
dbReadOptions = def

dbWriteOptions :: WriteOptions
dbWriteOptions = def


-----------------------------------------------------------------------
-- scanFile
-----------------------------------------------------------------------

scanFile :: Annie -> FilePath -> IO ()
scanFile a file = do
    liftIO $ putStrLn $ "Scanning " ++ file
    h <- liftIO $ openFile file ReadMode
    runOnLinesFromHandle a h
    liftIO $ hClose h

runOnLinesFromHandle :: Annie -> Handle -> IO ()
runOnLinesFromHandle a h = do
    isoef <- liftIO $ hIsEOF h
    unless isoef $ do
      ln <- liftIO $ B.hGetLine h
      anonLine a Nothing ln
      runOnLinesFromHandle a h


-----------------------------------------------------------------------
-- mapFile
-----------------------------------------------------------------------

mapFile :: Annie -> FilePath -> IO ()
mapFile a inputPath = do
    liftIO $ putStrLn $ "Mapping " ++ inputPath
    ifh <- liftIO $ openFile inputPath  ReadMode
    ofh <- liftIO $ openFile outputPath WriteMode
    mapLinesFromTo a ifh ofh
    liftIO $ hClose ifh
    liftIO $ hClose ofh
  where
    outputPath = inputPath ++ ".anon"

mapLinesFromTo :: Annie -> Handle -> Handle -> IO ()
mapLinesFromTo a ifh ofh = do
    iseof <- liftIO $ hIsEOF ifh
    unless iseof $ do
        ln <- liftIO $ B.hGetLine ifh
        anonLine a (Just ofh) ln
        mapLinesFromTo a ifh ofh


-----------------------------------------------------------------------
-- InputPath
-----------------------------------------------------------------------

-- | Everything required to make a concrete mapping from a line
type InputPath = (Highlighter, MappingGenerator)

getRandomWord :: IO B.ByteString
getRandomWord = runRVar (choice someByteStringWords) DevURandom

-- | The highlighters of all input paths
highlighters :: [Highlighter]
highlighters = map fst inputPaths

-- | The input paths available
inputPaths :: [InputPath]
inputPaths =
  [ (,) emailHighlighter           $ randomEmail
  , (,) dutchPostalCodeHighlighter $ constant "1234AA"
  , (,) namesHighlighter           $ constant "Willem Wever"
  , (,) phoneNumberHighlighter     $ constant "03012345678"
  ]


-----------------------------------------------------------------------
-- Annie
-----------------------------------------------------------------------


data Annie =
  Annie
    { getMapRef :: !(IORef (HM.HashMap B.ByteString B.ByteString))
    , getDB     :: !DB
    }

runAnnie :: FilePath -> (Annie->IO a) -> IO a
runAnnie db_fp f = runResourceT $ do
    db <- open db_fp dbOptions
    liftIO $ flip Annie db <$> newIORef HM.empty >>= f

anonLine :: Annie -> Maybe Handle -> B.ByteString -> IO ()
anonLine a mb_h ln =
    loop 0 mempty $ allMatches $ ln *=~ combinedHighlighter
  where
    loop :: Int -> B.Builder -> [Match B.ByteString] -> IO ()
    loop bc bdr [] = putBuilder mb_h $ bdr <> B.byteString bs
      where
        bs = B.drop bc ln
    loop bc bdr (mtch:mtchs) = do
        bs' <- anon a co capturedText
        loop bc' (bdr<>B.byteString pfx<>B.byteString bs') mtchs
      where
        pfx = B.take (captureOffset-bc) $ B.drop captureOffset ln
        bc' = captureOffset + captureLength
        (co,Capture{..}) = case assocs $ matchArray mtch of
          []           -> error "anonLine: the impossible happened!"
          (co_,cap_):_ -> (co_,cap_)

anon :: Annie -> CaptureOrdinal -> B.ByteString -> IO B.ByteString
anon Annie{..} co bs = do
  hm <- readIORef getMapRef
  case HM.lookup bs hm of
    Just bs' -> return bs'
    Nothing  -> do
      mb <- get getDB dbReadOptions bs
      case mb of
        Just bs' -> do
          writeIORef getMapRef $ HM.insert bs bs' hm
          return bs'
        Nothing -> do
          bs' <- generators ! co $ bs
          writeIORef getMapRef $ HM.insert bs bs' hm
          put getDB dbWriteOptions bs bs'
          return bs'

generators :: Array CaptureOrdinal MappingGenerator
generators = array (mn,mx) ps
  where
    mn, mx :: CaptureOrdinal
    ps     :: [(CaptureOrdinal,MappingGenerator)]

    mn = minimum $ map fst ps
    mx = maximum $ map fst ps

    ps =
      [ (fromMaybe uk mb,mg)
        | (nm,mg) <- zip captureNameSupply $ map snd inputPaths
        , let mb = HM.lookup (CaptureName $ T.pack nm) hm
        ]

    hm = reCaptureNames combinedHighlighter

    uk = error "generators: the imposible happened!"

combinedHighlighter :: Highlighter
combinedHighlighter = fromMaybe urk $ compileRegex $ foldr mk ")" trs
  where
    mk (isb,cnm,hl) t = cl_b ++ "${" ++ cnm ++ "}(" ++ reSource hl ++ t
      where
        cl_b = case isb of
          True  -> ""
          False -> ")|"

    trs = zip3 (True:repeat False) captureNameSupply highlighters

    urk = error "combinedHighlighter: the impossible happened!"

captureNameSupply :: [String]
captureNameSupply = concat
    [ [ [c] | c<-['a'..'z'] ]
    , [ [c] | c<-['A'..'Z'] ]
    , [ 'c':show i | i<-[0..] :: [Int] ]
    ]

putBuilder :: Maybe Handle -> B.Builder -> IO ()
putBuilder = maybe (const $ return ()) B.hPutBuilder


-----------------------------------------------------------------------
-- Highlighter
-----------------------------------------------------------------------

-- | a text pattern -- i.e., a RE
type Highlighter = RE

-- |Highlight any x@x.x string
emailHighlighter :: Highlighter
emailHighlighter = [re|[^[:space:]]+@[^[:space:]]+\.[^[:space:]]+|]

dutchPostalCodeHighlighter :: Highlighter
dutchPostalCodeHighlighter = [re|[0-9]{4} *[A-Za-z]{2}|]

phoneNumberHighlighter :: Highlighter
phoneNumberHighlighter = [re|[+]?[0-9]{8,13}|]

namesHighlighter :: Highlighter
namesHighlighter = [re|[A-Z][a-z]+ +[A-Z][a-z]+|]


-----------------------------------------------------------------------
-- MappingGenerator
-----------------------------------------------------------------------

-- | Generate a new replacement from a given key
type MappingGenerator = B.ByteString -> IO B.ByteString

-- | Generate random@random.com
randomEmail :: MappingGenerator
randomEmail _ = do
    a <- getRandomWord
    b <- getRandomWord
    return $ B.concat [ a, "@", b, ".com" ]

-- | MappingGenerator that always results in a constant value
constant :: String -> MappingGenerator
constant s _ = return $ B.pack s

someByteStringWords :: [B.ByteString]
someByteStringWords = B.words
  "stane furor polder uppsala atomised ruffler paten recco hipping\
  \ calcaneus wampanoag eulogium brainier semipious legalised vinethene\
  \ unvirile mignonne untelic seasick umtali nontonic curler oeuvre ube\
  \ boggart megiddo seconde juryless trounce tarn korona unfealty corrade\
  \ rompingly tachisme greer unaverred revetment nitralloy solarium\
  \ depositor cong comate matlock fromentin acetal darlan field\
  \ favours paragraph erlanger taconite facilely nooky passable\
  \ tableaux regarding hooke boggler topeka insular microcopy\
  \ tsaritsyn cumulated lasket syruplike telegony eagre unjamming\
  \ simbirsk judaized substrate sulawesi jemadar preta rebind\
  \ psalmody perigone euthenist dean wove grunth sarabande reembrace\
  \ ller nccl sightable keb dentes degassing hooves vigilante\
  \ rockiness varanasi couchant porrect subjugate"
