{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE BangPatterns               #-}

module Annie
    ( scanFiles
    , mapFiles
    , listMapping
    , gen
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Array
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as B
import           Data.Char
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

infixl 6 <%>

-- | Open each of the files and create translations for each candidate
scanFiles :: FilePath -> [FilePath] -> IO ()
scanFiles = annieFileLoop $ \a ifp -> do
    liftIO $ putStrLn $ "[Annie] Scanning " ++ ifp
    ifh <- liftIO $ openFile ifp ReadMode
    processFile a ifh Nothing
    liftIO $ hClose ifh

-- | Map each of the argument files
mapFiles :: FilePath -> [FilePath] -> IO ()
mapFiles = annieFileLoop $ \a ifp -> do
    let ofp = ifp ++ ".anon"
    liftIO $ putStrLn $ "[Annie] Mapping " ++ ifp ++ " --> " ++ ofp
    ifh <- liftIO $ openFile ifp ReadMode
    ofh <- liftIO $ openFile ofp WriteMode
    processFile a ifh $ Just ofh
    liftIO $ hClose ifh
    liftIO $ hClose ofh

-- | Show all mappings available in the database
listMapping :: FilePath -> IO ()
listMapping db_fp = do
    putStrLn "[Annie] Listing dictionary"
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

gen :: Int -> FilePath -> IO ()
gen n0 ifp = do
    ofh <- liftIO $ openFile ifp WriteMode
    loop n0 ofh
  where
    loop n ofh = case n <= 0 of
        True  -> return ()
        False -> genRandomLine ofh >> loop (n-1) ofh

annieFileLoop :: (Annie->FilePath->IO ())
              -> FilePath
              -> [FilePath]
              -> IO ()
annieFileLoop bdy db_fp fps = runAnnie db_fp $ \a -> mapM_ (bdy a) fps

dbOptions :: Options
dbOptions = def { createIfMissing = True }

dbReadOptions :: ReadOptions
dbReadOptions = def

dbWriteOptions :: WriteOptions
dbWriteOptions = def


-----------------------------------------------------------------------
-- processFile
-----------------------------------------------------------------------

processFile :: Annie -> Handle -> Maybe Handle -> IO ()
processFile a ifh mb_ofh = do
    iseof <- liftIO $ hIsEOF ifh
    unless iseof $ do
        ln <- liftIO $ B.hGetLine ifh
        anonLine a mb_ofh ln
        processFile a ifh mb_ofh


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
    loop 0 mb_b $ allMatches $ ln *=~ combinedHighlighter
  where
    mb_b :: Maybe B.Builder
    mb_b = const mempty <$> mb_h

    loop :: Int -> Maybe B.Builder -> [Match B.ByteString] -> IO ()
    loop bc mb [] = do
        putBuilder mb_h $ mb <%> build bs <%> "\n"
      where
        bs = B.drop bc ln
    loop bc mb (mtch:mtchs) = do
        bs' <- anon a co $ capturedText cap
        loop bc' (mb<%>build pfx<%>build bs') mtchs
      where
        pfx = segment bc (cof-bc) ln
        bc' = cof + captureLength cap
        cof = captureOffset cap
        (co,cap) = case as of
          []           -> error "anonLine: the impossible happened!"
          (co_,cap_):_ -> (co_,cap_)
        as =
          [ (co_,cap_)
            | (co_,cap_)<-assocs $ matchArray mtch
            , captureOffset cap_ /= -1
            , getCaptureOrdinal co_ /= 0
            ]

(<%>) :: Maybe B.Builder -> B.Builder -> Maybe B.Builder
(<%>)  Nothing   _   = Nothing
(<%>) (Just bu0) bu1 = Just $ bu0<>bu1

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
        | (nm,mg) <- zip captureNameSupply $ map snd anonymizers
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

segment :: Int -> Int -> B.ByteString -> B.ByteString
segment st ln = B.take ln . B.drop st

putBuilder :: Maybe Handle -> Maybe B.Builder -> IO ()
putBuilder mb_h mb_b = fromMaybe (return ()) $ do
    h <- mb_h
    b <- mb_b
    return $ B.hPutBuilder h b

build :: B.ByteString -> B.Builder
build = B.byteString


-----------------------------------------------------------------------
-- highlighters & anonymizers
-----------------------------------------------------------------------

-- | The highlighters of all input paths
highlighters :: [Highlighter]
highlighters = map fst anonymizers

-- | The input paths available
anonymizers :: [(Highlighter, MappingGenerator)]
anonymizers =
  [ (,) emailHighlighter           $ randomEmail
  , (,) dutchPostalCodeHighlighter $ constant "1234AA"
  , (,) namesHighlighter           $ randomName
  , (,) phoneNumberHighlighter     $ randomPhoneNumber
  ]


-----------------------------------------------------------------------
-- Highliger & the highlighters
-----------------------------------------------------------------------

-- | a text pattern -- i.e., a RE
type Highlighter = RE

-- |Highlight any x@x.x string
emailHighlighter :: Highlighter
emailHighlighter = [re|\S+@\S+\.\S+|]

dutchPostalCodeHighlighter :: Highlighter
dutchPostalCodeHighlighter = [re|[0-9]{4} *[A-Za-z]{2}|]

phoneNumberHighlighter :: Highlighter
phoneNumberHighlighter = [re|[+]?[0-9]{8,13}|]

namesHighlighter :: Highlighter
namesHighlighter = [re|[A-Z][a-z]+ +[A-Z][a-z]+|]


-----------------------------------------------------------------------
-- MappingGenerator & the mapping genereators
-----------------------------------------------------------------------

genRandomLine :: Handle -> IO ()
genRandomLine h = do
    w <- randomWord
    e <- randomEmail       ""
    p <- randomPhoneNumber ""
    n <- randomName        ""
    B.hPutStrLn h $ B.unwords [w,e,p,n]


-----------------------------------------------------------------------
-- MappingGenerator & the mapping genereators
-----------------------------------------------------------------------

-- | Generate a new replacement from a given key
type MappingGenerator = B.ByteString -> IO B.ByteString

-- | MappingGenerator that always results in a constant value
constant :: String -> MappingGenerator
constant s _ = return $ B.pack s

-- | Generate random@random.com
randomEmail :: MappingGenerator
randomEmail _ = do
    a <- randomWord
    b <- randomWord
    return $ B.concat [ a, "@", b, ".com" ]

randomPhoneNumber :: MappingGenerator
randomPhoneNumber _ = fmap B.pack $ runRVar mk DevURandom
  where
    mk = sequence $ replicate 8 $ dc <$> stdUniform

    dc :: Int -> Char
    dc i = intToDigit $ abs i `mod` 10

randomName :: MappingGenerator
randomName _ = do
    a <- randomWord
    b <- randomWord
    return $ B.unwords [capse a,capse b]
  where
    capse x = case B.uncons x of
      Nothing    -> x
      Just (c,t) -> B.cons (toUpper c) t

randomWord :: IO B.ByteString
randomWord = runRVar (choice someByteStringWords) DevURandom

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
