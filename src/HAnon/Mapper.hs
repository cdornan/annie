{-# LANGUAGE QuasiQuotes                #-}

module HAnon.Mapper
    ( InputPath
    , Highlighter
    , HAnon(..)
    , stringHAnon
    , byteStringHAnon
    , inputPaths
    , combinedHighlighter
    , highlighters
    , emailHighlighter
    , dutchPostalCodeHighlighter
    , namesHighlighter
    , phoneNumberHighlighter
    ) where

import qualified Data.ByteString.Char8      as B
import           Data.List
import           Data.Maybe
import           Data.Random
import           Data.Random.Source.DevRandom
import           Data.Random.Extras
import qualified Data.Text                  as T
import           Database.LevelDB.Higher
import           System.IO
import           Text.RE.TDFA


-- | Everything required to make a concrete mapping from a line
type InputPath t = (Highlighter, MappingGenerator t)

-- | a text pattern -- i.e., a RE
type Highlighter = RE

-- | Generate a new replacement from a given key
type MappingGenerator t = t -> IO t


data HAnon t =
  HAnon
    { fromStringAT    :: String -> t
    , concatAT        :: [t] -> t
    , lengthAT        :: t -> Int
    , replaceAT       :: t -> t -> t -> t
    , hGetLineAT      :: Handle -> IO t
    , hPutStrLnAT     :: Handle -> t -> IO ()
    , searchAT        :: Highlighter -> t -> [t]
    , getRandomWordAT :: IO t
    , putAT           :: t -> t -> LevelDB ()
    , getAT           :: t -> LevelDB (Maybe t)
    }

instance Show (HAnon t) where
  show _ = "Hanon <...>"

stringHAnon :: HAnon String
stringHAnon =
  HAnon
    { fromStringAT    = id
    , concatAT        = concat
    , lengthAT        = length
    , replaceAT       =
        \a b ln ->
            T.unpack $ T.replace (T.pack a) (T.pack b) (T.pack ln)
    , hGetLineAT      = hGetLine
    , hPutStrLnAT     = hPutStrLn
    , searchAT        = \rex ln -> matches $ ln *=~ rex
    , getRandomWordAT =
        runRVar (choice someStringWords) DevURandom
    , putAT           = \k v -> put (B.pack k) (B.pack v)
    , getAT           = \k   -> fmap B.unpack <$> get (B.pack k)
    }

byteStringHAnon :: HAnon B.ByteString
byteStringHAnon =
  HAnon
    { fromStringAT    = B.pack
    , concatAT        = B.concat
    , lengthAT        = B.length
    , replaceAT       = replace
    , hGetLineAT      = B.hGetLine
    , hPutStrLnAT     = B.hPutStrLn
    , searchAT        = \rex ln -> matches $ ln *=~ rex
    , getRandomWordAT =
        runRVar (choice someByteStringWords) DevURandom
    , putAT           = put
    , getAT           = get
    }

-- | The input paths available
inputPaths :: HAnon t -> [InputPath t]
inputPaths at =
  [ (,) emailHighlighter           $ randomEmail at
  , (,) dutchPostalCodeHighlighter $ constant    at "1234AA"
  , (,) namesHighlighter           $ constant    at "Willem Wever"
  , (,) phoneNumberHighlighter     $ constant    at "03012345678"
  ]


combinedHighlighter :: Highlighter
combinedHighlighter = fromMaybe urk $ compileRegex $
    "(" ++ intercalate ")|(" (map reSource highlighters) ++ ")"
  where
    urk = error "combinedHighlighter: the impossible happened!"

-- | The highlighters of all input paths
highlighters :: [Highlighter]
highlighters = map fst $ inputPaths urk
  where
    urk = error "highlighters: the impossible happened!"


-- |Highlight any x@x.x string
emailHighlighter :: Highlighter
emailHighlighter = [re|[^[:space:]]+@[^[:space:]]+\.[^[:space:]]+|]

dutchPostalCodeHighlighter :: Highlighter
dutchPostalCodeHighlighter = [re|[0-9]{4} *[A-Za-z]{2}|]

phoneNumberHighlighter :: Highlighter
phoneNumberHighlighter = [re|[+]?[0-9]{8,13}|]

namesHighlighter :: Highlighter
namesHighlighter = [re|[A-Z][a-z]+ +[A-Z][a-z]+|]


-- | Generate random@random.com
randomEmail :: HAnon t -> MappingGenerator t
randomEmail at _ = do
    a <- getRandomWordAT at
    b <- getRandomWordAT at
    return $ concatAT at
      [ a
      , fromStringAT at "@"
      , b
      , fromStringAT at ".com"
      ]

-- | MappingGenerator that always results in a constant value
constant :: HAnon t -> String -> MappingGenerator t
constant at s _ = return $ fromStringAT at s

someByteStringWords :: [B.ByteString]
someByteStringWords = B.words $ B.pack sampleWords

someStringWords :: [String]
someStringWords = words sampleWords

sampleWords :: String
sampleWords = "stane furor polder uppsala atomised ruffler paten recco hipping calcaneus wampanoag eulogium brainier semipious legalised vinethene \
    \ unvirile mignonne untelic seasick umtali nontonic curler oeuvre ube boggart megiddo seconde juryless trounce tarn korona unfealty corrade \
    \ rompingly tachisme greer unaverred revetment nitralloy solarium depositor cong comate matlock fromentin acetal darlan field favours paragraph erlanger \
    \ taconite facilely nooky passable tableaux regarding hooke boggler topeka insular microcopy tsaritsyn cumulated lasket syruplike telegony eagre unjamming \
    \ simbirsk judaized substrate sulawesi jemadar preta rebind psalmody perigone euthenist dean wove grunth sarabande reembrace ller nccl sightable keb dentes \
    \ degassing hooves vigilante rockiness varanasi couchant porrect subjugate"


replace :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replace a b = B.intercalate b . splitOn a

splitOn :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOn ndl = splt
    where
      splt txt = pre : sps
        where
          sps = case B.null rst of
            True  -> []
            False -> splt $ B.drop len rst

          (pre,rst) = B.breakSubstring ndl txt

      len = B.length ndl
