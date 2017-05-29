{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import qualified Annie                      as A
import qualified HAnon.Lib                  as H
import           Control.Monad
import qualified Data.ByteString.Char8      as B
import           Data.Maybe
import           Data.Monoid
import qualified Options                    as O
import           Options.Applicative        as OP

main :: IO ()
main = do
  CLI{..} <- execParser $ mkPI cli_p
  case getBackEnd of
    AnnieBE -> case getCommand of
      Scan fps -> A.scanFiles      "annie_mapping" fps
      Map  fps -> A.mapFiles       "annie_mapping" fps
      List     -> A.listMapping    "annie_mapping"
    HAnonStringBE ha -> case getCommand of
      Scan fps -> H.scanFiles   ha "nonah_mapping" fps
      Map  fps -> H.mapFiles    ha "nonah_mapping" fps
      List     -> H.listMapping    "nonah_mapping"
    HAnonBytesBE ha -> case getCommand of
      Scan fps -> H.scanFiles   ha "nonah_mapping" fps
      Map  fps -> H.mapFiles    ha "nonah_mapping" fps
      List     -> H.listMapping    "nonah_mapping"

old_main :: IO ()
old_main = O.runCommand $ \opts args ->
  case AnnieBE of
    HAnonStringBE ha -> do
      when (optScan opts) $ H.scanFiles   ha "nonah_mapping" args
      when (optMap opts)  $ H.mapFiles    ha "nonah_hanon_mapping" args
      when (optList opts) $ H.listMapping    "nonah_hanon_mapping"
    HAnonBytesBE ha -> do
      when (optScan opts) $ H.scanFiles   ha "nonah_mapping" args
      when (optMap opts)  $ H.mapFiles    ha "nonah_hanon_mapping" args
      when (optList opts) $ H.listMapping    "nonah_hanon_mapping"
    AnnieBE -> do
      when (optScan opts) $ A.scanFiles      "annie_mapping" args
      when (optMap opts)  $ A.mapFiles       "annie_mapping" args
      when (optList opts) $ A.listMapping    "annie_mapping"


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

data CLI =
  CLI
    { getBackEnd :: BackEnd
    , getCommand :: Command
    }
  deriving (Show)

data BackEnd
    = AnnieBE
    | HAnonStringBE (H.HAnon String)
    | HAnonBytesBE  (H.HAnon B.ByteString)
  deriving (Show)

data Command
  = Scan [FilePath]
  | Map  [FilePath]
  | List
  deriving (Show)

cli_p :: Parser CLI
cli_p = CLI <$> backend_p <*> command_p

backend_p :: Parser BackEnd
backend_p = fmap (fromMaybe AnnieBE) $
  optional
     $  b_opt "annie"              AnnieBE
    <|> b_opt "hanon-string"      (HAnonStringBE H.stringHAnon    )
    <|> b_opt "hanon-byte-string" (HAnonBytesBE  H.byteStringHAnon)
  where
    b_opt lng be =
      flag' be
        $  long lng
        <> help hlp
      where
        hlp = "use " ++ lng ++ " backend"

command_p :: Parser Command
command_p = subparser $ foldr (<>) mempty
    [ cmd "scan"        scn $      Scan <$> many file_arg_p
    , cmd "map"         mpp $      Map  <$> many file_arg_p
    , cmd "list"        lst $ pure List
    ]
  where
    scn = "Scan files building substitution DB"
    mpp = "Anonymise files, building substitution DB"
    lst = "List the substitution DB"

file_arg_p :: Parser FilePath
file_arg_p =
    argument str
      $  metavar "FILEPATH"
      <> help    "a file"

parseCLI :: [String] -> IO CLI
parseCLI =
  handleParseResult . OP.execParserPure (prefs idm) (mkPI cli_p)

pureParse :: Parser a -> [String] -> Maybe a
pureParse p = getParseResult . execParserPure (prefs idm) (mkPI p)

test_cli :: Show a => Parser a -> [String] -> IO ()
test_cli psr ss = do
  x <- handleParseResult $ OP.execParserPure (prefs idm) (mkPI psr) ss
  print x

mkPI :: Parser a -> ParserInfo a
mkPI p =
  info (helper <*> p)
       $  fullDesc
       <> progDesc "the annie CLI"
       <> header   "for benchmarking regexby anonymizing text"
       <> footer   "see --help for details of each sub-command"

cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd pri dsc pr = command pri $ info (helper <*> pr) $ progDesc dsc
