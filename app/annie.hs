{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import qualified Annie                      as A
import qualified HAnon.Lib                  as H
import qualified Data.ByteString.Char8      as B
import           Data.Maybe
import           Data.Monoid
import           Options.Applicative        as OP
import           Text.Read

main :: IO ()
main = do
  CLI{..} <- execParser $ mkPI cli_p
  case getBackEnd of
    AnnieBE -> case getCommand of
      Scan fps -> A.scanFiles      "annie_mapping" fps
      Map  fps -> A.mapFiles       "annie_mapping" fps
      List     -> A.listMapping    "annie_mapping"
      Gen n fp -> A.gen n fp
    HAnonStringBE ha -> case getCommand of
      Scan fps -> H.scanFiles   ha "nonah_mapping" fps
      Map  fps -> H.mapFiles    ha "nonah_mapping" fps
      List     -> H.listMapping    "nonah_mapping"
      Gen n fp -> A.gen n fp
    HAnonBytesBE ha -> case getCommand of
      Scan fps -> H.scanFiles   ha "nonah_mapping" fps
      Map  fps -> H.mapFiles    ha "nonah_mapping" fps
      List     -> H.listMapping    "nonah_mapping"
      Gen n fp -> A.gen n fp


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
  | Gen  Int FilePath
  deriving (Show)

cli_p :: Parser CLI
cli_p = CLI <$> backend_p <*> command_p

backend_p :: Parser BackEnd
backend_p = fmap (fromMaybe AnnieBE) $
  optional
     $  b_opt "annie"              AnnieBE
    <|> b_opt "hanon-string"      (HAnonStringBE H.stringHAnon    )
    <|> b_opt "hanon-bytestring"  (HAnonBytesBE  H.byteStringHAnon)
  where
    b_opt lng be =
      flag' be
        $  long lng
        <> help hlp
      where
        hlp = "use " ++ lng ++ " backend"

command_p :: Parser Command
command_p = subparser $ foldr (<>) mempty
    [ cmd "scan"        scn $      Scan <$> many file_p
    , cmd "map"         mpp $      Map  <$> many file_p
    , cmd "list"        lst $ pure List
    , cmd "gen"         gnn $      Gen  <$> num_p <*> file_p
    ]
  where
    scn = "Scan files building substitution DB"
    mpp = "Anonymise files, building substitution DB"
    lst = "List the substitution DB"
    gnn = "Generate a random file with n lines"

file_p :: Parser FilePath
file_p =
    argument str
      $  metavar "FILEPATH"
      <> help    "a file"

num_p :: Parser Int
num_p =
    argument (eitherReader psr)
      $  metavar "NUMBER-OF-LINES"
      <> help    "the number of random limes to generate"
  where
    psr = maybe (Left "bad number syntax") Right . readMaybe

mkPI :: Parser a -> ParserInfo a
mkPI p =
  info (helper <*> p)
       $  fullDesc
       <> progDesc "the annie CLI"
       <> header   "for benchmarking regexby anonymizing text"
       <> footer   "see --help for details of each sub-command"

cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd pri dsc pr = command pri $ info (helper <*> pr) $ progDesc dsc
