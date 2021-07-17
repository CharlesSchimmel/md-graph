module Options where

import           Node

import           Data.Functor
import           Data.Text                     as T
import           Options.Applicative
import           Prelude                       as P
import           System.FilePath

data Arguments = Arguments
    { argLibrary   :: [FilePath]
    , argDefExt    :: FilePath
    , argRunType   :: RunType
    , argIncStatic :: Bool
    , argIncNonex  :: Bool
    }

data RunType =
    Orphans
      | Unreachable
      | Nonexes
      | Statics
      | Subgraph { fileToSubgraph :: Node }
      | Backlinks { fileToBacklink :: Node }
      deriving Show

opts =
    info parseArguments
        $  fullDesc
        <> progDesc
               "A utility for graph operations on a collection of markdown files"
        <> header "Markdown Graph"

parseArguments :: Parser Arguments
parseArguments =
    Arguments
        <$> parseLibrary
        <*> parseDefaultExt
        <*> parseRunType
        <*> parseIncludeStatic
        <*> parseIncludeNonExistant

parseLibrary :: Parser [FilePath]
parseLibrary = some $ strOption
    (long "library" <> short 'l' <> help "Files or directories to parse")

parseDefaultExt :: Parser FilePath
parseDefaultExt = P.dropWhile (== '.') <$> strOption
    (  long "default-ext"
    <> short 'd'
    <> help "Default extension to use for files linked without extension"
    <> showDefault
    <> value "md"
    )


parseRunType :: Parser RunType
parseRunType =
    parseOrphans <|> parseUnreachable <|> parseSubgraph <|> parseBacklink

parseSubgraph :: Parser RunType
parseSubgraph = Subgraph <$> option
    nodeReader
    (long "subgraph" <> short 's' <> help "Find the subgraph of given file")

parseOrphans :: Parser RunType
parseOrphans = flag' Orphans $ long "orphans" <> short 'o' <> help
    "Find files without forward or backward links"

parseUnreachable :: Parser RunType
parseUnreachable = flag' Unreachable $ long "unreachable" <> short 'u' <> help
    "Find files that are unreachable, (have no backward links)"

parseBacklink :: Parser RunType
parseBacklink = Backlinks <$> option
    nodeReader
    (long "backlink" <> short 'b' <> help "Find the backlinks for a given file")

parseIncludeStatic :: Parser Bool
parseIncludeStatic =
    option auto $ long "inc-static" <> value True <> showDefault <> help
        "Include static files in output"

parseIncludeNonExistant :: Parser Bool
parseIncludeNonExistant =
    option auto $ long "inc-nonex" <> value False <> showDefault <> help
        "Include non-existant files in output"

nodeReader :: ReadM Node
nodeReader = str <&> \s -> case s of
    ('#' : tagText) -> Tag $ T.pack tagText
    file            -> Link $ normalise file
