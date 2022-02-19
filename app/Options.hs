{-# LANGUAGE LambdaCase #-}

module Options
    ( Arguments(..)
    , RunType(..)
    , opts
    ) where

import           File
import           Node
import           TagDirection

import           Data.Char                     as C
                                                ( toLower )
import           Data.Functor
import           Data.Text                     as T
import           Options.Applicative
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

data Arguments = Arguments
    { argLibrary   :: [FilePath]
    , argDefExt    :: FilePath
    , argRunType   :: RunType
    , argIncStatic :: Bool
    , argIncNonex  :: Bool
    , argTagDir    :: TagDirection
    }

data RunType =
    Orphans
      | Unreachable
      | Nonexes
      | Statics
      | Subgraph { fileToSubgraph :: Node }
      | Backlinks { fileToBacklink :: Node }
      deriving Show

opts :: ParserInfo Arguments
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
        <*> parseIncludeNonExistent
        <*> parseTagDirection

parseLibrary :: Parser [FilePath]
parseLibrary = some $ strOption
    (  long "library"
    <> short 'l'
    <> help "Files or directories to parse"
    <> metavar "FILE|DIR"
    )

parseDefaultExt :: Parser FilePath
parseDefaultExt = P.dropWhile (== '.') <$> strOption
    (  long "default-ext"
    <> short 'd'
    <> help "Default extension to use for files linked without extension"
    <> showDefault
    <> value "md"
    <> metavar "EXT"
    )


parseRunType :: Parser RunType
parseRunType =
    parseOrphans <|> parseUnreachable <|> parseSubgraph <|> parseBacklink

parseOrphans :: Parser RunType
parseOrphans = flag' Orphans $ long "orphans" <> short 'o' <> help
    "Find files without forward or backward links"

parseUnreachable :: Parser RunType
parseUnreachable = flag' Unreachable $ long "unreachable" <> short 'u' <> help
    "Find files that are unreachable, (have no backward links)"

parseSubgraph :: Parser RunType
parseSubgraph = Subgraph <$> option
    readNode
    (  long "subgraph"
    <> short 's'
    <> help "Find the subgraph of a given node"
    <> metavar "NODE"
    )

parseBacklink :: Parser RunType
parseBacklink = Backlinks <$> option
    readNode
    (  long "backlinks"
    <> short 'b'
    <> help "Find the backlinks for a given node"
    <> metavar "NODE"
    )

parseIncludeStatic :: Parser Bool
parseIncludeStatic =
    option auto
        $  long "inc-static"
        <> value True
        <> showDefault
        <> help "Include static files in output"
        <> metavar "True|False"

parseIncludeNonExistent :: Parser Bool
parseIncludeNonExistent =
    option auto
        $  long "inc-nonex"
        <> value False
        <> showDefault
        <> help "Include non-existent files in output"
        <> metavar "True|False"

readNode :: ReadM Node
readNode = str <&> \case
    ('#' : tagText) -> Tag $ T.pack tagText
    file            -> Link $ normalise file

asLower :: (String -> b) -> String -> b
asLower fn = fn . fmap C.toLower

parseTagDirection :: Parser TagDirection
parseTagDirection =
    option readTagDirection
        $  long "tag-direction"
        <> help "Change the direction of tags"
        <> value TagDirection.In
        <> showDefault
        <> metavar "In|Out|Both"

readTagDirection :: ReadM TagDirection
readTagDirection = maybeReader . asLower $ \case
    "in"   -> Just TagDirection.In
    "out"  -> Just TagDirection.Out
    "both" -> Just TagDirection.Both
    _      -> Nothing

