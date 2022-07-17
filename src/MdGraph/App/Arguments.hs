{-# LANGUAGE LambdaCase #-}

module MdGraph.App.Arguments
    ( Arguments(..)
    , opts
    ) where

import           MdGraph.App.Command
import           MdGraph.File
import           MdGraph.Node
import           MdGraph.TagDirection          as TagDirection

import           Data.Char                     as C
                                                ( toLower )
import           Data.Functor
import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Text                     as T
import           Options.Applicative
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F


data Arguments = Arguments
    { argLibrary  :: FilePath
    , argDefExt   :: FilePath
    , argCommand  :: Command
    , argDatabase :: Text
    }
    deriving Show

parseCommand :: Parser Command
parseCommand = hsubparser
    (  command
          "subgraph"
          ( info (Subgraph <$> (parseSubgraphOptions <*> parseDepth (-1)))
          $ progDesc "The subgraph of a node"
          )
    <> command
           "backlinks"
           ( info (Backlinks <$> (parseBacklinkOptions <*> parseDepth 1))
           $ progDesc "The backlinks (reverse subgraph) of a node"
           )
    <> command
           "unreachable"
           (info (pure Unreachable) $ progDesc "Files that are not linked to")
    <> command "orphans"
               (info (pure Orphans) $ progDesc "Files without any links")
    <> command
           "nonexistant"
           (info (pure Nonexes) $ progDesc "Links that cannot be resolved")
    <> command
           "static"
           ( info (pure Statics)
           $ progDesc "Links that can be resolved but are not notes"
           )
    )

opts :: IO Arguments
opts =
    customExecParser (prefs disambiguate)
        $  info (helper <*> parseArguments)
        $  fullDesc
        <> header
               "md-graph - A utility for graph operations on a collection of markdown files"

parseArguments :: Parser Arguments
parseArguments =
    Arguments
        <$> parseLibrary
        <*> parseDefaultExt
        <*> parseCommand
        <*> parseDatabase

parseDatabase :: Parser Text
parseDatabase = T.pack <$> strOption
    (  long "database"
    <> short 'd'
    <> help "Sqlite database to be used"
    <> metavar "DB"
    <> value ":memory:"
    )

parseLibrary :: Parser FilePath
parseLibrary = strOption
    (  long "library"
    <> short 'l'
    <> help "Directory to search"
    <> metavar "FILE|DIR"
    <> value "./"
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

-- parseSubgraphOptions :: Parser SubgraphOptions
parseSubgraphOptions =
    SubgraphOptions
        <$> parseSubgraphTargets
        <*> parseIncludeNonExistent
        <*> parseIncludeStatic
        <*> parseTagDirection

parseBacklinkOptions = BacklinkOptions <$> parseSubgraphTargets

parseDepth :: Integer -> Parser Integer
parseDepth def =
    option auto
        $  long "depth"
        <> help "How deep traversal should go"
        <> showDefault
        <> value def

parseSubgraphTargets :: Parser [SubgraphTarget]
parseSubgraphTargets = some $ argument
    readNode
    (metavar "NODES" <> help "Nodes (files or #tags) to process")

parseIncludeStatic :: Parser Bool
parseIncludeStatic =
    option readCaseInsensitiveBool
        $  long "inc-static"
        <> value True
        <> showDefault
        <> help "Include static files in output"
        <> metavar "True|False"

parseIncludeNonExistent :: Parser Bool
parseIncludeNonExistent =
    option readCaseInsensitiveBool
        $  long "inc-nonex"
        <> value False
        <> showDefault
        <> help "Include non-existent files in output"
        <> metavar "True|False"

readCaseInsensitiveBool :: ReadM Bool
readCaseInsensitiveBool = maybeReader . asLower $ \case
    "true"  -> Just True
    "false" -> Just False
    _       -> Nothing

readNode :: ReadM SubgraphTarget
readNode = str <&> \case
    ('#' : tagText) -> TagTarget tagText
    file            -> FileTarget $ normalise file

asLower :: (String -> b) -> String -> b
asLower fn = fn . fmap C.toLower

parseTagDirection :: Parser TagDirection
parseTagDirection =
    option readTagDirection
        $  long "tag-direction"
        <> help "Change the direction of tags"
        <> value TagDirection.Out
        <> showDefault
        <> metavar "In|Out|Both"

readTagDirection :: ReadM TagDirection
readTagDirection = maybeReader . asLower $ \case
    "in"   -> Just TagDirection.In
    "out"  -> Just TagDirection.Out
    "both" -> Just TagDirection.Both
    _      -> Nothing

