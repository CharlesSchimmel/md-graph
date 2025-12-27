{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module MdGraph.App.Arguments
  ( Arguments (..),
    DatabaseArg (..),
    cliArguments,
  )
where

import Data.Char as C
  ( toLower,
  )
import Data.Functor
import Data.List.NonEmpty
import Data.Maybe
import Data.Text as T
import MdGraph.App.Command
import MdGraph.App.LogLevel as LogLevel
import MdGraph.Node
import MdGraph.TagDirection as TagDirection
import Options.Applicative
import System.Directory as D
import System.FilePath as F
import Prelude as P

newtype DatabaseArg = DbFile {dbFile :: Text}
  deriving (Show)

data Arguments = Arguments
  { argLibrary :: FilePath,
    argDefExt :: FilePath,
    argDatabase :: DatabaseArg,
    argLogLevel :: LogLevel,
    argCommand :: Command -- OptParse determines arguments order from the order in which parsers are applied, so this should stay last
  }
  deriving (Show)

cliArguments :: IO Arguments
cliArguments =
  customExecParser (prefs disambiguate) $
    info (helper <*> parseArguments) $
      fullDesc
        <> header
          "md-graph - A utility for graph operations on a collection of markdown files"

parseArguments :: Parser Arguments
parseArguments =
  Arguments
    <$> parseLibrary
    <*> parseDefaultExt
    <*> parseDatabase
    <*> parseLogLevel
    <*> parseCommand

parseCommand :: Parser Command
parseCommand =
  hsubparser
    ( command
        "subgraph"
        ( info (Subgraph <$> parseSubgraphOptions) $
            progDesc "The subgraph of a node"
        )
        <> command
          "backlinks"
          ( info (Backlinks <$> parseBacklinkOptions) $
              progDesc "The backlinks (reverse subgraph) of a node"
          )
        <> command
          "unreachable"
          (info (pure Unreachable) $ progDesc "Files that are not linked to")
        <> command
          "orphans"
          (info (pure Orphans) $ progDesc "Files without any links")
        <> command
          "nonexistent"
          (info (pure Nonexes) $ progDesc "Links that cannot be resolved")
        <> command
          "static"
          ( info (pure Statics) $
              progDesc "Links that can be resolved but are not notes"
          )
        <> command
          "populate"
          (info (pure Populate) $ progDesc "Just populate the database")
    )

parseDatabase :: Parser DatabaseArg
parseDatabase =
  DbFile
    <$> strOption
      ( long "database"
          <> short 'd'
          <> help "Sqlite database to use. If no argument is given, an in-memory database will be used for this command."
          <> metavar "DB"
          <> value ":memory:"
          <> showDefault
      )

parseLibrary :: Parser FilePath
parseLibrary =
  strOption
    ( long "library"
        <> short 'l'
        <> help "Directory to search"
        <> metavar "FILE|DIR"
        <> value "./"
        <> showDefault
    )

parseDefaultExt :: Parser FilePath
parseDefaultExt =
  P.dropWhile (== '.')
    <$> strOption
      ( long "default-ext"
          <> short 'x'
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
    <*> parseMaxDepth subgraphDefaultMaxDepth
    <*> parseMinDepth subgraphDefaultMinDepth

parseBacklinkOptions =
  BacklinkOptions
    <$> parseSubgraphTargets
    <*> parseMaxDepth backlinksDefaultMaxDepth
    <*> parseMinDepth backlinksDefaultMinDepth

parseMaxDepth :: Integer -> Parser Integer
parseMaxDepth def =
  option auto $
    long "max-depth"
      <> help "How many links deep traversal should go"
      <> showDefault
      <> value def

parseMinDepth :: Integer -> Parser Integer
parseMinDepth _default =
  option auto $
    long "min-depth"
      <> help "Only return links that are this many deep. The target node is at depth 0."
      <> showDefault
      <> value _default

parseSubgraphTargets :: Parser [SubgraphTarget]
parseSubgraphTargets =
  some $
    argument
      readNode
      (metavar "NODES" <> help "Nodes (files or #tags) to process")

parseIncludeStatic :: Parser Bool
parseIncludeStatic =
  option readCaseInsensitiveBool $
    long "inc-static"
      <> value True
      <> showDefault
      <> help "Include static files in output"
      <> metavar "True|False"

parseIncludeNonExistent :: Parser Bool
parseIncludeNonExistent =
  option readCaseInsensitiveBool $
    long "inc-nonex"
      <> value False
      <> showDefault
      <> help "Include non-existent files in output"
      <> metavar "True|False"

readCaseInsensitiveBool :: ReadM Bool
readCaseInsensitiveBool = maybeReader . asLower $ \case
  "true" -> Just True
  "false" -> Just False
  _ -> Nothing

readNode :: ReadM SubgraphTarget
readNode =
  str <&> \case
    ('#' : tagText) -> TagTarget tagText
    file -> FileTarget $ normalise file

asLower :: (String -> b) -> String -> b
asLower fn = fn . fmap C.toLower

parseTagDirection :: Parser TagDirection
parseTagDirection =
  option readTagDirection $
    long "tag-direction"
      <> help "Change the direction of tags"
      <> value TagDirection.Out
      <> showDefault
      <> metavar "In|Out|Both"
  where
    readTagDirection :: ReadM TagDirection
    readTagDirection = maybeReader . asLower $ \case
      "in" -> Just TagDirection.In
      "out" -> Just TagDirection.Out
      "both" -> Just TagDirection.Both
      _ -> Nothing

parseLogLevel :: Parser LogLevel.LogLevel
parseLogLevel =
  option readVerbosity $
    long "verbosity"
      <> help "Modify the logging level"
      <> short 'v'
      <> metavar "DEBUG|INFO|ERROR|NONE"
      <> value LogLevel.None
      <> showDefault
  where
    readVerbosity :: ReadM LogLevel
    readVerbosity = maybeReader . asLower $ \case
      "debug" -> Just LogLevel.Debug
      "info" -> Just LogLevel.Info
      "error" -> Just LogLevel.Error
      "none" -> Just LogLevel.None
      _ -> Nothing
