{-# LANGUAGE LambdaCase #-}

module Arguments
    ( Arguments(..)
    , opts
    ) where

import           Command
import           File
import           Node
import           TagDirection

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
    { argLibrary :: NonEmpty FilePath
    , argDefExt  :: FilePath
    , argCommand :: Command
    }
    deriving Show

-- rename to "Command"?
-- The other options (Nonex, Static, TagDir) could probably be part of this type, too
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
    Arguments <$> parseLibrary <*> parseDefaultExt <*> parseCommand

parseLibrary :: Parser (NonEmpty FilePath)
parseLibrary =
    fromMaybe ("./" :| [])
        <$> (optional $ some1
                (strOption
                    (  long "library"
                    <> short 'l'
                    <> help "Files or directories to parse"
                    <> metavar "FILE|DIR"
                    )
                )
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

parseSubgraphTargets :: Parser [Node]
parseSubgraphTargets = some $ argument
    readNode
    (metavar "NODES" <> help "Nodes (files or #tags) to process")

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

