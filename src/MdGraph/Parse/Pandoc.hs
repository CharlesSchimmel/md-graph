module MdGraph.Parse.Pandoc
    ( sieveLinks
    ) where

import           MdGraph.Node

import           Control.Applicative
import           Data.HashSet                  as S
import           Data.Text                     as T
import qualified Network.URI                   as URI
                                                ( unEscapeString )
import           Prelude                       as P
import           Text.Pandoc.Class              ( runPure )
import           Text.Pandoc.Definition        as Pandoc
                                                ( Block(..)
                                                , Inline(Image, Str)
                                                )
import qualified Text.Pandoc.Definition        as Pandoc
                                                ( Inline(Link) )
import           Text.Pandoc.Error              ( PandocError )
import           Text.Pandoc.Options
import           Text.Pandoc.Readers            ( readMarkdown
                                                , readVimwiki
                                                )
import           Text.Pandoc.Walk               ( query )

sieveLinks :: Text -> Maybe [Node]
sieveLinks content = either (const Nothing) (Just . S.toList) allLinks
  where
    mdLinks  = extractMarkdownLinks content
    vwLinks  = extractVimWikiLinks content
    tags     = query extractHashTag <$> (runPure . readVimwiki def $ content)
    fUnion   = liftA2 S.union
    allLinks = mdLinks `fUnion` vwLinks `fUnion` tags

extractUrl :: Inline -> HashSet Node
extractUrl (Pandoc.Link _ _ (path, title)) =
    -- pandoc URI escapes (%20) markdown links
    S.singleton . Link . URI.unEscapeString . T.unpack $ ignoreAnchors path
-- if you've got anchors in your image URLs what are you doing
extractUrl (Image _ _ (path, title)) =
    S.singleton . Link . T.unpack $ ignoreAnchors path
extractUrl _ = S.empty

ignoreAnchors :: Text -> Text
ignoreAnchors = T.takeWhile (/= '#')

-- Pandoc splits Str on whitespace; they are whitespace-less
extractHashTag :: Inline -> HashSet Node
extractHashTag (Str tag) = case T.uncons tag of
    Just ('#', tagText) -> if T.all isValidTagChar tagText
        then S.singleton $ Tag tagText
        else S.empty
    Just _  -> S.empty
    Nothing -> S.empty
  where
    validTagChars = S.fromList
        $ P.concat [['a' .. 'z'], ['A' .. 'Z'], ['0' .. '9'], ['-', '_']]
    isValidTagChar = flip S.member validTagChars
extractHashTag _ = S.empty

extractMarkdownLinks :: Text -> Either PandocError (HashSet Node)
extractMarkdownLinks t = query extractUrl <$> (runPure . readMarkdown def $ t)

extractVimWikiLinks :: Text -> Either PandocError (HashSet Node)
extractVimWikiLinks t = query extractUrl <$> (runPure . readVimwiki def $ t)
