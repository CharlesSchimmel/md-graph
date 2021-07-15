module PParser where

import           Data.HashSet                  as S
import           Data.Text                     as T
import           Text.Pandoc.Class              ( runPure )
import           Text.Pandoc.Definition         ( Inline(..) )
import           Text.Pandoc.Error              ( PandocError )
import           Text.Pandoc.Options
import           Text.Pandoc.Readers            ( readMarkdown
                                                , readVimwiki
                                                )
import           Text.Pandoc.Walk               ( query )

sieveLinks :: Text -> Maybe [Text]
sieveLinks content = either (const Nothing) (Just . S.toList) allLinks
  where
    mdLinks  = extractMarkdownLinks content
    vwLinks  = extractVimWikiLinks content
    allLinks = S.union <$> mdLinks <*> vwLinks

extractUrl :: Inline -> HashSet Text
extractUrl (Link  _ _ (u, _)) = S.singleton u
extractUrl (Image _ _ (u, _)) = S.singleton u
extractUrl _                  = S.empty

extractMarkdownLinks :: Text -> Either PandocError (HashSet Text)
extractMarkdownLinks t = query extractUrl <$> (runPure . readMarkdown def $ t)

extractVimWikiLinks :: Text -> Either PandocError (HashSet Text)
extractVimWikiLinks t = query extractUrl <$> (runPure . readVimwiki def $ t)
