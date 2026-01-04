{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module MdGraph.Parse.Pandoc
  ( sieveLinks,
    PandocResult (..),
  )
where

import Control.Applicative
import Data.Functor
import Data.HashSet as S
import Data.List as L
import Data.Maybe
  ( catMaybes,
    fromMaybe,
  )
import Data.Text as T
import qualified Data.Text as Text
import Debug.Trace (trace)
import MdGraph.Node
import MdGraph.Parse.Types
import MdGraph.Util (trace')
import qualified Network.URI as URI
  ( unEscapeString,
  )
import System.FilePath (normalise)
import Text.Pandoc (PandocMonad)
import Text.Pandoc.Builder
  ( MetaValue
      ( MetaInlines,
        MetaList
      ),
    ToMetaValue (toMetaValue),
    lookupMeta,
  )
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition as Pandoc
  ( Block (..),
    Inline (Image, Str),
    Meta (Meta),
    MetaValue,
    Pandoc (Pandoc),
  )
import qualified Text.Pandoc.Definition as Pandoc
  ( Inline (Link),
  )
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Options
import Text.Pandoc.Parsing (ToSources)
import Text.Pandoc.Readers (readMarkdown, readMediaWiki, readVimwiki)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (query)
import Prelude as P

data PandocResult = PandocResult
  { tags :: HashSet Tag,
    links :: HashSet Link
  }
  deriving (Eq, Show)

type PandocReader = forall m a. (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc

getFormatReader :: DocumentFormat -> PandocReader
getFormatReader Markdown = readMarkdown
getFormatReader MediaWiki = readMediaWiki

singleTag tag = PandocResult {tags = S.singleton tag, links = S.empty}

singleLink link = PandocResult {tags = S.empty, links = S.singleton link}

instance Semigroup PandocResult where
  (<>) PandocResult {tags = tagsA, links = linksA} PandocResult {tags = tagsB, links = linksB} =
    PandocResult (tagsA <> tagsB) (linksA <> linksB)

instance Monoid PandocResult where
  mempty = PandocResult mempty mempty

sieveLinks :: Text -> Either PandocError PandocResult
sieveLinks content = do
  let markdownReaderOptions = def {readerExtensions = extensionsFromList [Ext_yaml_metadata_block]}
  acc <- queryMarkdown <$> (runPure . readMarkdown markdownReaderOptions $ content)
  return $ PandocResult acc.qaTags acc.qaLinks

-- mdLinks <- extractMarkdownLinks content
-- vwLinks <- extractVimWikiLinks content
-- tags <- extractTags content
-- return $ PandocResult tags (mdLinks <> vwLinks)

-- TODO: does pandoc URI %20 escape markdown links?
-- "title" is the hint text, not useful.
extractUrl :: Inline -> HashSet Link
extractUrl (Pandoc.Link _ label (path, title)) =
  let cleanedPath = (normalise . URI.unEscapeString . T.unpack $ ignoreAnchors path)
      stringifiedLinkLabel = stringify label
   in S.singleton $ Link cleanedPath stringifiedLinkLabel
extractUrl (Image _ _ (path, title)) =
  S.singleton $ Link (T.unpack path) title
extractUrl _ = S.empty

-- Potentially problematic...Some note apps support linking to sections like "foo.md#header-1", the intent of this is to skip that. If a hash is genuinely in the filename though, this will mangle it.
ignoreAnchors :: Text -> Text
ignoreAnchors = T.takeWhile (/= '#')

extractLinks :: PandocReader -> Text -> Either PandocError (HashSet Link)
extractLinks reader text = query extractUrl <$> (runPure . reader def $ text)

extractMarkdownLinks :: Text -> Either PandocError (HashSet Link)
extractMarkdownLinks t = query extractUrl <$> (runPure . readMarkdown def $ t)

extractVimWikiLinks :: Text -> Either PandocError (HashSet Link)
extractVimWikiLinks t = query extractUrl <$> (runPure . readVimwiki def $ t)

extractTags content = liftA2 S.union metadataTags inlineHashtags
  where
    inlineHashtags =
      query extractHashTag <$> (runPure . readVimwiki def $ content)
    metadataTags =
      extractMetadataTags
        <$> (runPure . readMarkdown markdownReaderOptions $ content)
    markdownReaderOptions :: ReaderOptions
    markdownReaderOptions =
      def {readerExtensions = extensionsFromList [Ext_yaml_metadata_block]}

data QueryAcc = QueryAcc
  { qaTags :: HashSet Tag,
    qaLinks :: HashSet Link
  }
  deriving (Show)

instance Semigroup QueryAcc where
  (<>) qa1 qa2 = QueryAcc (qa1.qaTags <> qa2.qaTags) (qa1.qaLinks <> qa2.qaLinks)

instance Monoid QueryAcc where
  mempty = QueryAcc S.empty S.empty

queryMarkdown :: Pandoc -> QueryAcc
queryMarkdown pandoc@(Pandoc meta _) =
  let maybeMetaValues = lookupMeta "tags" meta
      maybeMetaTags = query extractMetaTags <$> maybeMetaValues
      metaTags = fromMaybe mempty maybeMetaTags
      inlineTagsAndLinks = query extractInline pandoc
   in metaTags <> inlineTagsAndLinks
  where
    extractMetaTags :: MetaValue -> QueryAcc
    extractMetaTags (MetaInlines lines) =
      let cleanLine = T.takeWhile isValidTagChar . stringify
          cleanedLines = P.filter (not . Text.null) $ cleanLine <$> lines
          tagSet = S.fromList $ Tag <$> cleanedLines
       in mempty {qaTags = tagSet}
    extractTagsFromMeta _ = mempty

extractInline :: Inline -> QueryAcc
extractInline link@(Pandoc.Link {}) = mempty {qaLinks = extractUrl link}
extractInline image@(Pandoc.Image {}) = mempty {qaLinks = extractUrl image}
extractInline (Str str)
  | "#" `Text.isPrefixOf` str = mempty {qaTags = S.singleton . Tag . Text.tail $ str}
  | otherwise = mempty
extractInline _ = mempty

queryVimWiki :: Pandoc -> QueryAcc
queryVimWiki pandoc@(Pandoc {}) = query extractInline pandoc

-- Pandoc splits Str on whitespace; they are whitespace-less
extractHashTag :: Inline -> HashSet Tag
extractHashTag (Str tag) = case T.uncons tag of
  Just ('#', tagText) ->
    if T.all isValidTagChar tagText && tagText /= T.empty
      then S.singleton $ Tag tagText
      else S.empty
  Just _ -> S.empty
  Nothing -> S.empty
extractHashTag _ = S.empty

validTagChars =
  S.fromList $ P.concat [['a' .. 'z'], ['A' .. 'Z'], ['0' .. '9'], ['-', '_']]

isValidTagChar = flip S.member validTagChars

parseMetadataTag :: Text -> Tag
parseMetadataTag text = Tag actualTagText
  where
    actualTagText = T.takeWhile isValidTagChar text

extractMetadataTags :: Pandoc -> HashSet Tag
extractMetadataTags (Pandoc meta _) =
  maybe S.empty S.fromList $ actualExtract <$> tags
  where
    tags = lookupMeta "tags" meta
    actualExtract :: MetaValue -> [Tag]
    actualExtract (MetaList values) = values >>= actualExtract
    actualExtract (MetaInlines lines) =
      parseMetadataTag . stringify <$> lines
    actualExtract _ = []
