module MdGraph.Parse.Pandoc
    ( sieveLinks
    , PandocResult(..)
    ) where


import           Control.Applicative
import           Data.HashSet                  as S
import           Data.List                     as L
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Text                     as T
import           Debug.Trace                    ( trace )
import           MdGraph.Node
import           MdGraph.Util                   ( trace' )
import qualified Network.URI                   as URI
                                                ( unEscapeString )
import           Prelude                       as P
import           System.FilePath                ( normalise )
import           Text.Pandoc.Builder            ( MetaValue
                                                    ( MetaInlines
                                                    , MetaList
                                                    )
                                                , ToMetaValue(toMetaValue)
                                                , lookupMeta
                                                )
import           Text.Pandoc.Class              ( runPure )
import           Text.Pandoc.Definition        as Pandoc
                                                ( Block(..)
                                                , Inline(Image, Str)
                                                , Meta(Meta)
                                                , MetaValue
                                                , Pandoc(Pandoc)
                                                )
import qualified Text.Pandoc.Definition        as Pandoc
                                                ( Inline(Link) )
import           Text.Pandoc.Error              ( PandocError )
import           Text.Pandoc.Options
import           Text.Pandoc.Readers            ( readMarkdown
                                                , readVimwiki
                                                )
import           Text.Pandoc.Walk               ( query )

data PandocResult = PandocResult
    { tags  :: HashSet Tag
    , links :: HashSet Link
    }
    deriving (Eq, Show)

singleTag tag = PandocResult { tags = S.singleton tag, links = S.empty }
singleLink link = PandocResult { tags = S.empty, links = S.singleton link }

instance Semigroup PandocResult where
    (<>) PandocResult { tags = tagsA, links = linksA } PandocResult { tags = tagsB, links = linksB }
        = PandocResult (tagsA <> tagsB) (linksA <> linksB)

instance Monoid PandocResult where
    mempty = PandocResult mempty mempty

sieveLinks :: Text -> Either PandocError PandocResult
sieveLinks content = do
    mdLinks <- extractMarkdownLinks content
    vwLinks <- extractVimWikiLinks content
    tags    <- extractTags content
    return $ PandocResult tags (mdLinks <> vwLinks)

-- TODO: does pandoc URI %20 escape markdown links?
extractUrl :: Inline -> HashSet Link
extractUrl (Pandoc.Link _ _ (path, title)) = S.singleton $ Link
    (normalise . URI.unEscapeString . T.unpack $ ignoreAnchors path)
    title
-- if you've got anchors in your image URLs what are you doing
extractUrl (Image _ _ (path, title)) =
    S.singleton $ Link (T.unpack $ ignoreAnchors path) title
extractUrl _ = S.empty

ignoreAnchors :: Text -> Text
ignoreAnchors = T.takeWhile (/= '#')

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
        def { readerExtensions = extensionsFromList [Ext_yaml_metadata_block] }

-- Pandoc splits Str on whitespace; they are whitespace-less
extractHashTag :: Inline -> HashSet Tag
extractHashTag (Str tag) = case T.uncons tag of
    Just ('#', tagText) ->
        if T.all isValidTagChar tagText && tagText /= T.empty
            then S.singleton $ Tag tagText
            else S.empty
    Just _  -> S.empty
    Nothing -> S.empty
extractHashTag _ = S.empty

validTagChars =
    S.fromList $ P.concat [['a' .. 'z'], ['A' .. 'Z'], ['0' .. '9'], ['-', '_']]
isValidTagChar = flip S.member validTagChars

parseMetadataTag :: Text -> Tag
parseMetadataTag text = Tag actualTagText
    where actualTagText = T.takeWhile isValidTagChar text

extractMetadataTags :: Pandoc -> HashSet Tag
extractMetadataTags (Pandoc meta _) =
    maybe S.empty S.fromList $ actualExtract <$> tags
  where
    tags = lookupMeta "tags" meta
    actualExtract :: MetaValue -> [Tag]
    actualExtract (MetaList values) = values >>= actualExtract
    actualExtract (MetaInlines lines) =
        parseMetadataTag <$> catMaybes (unInlines <$> lines)
    actualExtract _ = []
    unInlines (Str text) = Just text
    unInlines _          = Nothing
