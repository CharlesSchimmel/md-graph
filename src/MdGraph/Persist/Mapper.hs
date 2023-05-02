{-# LANGUAGE FlexibleInstances #-}
module MdGraph.Persist.Mapper where

import qualified Data.Text                     as T
import           Data.Traversable               ( for )
import qualified MdGraph.File.Internal         as Dto
import qualified MdGraph.Node                  as Node
import           MdGraph.Parse                  ( ParseResult(..) )
import           MdGraph.Persist.Schema

class FromFile a where
  fromFile :: Dto.File -> a

instance FromFile Document where
    fromFile Dto.File { relativePath, modificationTime } =
        Document (Dto.unRelativePath relativePath) modificationTime

instance FromFile TempDocument where
    fromFile Dto.File { relativePath, modificationTime } =
        TempDocument (Dto.unRelativePath relativePath) modificationTime

class FromTempDocument a where
  fromTempDocument :: TempDocument -> a

instance FromTempDocument Document where
    fromTempDocument TempDocument { tempDocumentPath, tempDocumentModifiedAt }
        = Document tempDocumentPath tempDocumentModifiedAt

class FromParseResult a where
  fromParseResult :: Key Document -> ParseResult -> a

instance FromParseResult [Edge] where
    fromParseResult docKey ParseResult { links } =
        map (fromNodeLink docKey) links

instance FromParseResult [Tag] where
    fromParseResult docKey ParseResult { tags } =
        map (fromNodeTag docKey) $ tags

fromNodeTag :: Key Document -> Node.Tag -> Tag
fromNodeTag docKey Node.Tag { tagLabel } =
    Tag { tagName = T.unpack tagLabel, tagFile = docKey }

fromNodeLink :: Key Document -> Node.Link -> Edge
fromNodeLink docKey Node.Link { linkPath, linkText } = Edge
    { edgeTail  = docKey
    , edgeHead  = linkPath
    , edgeLabel = T.unpack linkText
    }
