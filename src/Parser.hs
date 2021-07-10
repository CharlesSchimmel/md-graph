{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseLinks
    ) where

import           Link

import           Control.Monad                  ( mfilter )
import           Data.Functor
import           Data.Functor.Identity          ( Identity )
import           Data.Maybe
import           Data.Text                     as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char          as M
import           Text.Megaparsec.Char.Lexer    as L

type Parser = ParsecT Void Text Identity

wikiLinkBrackets innerParser =
    between openingDoubleBrackets closingDoubleBrackets
        $ mfilter nonBracketContent innerParser
    where nonBracketContent x = x /= '[' && x /= ']'

wikiLinkSpecialChars :: String
wikiLinkSpecialChars = "[|]"

simpleWikiLink :: Parser Link
simpleWikiLink = do
    openingDoubleBrackets
    (linkContent, anchor) <- wikiLinkContent
    closingDoubleBrackets
    return $ Link linkContent linkContent anchor

wikiLinkWithTitle :: Parser Link
wikiLinkWithTitle = do
    openingDoubleBrackets
    (linkContent, anchor) <- wikiLinkContent
    M.char '|'
    linkTitle <- many $ satisfy (`notElem` ("[]" :: String))
    closingDoubleBrackets
    return $ Link linkContent (T.pack linkTitle) anchor

wikiLinkContent :: Parser (Text, Maybe Text)
wikiLinkContent = do
    linkContent <- many $ satisfy (`notElem` ("[|]#" :: String))
    anchor      <- optional $ do
        _ <- M.char '#'
        many $ satisfy (`notElem` ("[|]" :: String))
    return (T.pack linkContent, T.pack <$> anchor)

parseLinks :: Text -> Maybe [Link]
parseLinks = parseMaybe (catMaybes <$> many document)

linkTest = parseTest (catMaybes <$> many document)

document = (Just <$> try links) <|> (anyChar $> Nothing)

links = try wikiLinkWithTitle <|> try simpleWikiLink <|> try markdownLink

openingDoubleBrackets :: Parser (Tokens Text)
openingDoubleBrackets = string "[["
closingDoubleBrackets :: Parser (Tokens Text)
closingDoubleBrackets = string "]]"

anyChar :: Parser Char
anyChar = satisfy $ const True

markdownLinkContent :: Parser (Text, Maybe Text)
markdownLinkContent = do
    link   <- many $ satisfy (`notElem` ("[]()#" :: String))
    anchor <- optional $ do
        _ <- M.char '#'
        many $ satisfy (`notElem` ("[]()" :: String))
    return (T.pack link, T.pack <$> anchor)

markdownLink :: Parser Link
markdownLink = do
    M.char '['
    titleContent <- many
        $ satisfy (\t -> t /= ']' && t /= '(' && t /= ')' && t /= '[')
    M.char ']'
    M.char '('
    (linkContent, anchor) <- markdownLinkContent
    M.char ')'
    return $ Link linkContent (T.pack titleContent) anchor


