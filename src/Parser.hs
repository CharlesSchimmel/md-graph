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

simpleWikiLink :: Parser Link
simpleWikiLink = do
    openingDoubleBrackets
    innerText <- many $ satisfy (\x -> x /= '|' && x /= ']' && x /= '[')
    let link = T.pack innerText
    closingDoubleBrackets
    return $ Link link link

wikiLinkWithTitle :: Parser Link
wikiLinkWithTitle = do
    openingDoubleBrackets
    linkContent <- many $ satisfy (\t -> t /= '|' && t /= ']' && t /= '[')
    string "|"
    linkTitle <- many $ satisfy (\t -> t /= ']' && t /= '[')
    closingDoubleBrackets
    return $ Link (T.pack linkContent) (T.pack linkTitle)

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

markdownLink :: Parser Link
markdownLink = do
    M.char '['
    titleContent <- many
        $ satisfy (\t -> t /= ']' && t /= '(' && t /= ')' && t /= '[')
    M.char ']'
    M.char '('
    linkContent <- many
        $ satisfy (\t -> t /= ']' && t /= '(' && t /= ')' && t /= '[')
    M.char ')'
    return $ Link (T.pack linkContent) (T.pack titleContent)

