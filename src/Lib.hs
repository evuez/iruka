{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( parser
  , buildCards
  , Card(..)
  ) where

import Control.Applicative (empty, (<|>))
import Control.Applicative.Permutations
import Control.Monad (void)
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as A (object)
import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec
  , between
  , eof
  , many
  , manyTill
  , parse
  , sepBy
  , some
  , takeWhile1P
  , try
  , (<?>)
  )
import qualified Text.Megaparsec.Char as C (char, eol, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L (charLiteral, space, symbol)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)

type Parser = Parsec Void String

type ParseError = ParseErrorBundle String Void

type Term = String

type Definition = String

type Tags = [String]

type Image = String

type Color = String

type Link = String

data Card = Card
  { term :: Term
  , definition :: Definition
  , tags :: Tags
  , image :: Maybe Image
  , color :: Maybe Color
  , link :: Maybe Link
  }
  deriving (Show, Eq)

--
-- Lexer
--
spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 empty empty

space :: Parser ()
space = void $ C.char ' '

symbol :: String -> Parser String
symbol = L.symbol (L.space space empty empty)

comma :: Parser String
comma = symbol ","

string :: Parser String
string = takeWhile1P Nothing (not . \c -> c == '\n' || c == '\r')

commaFreeString :: Parser String
commaFreeString =
  takeWhile1P Nothing (not . \c -> c == ',' || c == '\n' || c == '\r')

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser' = parser' `sepBy` comma

--
-- Parser
--
parseTerm :: Parser Term
parseTerm = C.string "term: " *> string <* C.eol

parseTags :: Parser Tags
parseTags = C.string "tags: " *> commaSeparated commaFreeString <* C.eol

parseImage :: Parser Image
parseImage = C.string "image: " *> string <* C.eol

parseColor :: Parser Color
parseColor = C.string "color: " *> string <* C.eol

parseLink :: Parser Link
parseLink = C.string "link: " *> string <* C.eol

parseDefinition :: Parser Definition
parseDefinition =
  d *> C.eol *>
  ((d *> C.string "") <|> (L.charLiteral `manyTill` try (C.eol *> d)))
  where
    d = C.string "---"

parseExtra :: Parser (Tags, Maybe Image, Maybe Color, Maybe Link)
parseExtra =
  runPermutation $
  (,,,) <$> toPermutationWithDefault [] (parseTags <?> "tags") <*>
  toPermutationWithDefault Nothing (Just <$> parseImage <?> "image") <*>
  toPermutationWithDefault Nothing (Just <$> parseColor <?> "color") <*>
  toPermutationWithDefault Nothing (Just <$> parseLink <?> "link")

parseCard :: Parser Card
parseCard = do
  term' <- parseTerm <?> "term"
  (tags', image', color', link') <- parseExtra
  definition' <- parseDefinition <?> "definition"
  pure $ Card term' definition' tags' image' color' link'

parser :: String -> Either ParseError [Card]
parser = parse (between spaceConsumer eof $ many (parseCard <* some C.eol)) ""

buildCards :: String -> Either String [Card]
buildCards s = first errorBundlePretty (parser s)

--
-- Encoder
--
instance ToJSON Card where
  toJSON card =
    A.object
      [ "term" .= term card
      , "definition" .= definition card
      , "tags" .= tags card
      , "image" .= image card
      , "color" .= color card
      , "link" .= link card
      ]
