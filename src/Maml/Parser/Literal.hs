module Maml.Parser.Literal ( pLit
                           , intLit
                           , realLit
                           , charLit
                           , stringLit
                           ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Maml.Parser.Types
import           Maml.Types

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

pLit :: Parser Literal
pLit = lexeme $ choice
  [ try (Real <$> realLit)
  , Int <$> intLit
  , Char <$> charLit
  , String <$> stringLit]

intLit :: Parser Integer
intLit = label "Integer" (L.signed sc L.decimal)

realLit :: Parser Double
realLit = label "Real Number" (L.signed sc $ lexeme L.float)

charLit :: Parser Char
charLit = label "Character" p
  where
    p :: Parser Char
    p = between (char '\'') (char '\'') L.charLiteral

stringLit :: Parser Text
stringLit = label "String" p
  where
    p :: Parser Text
    p = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
