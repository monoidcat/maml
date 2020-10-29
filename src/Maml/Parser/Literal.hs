module Maml.Parser.Literal ( pLit
                           , natLit
                           , intLit
                           , realLit
                           , charLit
                           , textLit
                           ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Maml.Parser.Types
import           Maml.Types

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

pLit :: Parser Literal
pLit = choice [ try (R <$> realLit)
              , N <$> natLit
              , Z <$> intLit
              , Char <$> charLit
              , Text <$> textLit]

natLit :: Parser Integer
natLit = label "Natural Number" (lexeme L.decimal)

intLit :: Parser Integer
intLit = label "Integer" (L.signed sc natLit)

realLit :: Parser Double
realLit = label "Real Number" (L.signed sc $ lexeme L.float)

charLit :: Parser Char
charLit = label "Character" p
  where
    p :: Parser Char
    p = between (char '\'') (char '\'') L.charLiteral

textLit :: Parser Text
textLit = label "Text" p
  where
    p :: Parser Text
    p = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
