module Maml.Parser.Types ( module Maml.Parser.Types
                         ) where

import           Control.Monad.Combinators.Expr

import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Void                      (Void)

import           Maml.Types

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void Text

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "{#" "#}"

sc :: Parser ()
sc = L.space space1 empty blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser Text
keyword k = string k <* space1

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

pName :: Parser Char -> Parser Name
pName p = T.pack <$> some p <> many charset <> many suffix
  where
    charset :: Parser Char
    charset = choice [alphaNumChar, char '_']

    suffix :: Parser Char
    suffix = choice [char '\'', char '?', char '!']

pVarId :: Parser Name
pVarId = label "Variable Name" p
  where
    p :: Parser Name
    p = lexeme (pName lowerChar)

pTypeId :: Parser Name
pTypeId = label "Type Name" p
  where
    p :: Parser Name
    p = lexeme (pName upperChar)

pProgId :: Parser [ Name ]
pProgId = label "Program Id" p
  where
    p :: Parser [ Name ]
    p = lexeme (progId `sepBy1` char '.')

    progId :: Parser Name
    progId = pName upperChar

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)
