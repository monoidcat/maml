module Maml.Parser ( lineComment
                   , blockComment
                   , sc
                   , lexeme
                   , symbol
                   , keyword
                   , parens
                   , curly
                   , brackets
                   , pName
                   , pVarId
                   , pTypeId
                   , pProgId
                   , pProgram
                   , pDecl
                   , pTypeExpr
                   , parse
                   , parseTest -- TODO: Remove
                   ) where

import           Control.Monad.Combinators.Expr

import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Void

import           Maml.Types

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = empty

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "{#" "#}"

sc :: Parser ()
sc = L.space space1 lineComment blockComment

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
pName p = T.pack <$> some p <> many charset
  where
    charset :: Parser Char
    charset = choice [alphaNumChar, char '_']

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

pProgram :: Parser Program
pProgram = Program <$> progName <*> decl <* eof
  where
    progName :: Parser [ Name ]
    progName = L.nonIndented sc (char '@' *> pProgId)

    decl :: Parser [ Decl ]
    decl = L.nonIndented sc (many pDecl)

pDecl :: Parser Decl
pDecl = choice [pVarDecl]

pVarDecl :: Parser Decl
pVarDecl = label "Variable Declaration" p
  where
    p :: Parser Decl
    p = Var <$> pVarId <* symbol ":" <*> pTypeExpr

pTypeExpr :: Parser TypeExpr
pTypeExpr = label "Type Expression" (makeExprParser term table)
  where
    term :: Parser TypeExpr
    term = label "Type Term" (choice [typeId])

    typeId :: Parser TypeExpr
    typeId = TypeId <$> pTypeId

    table = [[]]
