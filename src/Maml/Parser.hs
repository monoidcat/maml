module Maml.Parser ( lineComment
                   , blockComment
                   , scn
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

import           Control.Monad
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

scn :: Parser ()
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space (void p) lineComment blockComment
  where
    p :: Parser [ Char ]
    p = some $ choice [char ' ', tab]

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
pVarId = label "Variable Name" . lexeme $ pName lowerChar

pTypeId :: Parser Name
pTypeId = label "Type Name" . lexeme $ pName upperChar

pProgId :: Parser [ Name ]
pProgId = label "Program Id" . lexeme $ progId `sepBy1` char '.'
  where
    progId :: Parser Name
    progId = pName upperChar

pProgram :: Parser Program
pProgram = Program <$> progName <*> decl
  where
    progName :: Parser [ Name ]
    progName = L.nonIndented scn (char '@' *> pProgId)

    decl :: Parser [ Decl ]
    decl = L.nonIndented scn (many pDecl)

pDecl :: Parser Decl
pDecl = choice [pVarDecl]

pVarDecl :: Parser Decl
pVarDecl = Var <$> pVarId <* symbol ":" <*> pTypeExpr <?> "Variable Declaration"

pTypeExpr :: Parser TypeExpr
pTypeExpr = makeExprParser term table <?> "Type Expression"
  where
    term :: Parser TypeExpr
    term = choice [TypeId <$> pTypeId] <?> "Type Term"

    table = [[]]
