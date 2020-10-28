module Maml.Parser ( lineComment
                   , blockComment
                   , sc
                   , lexeme
                   , symbol
                   , keyword
                   , parens
                   , curly
                   , brackets
                   , nat
                   , int
                   , real
                   , pLit
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

nat :: Parser Integer
nat = label "Natural Number" (lexeme L.decimal)

int :: Parser Integer
int = label "Integer" (L.signed sc nat)

real :: Parser Double
real = label "Real Number" (L.signed sc $ lexeme L.float)

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

pProgram :: Parser Program
pProgram = Program <$> progName <*> decl <* eof
  where
    progName :: Parser [ Name ]
    progName = L.nonIndented sc (char '@' *> pProgId)

    decl :: Parser [ Decl ]
    decl = L.nonIndented sc (many pDecl)

pDecl :: Parser Decl

pDecl = choice [try pRndDecl, try pDataDecl, pSetDecl, pVarDecl]

pVarDecl :: Parser Decl
pVarDecl = label "Variable Declaration" p
  where
    p :: Parser Decl
    p = VarDecl <$> pVarId <* symbol ":" <*> pTypeExpr <*> optional
      (symbol ":=" *> pTypeExpr)

pSetDecl :: Parser Decl
pSetDecl = label "Set Declaration" p
  where
    p :: Parser Decl
    p = SetDecl <$> pTypeId <* symbol ":=" <*> pTypeExpr

pRndDecl :: Parser Decl
pRndDecl = label "Random Variable Declaration" p
  where
    p :: Parser Decl
    p = RndDecl <$> pTypeId <* symbol "~" <*> pTypeExpr

pDataDecl :: Parser Decl
pDataDecl = Data <$> (keyword "data" *> pTypeId)
  <*> (symbol ":" *> curly (many pVarDecl))

pTypeExpr :: Parser Expr
pTypeExpr = label "Type Expression" (makeExprParser term table)
  where
    term :: Parser Expr
    term = label "Type Term" (choice [lit, typeId])

    typeId :: Parser Expr
    typeId = TypeId <$> pTypeId

    lit :: Parser Expr
    lit = Lit <$> pLit

    table = [[]]

pLit :: Parser Literal
pLit = choice [try $ R <$> real, N <$> nat, Z <$> int]
