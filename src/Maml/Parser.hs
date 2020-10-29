module Maml.Parser ( module Maml.Parser
                   , module Parser
                   , parse
                   , parseTest -- TODO: Remove
                   ) where

import           Control.Monad.Combinators.Expr

import           Maml.Parser.Literal            as Parser
import           Maml.Parser.Types              as Parser
import           Maml.Types

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

pProgram :: Parser Program
pProgram = Program <$> progName <*> decl <* eof
  where
    progName :: Parser [ Name ]
    progName = L.nonIndented sc (char '@' *> pProgId)

    decl :: Parser [ Def ]
    decl = L.nonIndented sc (many pDef)

pDef :: Parser Def
pDef = choice [try pDefData, pDefVar]

pDefVar :: Parser Def
pDefVar = label "variable definition" p
  where
    p :: Parser Def
    p = DefVar <$> pVarId <* symbol ":" <*> pTypeExpr

pDefData :: Parser Def
pDefData = DefData <$> (keyword "data" *> pTypeId)
  <*> (symbol ":" *> curly (many pDefVar))

pTypeExpr :: Parser TypeExpr
pTypeExpr = label "type expression" p
  where
    p :: Parser TypeExpr
    p = choice [typeName]

    typeName :: Parser TypeExpr
    typeName = Type <$> pTypeId <*> optional (curly pTypeCons)

pTypeCons :: Parser TypeCons
pTypeCons = Eq <$> (symbol "=" *> pExpr)

pExpr :: Parser Expr
pExpr = makeExprParser term ops
  where
    term :: Parser Expr
    term = choice [parens pExpr, var, lit]

    lit :: Parser Expr
    lit = Lit <$> pLit

    var :: Parser Expr
    var = Var <$> pVarId

    ops = [[]]
