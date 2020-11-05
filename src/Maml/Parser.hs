module Maml.Parser ( module Maml.Parser
                   , module Parser
                   , parse
                   , parseTest -- TODO: Remove
                   ) where

import           Control.Monad.Combinators.Expr

import           Maml.Parser.Literal            as Parser
import           Maml.Parser.Types              as Parser
import           Maml.Types                     as Maml

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

pProgram :: Parser Prog
pProgram = Program <$> progName <*> decl <* eof
  where
    progName :: Parser [ Name ]
    progName = L.nonIndented sc (char '@' *> pProgId)

    decl :: Parser [ Decl ]
    decl = L.nonIndented sc (many pDef)

pDef :: Parser Decl
pDef = choice [try pDefData, pDefVar]

pDefVar :: Parser Decl
pDefVar = label "variable definition" p
  where
    p :: Parser Decl
    p = DefVar <$> pVarId <* symbol ":" <*> pTypeExpr

pDefData :: Parser Decl
pDefData = DefData <$> (keyword "data" *> pTypeId)
  <*> (symbol ":" *> curly (many pDefVar))

pTypeExpr :: Parser TExpr
pTypeExpr = makeExprParser term ops
  where
    term :: Parser TExpr
    term = choice [try (parens pTypeExpr), pBind, typeName]

    typeName :: Parser TExpr
    typeName = Type <$> pTypeId <*> many (curly pTypeCons)

    ops :: [ [ Operator Parser TExpr ] ]
    ops = [[binary "*" Prod], [binary "+" Sum], [binary "->" Arrow]]

pBind :: Parser TExpr
pBind = parens $ Bind <$> pVarId <* symbol ":" <*> pTypeExpr

cons :: Name -> (CExpr -> TCons) -> Parser TCons
cons name f = f <$> (symbol name *> pExpr)

pTypeCons :: Parser TCons
pTypeCons = choice
  [ cons "=" Eq
  , try $ cons "<=" Lteq
  , cons "<" Lt
  , try $ cons ">=" Gteq
  , cons ">" Gt]

pExpr :: Parser CExpr
pExpr = makeExprParser term ops
  where
    term :: Parser CExpr
    term = choice [parens pExpr, it, var, lit]

    it :: Parser CExpr
    it = It <$> (char '_' *> pVarId)

    lit :: Parser CExpr
    lit = Lit <$> pLit

    var :: Parser CExpr
    var = Var <$> pVarId

    ops :: [ [ Operator Parser CExpr ] ]
    ops = [ [prefix "-" Neg]
          , [binary "^" Pow]
          , [binary "*" Mul, binary "/" Div]
          , [binary "+" Add, binary "-" Sub]]
