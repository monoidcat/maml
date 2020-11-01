{-# LANGUAGE TemplateHaskell #-}

module Maml.Types ( module Maml.Types
                  ) where

import           Data.Functor.Foldable.TH
import           Data.Text                (Text)

type Name = Text

type Prog = Program Name

type Decl = Def Name

type TExpr = TypeExpr Name

type CExpr = Expr Name

type TCons = TypeCons Name

data Program a = Program [ Name ] [ Def a ]
  deriving stock (Eq, Show)

data Def a
  = DefVar Name (TypeExpr a)
  | DefData Name [ Def a ]
  deriving (Eq, Show)

data TypeExpr a
  = Type a [ TypeCons a ]
  | Arrow (TypeExpr a) (TypeExpr a)
  | Prod (TypeExpr a) (TypeExpr a)
  | Sum (TypeExpr a) (TypeExpr a)
  | Bind Name (TypeExpr a)
  deriving (Eq, Show)

data TypeCons a
  = Eq (Expr a)
  | Gt (Expr a)
  | Lt (Expr a)
  | Gteq (Expr a)
  | Lteq (Expr a)
  deriving (Eq, Show)

data Expr a
  = Var a
  | It a
  | Lit Literal
  | Neg (Expr a)
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)
  deriving (Eq, Show)

data Literal
  = Int Integer
  | Real Double
  | Char Char
  | String String
  deriving (Eq, Show)

makeBaseFunctor ''TypeExpr

makeBaseFunctor ''TypeCons

makeBaseFunctor ''Expr
