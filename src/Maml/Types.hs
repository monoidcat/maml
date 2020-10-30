{-# LANGUAGE TemplateHaskell #-}

module Maml.Types ( module Maml.Types
                  ) where

import           Data.Functor.Foldable.TH
import           Data.Text                (Text)

type Name = Text

data Program = Program [ Name ] [ Def ]
  deriving stock (Eq, Show)

data Def
  = DefVar Name TypeExpr
  | DefData Name [ Def ]
  deriving (Eq, Show)

data TypeExpr
  = Type Name [ TypeCons ]
  | Arrow TypeExpr TypeExpr
  | Prod TypeExpr TypeExpr
  | Sum TypeExpr TypeExpr
  | Bind Name TypeExpr
  deriving (Eq, Show)

data TypeCons = Eq Expr
  deriving (Eq, Show)

data Expr
  = Var Name
  | Lit Literal
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
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
