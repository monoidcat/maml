{-# LANGUAGE TemplateHaskell #-}

module Maml.Types ( Name
                  , Program(..)
                  , ProgramF(..)
                  , Def(..)
                  , DefF(..)
                  , Expr(..)
                  , ExprF(..)
                  , Literal(..)
                  ) where

import           Data.ByteString          (ByteString)
import           Data.Functor.Foldable.TH
import           Data.Text                (Text)

type Name = Text

data Program = Program [ Name ] [ Def ]
  deriving stock (Eq, Show)

data Def
  = DefVar Name Expr
  | DefData Name [ Def ]
  deriving (Eq, Show)

data Expr
  = TypeId Name
  | VarId Name
  | Lit Literal
  deriving (Eq, Show)

data Literal
  = N Integer
  | Z Integer
  | R Double
  | Char Char
  | String ByteString
  | Text Text
  deriving (Eq, Show)

makeBaseFunctor ''Program

makeBaseFunctor ''Def

makeBaseFunctor ''Expr
