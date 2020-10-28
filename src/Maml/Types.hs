module Maml.Types ( Name
                  , Block
                  , Program(..)
                  , Decl(..)
                  , Expr(..)
                  , Data(..)
                  , Literal(..)
                  ) where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

type Name = Text

type Block = Text

data Program = Program [ Name ] [ Decl ]
  deriving stock (Eq, Show)

data Decl
  = VarDecl Name Expr (Maybe Expr)
  | SetDecl Name Expr
  | RndDecl Name Expr
  | Data Name [ Decl ]
  deriving (Eq, Show)

data Data = Type Name Expr
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
