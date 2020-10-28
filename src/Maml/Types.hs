module Maml.Types ( Name
                  , Block
                  , Program(..)
                  , Decl(..)
                  , TypeExpr(..)
                  ) where

import           Data.Text (Text)

type Name = Text

type Block = Text

data Program = Program [ Name ] [ Decl ]
  deriving stock (Eq, Show)

data Decl = Var Name TypeExpr
  deriving stock (Eq, Show)

data TypeExpr = TypeId Name
  deriving (Eq, Show)
