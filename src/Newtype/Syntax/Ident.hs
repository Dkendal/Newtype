{-# LANGUAGE DeriveDataTypeable #-}

module Newtype.Syntax.Ident where
import qualified Data.Generics
import Prettyprinter

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Data.Generics.Data, Data.Generics.Typeable)

instance Pretty Ident where
  pretty (Ident s) = pretty s
