{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Newtype.Syntax.Ident where

import qualified Data.Generics as Generics
import qualified Newtype.Syntax.Typescript as TS
import Prettyprinter
import Newtype.Syntax.Typescript (Typescript)

newtype Ident = Ident { getIdent :: String }
  deriving (Eq, Ord, Show, Generics.Data, Generics.Typeable)

instance Pretty Ident where
  pretty (Ident s) = pretty s

instance Typescript Ident String where
  toTypescript (Ident x) = x
