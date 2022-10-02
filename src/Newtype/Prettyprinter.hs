{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.Prettyprinter where

import Control.Monad
import qualified Data.Data as D
import qualified Data.List as List
import Data.Maybe
import Prettyprinter
import Text.Regex.TDFA

prettyReadonly :: Maybe Bool -> Doc ann
prettyReadonly Nothing = emptyDoc
prettyReadonly (Just False) = "-readonly" <> space
prettyReadonly (Just True) = "readonly" <> space

prettyOptional :: Maybe Bool -> Doc ann
prettyOptional Nothing = emptyDoc
prettyOptional (Just False) = "-?"
prettyOptional (Just True) = "?"

prettyOpList :: Doc ann -> Doc ann
prettyOpList a =
  group $ align $ enclose (flatAlt "( " "(") (flatAlt " )" ")") a

-- Test if a string is a valid Javascript identifier name
isIdentifierName :: String -> Bool
isIdentifierName str = str =~ ("^[a-zA-Z_$][a-zA-Z0-9_$]*$" :: String)
