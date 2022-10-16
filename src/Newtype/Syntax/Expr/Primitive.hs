{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Newtype.Syntax.Expr.Primitive where

import qualified Data.Data as Generics
import Prettyprinter
import qualified Newtype.Syntax.Typescript as TS

data PrimitiveType
  = PrimitiveNever
  | PrimitiveAny
  | PrimitiveUnknown
  | PrimitiveNumber
  | PrimitiveBigInt
  | PrimitiveString
  | PrimitiveBoolean
  | PrimitiveNull
  | PrimitiveUndefined
  | PrimitiveVoid
  | PrimitiveObject
  | PrimitiveSymbol
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty PrimitiveType where
  pretty a = case a of
    PrimitiveNever -> "never"
    PrimitiveAny -> "any"
    PrimitiveUnknown -> "unknown"
    PrimitiveNumber -> "number"
    PrimitiveString -> "string"
    PrimitiveBoolean -> "boolean"
    PrimitiveNull -> "null"
    PrimitiveUndefined -> "undefined"
    PrimitiveVoid -> "void"
    PrimitiveBigInt -> "bigint"
    PrimitiveSymbol -> "symbol"
    PrimitiveObject -> "object"

instance TS.Typescript PrimitiveType TS.PrimitiveType where
  toTypescript a = case a of
    PrimitiveNever -> TS.PrimitiveNever
    PrimitiveAny -> TS.PrimitiveAny
    PrimitiveUnknown -> TS.PrimitiveUnknown
    PrimitiveNumber -> TS.PrimitiveNumber
    PrimitiveString -> TS.PrimitiveString
    PrimitiveBoolean -> TS.PrimitiveBoolean
    PrimitiveNull -> TS.PrimitiveNull
    PrimitiveUndefined -> TS.PrimitiveUndefined
    PrimitiveVoid -> TS.PrimitiveVoid
    PrimitiveBigInt -> TS.PrimitiveBigInt
    PrimitiveSymbol -> TS.PrimitiveSymbol
    PrimitiveObject -> TS.PrimitiveObject
