module Newtype.Syntax.Internal where

import Data.Data (Data)
import Data.Generics (Typeable)

newtype Ident = Ident {getIdent :: String}
  deriving (Show, Eq, Data, Typeable)

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
  deriving (Show, Eq, Data, Typeable)

data ConditionalType a = ConditionalType
  { lhs :: a
  , rhs :: a
  , then' :: a
  , else' :: a
  }
  deriving (Show, Eq, Data, Typeable, Functor)

data TemplateString a
  = TemplateRaw String
  | TemplateSubstitution a
  deriving (Show, Eq, Data, Typeable, Functor)

data GenericApplication a = GenericApplication {name :: Ident, args :: [a]}
  deriving (Show, Eq, Data, Typeable, Functor)

data Property a
  = DataProperty
      { isReadonly :: Maybe Bool
      , isOptional :: Maybe Bool
      , key :: String
      , value :: a
      }
  | ComputedProperty
      { isReadonly :: Maybe Bool
      , isOptional :: Maybe Bool
      , key :: String
      , value :: a
      }
  | IndexSignature
      { isReadonly :: Maybe Bool
      , key :: String
      , keySource :: a
      , value :: a
      }
  deriving (Show, Eq, Data, Typeable, Functor)

data ListValue a
  = ListValue {label :: Maybe String, value :: a}
  | ListRest {label :: Maybe String, value :: a}
  deriving (Show, Eq, Data, Typeable, Functor)

data Literal a
  = LString String
  | LNumberInteger Integer
  | LNumberDouble Double
  | LFunction
      { typeParams :: Maybe [String]
      , params :: [FnFormalParam a]
      , rest :: Maybe (FnFormalParam a)
      , returnType :: a
      }
  | LBoolean Bool
  | LObject [Property a]
  deriving (Show, Eq, Data, Typeable, Functor)

data TypeParam a = TypeParam
  { name :: String
  , defaultValue :: Maybe a
  , constraint :: Maybe a
  }
  deriving (Show, Eq, Data, Typeable, Functor)

data MappedType a = MappedType
  { value :: a
  , key :: String
  , source :: a
  , asExpr :: Maybe a
  , isReadonly :: Maybe Bool
  , isOptional :: Maybe Bool
  }
  deriving (Show, Eq, Data, Typeable, Functor)

type FnFormalParam a = (String, a)

data ImportClause
  = ImportClauseDefault String
  | ImportClauseNS String
  | ImportClauseNamed [ImportSpecifier]
  | ImportClauseDefaultAndNS
      { defaultBinding :: String
      , namespaceBinding :: String
      }
  | ImportClauseDefaultAndNamed
      { defaultBinding :: String
      , namedBindings :: [ImportSpecifier]
      }
  deriving (Show, Eq, Data, Typeable)

data ImportSpecifier
  = ImportedBinding String
  | ImportedAlias {from :: String, to :: String}
  deriving (Show, Eq, Data, Typeable)
