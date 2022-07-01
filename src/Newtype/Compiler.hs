module Newtype.Compiler where

import Control.Monad (join)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Newtype.Parser (pProgram)
import Newtype.Syntax (Ident (..), Program (..), Statement (..), mkIdent)
import Text.Megaparsec (ParseErrorBundle, parse)

-- Loose outline of compilation:
-- read file -> parse -> post processing -> pretty printing

type CompilerError = ParseErrorBundle Text Void

compile :: String -> Text -> Either CompilerError Program
compile filename sourceCode =
  fmap addImplicitExport (parse pProgram filename sourceCode)

addImplicitExport :: Program -> Program
addImplicitExport (Program statements) =
  Program statements'
  where
    statements' :: [Statement]
    statements' = exports statements : statements

    exports :: [Statement] -> Statement
    exports s = ExportStatement (mapMaybe identifier s)

    identifier :: Statement -> Maybe Ident
    identifier ImportDeclaration {} = Nothing
    identifier ExportStatement {} = Nothing
    identifier TypeDefinition {name} = Just (Ident name)
    identifier InterfaceDefinition {name} = Just (Ident name)
