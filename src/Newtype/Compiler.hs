module Newtype.Compiler where

import Control.Monad (join)
import Control.Monad.Identity
import Control.Monad.State (evalState)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Newtype.Parser (pProgram, runNewTypeParser)
import Newtype.Parser.Tokens (Parser)
import Newtype.Syntax.Newtype
import Text.Megaparsec

-- Loose outline of compilation:
-- read file -> parse -> post processing -> pretty printing

type CompilerError = ParseErrorBundle Text Void

compile :: String -> Text -> Either CompilerError Program
compile filename sourceCode =
  addImplicitExport <$> program
  where
    program = runNewTypeParser pProgram filename sourceCode

-- compile filename sourceCode =
--   fmap addImplicitExport (run pProgram (filename :: String) sourceCode)
--
addImplicitExport :: Program -> Program
addImplicitExport (Program statements) =
  Program statements'
  where
    statements' :: [Statement]
    statements' = exports statements : statements

    exports :: [Statement] -> Statement
    exports s = ExportStatement (mapMaybe identifier s)

    identifier :: Statement -> Maybe Ident
    identifier = \case
      TypeDefinition {name} -> Just (Ident name)
      InterfaceDefinition {name} -> Just (Ident name)
      _ -> Nothing
