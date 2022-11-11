-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Test.Hspec.Newtype where

import Data.Text (Text)
import Data.Text qualified as T
import Newtype.Compiler qualified
import Newtype.Parser (Parser, ParserResult, runNewTypeParser)
import Prettyprinter (Pretty, pretty)
import Test.Hspec hiding (Expectation, expectationFailure, shouldBe)
import Test.Hspec.Expectations.Pretty (
  Expectation,
  expectationFailure,
  shouldBe,
 )
import Text.Megaparsec (eof)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Heredoc (str)
import qualified Language.Haskell.TH.Quote


ts :: Language.Haskell.TH.Quote.QuasiQuoter
ts = str

nt :: Language.Haskell.TH.Quote.QuasiQuoter
nt = str

{-
 Example input:
 "
 |   | a | b |
 | x | 1 | 0 |
 | y |   | 1 |
 "
 Example output:
 [("x", [("a", "1"), ("b", "0")]), ("y", [("a", ""), ("b", "1")])]
 -}
parseAdjMatrix :: Char -> [Text] -> [(Text, [(Text, Text)])]
parseAdjMatrix colSep textLines =
  do
    let headers = tail . head $ tbl
    do
      row <- tail tbl
      let colHeader = head row
      let rowWithHeaders = zip headers (tail row)
      return (colHeader, rowWithHeaders)
  where
    tbl =
      do
        l <- textLines
        return $ do
          cell <- T.splitOn (T.singleton colSep) (T.dropAround (colSep ==) l)
          return $ T.strip cell

parse :: Parser a -> Text -> ParserResult a
parse parser = runNewTypeParser (parser <* eof) ""

shouldCompile :: (HasCallStack, Show a) => Parser a -> Text -> Text -> Expectation
shouldCompile parser = shouldCompileT parser show

-- shouldCompileDebug parser =
--   shouldCompileT
--     parser
--     (show . pretty . pp "\nTypescript output:" . toTypescript . pp "\nEvaluation output:" . evalProgram . pp "\nParser Output:")

shouldCompileT :: (HasCallStack) => Parser a -> (a -> String) -> Text -> Text -> Expectation
shouldCompileT parser f src expected =
  case parse parser src of
    Left e -> expectationFailure . errorBundlePretty $ e
    Right result -> f result `shouldBe` T.unpack expected

shouldCompileTo :: (HasCallStack, Pretty a) => Either Newtype.Compiler.CompilerError a -> Text -> Expectation
shouldCompileTo (Left e) actual =
  expectationFailure $
    "expected: "
      ++ show actual
      ++ "\nbut parsing failed with error:\n"
      ++ errorBundlePretty e
shouldCompileTo (Right actual) expected = (show . pretty) actual `shouldBe` T.unpack expected
