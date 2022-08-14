module Test.Hspec.Newtype where

import Data.Text (Text)
import qualified Data.Text as T

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
