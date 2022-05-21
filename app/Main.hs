module Main where

import Data.Text.IO hiding (putStrLn)
import Newtype.Parser (pProgram)
import qualified Newtype.Parser ()
import Prettyprinter (pretty)
import System.Environment
import System.Exit
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Text.Megaparsec ()
import Prelude hiding (readFile)

main :: IO ()
main =
  getArgs >>= parseArgs >>= putStrLn

parseArgs :: [[Char]] -> IO String
parseArgs ["-h"] = usage >> exitSuccess
parseArgs ["-v"] = version >> exitSuccess
parseArgs [] = usage >> exitSuccess
parseArgs fs = concat `fmap` mapM compileFile fs

compileFile :: String -> IO String
compileFile path =
  do
    source <- readFile path
    return (either errorBundlePretty (show . pretty) (parse pProgram path source))

usage :: IO ()
usage = putStrLn "Usage: nt [-vh] <file> ..."

version :: IO ()
version = putStrLn "Newtype compiler 0.1"
