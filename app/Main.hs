module Main where

import Data.Text.IO (readFile)
import Newtype.Compiler (compile)
import Prettyprinter (pretty)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Text.Megaparsec (errorBundlePretty)
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
    sourceCode <- readFile path
    return (either errorBundlePretty (show . pretty) (compile path sourceCode))

usage :: IO ()
usage = putStrLn "Usage: nt [-vh] <file> ..."

version :: IO ()
version = putStrLn "Newtype compiler 0.1"
