module Debug where

import Data.Text
import Debug.Trace qualified as Trace
import GHC.Stack (HasCallStack)
import Prettyprinter (
  LayoutOptions (..),
  PageWidth (..),
  layoutPretty,
  pretty,
 )
import Prettyprinter.Render.String (renderString)
import Text.Nicify

log :: (HasCallStack, Show a) => a -> a
log a = Trace.traceStack ("\n" ++ (nicify . show $ a)) a

log' :: (HasCallStack, Show a) => a -> b -> b
log' a = Trace.traceStack ("\n" ++ (nicify . show $ a))

pp :: (Show a) => [Char] -> a -> a
pp label a = Trace.trace (label ++ ": " ++ (nicify . show) a) a

ppp label ast = Trace.trace (label ++ ": " ++ prettyPrint ast) ast

prettyPrint ast =
  renderString (layoutPretty (LayoutOptions (AvailablePerLine 1 1)) (pretty ast))
