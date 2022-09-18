module Debug where

import Data.Text
import Debug.Trace
import Newtype.Syntax
import Newtype.Syntax.Conditionals
import Prettyprinter
  ( LayoutOptions (..),
    PageWidth (..),
    layoutPretty,
    pretty,
  )
import Prettyprinter.Render.String (renderString)
import Text.Nicify

pp label a = trace (label ++ ": " ++ (nicify . show) a) a

ppp label ast = trace (label ++ ": " ++ prettyPrint ast) ast

prettyPrint ast =
  renderString (layoutPretty (LayoutOptions (AvailablePerLine 1 1)) (pretty ast))
