module Language.HaSM.Syntax
( parse
, module Language.HaSM.Syntax.AST ) where

import qualified Text.Megaparsec as P
import Language.HaSM.Syntax.AST
import Language.HaSM.Syntax.Rules.Program (program)
import Data.Void (Void)

parse :: String -> String -> Either (P.ParseErrorBundle String Void) [Instruction]
parse = P.parse program
