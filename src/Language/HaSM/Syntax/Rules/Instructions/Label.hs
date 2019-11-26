module Language.HaSM.Syntax.Rules.Instructions.Label where

import Language.HaSM.Syntax.AST
import Language.HaSM.Syntax.Parser

label :: Parser Instruction
label = Label <$> (identifier <* symbol ":")