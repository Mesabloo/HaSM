module Language.HaSM.Syntax.Rules.Instructions.Add where

import Language.HaSM.Syntax.AST
import Language.HaSM.Syntax.Parser
import Language.HaSM.Syntax.Rules.Expression

add :: Parser Instruction
add = (Add <$ reserved "add") <*> exprVal <*> (comma *> exprAddr)