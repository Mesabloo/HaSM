module Language.HaSM.Syntax.Rules.Instructions.Mov where

import Language.HaSM.Syntax.AST
import Language.HaSM.Syntax.Parser
import Language.HaSM.Syntax.Rules.Expression

mov :: Parser Instruction
mov = (Mov <$ reserved "mov") <*> exprVal <*> (comma *> exprAddr)