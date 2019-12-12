module Language.HaSM.Syntax.Rules.Instructions.Jmp where

import Language.HaSM.Syntax.AST
import Language.HaSM.Syntax.Parser
import Language.HaSM.Syntax.Rules.Expression

jmp :: Parser Instruction
jmp = (Jmp <$ reserved "jmp") <*> exprAddr