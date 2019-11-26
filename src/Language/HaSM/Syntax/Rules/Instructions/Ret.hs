module Language.HaSM.Syntax.Rules.Instructions.Ret where

import Language.HaSM.Syntax.Parser
import Language.HaSM.Syntax.AST

ret :: Parser Instruction
ret = Ret <$ reserved "ret"