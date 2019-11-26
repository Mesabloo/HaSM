module Language.HaSM.Syntax.Rules.Instructions.Nop where

import Language.HaSM.Syntax.AST
import Language.HaSM.Syntax.Parser

nop :: Parser Instruction
nop = Nop <$ reserved "nop"