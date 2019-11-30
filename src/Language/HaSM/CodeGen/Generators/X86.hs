module Language.HaSM.CodeGen.Generators.X86 where

import Data.Word (Word8)
import Language.HaSM.Syntax (Instruction(..))

generate :: [Instruction] -> [Word8]
generate [] = [0x90]
generate (i:is) = [] <> generate is

