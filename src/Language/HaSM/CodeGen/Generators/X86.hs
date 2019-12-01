module Language.HaSM.CodeGen.Generators.X86
( generate ) where

import Language.HaSM.CodeGen.Generators.X86.Converter
import Language.HaSM.CodeGen.Generators.X86.Generator
import Language.HaSM.Syntax
import Data.Word (Word8)

generate :: [Instruction] -> [Word8]
generate = convert . generate'