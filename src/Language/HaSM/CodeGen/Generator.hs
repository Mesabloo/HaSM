module Language.HaSM.CodeGen.Generator where

import Language.HaSM.CodeGen.Architecture
import Language.HaSM.Syntax (Instruction)
import qualified Language.HaSM.CodeGen.Generators.X86 as X86
import Data.Word (Word8)

generate :: Arch -> [Instruction] -> [Word8]
generate X86 is = X86.generate is
generate a is = error ("Unsupported platform " <> show a)