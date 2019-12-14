module Language.HaSM.CodeGen.Generators.X86
( generate, convert ) where

import Language.HaSM.CodeGen.Generators.X86.Converter
import Language.HaSM.CodeGen.Generators.X86.Generator
import Language.HaSM.CodeGen.Core
import Language.HaSM.Syntax
import Data.Word (Word8)
import Foreign.Ptr (Ptr)

generate :: [Instruction] -> [Core]
generate = generate'

convert :: Ptr a -> [Core] -> [Word8]
convert = convert'