module Language.HaSM.CodeGen.Generators.X86
( generate, convert ) where

import Language.HaSM.CodeGen.Generators.X86.Converter
import Language.HaSM.CodeGen.Generators.X86.Generator
import Language.HaSM.CodeGen.Core
import Language.HaSM.CodeGen.Generator (runGenerator)
import Language.HaSM.Syntax
import Data.Word (Word8)
import Foreign.Ptr (Ptr)

generate :: [Instruction] -> Either String [Core]
generate = runGenerator . generate'

convert :: Ptr a -> [Core] -> Either String [Word8]
convert = runGenerator .: convert'

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)