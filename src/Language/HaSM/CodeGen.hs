module Language.HaSM.CodeGen
( Arch(..)
, generate
, convert ) where

import Language.HaSM.CodeGen.Architecture (Arch(..))
import Language.HaSM.CodeGen.Core (Core)
import Language.HaSM.Syntax (Instruction)
import qualified Language.HaSM.CodeGen.Generators.X86 as X86
import Foreign.Ptr (Ptr)
import Data.Word (Word8)

generate :: Arch -> [Instruction] -> [Core]
generate X86 is = X86.generate is
generate a _    = error ("Unsupported platform " <> show a)

convert :: Arch -> Ptr a -> [Core] -> [Word8]
convert X86 mem cs = X86.convert mem cs
convert a _ _      = error ("Unsupported platform " <> show a)