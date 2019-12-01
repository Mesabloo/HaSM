module Language.HaSM.CodeGen.Generators.X86.Core where

import Data.Word (Word8)

data Core
    = Byte Word8
    | Label String
    | Jmp String    -- ^ Only used with labels
    | Call String   -- ^ Only used with labels