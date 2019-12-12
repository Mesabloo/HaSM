module Language.HaSM.CodeGen.Generators.X86.Core where

import Data.Word (Word8)

data Core
    = Byte Word8
    | Label String Integer
    | JmpL String    -- ^ Only used with labels
    | JmpRel Integer -- ^ Used for relative jumping
    | Call String    -- ^ Only used with labels