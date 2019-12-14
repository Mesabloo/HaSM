module Language.HaSM.CodeGen.Core where

import Data.Word (Word8)

data Core
    = Byte Word8
    | Label String
    | Id String