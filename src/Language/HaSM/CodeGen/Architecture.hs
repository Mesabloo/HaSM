module Language.HaSM.CodeGen.Architecture where

data Arch
    = X86
    -- | X64
    -- | MIPS

instance Show Arch where
    show X86 = "x86"