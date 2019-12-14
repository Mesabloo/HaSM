{-# LANGUAGE TemplateHaskell, GADTs, BinaryLiterals #-}

module Language.HaSM.CodeGen.Generators.X86.Generator where

import Language.HaSM.CodeGen.Core
import Language.HaSM.Syntax hiding (Label, Id)
import qualified Language.HaSM.Syntax as Syn (Instruction(Label))
import Language.HaSM.CodeGen.Generators.X86.Encoder (toBytes32)
import Control.Monad.State (State, runState)
import Data.Word (Word8)
import Data.Bits ((.|.), (.&.), shiftL)
import Debug.Trace (traceShow)

modRM :: Word8
modRM = 0xC0

generate' :: [Instruction] -> [Core]
generate' = generator

generator :: [Instruction] -> [Core]
generator []                            = []
generator (Syn.Label l : is)            =
    Label l : generator is
generator (Jmp (Name n) : is)           =
    [Byte 0xEB, Id n] <> generator is
generator (Mov (Imm i) (Reg dst) : is)  =
    [Byte (0xB8 + index dst)] <> immediate i <>  generator is
generator (Mov (Addr a) (Reg dst) : is) =
    [Byte 0x8B, Byte 0x05] <> immediate (I a) <> generator is
generator (Mov (Reg src) (Reg dst) : is) =
    [Byte 0x89, Byte $ modRM .|. index src `shiftL` 3 .|. index dst] <> generator is
generator (Nop : is) =
    Byte 0x90 : generator is
generator (Ret : is) =
    Byte 0xC3 : generator is
generator (Add (Imm i) (Reg EAX) : is) =
    Byte 0x05 : (immediate i <> generator is)
generator (Add (Imm i) (Reg dst) : is) =
    [Byte 0x81, Byte $ modRM .|. index dst] <> immediate i <> generator is
generator (Add (Reg src) (Reg dst) : is) =
    [Byte 0x01, Byte $ modRM .|. index src `shiftL` 3 .|. index dst] <> generator is
generator (i : is) = error ("No translation found for " <> show i)

immediate :: Immediate -> [Core]
immediate (I int) = Byte <$> toBytes32 int
immediate _       = undefined

index :: Register -> Word8
index = fromIntegral . fromEnum