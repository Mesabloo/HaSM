{-# LANGUAGE TemplateHaskell, GADTs, BinaryLiterals #-}

module Language.HaSM.CodeGen.Generators.X86.Generator where

import Language.HaSM.CodeGen.Generators.X86.Core
import Language.HaSM.Syntax hiding (Label)
import qualified Language.HaSM.Syntax as Syn (Instruction(Label))
import Control.Lens (makeLenses, (%=), (+=), (%~), use)
import Control.Monad.State (State, runState)
import Data.Word (Word8)
import Data.Bits ((.|.), (.&.), shiftL)
import qualified Data.ByteString.Lazy.Internal as BS (unpackBytes)
import Data.Binary.Put (runPut, putWord32le)
import Debug.Trace (traceShow)

rexW :: Core
rexW = Byte 0x48

modRM :: Word8
modRM = 0xC0

_0, _1, _2, _3, _4, _5, _6, _7 :: Word8
_0 = 0x05
_1 = 0xC8
_2 = 0xD0
_3 = 0xD8
_4 = 0xE0
_5 = 0xE8
_6 = 0xF0
_7 = 0xF8

type Generator = State GeneratorState

data GeneratorState
    = GenState
    { _code :: [Core]
    , _off  :: Integer }

makeLenses ''GeneratorState

generate' :: [Instruction] -> GeneratorState
generate' = snd . (`runState` GenState mempty 0) . generator

generator :: [Instruction] -> Generator ()
generator []                           = pure ()
generator (Syn.Label l : is) = do
    o <- use off
    code %= (<> [Label l o])
    generator is
generator (Jmp (Name n) : is) = do
    code %= (<> [JmpL n])
    off += 5
    generator is
generator (Mov (Imm i) (Reg dst) : is) = do
    emit [Byte (0xB8 + index dst)]
    immediate i
    generator is
generator (Mov (Addr a) (Reg dst) : is) = do
    emit [Byte 0x8B, Byte 0x05]
    immediate (I a)
    generator is
generator (Mov (Reg src) (Reg dst) : is) = do
    emit [Byte 0x89, Byte $ modRM .|. index src `shiftL` 3 .|. index dst]
    generator is
generator (Nop : is) = do
    emit [Byte 0x90]
    generator is
generator (Ret : is) = do
    emit [Byte 0xC3]
    generator is
generator (Add (Imm i) (Reg EAX) : is) = do
    emit [Byte 0x05]
    immediate i
    generator is
generator (Add (Imm i) (Reg dst) : is) = do
    emit [Byte 0x81, Byte $ modRM .|. index dst]
    immediate i
    generator is
generator (Add (Reg src) (Reg dst) : is) = do
    emit [Byte 0x01, Byte $ modRM .|. index src `shiftL` 3 .|. index dst]
    generator is
generator (i : is) = error ("No translation found for \"" <> show i <> "\"")

immediate :: Immediate -> Generator ()
immediate (I int) = emit (Byte <$> toBytes int)
immediate _       = undefined

index :: Register -> Word8
index = fromIntegral . fromEnum

-- | Transforms an integral value into a little endian representation of its bytes.
toBytes :: Integral a => a -> [Word8]
toBytes x = BS.unpackBytes bs where bs = runPut $ putWord32le (fromIntegral x)

emit :: [Core] -> Generator ()
emit i = do
    code %= (<> i)
    off += fromIntegral (length i)
