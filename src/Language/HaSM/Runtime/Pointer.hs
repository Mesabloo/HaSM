{-# LANGUAGE ForeignFunctionInterface #-}

module Language.HaSM.Runtime.Pointer where

import Data.Word (Word8, Word32)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.Ptr (ptrToIntPtr, FunPtr, Ptr)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Storable
import Unsafe.Coerce (unsafeCoerce)

-- | This function converts a single opcode list into a vector of opcodes.
codePtr :: [Word8] -> IO (VM.IOVector Word8)
codePtr = V.thaw . V.fromList

-- | This function converts a vector of opcodes into a 'ForeignPtr'.
vecPtr :: Storable a => VM.MVector s a -> ForeignPtr a
vecPtr = fst . VM.unsafeToForeignPtr0

-- | Returns the address held by a pointer.
heapPtr :: Ptr a -> Word32
heapPtr = fromIntegral . ptrToIntPtr

foreign import ccall "dynamic"
    mkFun :: FunPtr (IO Int) -> IO Int
  
-- | A simple way to obtain a "haskell function" from a function pointer.
getFunction :: Ptr Word8 -> IO Int
getFunction mem = do
    let fptr = unsafeCoerce mem :: FunPtr (IO Int)
    mkFun fptr