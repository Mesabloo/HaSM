module Language.HaSM.Runtime.Memory where

import System.Mmap
import Foreign.C.Types
import Foreign.Ptr
import Data.Word (Word8)
import Data.Bits ((.|.))

-- | Allocates a chunk of fixed size and returns a pointer to the beginning of the chunk.
allocateMemory :: CSize -> IO (Ptr Word8)
allocateMemory size = mmap nullPtr size pflags mflags (-1) 0
  where
    pflags = protRead <> protWrite
    mflags = mmapAnon .|. mmapPrivate

-- | Frees a chunk of memory given its base pointer and its size.
freeMemory :: Ptr Word8 -> CSize -> IO Int
freeMemory = munmap