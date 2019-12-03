module Language.HaSM.Runtime where

import Language.HaSM.Runtime.Memory (allocateMemory, freeMemory)
import Language.HaSM.Runtime.Pointer
import Data.Word (Word8, Word32)
import System.Mmap
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castFunPtrToPtr)
import Foreign.Marshal.Utils (copyBytes)
import System.Posix.DynamicLinker

run :: [Word8] -> IO Int
run [] = pure 0
run is = do
    let icount = length is
        size   = toEnum icount
    mem <- allocateMemory size

    code <- codePtr is
    withForeignPtr (vecPtr code) (flip (copyBytes mem) icount)

    getFunction mem <* freeMemory mem size

-- | Returns the address of an external function to be called.
extern :: String -> IO Word32
extern name = do
    dl <- dlopen "" [RTLD_LAZY, RTLD_GLOBAL]
    fn <- dlsym dl name
    pure (heapPtr $ castFunPtrToPtr fn)