{-# LANGUAGE CPP #-}

module Language.HaSM.Runtime where

import Language.HaSM.Runtime.Memory (allocateMemory, freeMemory)
import Language.HaSM.Runtime.Pointer
import Data.Word (Word8, Word32)
import System.Mmap
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
#ifndef mingw32_HOST_OS
import System.Posix.DynamicLinker
import Foreign.Ptr (castFunPtrToPtr)
#else
import System.Win32.DLL
#endif

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
#ifndef mingw32_HOST_OS
    dl <- dlopen "" [RTLD_LAZY, RTLD_GLOBAL]
    fn <- dlsym dl name
    pure (heapPtr $ castFunPtrToPtr fn)
#else
    dl <- loadLibrary ""
    fn <- getProcAddress dl name
    pure (heapPtr fn)
#endif
