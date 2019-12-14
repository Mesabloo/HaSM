{-# LANGUAGE CPP #-}

module Language.HaSM.Runtime where

import Language.HaSM.Runtime.Memory (allocateMemory, freeMemory)
import Language.HaSM.Runtime.Pointer
import Language.HaSM.CodeGen (Arch, generate, convert)
import Language.HaSM.Syntax (Instruction)
import Data.Word (Word8, Word32)
import System.Mmap
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Debug.Trace (traceShow)
import Numeric (showHex)
#ifndef mingw32_HOST_OS
import System.Posix.DynamicLinker
import Foreign.Ptr (castFunPtrToPtr)
#else
import System.Win32.DLL
#endif

run :: Arch -> [Instruction] -> IO Int
run _ [] = pure 0
run a is = do
    let bs   = generate a is
        size = fromIntegral (length bs)

    mem <- allocateMemory size

    let bytes  = convert a mem bs
        icount = traceDump bytes $ length bytes

    code <- codePtr bytes
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

traceDump :: [Word8] -> a -> a
traceDump = traceShow . f
  where f = fmap (mappend "0x" . (`showHex` ""))