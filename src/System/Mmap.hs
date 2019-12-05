{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.Mmap
( mmap, munmap
, module System.Mmap.Flags
, module System.Mmap.Exception ) where

import Foreign.Ptr
import Foreign.C.Types
import System.Mmap.Flags
import System.Mmap.Exception
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Word
import System.Posix.Types

#ifndef mingw32_HOST_OS
foreign import ccall unsafe "sys/mman.h mmap"
#else
foreign import ccall unsafe "./MMap/windows-mmap.h mmap"
#endif
    c_mmap :: Ptr () -> CSize -> ProtOption -> MmapOption -> Fd -> COff -> IO (Ptr a)

#ifndef mingw32_HOST_OS
foreign import ccall unsafe "sys/mman.h munmap"
#else
foreign import ccall unsafe "./MMap/windows-mmap.h munmap"
#endif
    c_munmap :: Ptr a -> CSize -> IO Int

-- | The 'mmap' function is used to allocate pages.
mmap :: Ptr () -> CSize -> ProtOption -> MmapOption -> Fd -> COff -> IO (Ptr Word8)
mmap addr len prot flags fd offset = do
    ptr <- c_mmap addr len prot flags fd offset
    when (ptr == intPtrToPtr (-1)) $ throwIO MmapException
    pure ptr

-- | The 'munmap' function is used to free pages.
munmap :: Ptr a -> CSize -> IO Int
munmap addr len = do
    ret <- c_munmap addr len
    when (ret == -1) $ throwIO MmapException
    pure ret