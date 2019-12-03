{-# LANGUAGE ForeignFunctionInterface #-}

module System.Mmap
( mmap, munmap
, module System.Mmap.Flags
, module System.Mmap.Exception ) where

import System.Mmap.Exception
import System.Mmap.Flags
import Control.Exception (throwIO)
import Foreign.C.Types
import Foreign.Ptr
import System.Posix.Types
import Data.Word (Word8)
import Control.Monad (when)

foreign import ccall unsafe "sys/mman.h mmap"
    c_mmap :: Ptr () -> CSize -> ProtOption -> MmapOption -> Fd -> COff -> IO (Ptr a)
foreign import ccall unsafe "sys/mman.h munmap"
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