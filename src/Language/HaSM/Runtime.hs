{-# LANGUAGE CPP, LambdaCase, DeriveAnyClass #-}

module Language.HaSM.Runtime
( run, extern, JITException ) where

import Language.HaSM.Runtime.Memory (allocateMemory, freeMemory)
import Language.HaSM.Runtime.Pointer
import Language.HaSM.CodeGen (Arch, generate, convert)
import Language.HaSM.Syntax (Instruction)
import Data.Word (Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Control.Monad.State (liftIO)
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Exception (Exception, throw)
#ifndef mingw32_HOST_OS
import System.Posix.DynamicLinker
import Foreign.Ptr (castFunPtrToPtr)
#else
import System.Win32.DLL
#endif

newtype JITException = JITException String
  deriving (Show, Exception)

run :: Arch -> [Instruction] -> IO Int
run a is =
    let act = run' a is
    in runExceptT act >>= \case
        Left  e -> throw (JITException e)
        Right r -> r

run' :: Arch -> [Instruction] -> ExceptT String IO (IO Int)
run' _ [] = pure (pure 0)
run' a is = do
    bs <- liftEither (generate a is)
    let size = fromIntegral (length bs)

    mem <- liftIO (allocateMemory size)

    bytes <- liftEither (convert a mem bs)
    let icount = length bytes

    code <- liftIO (codePtr bytes)
    liftIO (withForeignPtr (vecPtr code) (flip (copyBytes mem) icount))

    pure (getFunction mem <* freeMemory mem size)

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