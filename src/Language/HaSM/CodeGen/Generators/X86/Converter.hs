{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Language.HaSM.CodeGen.Generators.X86.Converter where

import Language.HaSM.CodeGen.Core
import Language.HaSM.CodeGen.Generators.X86.Encoder (toBytes32)
import Data.Word (Word8)
import Control.Monad.State (State, evalState)
import qualified Data.Map as Map
import Control.Lens ((%=), makeLenses, uses, use, (+=))
import Foreign.Ptr (Ptr, IntPtr(..), ptrToIntPtr)
import Control.Conditional ((<<|))

type Converter = State ConverterState

data ConverterState
    = CState
    { _labels :: Map.Map String Integer
    , _base   :: Integer
    , _offset :: Integer }

makeLenses ''ConverterState

convert' :: Ptr a -> [Core] -> [Word8]
convert' p = (`evalState` CState mempty (baseAddress p) 0) . register
  where baseAddress p = fromIntegral (f (ptrToIntPtr p))
        f (IntPtr i)  = i

register :: [Core] -> Converter [Word8]
register c = go 0 c *> converter c

go :: Integer -> [Core] -> Converter ()
go _ []             = pure ()
go n (Label o:cs)   = do
    labels %= Map.insert o n
    go n cs
go n (Id _:cs)      = go (n + 4) cs
go n (_:cs)         = go (n + 1) cs

converter :: [Core] -> Converter [Word8]
converter = go' []

go' :: [Word8] -> [Core] -> Converter [Word8]
go' gen []             = pure gen
go' gen (Byte  b : cs) = do
    offset += 1
    go' (gen <> [b]) cs
go' gen (Label _ : cs) = go' gen cs
go' gen (Id o:cs)      = do
    off     <- labels `uses` (fail ("Label " <> o <> " not found") <<| Map.lookup o)
    offset' <- use offset

    let off' = -offset' + off - 1
    go' (gen <> toBytes32 off') cs
