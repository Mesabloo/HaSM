{-# LANGUAGE CPP #-}

module Language.HaSM.CodeGen.Generators.X86.Encoder
( toBytes8, toBytes16, toBytes32 ) where

import Data.Word (Word8)
import qualified Data.ByteString.Lazy.Internal as BS (unpackBytes)
import Data.Binary.Put (runPut, putWord16le, putWord32le,  Put)

-- | Transforms an integral value into a little endian representation of its bytes.
toBytes :: (Integral a, Num b) => (b -> Put) -> a -> [Word8]
toBytes f x = BS.unpackBytes bs
  where bs = runPut $ f (fromIntegral x)

toBytes8, toBytes16, toBytes32 :: Integral a => a -> [Word8]
toBytes8  = (: []) . fromIntegral
toBytes16 = toBytes putWord16le
toBytes32 = toBytes putWord32le