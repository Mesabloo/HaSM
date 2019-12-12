{-# LANGUAGE LambdaCase #-}

module Language.HaSM.CodeGen.Generators.X86.Converter where

import Language.HaSM.CodeGen.Generators.X86.Core
import Language.HaSM.CodeGen.Generators.X86.Generator
import Data.Word (Word8)
import Control.Monad.State (State, evalState, modify, gets)
import qualified Data.Map as Map
import Control.Lens ((^.))

type Converter = State (Map.Map String Integer)

convert :: GeneratorState -> [Word8]
convert = (`evalState` mempty) . register

register :: GeneratorState -> Converter [Word8]
register c = go (c ^. code) *> converter c

go :: [Core] -> Converter ()
go []             = pure ()
go (Label n o:cs) = do
    modify (Map.insert n o)
    go cs
go (_:cs)         = go cs

converter :: GeneratorState -> Converter [Word8]
converter = go' [] . (^. code)

go' :: [Word8] -> [Core] -> Converter [Word8]
go' gen []             = pure gen
go' gen (Byte b:cs)    = go' (gen <> [b]) cs
go' gen (Label n o:cs) = go' gen cs
go' gen (JmpL n:cs)     = do
    gets (Map.lookup n) >>= \case
        Nothing -> error ("Label " <> n <> " not found")
        Just n' -> go' (gen <> jmp n') cs
  where jmp n = [0xFF] <> []
go' gen _              = undefined
