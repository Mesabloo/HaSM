module Language.HaSM.CodeGen.Generators.X86.Converter where

import Language.HaSM.CodeGen.Generators.X86.Core
import Language.HaSM.CodeGen.Generators.X86.Generator
import Data.Word (Word8)
import Control.Monad.State (State, evalState)
import qualified Data.Map as Map
import Control.Lens ((^.))

type Converter = State (Map.Map String Integer)

convert :: GeneratorState -> [Word8]
convert = (`evalState` mempty) . register

register :: GeneratorState -> Converter [Word8]
register c = go 0 (c ^. code) *> converter c

go :: Integer -> [Core] -> Converter ()
go n [] = pure ()

converter :: GeneratorState -> Converter [Word8]
converter = go' [] . (^. code)

go' :: [Word8] -> [Core] -> Converter [Word8]
go' gen []            = pure gen
go' gen (Byte b : cs) = go' (gen <> [b]) cs
go' gen _             = undefined
