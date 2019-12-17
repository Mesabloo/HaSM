{-# LANGUAGE KindSignatures #-}

module Language.HaSM.CodeGen.Generator where

import Control.Monad.Except (Except, runExcept)

type Generator (m :: (* -> *) -> * -> *) a = m (Except String) a

runGenerator :: Except e a -> Either e a
runGenerator = runExcept