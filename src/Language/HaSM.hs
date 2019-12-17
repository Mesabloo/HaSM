module Language.HaSM
( hasm
, run, extern
, Arch(..)
, JITException(..)
, module Language.HaSM.Syntax ) where

import Language.HaSM.TH (hasm)
import Language.HaSM.CodeGen.Architecture (Arch(..))
import Language.HaSM.Runtime (run, extern, JITException(..))
import Language.HaSM.Syntax