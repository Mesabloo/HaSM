module Language.HaSM
( hasm
, run, extern
, Arch(..)
, module Language.HaSM.Syntax ) where

import Language.HaSM.TH (hasm)
import Language.HaSM.CodeGen.Architecture (Arch(..))
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.HaSM.Runtime (run, extern)
import Language.HaSM.Syntax