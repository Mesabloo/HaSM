module Language.HaSM (hasm_x86) where

import Language.HaSM.TH (hasm)
import Language.HaSM.CodeGen.Architecture (Arch(..))
import Language.Haskell.TH.Quote (QuasiQuoter)

hasm_x86 :: QuasiQuoter
hasm_x86 = hasm X86