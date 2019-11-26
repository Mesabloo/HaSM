module Language.HaSM.TH where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (Q, Exp(..), runIO)
import Language.HaSM.Syntax (parse, Instruction)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Data.Void (Void)

hasm :: QuasiQuoter
hasm = QuasiQuoter
    (translate . parse "hasm")
    (notHandled "pattern")
    (notHandled "type")
    (notHandled "top-level binding")
  where notHandled t = error ("The hasm quasiquoter cannot be used as a " <> t)

translate :: Either (ParseErrorBundle String Void) [Instruction] -> Q Exp
translate (Left err) = fail (errorBundlePretty err)
translate (Right x)  = ListE [] <$ runIO (putStrLn ("Generated AST from hasm quote: " <> show x))