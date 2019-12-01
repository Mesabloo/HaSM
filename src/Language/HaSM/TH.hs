module Language.HaSM.TH where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (Q, Exp(..), runIO, Lit(..))
import Language.HaSM.Syntax (parse, Instruction)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Data.Void (Void)
import Language.HaSM.CodeGen (Arch, generate)
import Numeric (showHex)

hasm :: Arch -> QuasiQuoter
hasm arch = QuasiQuoter
    (translate arch . parse "hasm")
    (notHandled "pattern")
    (notHandled "type")
    (notHandled "top-level binding")
  where notHandled t = error ("The hasm quasiquoter cannot be used as a " <> t)

translate :: Arch -> Either (ParseErrorBundle String Void) [Instruction] -> Q Exp
translate _ (Left err) = fail (errorBundlePretty err)
translate a (Right x)  =
    let generated = generate a x
    in do
        runIO (print $ flip showHex "" <$> generated)
        pure (ListE $ LitE . IntegerL . fromIntegral <$> generated)