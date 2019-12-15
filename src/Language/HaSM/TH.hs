{-# LANGUAGE GADTs #-}

module Language.HaSM.TH
( hasm ) where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (Q, Exp(..), Lit(..), mkName, Name, Type(..))
import Language.HaSM.Syntax (parse, Instruction(..), Expr(..), Immediate(..), Register(..))
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Data.Void (Void)
import Control.Lens ((??))

hasm :: QuasiQuoter
hasm = QuasiQuoter
    (toQExp . parse "hasm")
    (notHandled "pattern")
    (notHandled "type")
    (notHandled "top-level binding")
  where notHandled t = fail ("The hasm quasiquoter cannot be used as a " <> t)

toQExp :: Either (ParseErrorBundle String Void) [Instruction] -> Q Exp
toQExp (Left e)    = fail (errorBundlePretty e)
toQExp (Right ast) = SigE <$> convert ast <*> (AppT ListT . ConT <$> getName "Instruction")



class ToExp a where
    convert :: a -> Q Exp

instance ToExp a => ToExp [a] where
    convert = fmap ListE . mapM convert

instance ToExp Instruction where
    convert (Mov irm rm) =
        AppE <$> (AppE . ConE <$> getName "Mov" <*> convert irm) <*> convert rm
    convert Nop          = ConE <$> getName "Nop"
    convert Ret          = ConE <$> getName "Ret"
    convert (Label l)    = AppE . ConE <$> getName "Label" ?? LitE (StringL l)
    convert (Add irm rm) =
        AppE <$> (AppE . ConE <$> getName "Add" <*> convert irm) <*> convert rm
    convert (Jmp rm)     = AppE . ConE <$> getName "Jmp" <*> convert rm

instance ToExp (Expr t) where
    convert (Imm i)     = AppE . ConE <$> getName "Imm"   <*> convert i
    convert (Reg r)     = AppE . ConE <$> getName "Reg"   <*> convert r
    convert (Addr a)    = AppE . ConE <$> getName "Addr"  ??  LitE (IntegerL a)
    convert (Name n)    = AppE . ConE <$> getName "Name"  ??  LitE (StringL n)
    convert (Shift i e) =
        AppE <$> (AppE . ConE <$> getName "Shift" ?? LitE (IntegerL i)) <*> convert e

instance ToExp Immediate where
    convert (I i)  = AppE . ConE <$> getName "I"  ?? LitE (IntegerL i)
    convert (D d)  = AppE . ConE <$> getName "D"  ?? LitE (RationalL (toRational d))
    convert (Id i) = AppE . ConE <$> getName "Id" ?? LitE (StringL i)

instance ToExp Register where
    convert = fmap ConE . getName . show

getName :: String -> Q Name
getName = pure . mkName