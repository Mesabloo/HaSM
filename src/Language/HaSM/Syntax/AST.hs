{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

module Language.HaSM.Syntax.AST where

import Numeric (showHex)

data Instruction where
    Mov   :: Expr Valuable -> Expr Addressable -> Instruction
    Ret   :: Instruction
    Nop   :: Instruction
    Label :: String -> Instruction
    Add   :: Expr Valuable -> Expr Addressable -> Instruction

data EType
    = Valuable
    | Addressable

data Expr :: EType -> * where
    Imm   :: Immediate -> Expr Valuable
    Reg   :: Register -> Expr t
    Addr  :: Integer -> Expr t
    Name  :: String -> Expr t
    Shift :: Integer -> Expr Addressable -> Expr t

data Immediate
    = I Integer
    | D Double
    | Id String

data Register
    = EAX
    | ECX
    | EDX
    | EBX
    | ESP
    | EBP
    | ESI
    | EDI
  deriving (Eq, Show, Ord, Enum)




-------------------------------------------------------------------------------------

instance Show (Expr t) where
    show (Imm i)     = show i
    show (Reg r)     = show r
    show (Addr a)    = "0x" <> showHex a ""
    show (Name l)    = l
    show (Shift i e) = show i <> "(" <> show e <> ")"

instance Show Immediate where
    show (I i)  = "$" <> show i
    show (D d)  = "$" <> show d
    show (Id s) = "$" <> s

instance Show Instruction where
    show (Mov v a) = "\"mov " <> show v <> ", " <> show a <> "\""
    show Ret       = show "ret"
    show Nop       = show "nop"
    show (Label l) = "\"" <> l <> ":\""
    show (Add v a) = "\"add " <> show v <> ", " <> show a <> "\""