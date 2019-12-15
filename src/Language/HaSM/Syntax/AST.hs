{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

module Language.HaSM.Syntax.AST where

import Numeric (showHex)

data Instruction where
    Mov   :: Expr Valuable -> Expr Addressable -> Instruction
    Ret   :: Instruction
    Nop   :: Instruction
    Label :: String -> Instruction
    Add   :: Expr Valuable -> Expr Addressable -> Instruction
    Jmp   :: Expr Addressable -> Instruction

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
  deriving (Eq, Ord)

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
    show (Jmp a)   = "\"jmp " <> show a <> "\""

instance Eq Instruction where
    Mov v1 a1 == Mov v2 a2 = v1 == v2 && a1 == a2
    Ret       == Ret       = True
    Nop       == Nop       = True
    Label l1  == Label l2  = l1 == l2
    Add v1 a1 == Add v2 a2 = v1 == v2 && a1 == a2
    Jmp a1    == Jmp a2    = a1 == a2

instance Eq (Expr t) where
    Imm i1      == Imm i2      = i1 == i2
    Reg r1      == Reg r2      = r1 == r2
    Addr a1     == Addr a2     = a1 == a2
    Name n1     == Name n2     = n1 == n2
    Shift o1 a1 == Shift o2 a2 = o1 == o2 && a1 == a2