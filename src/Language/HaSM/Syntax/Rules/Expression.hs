{-# LANGUAGE DataKinds, RankNTypes, KindSignatures
           , TypeFamilies #-}

module Language.HaSM.Syntax.Rules.Expression where

import Text.Megaparsec (choice, (<|>), try, option, (<?>))
import Language.HaSM.Syntax.Parser
import Language.HaSM.Syntax.AST
import Language.HaSM.Syntax.Rules.Register

exprVal :: Parser (Expr 'Valuable)
exprVal = try exprAny <|> Imm <$> (symbol "$" *> exprImm)

exprImm :: Parser Immediate
exprImm = choice
    [ I  <$> integer
    , D  <$> float
    , Id <$> identifier <?> "label" ]

exprAddr :: Parser (Expr t)
exprAddr = exprAny

exprAny :: Parser (Expr t)
exprAny = choice
    [ Reg <$> register
    , try (Shift <$> option 0 integer <*> parens exprAddr) <?> "address offset"
    , Addr <$> integer
    , Name <$> identifier <?> "label" ]