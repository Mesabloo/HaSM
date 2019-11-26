module Language.HaSM.Syntax.Rules.Program where

import Language.HaSM.Syntax.AST
import Language.HaSM.Syntax.Parser
import Text.Megaparsec (many, (<|>), eof, some, choice, (<?>), satisfy, try, option)
import Text.Megaparsec.Char (newline)
import Control.Monad (void)
import Data.Char (isSpace)
import Language.HaSM.Syntax.Rules.Instructions.Mov
import Language.HaSM.Syntax.Rules.Instructions.Ret
import Language.HaSM.Syntax.Rules.Instructions.Nop
import Language.HaSM.Syntax.Rules.Instructions.Label

program :: Parser [Instruction]
program = init *> body <* eof
  where init = many (() <$ satisfy isSpace <|> blockCmnt <|> lineCmnt) <?> ""
        body = mconcat <$> many (stt <* eol)
        stt  = (<>) <$> option [] ((:[]) <$> try label) <*> ((:[]) <$> lexeme statement)
        eol  = (some (lexeme (() <$ newline <|> symbol ";")) <|> [] <$ eof) <?> ""

statement :: Parser Instruction
statement = choice
    [ mov <?> "MOV instruction"
    , ret <?> "RET instruction"
    , nop <?> "NOP instruction" ] <?> ""