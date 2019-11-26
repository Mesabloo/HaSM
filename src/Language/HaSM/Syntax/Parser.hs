module Language.HaSM.Syntax.Parser where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Text.Megaparsec.Char as P hiding (space)
import Language.HaSM.Syntax.Reserved (instructions, registers)
import Data.Char as Ch (ord)
import Data.Void

type Parser = P.Parsec Void String

----------------------------------------------------------------------

lexeme :: Parser a -> Parser a
lexeme = P.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = P.space space1 lineCmnt blockCmnt

space1 :: Parser ()
space1 = P.skipSome (P.satisfy isSpace)
  where isSpace :: Char -> Bool
        isSpace c =
            let code = Ch.ord c
            in code == 9 || code == 32 || code == 160 || code == 8200 || code == 8201 || code == 8202

lineCmnt :: Parser ()
lineCmnt = P.skipLineComment "#"

blockCmnt :: Parser ()
blockCmnt = P.skipBlockComment "/*" "*/"

reserved :: String -> Parser ()
reserved k = () <$ lexeme (P.string k <* P.notFollowedBy P.alphaNumChar)

symbol :: String -> Parser ()
symbol = (() <$) . P.symbol spaceConsumer

integer :: Parser Integer
integer = P.signed spaceConsumer P.decimal

float :: Parser Double
float = P.signed spaceConsumer P.float

identifier :: Parser String
identifier = (:) <$> P.lowerChar <*> P.many (P.alphaNumChar P.<|> P.oneOf "._'@")

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

comma :: Parser ()
comma = symbol ","