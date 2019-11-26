module Language.HaSM.Syntax.Rules.Register where

import Language.HaSM.Syntax.Parser
import Language.HaSM.Syntax.Reserved (registers)
import Language.HaSM.Syntax.AST
import Text.Megaparsec (choice, (<?>))
import Control.Monad (liftM2)

register :: Parser Register
register = (symbol "%" *> oneReg) <?> "register"
  where oneReg = choice (liftM2 (<*) makeRegister reserved <$> registers)

        makeRegister "eax" = pure EAX
        makeRegister "ebx" = pure EBX
        makeRegister "ecx" = pure ECX
        makeRegister "edx" = pure EDX
        makeRegister "esp" = pure ESP
        makeRegister "ebp" = pure EBP
        makeRegister "edi" = pure EDI
        makeRegister "esi" = pure ESI
        makeRegister r     = fail ("Unknwon register \"" <> r <> "\".")