{-# LANGUAGE QuasiQuotes #-}

import Language.HaSM
import Data.Word (Word8)
import Numeric (showHex)

main :: IO ()
main = do
    let x = [hasm| |]
    let y = [hasm|

mov $5, %ebx
add $5, %ebx
mov %ebx, %eax
ret

        |]
    let z = [hasm|

mov $0, %ebx
loop:
    add $1, %ebx
    jmp loop
mov %ebx, %eax
ret

        |]
    let a = [hasm|

loop: jmp loop

        |]

    print x
    print y
    print z
    print a

    res  <- run X86 x
    res2 <- run X86 y
    res3 <- run X86 z -- < infinite loop
    res4 <- run X86 a -- < infinite loop

    print res  -- < never happening
    print res2 -- < never happening
    print res3 -- < never happening
    print res4 -- < never happening

    pure ()