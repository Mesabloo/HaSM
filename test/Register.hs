{-# LANGUAGE QuasiQuotes #-}

import Language.HaSM (hasm_x86, run)
import Data.Word (Word8)
import Numeric (showHex)

main :: IO ()
main = do
    let x = [hasm_x86| |]
    let y = [hasm_x86|

mov $5, %ebx
add $5, %ebx
mov %ebx, %eax
ret

        |]
    let z = [hasm_x86|

mov $0, %ebx
loop:
    add $1, %ebx
    jmp loop

        |]

    dump x
    dump y
    dump z

    res  <- run x
    res2 <- run y
    res3 <- run z -- < infinite loop

    print res
    print res2
    print res3 -- < never happening

    pure ()

dump :: [Word8] -> IO ()
dump = print . fmap (("0x" <>) . (`showHex` ""))