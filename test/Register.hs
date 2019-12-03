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

    dump x
    dump y

    res <- run x
    res2 <- run y

    print res
    print res2

    pure ()

dump :: [Word8] -> IO ()
dump = print . fmap (("0x" <>) . (`showHex` ""))