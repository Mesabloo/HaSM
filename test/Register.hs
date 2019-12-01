{-# LANGUAGE QuasiQuotes #-}

import Language.HaSM (hasm_x86)
import Data.Word (Word8)
import Numeric (showHex)

main :: IO ()
main = do
    let x = [hasm_x86| |]
    let y = [hasm_x86|
        mov $5, %eax
        -- add $, %eax
        ret
        |]

    dump x
    dump y

    pure ()

dump :: [Word8] -> IO ()
dump = print . fmap (("0x" <>) . (`showHex` ""))