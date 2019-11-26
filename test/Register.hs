{-# LANGUAGE QuasiQuotes #-}

import Language.HaSM (hasm)

main :: IO ()
main = do
    let y = 0x10
    let x = [hasm| mov $0, %eax ; ret |]
    let z = [hasm|
            mov $0, %eax ;
            ret ;;;;; ret
            label: nop |]
    pure ()