{-# LANGUAGE QuasiQuotes #-}

import Language.HaSM (hasm_x86)

main :: IO ()
main = do
    let y = 0x10
    let x = [hasm_x86| mov $0, %eax ; ret |]
    let z = [hasm_x86|
            mov $0x0, -0x0 ;
            ret ;;;;; ret
            label: nop |]
    let a = [hasm_x86|
            label: # test comment
                   /* test multiline
                   comment */
                ret |]

    print x
    print z
    print a

    pure ()