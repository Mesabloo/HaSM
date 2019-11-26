# HaSM

A simple assembly EDSL (Embedded Domain Specific Language) for Haskell, generating native code for fast runtime.
It aims at being quite cross-platform.

### Code example

```haskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes, BlockArguments #-}

import Language.HaSM

main :: IO Int
main = do
    hello_world <- asciz "Hello, world!"
    printf      <- extern "printf"

    runX86 [hasm|
        push ${hello_world}
        call ${printf}
        xor %eax, %eax
        ret
    |]
```