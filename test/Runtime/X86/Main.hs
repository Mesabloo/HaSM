{-# LANGUAGE QuasiQuotes #-}

import Language.HaSM
import Test.Hspec
import Data.Foldable (sequenceA_)

main :: IO ()
main = hspec (sequenceA_ tests)

tests :: [Spec]
tests =
    [ testMov
    , testJmp
    , testAdd
    , testNop
    ]

testMov :: Spec
testMov = describe "Tests on `mov`:" $
    let a = [hasm| mov $3, %ebx ; mov %ebx, %ecx ; mov %ecx, %eax ; ret |]
        b = [hasm| mov 0x0, 0x0 ; ret |]
    in do
        it "they should all compile" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "the first test should produce `3` on X86" $
            run X86 a `shouldReturn` 3
        it " the second testshould throw an exception on X86" $
            run X86 b `shouldThrow` anyException

testJmp :: Spec
testJmp = describe "Tests on `jmp`:" $
    let a = [hasm| mov $2, %eax ; jmp lbl ; lbl: ret |]
        b = [hasm| mov $2, %eax ; jmp forward ; mov $4, %eax ; forward: ret |]
        -- c = [hasm| loop: jmp loop |]
        d = [hasm| jmp forward ; forward: ret |]
    in parallel $ do
        it "they should all compile" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "the first test should produce `2` on X86" $
            run X86 a `shouldReturn` 2
        it "the second test should produce `2` on X86" $
            run X86 b `shouldReturn` 2
        -- it "the third test should timeout after 5s on an infinite loop on X86" $
        --     timeout 5000000 (run X86 c) `shouldReturn` Nothing
        it "the fourth test should produce `0` on X86" $
            run X86 d `shouldReturn` 0

testAdd :: Spec
testAdd = describe "Tests on `add`:" $
    let a = [hasm| mov $0, %eax ; add $2, %eax ; ret |]
        b = [hasm| mov $0, %eax ; mov $3, %ebx ; add %ebx, %eax ; ret |]
        c = [hasm| mov $0, %eax ; mov $2, %ebx ; add $1, %ebx ; add %ebx, %eax ; ret |]
    in do
        it "they should all compile" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "the first test should produce `2` on X86" $
            run X86 a `shouldReturn` 2
        it "the second test should produce `3` on X86" $
            run X86 b `shouldReturn` 3
        it "the third test should produce `3` on X86" $
            run X86 c `shouldReturn` 3

testNop :: Spec
testNop = describe "Tests on `nop`:" $
    let a = [hasm| nop ; ret |]
        b = [hasm| mov $0, %eax ; jmp lbl ; lbl: nop ; ret |]
    in do
        it "they should all compile" $
            0 `shouldBe` 0
        it "the first test should produce `0` on X86" $
            run X86 a `shouldReturn` 0
        it "the second test should produce `0` on X86" $
            run X86 b `shouldReturn` 0