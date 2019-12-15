{-# LANGUAGE QuasiQuotes #-}

import Language.HaSM
import Test.Hspec
import Data.Foldable (sequenceA_)
import System.Timeout (timeout)

main :: IO ()
main = hspec (sequenceA_ tests)

tests :: [Spec]
tests =
    [ testMov
    , testJmp
    , testAdd
    ]

testMov :: Spec
testMov = describe "Test on `mov`" $
    let code = [hasm| mov $3, %ebx ; mov %ebx, %ecx ; mov %ecx, %eax ; ret |]
    in do
        it "should compile" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "should produce `3` on X86" $
            run X86 code `shouldReturn` 3

testJmp :: Spec
testJmp = describe "Test on `jmp`" $
    let a = [hasm| mov $2, %eax ; jmp lbl ; lbl: ret |]
        b = [hasm| mov $2, %eax ; jmp forward ; mov $4, %eax ; forward: ret |]
        -- c = [hasm| loop: jmp loop |]
        d = [hasm| jmp forward ; forward: ret |]
    in parallel $ do
        it "should compile all tests" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "should produce `2` for the first test on X86" $
            run X86 a `shouldReturn` 2
        it "should produce `2` for the second test on X86" $
            run X86 b `shouldReturn` 2
        -- it "should timeout after 5s on an infinite loop for the third test on X86" $
        --     timeout 5000000 (run X86 c) `shouldReturn` Nothing
        it "should produce `0` for the fourth test on X86" $
            run X86 d `shouldReturn` 0

testAdd :: Spec
testAdd = describe "Test on `add`" $
    let a = [hasm| mov $0, %eax ; add $2, %eax ; ret |]
        b = [hasm| mov $0, %eax ; mov $3, %ebx ; add %ebx, %eax ; ret |]
        c = [hasm| mov $0, %eax ; mov $2, %ebx ; add $1, %ebx ; add %ebx, %eax ; ret |]
    in do
        it "should compile all tests" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "should produce `2` for the first test on X86" $
            run X86 a `shouldReturn` 2
        it "should produce `3` for the second test on X86" $
            run X86 b `shouldReturn` 3
        it "should produce `3` for the third test on X86" $
            run X86 c `shouldReturn` 3
