{-# LANGUAGE QuasiQuotes #-}

import Language.HaSM
import Test.Hspec
import Data.Foldable (sequenceA_)

main :: IO ()
main = hspec (sequenceA_ tests)

tests :: [Spec]
tests =
    [ testMov
    , testNop
    , testLabel
    , testRet
    , testJmp
    , testAdd
    ]

testMov :: Spec
testMov = describe "Tests on `mov`:" $
    let a = [hasm| mov $0, %eax |]
        b = [hasm| mov %eax, %ebx |]
        c = [hasm| mov 25, %ecx |]
    in parallel $ do
        it "they should all compile" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "`mov $0, %eax` should be correctly parsed" $
            a `shouldBe` [Mov (Imm (I 0)) (Reg EAX)]
        it "`mov %eax, %ebx` should be correctly parsed" $
            b `shouldBe` [Mov (Reg EAX) (Reg EBX)]
        it "`mov 25, %eax` should be correctly parsed" $
            c `shouldBe` [Mov (Addr 25) (Reg ECX)]

testNop :: Spec
testNop = describe "Tests on `nop`:" $
    let a = [hasm| nop |]
    in parallel $ do
        it "they should all compile" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "`nop` should be correctly parsed" $
            a `shouldBe` [Nop]

testLabel :: Spec
testLabel = describe "Tests on labels:" $
    let a = [hasm| loop: nop |]
        b = [hasm| lo.op: nop |]
        c = [hasm| lo.op@2: nop |]
    in parallel $ do
        it "they should all compile" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "`loop: nop` should be correctly parsed" $
            a `shouldBe` [Label "loop", Nop]
        it "`lo.op: nop` should be correctly parsed" $
            b `shouldBe` [Label "lo.op", Nop]
        it "`lo.op@2: nop` should be correctly parsed" $
            c `shouldBe` [Label "lo.op@2", Nop]

testRet :: Spec
testRet = describe "Tests on `ret`:" $
    let a = [hasm| ret |]
    in parallel $ do
        it "they should all compile" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "`ret` should be correctly parsed" $
            a `shouldBe` [Ret]

testJmp :: Spec
testJmp = describe "Tests on `jmp`:" $
    let a = [hasm| jmp loop |]
        b = [hasm| jmp lo.op |]
        c = [hasm| jmp lo._op@2 |]
    in parallel $ do
        it "they should all compile" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "`jmp loop` should be correctly parsed" $
            a `shouldBe` [Jmp (Name "loop")]
        it "`jmp lo.op` should be correctly parsed" $
            b `shouldBe` [Jmp (Name "lo.op")]
        it "`jmp lo._op@2` should be correctly parsed" $
            c `shouldBe` [Jmp (Name "lo._op@2")]

testAdd :: Spec
testAdd = describe "Tests on `add`:" $
    let a = [hasm| add $0, %eax |]
        b = [hasm| add %eax, %ebx |]
    in parallel $ do
        it "they should all compile" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "`add $0, %eax` should be correctly parsed" $
            a `shouldBe` [Add (Imm (I 0)) (Reg EAX)]
        it "`add %eax, %ebx` should be correctly parsed" $
            b `shouldBe` [Add (Reg EAX) (Reg EBX)]