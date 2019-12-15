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
testMov = describe "Test on `mov`" $
    let a = [hasm| mov $0, %eax |]
        b = [hasm| mov %eax, %ebx |]
        c = [hasm| mov 25, %ecx |]
    in parallel $ do
        it "should compile all tests" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "should have correctly parsed `mov $0, %eax`" $
            a `shouldBe` [Mov (Imm (I 0)) (Reg EAX)]
        it "should have correctly parsed `mov %eax, %ebx`" $
            b `shouldBe` [Mov (Reg EAX) (Reg EBX)]
        it "should have correctly parsed `mov 25, %eax`" $
            c `shouldBe` [Mov (Addr 25) (Reg ECX)]

testNop :: Spec
testNop = describe "Test on `nop`" $
    let a = [hasm| nop |]
    in parallel $ do
        it "should compile all tests" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "should have correctly parsed `nop`" $
            a `shouldBe` [Nop]

testLabel :: Spec
testLabel = describe "Test on labels" $
    let a = [hasm| loop: nop |]
        b = [hasm| lo.op: nop |]
        c = [hasm| lo.op@2: nop |]
    in parallel $ do
        it "should compile all tests" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "should have correctly parsed `loop: nop`" $
            a `shouldBe` [Label "loop", Nop]
        it "should have correctly parsed `lo.op: nop`" $
            b `shouldBe` [Label "lo.op", Nop]
        it "should have correctly parsed `lo.op@2: nop`" $
            c `shouldBe` [Label "lo.op@2", Nop]

testRet :: Spec
testRet = describe "Test on `ret`" $
    let a = [hasm| ret |]
    in parallel $ do
        it "should compile all tests" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "should have correctly parsed `ret`" $
            a `shouldBe` [Ret]

testJmp :: Spec
testJmp = describe "Test on `jmp`" $
    let a = [hasm| jmp loop |]
        b = [hasm| jmp lo.op |]
        c = [hasm| jmp lo._op@2 |]
    in parallel $ do
        it "should compile all tests" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "should have correctly parsed `jmp loop`" $
            a `shouldBe` [Jmp (Name "loop")]
        it "should have correctly parsed `jmp lo.op`" $
            b `shouldBe` [Jmp (Name "lo.op")]
        it "should have correctly parsed `jmp lo._op@2`" $
            c `shouldBe` [Jmp (Name "lo._op@2")]

testAdd :: Spec
testAdd = describe "Test on `add`" $
    let a = [hasm| add $0, %eax |]
        b = [hasm| add %eax, %ebx |]
    in parallel $ do
        it "should compile all tests" $
            0 `shouldBe` 0 -- dummy test as it won't run if the code doesn't compile
        it "should have correctly parsed `add $0, %eax`" $
            a `shouldBe` [Add (Imm (I 0)) (Reg EAX)]
        it "should have correctly parsed `add %eax, %ebx`" $
            b `shouldBe` [Add (Reg EAX) (Reg EBX)]