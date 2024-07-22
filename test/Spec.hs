{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Kind
import Data.Proxy
import GHC.Exts
import LiftType
import Test.Hspec

main :: IO ()
main = do
    let
        type_ = Proxy :: Proxy $(liftTypeQ @Type)
        type_' = Proxy :: Proxy $(liftTypeQ @TYPE)
        word# = Proxy :: Proxy $(liftTypeQ @Word#)
        bool = Proxy :: Proxy $(liftTypeQ @Bool)
        true = Proxy :: Proxy $(liftTypeQ @'True)
        three = Proxy :: Proxy $(liftTypeQ @3)
        valList = Proxy :: Proxy $(liftTypeQ @[Char])
        isTrue = valList == Proxy @[Char]
        list = Proxy :: Proxy $(liftTypeQ @'[Int, Char])
        tuple = Proxy :: Proxy $(liftTypeQ @'(Int, Char))
        isTrueTuple = tuple == Proxy @'(Int, Char)
        plainTuple = (Proxy :: Proxy $(liftTypeQ @(Int, Char))) == Proxy @(Int, Char)
        symbol = Proxy :: Proxy $(liftTypeQ @"hello")
        isTrue2 = symbol == Proxy @"hello"

    hspec $ do
        describe "LiftType" $ do
            describe "typeToName" $ do
                it "returns function arrow on functions" $ do
                    typeToName @(Int -> Char) `shouldBe` TypeName ''(->)
                it "works on a plain type" $ do
                    typeToName @Char `shouldBe` TypeName ''Char
                it "works on Maybe" $ do
                    typeToName @Maybe `shouldBe` TypeName ''Maybe
                it "works on a class" $ do
                    typeToName @Functor `shouldBe` TypeName ''Functor
                it "pulls the outermost type constructor" $ do
                    typeToName @(Maybe Int) `shouldBe` TypeName ''Maybe
                it "works on a ticked constructor" $ do
                    typeToName @'False `shouldBe` PromotedDataName 'False

assert :: String -> Bool -> IO ()
assert msg cond =
    if cond
        then pure ()
        else error msg
