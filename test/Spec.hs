{-# language MagicHash, TemplateHaskell, DataKinds, TypeApplications #-}

module Main where

import LiftType
import Data.Proxy
import Data.Kind
import GHC.Exts

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
    putStrLn "should compile"
