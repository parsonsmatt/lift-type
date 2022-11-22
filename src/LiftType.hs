{-# language TypeInType, ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications, PolyKinds, TemplateHaskell #-}

-- | Template Haskell has a class 'Lift' that allows you to promote values
-- from Haskell-land into the land of metaprogramming - 'Q'.
--
-- @
-- class 'Lift' a where
--     'lift' :: a -> 'Q' 'Exp'
--
--     'liftTyped' :: a -> 'Q' ('TExp' a)
-- @
--
-- However, there wasn't a way to promote a *type* into a @'Q' 'Type'@.
--
-- This library provides exactly that function. It requires a 'Typeable'
-- constraint, but this is automatically satisfied by GHC.
--
-- @since 0.1.0.0
module LiftType where

import Data.Char
import Control.Applicative
import Type.Reflection
import Language.Haskell.TH.Syntax
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- | 'liftType' promoted to the 'Q' monad.
--
-- @since 0.1.0.0
liftTypeQ :: forall t. Typeable t => Q Type
liftTypeQ = pure $ liftType @t

-- | Promote a 'SomeTypeRep' into a 'Type'.
--
-- @since 0.1.1.0
typeRepToType :: SomeTypeRep -> Type
typeRepToType (SomeTypeRep a) = go a
  where
    go :: forall k (a :: k). TypeRep a -> Type
    go tr =
        case tr of
            Con tyCon ->
                mk tyCon
            Fun trA trB ->
                ConT ''(->) `AppT` go trA `AppT` go trB
            App trA trB ->
                AppT (go trA) (go trB)

    mk :: TyCon -> Type
    mk tyCon =
        let
            tcName =
                tyConName tyCon
            trySymbol =
                case tcName of
                    '"' : cs ->
                        Just $ LitT (StrTyLit (zipWith const cs (drop 1 cs)))
                    _ ->
                        Nothing
            tryTicked =
                case tcName of
                    '\'' : dcName ->
                        let nameBase =
                                mkOccName dcName

                            flavor =
                                NameG
                                    DataName
                                    (mkPkgName $ tyConPackage tyCon)
                                    (mkModName $ tyConModule tyCon)
                            name =
                                Name
                                    nameBase
                                    flavor
                        in
                            Just (PromotedT name)
                    _ ->
                        Nothing
            tryNat =
                LitT . NumTyLit <$> readMaybe tcName
            plainType =
                let
                    nameBase =
                        mkOccName tcName
                    flavor =
                        NameG
                            TcClsName
                            (mkPkgName $ tyConPackage tyCon)
                            (mkModName $ tyConModule tyCon)
                    name =
                        Name
                            nameBase
                            flavor
                in
                    ConT name
        in fromMaybe plainType $ asum [tryTicked, trySymbol, tryNat]

-- | Convert a type argument into a Template Haskell 'Type'.
--
-- Use with @TypeApplications@.
--
-- Example:
--
-- @
-- >>> :set -XTypeApplications
-- >>> liftType \@Bool
-- ConT GHC.Types.Bool
-- >>> liftType \@[Char]
-- AppT (ConT GHC.Types.[]) (ConT GHC.Types.Char)
-- @
--
-- This works with data kinds, too.
--
-- @
-- >>> :set -XDataKinds
-- >>> liftType \@3
-- LitT (NumTyLit 3)
-- >>> liftType \@"hello"
-- LitT (StrTyLit "hello")
-- >>> liftType \@'[Int, Char]
-- AppT (AppT (PromotedT GHC.Types.:) (ConT GHC.Types.Int)) (AppT (AppT (PromotedT GHC.Types.:) (ConT GHC.Types.Char)) (PromotedT GHC.Types.[]))
-- >>> liftType \@'(Int, Char)
-- AppT (AppT (PromotedT GHC.Tuple.(,)) (ConT GHC.Types.Int)) (ConT GHC.Types.Char)
-- @
--
-- @since 0.1.0.0
liftType :: forall t. Typeable t => Type
liftType =
    go (typeRep @t)
  where
    go :: forall k (a :: k). TypeRep a -> Type
    go tr =
        case tr of
            Con tyCon ->
                mk tyCon
            Fun trA trB ->
                ConT ''(->) `AppT` go trA `AppT` go trB
            App trA trB ->
                AppT (go trA) (go trB)

    mk :: TyCon -> Type
    mk tyCon =
        let
            tcName =
                tyConName tyCon
            trySymbol =
                case tcName of
                    '"' : cs ->
                        Just $ LitT (StrTyLit (zipWith const cs (drop 1 cs)))
                    _ ->
                        Nothing
            tryTicked =
                case tcName of
                    '\'' : dcName ->
                        let nameBase =
                                mkOccName dcName

                            flavor =
                                NameG
                                    DataName
                                    (mkPkgName $ tyConPackage tyCon)
                                    (mkModName $ tyConModule tyCon)
                            name =
                                Name
                                    nameBase
                                    flavor
                        in
                            Just (PromotedT name)
                    _ ->
                        Nothing
            tryNat =
                LitT . NumTyLit <$> readMaybe tcName
            plainType =
                let
                    nameBase =
                        mkOccName tcName
                    flavor =
                        NameG
                            TcClsName
                            (mkPkgName $ tyConPackage tyCon)
                            (mkModName $ tyConModule tyCon)
                    name =
                        Name
                            nameBase
                            flavor
                in
                    ConT name
        in fromMaybe plainType $ asum [tryTicked, trySymbol, tryNat]
