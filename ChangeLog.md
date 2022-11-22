# Changelog for lift-typeable

## 0.1.1.0

- Cleanup and a slight performance improvement [#7](https://github.com/parsonsmatt/lift-type/pull/7)
- Implement `typeRepToType :: SomeTypeRep -> Type` [#8](https://github.com/parsonsmatt/lift-type/pull/8)

## 0.1.0.1

- Support GHC 8.2.2, which evidently required `TypeInType` for the `forall k (a :: k)` signature.

## 0.1.0.0

- Initial release
