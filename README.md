# Servant Kotlin

Generate Kotlin class to query your Servant API!

## Example

see [`example/Generater.hs`](/example/Generater.hs).

this file generate [`TodoAPI.kt`](/example/com/github/matsubara0507/TodoAPI.kt).

## dependencies for Kotlin

- [kittinunf/Fuel](https://github.com/kittinunf/Fuel): The easiest HTTP networking library for Kotlin/Android
- [google/gson](https://github.com/google/gson): A Java serialization/deserialization library to convert Java Objects into JSON and back

## not yet, no Implements

- [ ] Original QueryFlag
- [ ] Original Header

## Acknowledgement

This package is greatly inspired by [`elm-export`](https://hackage.haskell.org/package/elm-export) and [`servant-elm`](https://hackage.haskell.org/package/servant-elm).
