# Servant Kotlin

[![Hackage](https://img.shields.io/hackage/v/servant-kotlin.svg?style=flat)](https://hackage.haskell.org/package/servant-kotlin)
[![Build Status](https://travis-ci.org/matsubara0507/servant-kotlin.svg?branch=master)](https://travis-ci.org/matsubara0507/servant-kotlin)
[![Stackage LTS](http://stackage.org/package/servant-kotlin/badge/lts)](http://stackage.org/lts/package/servant-kotlin)
[![Stackage Nightly](http://stackage.org/package/servant-kotlin/badge/nightly)](http://stackage.org/nightly/package/servant-kotlin)

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
