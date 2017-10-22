module Servant.Kotlin
       ( generateKotlinForDefDataClass
       , defKotlinImports
       , generateKotlinForAPI
       , generateKotlinForAPIWith
       , generateKotlinForAPIClass
       , KotlinOptions(..)
       , UrlPrefix(..)
       , defKotlinOptions
       -- * Convenience re-exports from the "Kotlin" module
       , KotlinType
       -- * Convenience re-exports from "Data.Proxy"
       , Proxy(Proxy)
       ) where

import           Servant.Kotlin.Internal.Generate (KotlinOptions (..),
                                                   UrlPrefix (..),
                                                   defKotlinImports,
                                                   defKotlinOptions,
                                                   generateKotlinForAPI,
                                                   generateKotlinForAPIClass,
                                                   generateKotlinForAPIWith,
                                                   generateKotlinForDefDataClass)

import           Data.Proxy                       (Proxy (Proxy))
import           Servant.Kotlin.Type              (KotlinType)
