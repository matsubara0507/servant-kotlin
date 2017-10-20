module Servant.Kotlin
       ( generateKotlinForAPI
       , generateKotlinForAPIWith
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
                                                   defKotlinOptions,
                                                   generateKotlinForAPI,
                                                   generateKotlinForAPIWith)

import           Data.Proxy                       (Proxy (Proxy))
import           Servant.Kotlin.Type              (KotlinType)
