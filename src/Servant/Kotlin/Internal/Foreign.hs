{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Kotlin.Internal.Foreign where

import           Data.Proxy          (Proxy (Proxy))
import           Servant.Foreign     (Foreign, GenerateList, HasForeign,
                                      HasForeignType, Req, listFromAPI, typeFor)
import           Servant.Kotlin.Type (KotlinClass, KotlinType, toKotlinType')

data LangKotlin

instance (KotlinType a) => HasForeignType LangKotlin KotlinClass a where
  typeFor _ _ _ = toKotlinType' (Proxy :: Proxy a)

getEndpoints ::
  ( HasForeign LangKotlin KotlinClass api
  , GenerateList KotlinClass (Foreign KotlinClass api))
  => Proxy api
  -> [Req KotlinClass]
getEndpoints =
  listFromAPI (Proxy :: Proxy LangKotlin) (Proxy :: Proxy KotlinClass)
