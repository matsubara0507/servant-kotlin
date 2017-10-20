{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Servant.Kotlin.Type
    ( KotlinClass (..)
    , KotlinPrimitiveClass (..)
    , KotlinExClass (..)
    , KotlinDataClass (..)
    , KotlinFields (..)
    , KotlinField (..)
    , KotlinType (..)
    , GenericKotlinType (..)
    , GenericKotlinFields (..)
    , toKotlinType'
    ) where

import           Data.Int     (Int16, Int32, Int64, Int8)
import           Data.IntMap  (IntMap)
import           Data.Map     (Map)
import           Data.Maybe   (fromMaybe)
import           Data.Proxy   (Proxy (..))
import           Data.Text    (Text, pack)
import           Data.Time    (UTCTime)
import           GHC.Generics
import           Servant.API  (NoContent (..))

data KotlinClass
  = PrimitiveClass KotlinPrimitiveClass
  | DataClass KotlinDataClass
  | ExClass KotlinExClass
  deriving (Show, Eq)

data KotlinPrimitiveClass
  = KDouble
  | KFloat
  | KLong
  | KInt
  | KShort
  | KByte
  | KChar
  | KBoolean
  | KArray KotlinClass
  | KString
  | KUnit
  | KNullable KotlinClass
  | KAny
  deriving (Show, Eq)

data KotlinExClass
  = KList KotlinClass
  | KHashMap KotlinClass KotlinClass
  | KPair KotlinClass KotlinClass
  | KTime
  deriving (Show, Eq)

data KotlinDataClass = KotlinDataClass Text KotlinFields deriving (Show, Eq)

data KotlinFields
  = Node KotlinField
  | Brunch KotlinFields KotlinFields
  deriving (Show, Eq)

data KotlinField = KotlinField Text KotlinClass deriving (Show, Eq)

class KotlinType a where
  toKotlinType :: a -> Maybe KotlinClass
  toKotlinType = genericToKotlinType . from
  default toKotlinType :: (Generic a, GenericKotlinType (Rep a)) =>
    a -> Maybe KotlinClass

class GenericKotlinType f where
  genericToKotlinType :: f a -> Maybe KotlinClass

instance (Datatype d, GenericKotlinFields f)
  => GenericKotlinType (D1 d f) where
  genericToKotlinType datatype = fmap DataClass $
    KotlinDataClass (pack $ datatypeName datatype)
      <$> genericToKotlinFields (unM1 datatype)

class GenericKotlinFields f where
  genericToKotlinFields :: f a -> Maybe KotlinFields

instance (Constructor c, GenericKotlinFields f)
  => GenericKotlinFields (C1 c f) where
  genericToKotlinFields constructor =
    if conIsRecord constructor then
      genericToKotlinFields (unM1 constructor)
    else
      Nothing

instance GenericKotlinFields (f :+: g) where
  genericToKotlinFields _ = Nothing

instance (Selector s, GenericKotlinType a)
  => GenericKotlinFields (S1 s a) where
  genericToKotlinFields selector =
    case selName selector of
      ""   -> Nothing
      name ->
        Node . KotlinField (pack name)
          <$> genericToKotlinType (undefined :: a p)

instance (GenericKotlinFields f, GenericKotlinFields g)
  => GenericKotlinFields (f :*: g) where
  genericToKotlinFields _ =
    Brunch <$> genericToKotlinFields (undefined :: f p)
           <*> genericToKotlinFields (undefined :: g p)

instance GenericKotlinFields U1 where
  genericToKotlinFields _ = Nothing

instance KotlinType a => GenericKotlinType (Rec0 a) where
  genericToKotlinType _ = toKotlinType (Proxy :: Proxy a)

instance KotlinType a => KotlinType [a] where
  toKotlinType _ = ExClass . KList <$> toKotlinType (Proxy :: Proxy a)

instance KotlinType a => KotlinType (Maybe a) where
  toKotlinType _ =
    PrimitiveClass . KNullable <$> toKotlinType (Proxy :: Proxy a)

instance KotlinType () where
  toKotlinType _ = Just $ PrimitiveClass KUnit

instance KotlinType Text where
  toKotlinType _ = Just $ PrimitiveClass KString

instance KotlinType UTCTime where
  toKotlinType _ = Just $ ExClass KTime

instance KotlinType Float where
  toKotlinType _ = Just $ PrimitiveClass KFloat

instance KotlinType Double where
  toKotlinType _ = Just $ PrimitiveClass KDouble

instance KotlinType Int where
  toKotlinType _ = Just $ PrimitiveClass KInt

instance KotlinType Int8 where
  toKotlinType _ = Just $ PrimitiveClass KByte

instance KotlinType Int16 where
  toKotlinType _ = Just $ PrimitiveClass KShort

instance KotlinType Int32 where
  toKotlinType _ = Just $ PrimitiveClass KInt

instance KotlinType Int64 where
  toKotlinType _ = Just $ PrimitiveClass KLong

instance KotlinType Char where
  toKotlinType _ = Just $ PrimitiveClass KChar

instance (KotlinType a, KotlinType b) => KotlinType (a, b) where
  toKotlinType _ = fmap ExClass $
    KPair
      <$> toKotlinType (Proxy :: Proxy a)
      <*> toKotlinType (Proxy :: Proxy b)

instance (KotlinType a) => KotlinType (Proxy a) where
  toKotlinType _ = toKotlinType (undefined :: a)

instance (KotlinType k, KotlinType v) => KotlinType (Map k v) where
  toKotlinType _ = fmap ExClass $
    KHashMap
      <$> toKotlinType (Proxy :: Proxy k)
      <*> toKotlinType (Proxy :: Proxy v)

instance (KotlinType v) => KotlinType (IntMap v) where
  toKotlinType _ =
    PrimitiveClass . KArray <$> toKotlinType (Proxy :: Proxy v)

instance KotlinType NoContent where
  toKotlinType _ = Just $ PrimitiveClass KUnit

toKotlinType' :: (KotlinType a) => a -> KotlinClass
toKotlinType' = fromMaybe (PrimitiveClass KAny) . toKotlinType
