{-# LANGUAGE OverloadedStrings #-}

module Servant.Kotlin.Internal.Generate
    ( GenerateKotlin (..)
    , generateDefKotlinDataClass
    , generateDefKotlinDataClass'
    ) where

import Servant.Kotlin.Type
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

class GenerateKotlin a where
  generateKotlin :: a -> [Text]
  generateKotlin' :: a -> Text
  generateKotlin' = Text.concat . generateKotlin

instance GenerateKotlin KotlinClass where
  generateKotlin (PrimitiveClass c) = generateKotlin c
  generateKotlin (ExClass c) = generateKotlin c
  generateKotlin (DataClass (KotlinDataClass name _)) = [name]

instance GenerateKotlin KotlinPrimitiveClass where
  generateKotlin KDouble       = ["Double"]
  generateKotlin KFloat        = ["Float"]
  generateKotlin KLong         = ["Long"]
  generateKotlin KInt          = ["Int"]
  generateKotlin KShort        = ["Short"]
  generateKotlin KByte         = ["Byte"]
  generateKotlin KChar         = ["Char"]
  generateKotlin KBoolean      = ["Boolean"]
  generateKotlin (KArray c)    = ["Array<" <> generateKotlin' c <> ">"]
  generateKotlin KString       = ["String"]
  generateKotlin KUnit         = ["Unit"]
  generateKotlin (KNullable c) = [generateKotlin' c <> "?"]

instance GenerateKotlin KotlinExClass where
  generateKotlin (KList c) =
    ["List<" <> generateKotlin' c <> ">"]
  generateKotlin (KHashMap k v) =
    ["HashMap<" <> generateKotlin' k <> ", " <> generateKotlin' v <> ">"]
  generateKotlin (KPair a b) =
    ["Pair<" <> generateKotlin' a <> ", " <> generateKotlin' b <> ">"]
  generateKotlin KTime = ["Time"]

instance GenerateKotlin KotlinDataClass where
  generateKotlin (KotlinDataClass name fields) =
    [ "data class " <> name <> "(" <> generateKotlin' fields <> ")" ]

instance GenerateKotlin KotlinFields where
  generateKotlin (Node field) = generateKotlin field
  generateKotlin (Brunch a b) = [generateKotlin' a <> ", " <> generateKotlin' b]

instance GenerateKotlin KotlinField where
  generateKotlin (KotlinField name c) =
    ["val " <> name <> ": " <> generateKotlin' c]

generateDefKotlinDataClass :: KotlinClass -> Text
generateDefKotlinDataClass (DataClass c) = generateKotlin' c
generateDefKotlinDataClass _ = ""

generateDefKotlinDataClass' :: (KotlinType a) => a -> Text
generateDefKotlinDataClass' =
  maybe "" generateDefKotlinDataClass . toKotlinType
