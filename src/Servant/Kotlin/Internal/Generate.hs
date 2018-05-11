{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.Kotlin.Internal.Generate
    ( GenerateKotlin (..)
    , generateKotlinForDefDataClass
    , generateKotlinForDefDataClass'
    , defKotlinImports
    , generateKotlinForAPIClass
    , generateKotlinForAPI
    , generateKotlinForAPIWith
    , KotlinOptions (..)
    , defKotlinOptions
    , UrlPrefix (..)
    ) where

import           Control.Lens                    (to, (^.))
import           Data.List                       (nub)
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.Monoid                     ((<>))
import           Data.Proxy                      (Proxy)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.Lazy                  as L
import           Servant.API                     (NoContent (..))
import qualified Servant.Foreign                 as F
import           Servant.Kotlin.Internal.Foreign (LangKotlin, getEndpoints)
import           Servant.Kotlin.Type
import           Text.PrettyPrint.Leijen.Text    (Doc, (<+>))
import qualified Text.PrettyPrint.Leijen.Text    as PP

class GenerateKotlin a where
  generateKotlin :: a -> [Text]
  generateKotlin' :: a -> Text
  generateKotlin' = T.concat . generateKotlin

instance GenerateKotlin KotlinClass where
  generateKotlin (PrimitiveClass c)                   = generateKotlin c
  generateKotlin (ExClass c)                          = generateKotlin c
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
  generateKotlin KAny          = ["Any"]

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

generateKotlinForDefDataClass' :: KotlinClass -> [Text]
generateKotlinForDefDataClass' (DataClass c) = generateKotlin c
generateKotlinForDefDataClass' _             = []

generateKotlinForDefDataClass :: (KotlinType a) => Proxy a -> [Text]
generateKotlinForDefDataClass =
  maybe [] generateKotlinForDefDataClass' . toKotlinType

---

defKotlinImports :: Text
defKotlinImports = docToText . PP.vsep $ fmap ("import" <+>)
  [ "com.github.kittinunf.fuel.Fuel"
  , "com.github.kittinunf.fuel.core.FuelError"
  , "com.github.kittinunf.fuel.core.FuelManager"
  , "com.github.kittinunf.fuel.core.Request"
  , "com.github.kittinunf.fuel.core.Response"
  , "com.github.kittinunf.fuel.gson.responseObject"
  , "com.github.kittinunf.result.Result"
  , "com.google.gson.Gson"
  ]

---

generateKotlinForAPIClass :: Text -> [Text] -> [Text]
generateKotlinForAPIClass className body = mconcat
  [ [ docToText $ "class" <+> PP.textStrict className <> "(private val baseURL: String) {" ]
  , [ docToText $ PP.indent indentNum initialize ]
  , fmap (docToText . PP.vsep . fmap (PP.indent indentNum . PP.textStrict) . T.lines) body
  , [ "}" ]
  ]
  where
    initialize = PP.vsep [ "init {", PP.indent indentNum fuelManager, "}" ]
    fuelManager = PP.vsep
      [ "FuelManager.instance.apply {"
      , PP.indent indentNum "basePath = baseURL"
      , PP.indent indentNum $ "baseHeaders = mapOf(" <> header <> ")"
      , "}"
      ]
    header = PP.hsep . PP.punctuate PP.comma $
      fmap (\(k, v) -> PP.dquotes k <+> "to" <+> PP.dquotes v)
        [("Content-Type", "application/json"), ("Device", "Android")]

---

{-|
  Generate Kotlin code for the API with default options.
  Returns a list of Kotlin functions to query your Servant API from Kotlin.
-}
generateKotlinForAPI ::
  ( F.HasForeign LangKotlin KotlinClass api
  , F.GenerateList KotlinClass (F.Foreign KotlinClass api))
  => Proxy api
  -> [Text]
generateKotlinForAPI =
  generateKotlinForAPIWith defKotlinOptions

{-|
  Generate Kotlin code for the API with custom options.
-}
generateKotlinForAPIWith ::
  ( F.HasForeign LangKotlin KotlinClass api
  , F.GenerateList KotlinClass (F.Foreign KotlinClass api))
  => KotlinOptions
  -> Proxy api
  -> [Text]
generateKotlinForAPIWith opts =
  nub . fmap (docToText . generateKotlinForRequest opts) . getEndpoints

indentNum :: Int
indentNum = 4

{-|
  Generate an Kotlin function for one endpoint.
-}
generateKotlinForRequest :: KotlinOptions -> F.Req KotlinClass -> Doc
generateKotlinForRequest opts request = funcDef
  where
    funcDef =
      PP.vsep
        [ "fun" <+> fnName <> "(" <> args <> ") {"
        , PP.indent indentNum kotlinRequest
        , "}"
        ]

    fnName =
      request ^. F.reqFuncName . to (T.replace "-" "" . F.camelCase) . to stext

    args =
      mkArgs opts request

    kotlinRequest =
      mkRequest opts request

mkArgs :: KotlinOptions -> F.Req KotlinClass -> Doc
mkArgs opts request =
  (PP.hsep . PP.punctuate PP.comma . concat)
    [ urlPrefixArg
    , headerArgs
    , urlCaptureArgs
    , queryArgs
    , requestBodyArg
    , handlerArg
    ]
  where
    urlPrefixArg :: [Doc]
    urlPrefixArg =
      case urlPrefix opts of
        Dynamic  -> ["urlBase: String"]
        Static _ -> []

    headerArgs :: [Doc]
    headerArgs =
      [ kotlinHeaderArg header <> ": " <> kotlinHeaderType header
      | header <- request ^. F.reqHeaders
      ]

    urlCaptureArgs :: [Doc]
    urlCaptureArgs =
      [ kotlinCaptureArg segment <> ": " <> kotlinCaptureType segment
      | segment <- request ^. F.reqUrl . F.path, F.isCapture segment
      ]

    queryArgs :: [Doc]
    queryArgs =
      [ kotlinQueryArg arg  <> ": " <>  kotlinQueryType arg
      | arg <- request ^. F.reqUrl . F.queryStr
      ]

    requestBodyArg :: [Doc]
    requestBodyArg =
      maybe [] (\body -> [kotlinBodyArg <> ": " <> kotlinTypeRef body]) $
        request ^. F.reqBody

    handlerArg :: [Doc]
    handlerArg = [kotlinHandlerArg <> ": " <> handlerType]
      where
        handlerType =
          "(Request, Response, Result<" <> returnType <> ", FuelError>) -> Unit"

    returnType :: Doc
    returnType = kotlinTypeRef .
      fromMaybe (PrimitiveClass KUnit) $ request ^. F.reqReturnType

kotlinHeaderArg :: F.HeaderArg KotlinClass -> Doc
kotlinHeaderArg header = "header_" <>
  header ^. F.headerArg . F.argName
    . to (stext . T.replace "-" "_" . F.unPathSegment)

kotlinHeaderType :: F.HeaderArg KotlinClass -> Doc
#if MIN_VERSION_servant_foreign(0,11,0)
kotlinHeaderType header =
  header ^. F.headerArg . F.argType . to kotlinTypeRef
#else
kotlinHeaderType header =
  header ^. F.headerArg . F.argType . to (kotlinTypeRef . wrapper)
  where
    wrapper = PrimitiveClass . KNullable
#endif

kotlinCaptureArg :: F.Segment KotlinClass -> Doc
kotlinCaptureArg segment = "capture_" <>
  F.captureArg segment ^. F.argName . to (stext . F.unPathSegment)

kotlinCaptureType :: F.Segment KotlinClass -> Doc
kotlinCaptureType segment =
  F.captureArg segment ^. F.argType . to kotlinTypeRef

kotlinQueryArg :: F.QueryArg KotlinClass -> Doc
kotlinQueryArg arg = "query_" <>
  arg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)

kotlinQueryType :: F.QueryArg KotlinClass -> Doc
#if MIN_VERSION_servant_foreign(0,11,0)
kotlinQueryType arg =
  arg ^. F.queryArgName . F.argType . to kotlinTypeRef
#else
kotlinQueryType arg =
  arg ^. F.queryArgName . F.argType . to (kotlinTypeRef . wrapper)
  where
    wrapper = case arg ^. F.queryArgType of
      F.Normal -> PrimitiveClass . KNullable
      _        -> id
#endif

kotlinBodyArg :: Doc
kotlinBodyArg = "body"

kotlinHandlerArg :: Doc
kotlinHandlerArg = "handler"

kotlinTypeRef :: KotlinClass -> Doc
kotlinTypeRef = stext . generateKotlin'


mkRequest :: KotlinOptions -> F.Req KotlinClass -> Doc
mkRequest opts request = "Fuel" <> PP.align (PP.vsep methodChain)
  where
    methodChain = catMaybes
      [ Just $ mconcat [".", method, "(", url, ")"]
      , body
      , Just ".responseObject(handler)"
      ]

    method =
      request ^. F.reqMethod . to (stext . T.toLower . T.decodeUtf8)

    url =
      mkUrl opts (request ^. F.reqUrl . F.path) <> mkQueryParams request

    body = fmap (\b -> mconcat
        [ ".body(Gson().toJson("
        , kotlinBodyArg
        , ", "
        , kotlinTypeRef b
        , "::class.java))"
        ]
      ) $ request ^. F.reqBody


mkUrl :: KotlinOptions -> [F.Segment KotlinClass] -> Doc
mkUrl _opts segments = mconcat . PP.punctuate " + " $
  PP.dquotes "/" :
    PP.punctuate (" + " <> PP.dquotes "/") (map segmentToDoc segments)
  where
    segmentToDoc :: F.Segment KotlinClass -> Doc
    segmentToDoc segment =
      case F.unSegment segment of
        F.Static path -> PP.dquotes (stext (F.unPathSegment path))
        F.Cap _arg    -> kotlinCaptureArg segment

-- TODO: implements
mkQueryParams :: F.Req KotlinClass -> Doc
mkQueryParams _request = ""
  -- if null (request ^. F.reqUrl . F.queryStr) then
  --   empty
  -- else
  --   " +" <+> dquotes "?" <+> "+" <+>
  --     "params.joinToString(" <> dquotes "&" <> ")"

{-|
  Options to configure how code is generated.
-}
data KotlinOptions = KotlinOptions
  { urlPrefix                :: UrlPrefix
  , emptyResponseKotlinTypes :: [KotlinClass]
    -- ^ Types that represent an empty Http response.
  , stringKotlinTypes        :: [KotlinClass]
    -- ^ Types that represent a String.
  }

data UrlPrefix
  = Static Text
  | Dynamic

{-|
  Default options for generating Kotlin code.
  The default options are:
  > { urlPrefix                = Static ""
  > , emptyResponseKotlinTypes = [ toKotlinType NoContent ]
  > , stringKotlinTypes        = [ toKotlinType "" ]
  > }
-}
defKotlinOptions :: KotlinOptions
defKotlinOptions = KotlinOptions
  { urlPrefix = Static ""
  , emptyResponseKotlinTypes =
      [ toKotlinType' NoContent
      , toKotlinType' ()
      ]
  , stringKotlinTypes =
      [ toKotlinType' ("" :: String)
      , toKotlinType' ("" :: Text)
      ]
  }

---

docToText :: Doc -> Text
docToText =
  L.toStrict . PP.displayT . PP.renderPretty 0.4 100

stext :: Text -> Doc
stext = PP.text . L.fromStrict
