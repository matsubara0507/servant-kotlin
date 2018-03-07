{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.Kotlin.Internal.GenerateSpec
    ( main
    , spec
    , Todo (..)
    ) where

import           Data.Proxy                       (Proxy (Proxy))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Servant.Kotlin.Internal.Generate
import           Servant.Kotlin.Type
import           Test.Hspec                       (Spec, describe, hspec, it,
                                                   shouldBe)
import           Test.TestAPI

data Todo = Todo
  { todoId :: Int
  , title  :: Text
  , done   :: Bool
  } deriving (Generic, Show, Eq, KotlinType)

data Hoge
  = Hoge1 Int
  | Hoge2 Bool
  deriving (Generic, Show, Eq, KotlinType)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "generateKotlinForDefDataClass :: Proxy a -> [Text]" $ do
    it "sample Todo type" $
      generateKotlinForDefDataClass (Proxy :: Proxy Todo) `shouldBe`
        [ "data class Todo(val todoId: Int, val title: String, val done: Boolean)" ]
    it "not data class, e.g. Int" $
      generateKotlinForDefDataClass (Proxy :: Proxy Int) `shouldBe` []
    it "return Nothing e.g. Product Type" $
      generateKotlinForDefDataClass (Proxy :: Proxy Hoge) `shouldBe` []

  describe "generateKotlinForAPIClass :: Text -> [Text] -> [Text]" $ do
    it "body is empty" $
      generateKotlinForAPIClass "TodoAPI" [] `shouldBe`
        [ "class TodoAPI(private val baseURL: String) {"
        , T.intercalate "\n"
            [ "    init {"
            , "        FuelManager.instance.apply {"
            , "            basePath = baseURL"
            , "            baseHeaders = mapOf(\"Content-Type\" to \"application/json\", \"Device\" to \"Android\")"
            , "        }"
            , "    }"
            ]
        , "}"
        ]
    it "body is non empty" $
      generateKotlinForAPIClass "TodoAPI" ["aaa", "bbb\nccc"] `shouldBe`
        [ "class TodoAPI(private val baseURL: String) {"
        , T.intercalate "\n"
            [ "    init {"
            , "        FuelManager.instance.apply {"
            , "            basePath = baseURL"
            , "            baseHeaders = mapOf(\"Content-Type\" to \"application/json\", \"Device\" to \"Android\")"
            , "        }"
            , "    }"
            ]
        , "    aaa"
        , T.intercalate "\n"
            [ "    bbb"
            , "    ccc"
            ]
        , "}"
        ]

  describe "generateKotlinForAPI :: Proxy api -> [Text]" $
    it "sample CRUD" $
      generateKotlinForAPI (Proxy :: Proxy TestApi) `shouldBe`
        [ T.intercalate "\n"
            [ "fun getOne(handler: (Request, Response, Result<Int, FuelError>) -> Unit) {"
            , "    Fuel.get(\"/\" + \"one\")"
            , "        .responseObject(handler)"
            , "}"
            ]
        , T.intercalate "\n"
            [ "fun postTwo(body: String, handler: (Request, Response, Result<Int?, FuelError>) -> Unit) {"
            , "    Fuel.post(\"/\" + \"two\")"
            , "        .body(Gson().toJson(body, String::class.java))"
            , "        .responseObject(handler)"
            , "}"
            ]
        , T.intercalate "\n"
            [ "fun getBooksById(capture_id: Int, handler: (Request, Response, Result<Book, FuelError>) -> Unit) {"
            , "    Fuel.get(\"/\" + \"books\" + \"/\" + capture_id)"
            , "        .responseObject(handler)"
            , "}"
            ]
        , T.intercalate "\n"
            [ "fun getBooksByTitle(capture_title: String, handler: (Request, Response, Result<Book, FuelError>) -> Unit) {"
            , "    Fuel.get(\"/\" + \"books\" + \"/\" + capture_title)"
            , "        .responseObject(handler)"
            , "}"
            ]
        , T.intercalate "\n"
            [ "fun getBooks(query_published: Boolean, query_sort: String?, query_year: Int?, query_filters: List<Boolean?>, handler: (Request, Response, Result<List<Book>, FuelError>) -> Unit) {"
            , "    Fuel.get(\"/\" + \"books\")"
            , "        .responseObject(handler)"
            , "}"
            ]
        , T.intercalate "\n"
            [ "fun postBooks(body: Book, handler: (Request, Response, Result<Unit, FuelError>) -> Unit) {"
            , "    Fuel.post(\"/\" + \"books\")"
            , "        .body(Gson().toJson(body, Book::class.java))"
            , "        .responseObject(handler)"
            , "}"
            ]
        , T.intercalate "\n"
            [ "fun getNothing(handler: (Request, Response, Result<Unit, FuelError>) -> Unit) {"
            , "    Fuel.get(\"/\" + \"nothing\")"
            , "        .responseObject(handler)"
            , "}"
            ]
        , T.intercalate "\n"
            [ "fun putNothing(handler: (Request, Response, Result<Unit, FuelError>) -> Unit) {"
            , "    Fuel.put(\"/\" + \"nothing\")"
            , "        .responseObject(handler)"
            , "}"
            ]
        , T.intercalate "\n"
            [ "fun getWithaheader(header_myTextHeader: String?, header_MyIntHeader: Int?, handler: (Request, Response, Result<String, FuelError>) -> Unit) {"
            , "    Fuel.get(\"/\" + \"with-a-header\")"
            , "        .responseObject(handler)"
            , "}"
            ]
        ]
