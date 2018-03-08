{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module Test.TestAPI where

import           Data.Aeson          (ToJSON)
import           Data.Proxy          (Proxy (Proxy))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Servant.API         ((:<|>), (:>), Capture, Get, GetNoContent,
                                      Header, JSON, NoContent, Post,
                                      PostNoContent, Put, QueryFlag, QueryParam,
                                      QueryParams, ReqBody)
import           Servant.Kotlin.Type (KotlinType)

newtype Book = Book
    { title :: Text
    } deriving (Generic, KotlinType, Show, Eq)

instance ToJSON Book

type TestApi =
       "one"
         :> Get '[JSON] Int
  :<|> "two"
         :> ReqBody '[JSON] Text
         :> Post '[JSON] (Maybe Int)
  :<|> "books"
         :> Capture "id" Int
         :> Get '[JSON] Book
  :<|> "books"
         :> Capture "title" Text
         :> Get '[JSON] Book
  :<|> "books"
         :> QueryFlag "published"
         :> QueryParam "sort" Text
         :> QueryParam "year" Int
         :> QueryParams "filters" (Maybe Bool)
         :> Get '[JSON] [Book]
  :<|> "books"
         :> ReqBody '[JSON] Book
         :> PostNoContent '[JSON] NoContent
  :<|> "nothing"
         :> GetNoContent '[JSON] NoContent
  :<|> "nothing"
         :> Put '[JSON] () -- old way to specify no content
  :<|> "with-a-header"
         :> Header "myTextHeader" Text
         :> Header "MyIntHeader" Int
         :> Get '[JSON] Text

testApi :: Proxy TestApi
testApi = Proxy
