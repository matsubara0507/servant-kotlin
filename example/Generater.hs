{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Servant.API                 ((:<|>) (..), (:>), Capture,
                                              Delete, FormUrlEncoded, Get, JSON,
                                              Post, Put, ReqBody)
import           Servant.Kotlin
import           Shelly                      (cd, run_, shelly, which)
import           Web.Internal.FormUrlEncoded (FromForm)

data Todo = Todo
  { todoId :: Int
  , title  :: Text
  , done   :: Bool
  } deriving (Generic, Show, Eq, KotlinType)

instance FromJSON Todo
instance ToJSON Todo
instance FromForm Todo

type CRUD = "todos" :> Get '[JSON] [Todo]
       :<|> "todos" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] Todo
       :<|> "todos" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] ()
       :<|> "todos" :> Capture "id" Int :> Delete '[JSON] ()

body :: [Text]
body = mconcat
  [ [ defKotlinImports ]
  , generateKotlinForAPIClass "TodoAPI" $ mconcat
      [ generateKotlinForDefDataClass (Proxy :: Proxy Todo)
      , generateKotlinForAPI (Proxy :: Proxy CRUD)
      ]
  ]

spec :: Spec
spec = Spec ["com", "github", "matsubara0507"] "TodoAPI" body

main :: IO ()
main = do
  specsToDir [spec] "example/src/main/java"
  shelly $ do
    cd "example"
    which "gradle" >>= mapM_ (\_ -> run_ "gradle" ["build"])
