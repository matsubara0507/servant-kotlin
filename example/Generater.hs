{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Text                   (Text, unpack)
import           GHC.Generics                (Generic)
import           Servant.API                 ((:<|>) (..), (:>), FormUrlEncoded,
                                              Get, JSON, Post, ReqBody)
import           Servant.Kotlin
import           Web.Internal.FormUrlEncoded (FromForm)

data Score = Score
  { textLength :: Int
  , clearTime  :: Int
  , swapCount  :: Int
  } deriving (Generic, Show, KotlinType)

instance FromJSON Score
instance ToJSON Score
instance FromForm Score

type CRUD = "scores" :> Get '[JSON] [Score]
       :<|> "scores" :> ReqBody '[JSON, FormUrlEncoded] Score :> Post '[JSON] Score


body :: [Text]
body = mconcat
  [ [ defKotlinImports ]
  , generateKotlinForAPIClass "ScoreAPI" $ mconcat
      [ generateKotlinForDefDataClass (Proxy :: Proxy Score)
      , generateKotlinForAPI (Proxy :: Proxy CRUD)
      ]
  ]

spec :: Spec
spec = Spec ["com", "github", "matsubara0507"] "ScoreAPI" body

main :: IO ()
main = specsToDir [spec] "example"
