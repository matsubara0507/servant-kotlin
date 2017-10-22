{-# LANGUAGE OverloadedStrings #-}

module Servant.Kotlin.Internal.File
    ( Spec (..)
    , specsToDir
    ) where

import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack, unpack)
import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text
import           Formatting       as F
import           System.Directory (createDirectoryIfMissing)

makePath :: [Text] -> Text
makePath = Text.intercalate "/"

data Spec = Spec
  { namespace    :: [Text]
  , filename     :: Text
  , declarations :: [Text]
  } deriving (Show)

pathForSpec :: FilePath -> Spec -> [Text]
pathForSpec rootDir spec = pack rootDir : namespace spec <> [filename spec]

ensureDirectory :: FilePath -> Spec -> IO ()
ensureDirectory rootDir spec = createDirectoryIfMissing True $ unpack dir
  where
    dir = makePath . init $ pathForSpec rootDir spec

specToFile :: FilePath -> Spec -> IO ()
specToFile rootDir spec = do
  fprint ("Writing: " % F.stext % "\n") file
  Text.writeFile (unpack file) body
  where
    path = pathForSpec rootDir spec
    file = makePath path <> ".kt"
    namespaceText = Text.intercalate "." (namespace spec)
    body = Text.intercalate "\n\n" $
             "package " <> namespaceText : "" : declarations spec

specsToDir :: [Spec] -> FilePath -> IO ()
specsToDir specs rootDir = mapM_ processSpec specs
  where
    processSpec = ensureDirectory rootDir >> specToFile rootDir
