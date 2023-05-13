{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ipe.Github (fetchPrelude) where

import Control.Monad (foldM)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as Req
import qualified System.Exit
import Text.URI (URI)
import qualified Text.URI as URI

data GithubContent
  = ContentFile GithubFile
  | ContentDirectory GithubDirectory
  deriving (Show)

instance Aeson.FromJSON GithubContent where
  parseJSON = Aeson.withObject "GithubContent" $ \o -> do
    contentType <- o .: "type"

    case (contentType :: Text) of
      "file" -> ContentFile <$> Aeson.parseJSON (Aeson.Object o)
      "dir" -> ContentDirectory <$> Aeson.parseJSON (Aeson.Object o)
      _ -> fail "Invalid GithubContent"

data GithubFile = GithubFile
  { fileName :: Text,
    fileDownloadUrl :: URI
  }
  deriving (Show)

instance Aeson.FromJSON GithubFile where
  parseJSON = Aeson.withObject "GithubFile" $ \o ->
    GithubFile
      <$> (o .: "name")
      <*> (o .: "download_url")

data GithubDirectory = GithubDirectory
  { dirName :: Text,
    dirUrl :: URI
  }
  deriving (Show)

instance Aeson.FromJSON GithubDirectory where
  parseJSON = Aeson.withObject "GithubDirectory" $ \o ->
    GithubDirectory
      <$> (o .: "name")
      <*> (o .: "url")

instance Aeson.FromJSON URI where
  parseJSON = Aeson.withText "URI" $ \t ->
    case URI.mkURI t of
      Left err -> fail $ show err
      Right uri -> return uri

fetchPrelude :: Maybe Text -> IO [([Text], Text, ByteString)]
fetchPrelude preludeBranch =
  fetchDir
    []
    (Req.https "api.github.com" /: "repos" /: "henriquecbuss" /: "ipe" /: "contents" /: "src" /: "Ipe" /: "Prelude")
    ( case preludeBranch of
        Nothing -> mempty
        Just branch -> "ref" =: branch
    )

fetchDir :: [Text] -> Req.Url Req.Https -> Req.Option Req.Https -> IO [([Text], Text, ByteString)]
fetchDir currentPath uri options = do
  root <-
    Req.runReq Req.defaultHttpConfig $
      Req.req
        Req.GET
        uri
        Req.NoReqBody
        Req.jsonResponse
        ( options
            <> Req.header "User-Agent" "Ipe-Compiler"
        )

  let response = Req.responseBody root :: [GithubContent]

  foldM
    ( \acc content -> case content of
        ContentFile file -> do
          if Text.isSuffixOf ".ipe.js" (fileName file) || Text.isSuffixOf ".d.ts" (fileName file)
            then do
              contents <- getFileContents file
              return $ (currentPath, fileName file, contents) : acc
            else return acc
        ContentDirectory dir -> do
          let maybeUri = Req.useHttpsURI (dirUrl dir)

          case maybeUri of
            Nothing -> System.Exit.exitFailure
            Just (dirUri, _) -> do
              contents <- fetchDir (currentPath ++ [dirName dir]) dirUri options
              return (contents ++ acc)
    )
    []
    response

getFileContents :: GithubFile -> IO ByteString
getFileContents (GithubFile _ downloadUrl) = do
  let maybeUri = Req.useHttpsURI downloadUrl

  case maybeUri of
    Nothing -> System.Exit.exitFailure
    Just (uri, _) ->
      Req.responseBody
        <$> Req.runReq
          Req.defaultHttpConfig
          ( Req.req
              Req.GET
              uri
              Req.NoReqBody
              Req.bsResponse
              (Req.header "User-Agent" "Ipe-Compiler")
          )
