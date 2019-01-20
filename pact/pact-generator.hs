{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LazyByteString
import           Data.Text (Text)
import qualified Network.HTTP.Types as  HTTP
import           Pact

smith :: Pact
smith =
  Pact "smith" "smith-client" [
      Interaction "oauth2"
        (Request HTTP.POST "/oauth/token" Nothing)
        (Response HTTP.status200 [(HTTP.hContentType, "application/json")] . Just $
          Aeson.object [
              "access_token" .= ("test-token" :: Text)
            , "expires_in" .= (3600 :: Int)
            , "token_type" .= ("Bearer" :: Text)
            ])
     , Interaction "userinfo"
        (Request HTTP.GET "/userinfo" Nothing)
        (Response HTTP.status200 [(HTTP.hContentType, "application/json")] . Just $
          Aeson.object [
              "sub" .= ("2" :: Text)
            ])
     , Interaction "public-keys"
        (Request HTTP.GET "environment/public-keys/muppets" Nothing)
        (Response HTTP.status200 [(HTTP.hContentType, "application/json")] . Just $
          Aeson.object [
              "public-keys" .= [
                    ("ssh-rsa AAAA000000== example-key.pub" :: Text)
                  ]
            ])
     , Interaction "issue"
        (Request HTTP.POST "issue" Nothing)
        (Response HTTP.status200 [(HTTP.hContentType, "application/json")] . Just $
          Aeson.object [
              "certificate" .= ("ssh-rsa AAAA000000== example-key.pub" :: Text)
            ])
    ]

main :: IO ()
main =
  LazyByteString.writeFile "pact/json/smith.json" $
    serialise smith
