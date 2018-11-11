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
        (Request HTTP.GET "/environment/public-keys/muppets" Nothing)
        (Response HTTP.status200 [(HTTP.hContentType, "application/json")] . Just $
          Aeson.object [
              "public-keys" .= [
                    ("ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDH91hUsWDGHT7Ry6qoSS2HDWGL0NmWqOaTKSEqCnWf/4Vhf8X9t2sV7ADuRhP1UVXpHZHz2KL7yBk1ZXHUsL6zvAy1edjksJX3R3mpFOaEqh7TJGXWWRFKYpR69BIHVYS6gB9+GXXl5lz9fU6tKpSL50agYRgIcyGX8osTLUmQDEFyt9Hvi+qg3URuD8Bc8yeng9Kbu0r02Rcb07yHTPJFWbJDe1lY+vNLiH8PiuIu4Xk2qyBZl3NeBVUy3N0X+WCjNHk7ir/FnHp0NR21eGm3m0uDKmxhZgpY4X54Mnu3kZGJKILfq+7oZ33AZ+OiQqLhB8FGoiW7zIGM2kbRDVKR example-ca@example.com" :: Text)
                  ]
            ])
     , Interaction "issue"
        (Request HTTP.POST "/issue" Nothing)
        (Response HTTP.status200 [(HTTP.hContentType, "application/json")] . Just $
          Aeson.object [
              "certificate" .= ("ssh-rsa-cert-v01@openssh.com AAAAHHNzaC1yc2EtY2VydC12MDFAb3BlbnNzaC5jb20AAAAgQfzyYQgA3tOHCdUTOX/a/CFBmYxXewTDSfOt8Y9E4AEAAAADAQABAAABAQDJRDZnxY1ywG2x5Mn2Ndlr7noCDwuKJPSVCy9LHNPauvPubXNVLDBV9ygkUphP+A9rtPQQ6ziGrG0NkhlGQ8beSfLq+MF283EnnhAnxnzqyvFQFh0seHo1a+MOgGcUliLbQEKE5uzt4An1epxhRyF71DmnNtASUh2TraTHcBqWCejr6INzv3dy4QeaDIUUI/C3xWqlDev+IFszTHS/2w3WPPMaJQUIeHN4MxUx+lDDIIIIagF03KM+k3AGZn+4j8WgNp916gjKXBvzX3gE11BBRNojq1N0uICDBYl5Si6uQzt46PLB0IojQ1zpQQREX0GKdd40Ar9ZuXOQlcLnBEs7AAAAAAAAAAAAAAABAAAACXNsaW5nc2hvdAAAAAgAAAAEcm9vdAAAAABcQ+KYAAAAAFxFNBgAAAAAAAAAEgAAAApwZXJtaXQtcHR5AAAAAAAAAAAAAAIXAAAAB3NzaC1yc2EAAAADAQABAAACAQCkXksJuOJZ57vi5m6ZnMOV09ANgbEscHUmGEsqu9xKXNuY+kOtUzbeHwAdJx8KAKmA93Bx5yXX1nPLuPIjv9PeaC5IT8BIx+9nXHe8r+Nk8hISFRl9k8btSFBXMO87GB+jp0nyPNsbUXDcKKneZa7Y5pDSIldiFEwXHzVO2hRrAT9m6jQ3CscZjR4IQMtdYLG5Occ6d9YpbBi4tE53e/He9IyY26QW87qsT1NrFMa0c0QpeZk2jY6FOrwQE5NfxkDc5Qd2HHepgParxkR/xyzPUqxKNpKC/lKvQIMSK6rsC0nPF2lH+NdHXN0QVUyiA1dyQoo0gGVmup9XKed9yq55zZO51NEnrpPbplcIvSZ6lhE8icJDcCdDnUv2U4HSE1+2yoTRDXcbyqq2rpo3QbGE16fFv8ZNGxbLxoAiENQ7oFbHwRX3kuL5DG6mrZ/BzEljzymccbbJ9RSNYMJ6UknZNTyWHl0IKfjW/HJw3lYAKgWsnhzviRZs3z+iv2P9vkLt1cgzREwie6iUdD3+Gba9m0wIiKEELH3WILxKEjF9qi7fVzyYbXKT8C0QUYoLXHBqu5eWPmGIx0Yoh3LItVKX0wIhhTaD5YwMyqwr0XIY3y7ziGCE/r5vDIx9iOWpb+eb2B04+xx5gQ8ESQMVQC7IHKOacnHzlpvehIClogIW7QAAAg8AAAAHc3NoLXJzYQAAAgB8d+QPH4xXbupLQCp4eqAnnXEJ/sGTD2MHo1w/7GtfulskMzW3nzGPh4gAdBdDXsTV11ubKzSx1JL7wqaoMS/z86zFP5h+lkRmkiEglb2iw0H6MMSd0qGqM8Us1SI6Bxg1P16AQBjdWY0/+oLhnsj58sxELx0WkMK/iCq2pJRPicXiU9hXc5mIx3wUi2x3DotwMCJ6p+YGm9j2Zkl1aLXIfCfCVGJ5MuO0SS3vQ3AVto5Xuhe4OGK+LCPPPCKHFRSF1m8l1j2RWO3bC3C8AHl7S0Pn8A4HYhkdHreBoNJtFmj9aGIw+6zgZ0mRzOZL0cunkhbe84WwVab5ZU8ZWIgkRRsnVVsaNnejzAg+fT+Q644D0GQq5peQuCCPD+hMC82UUq7wbSKvy7vQYqNpNtigfVJE/cReuKUt3OuQYkIe7uTkTYpTjyJa31GNR1RDdB8qYjjywUBk9S2b5AkVhdbOz5d9BV+pOg37dpWHSlZSXQ0rHevYNvuJSkK5+XM6P74zEKZYU+9Yv3iiq8g/46R0shlXKmB756mA8BPxt2oPhAWrjQmQ6YxN5ypASz6zmPVIXQT/sErGpTZV7DG8YL9MZBOeuC1TDKJv+e+5JiDiHNDIx/vou8HSJHuVZyq1EiM/2V4vsTE7S9SK+8Vc+ATFoi5oHA61SDoHNiNO1UGEyA== smith-ca" :: Text)
            ])
    ]

main :: IO ()
main =
  LazyByteString.writeFile "pact/json/smith.json" $
    serialise smith
