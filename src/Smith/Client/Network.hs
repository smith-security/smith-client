-- |
-- Smith HTTP interactions.
--
-- Should be used as a qualified import from top-level module:
--
-- > import qualified Smith.Client as Smith
--
-- Example:
-- > Smith.runRequest configuration Smith.userinfo
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Client.Network (
    runRequest
  , runRequestT
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Bifunctor (BifunctorTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)

import           Data.Bifunctor (Bifunctor (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as  HTTP
import qualified Network.OAuth2.JWT.Client as OAuth2

import           Smith.Client.Config
import           Smith.Client.Error
import           Smith.Client.Request (Request (..), Requester (..))
import           Smith.Client.Response (Responder (..))

-- |
-- Takes Smith runtime data, and an API request definition and actually
-- runs the request. Results are in IO and the error cases handled
-- explicitly.
--
runRequest :: Smith -> Request a -> IO (Either SmithError a)
runRequest smith =
  runExceptT .  runRequestT smith

-- |
-- Takes Smith runtime data, and an API request definition and actually
-- runs the request. Results are embeded in ExceptT for convenience.
--
runRequestT :: Smith -> Request a -> ExceptT SmithError IO a
runRequestT (Smith endpoint manager oauth2) (Request method path responder requester) = do
  req <- ExceptT . pure . first (SmithUrlParseError . Text.pack . show) $
    HTTP.parseRequest (Text.unpack $ mconcat [getSmithEndpoint endpoint, "/", path])

  token <- firstT SmithAuthenticationError . ExceptT $
    OAuth2.grant oauth2

  res <- liftIO $ flip HTTP.httpLbs manager . runRequester requester $
    req {
        HTTP.method = HTTP.renderStdMethod method
      , HTTP.requestHeaders = [
            ("Authorization", mconcat ["Bearer ", Text.encodeUtf8 . OAuth2.renderAccessToken $ token])
          ]
      }

  ExceptT . pure $
    runResponder responder res
