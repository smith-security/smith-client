module Smith.Client.Network (
    runRequest
  ) where

import           Control.Monad.Trans.Bifunctor (BifunctorTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)

import qualified Network.HTTP.Client as HTTP
import qualified Network.OAuth2.JWT.Client as OAuth2

import           Smith.Client.Data.Config
import           Smith.Client.Request (Request (..))

data NetworkError =
    GrantNetworkError OAuth2.GrantError
  | EndpointNetworkError Text

runRequest :: Smith -> Request a -> IO (Either NetworkError a)
runRequest (Smith endpoint manager oauth2) (Request method path responder requester) = runExceptT $ do
  req <- ExceptT . pure . first (EndpointNetworkError . Text.pack . show) $
    HTTP.parseRequest (Text.unpack . getSmithEndpoint $ endpoint)

  token <- firstT GrantNetworkError $
    OAuth2.grant oauth2

  res <- liftIO $ flip HTTP.httpLbs manager . runRequester requester $
    req { HTTP.requestHeaders = [
        ("Authorization", mconcat ["Bearer ", getAccessToken token])
      ] }

  ExceptT . pure . Right $
    runResponder responder res
