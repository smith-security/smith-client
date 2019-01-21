-- |
-- Smith Client top-level module.
--
-- Designed to be import qualified:
--
-- > import qualified Smith.Client as Smith
--
module Smith.Client (
  -- * Smith runtime data
    Smith (..)

  -- * Smith OAuth2 Scopes
  , SmithScope (..)

  -- * Smith configuration operations.
  , configure
  , configureWith
  , configureT
  , configureWithT

  -- * Smith configuration errors and default handler.
  , SmithConfigureError (..)
  , renderSmithConfigureError

  -- * Smith API definition runners.
  , runRequest
  , runRequestT

  -- * Smith API definitions.
  , module Api
  ) where

import           Smith.Client.Config (Smith (..), SmithScope (..))
import           Smith.Client.Config (configure, configureWith, configureT, configureWithT)
import           Smith.Client.Config (SmithConfigureError (..), renderSmithConfigureError)
import           Smith.Client.Network (runRequest, runRequestT)
import           Smith.Client.Api as Api
