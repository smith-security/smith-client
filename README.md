# smith-client

This is a client-library for the [Smith](https://smith.st) API.

The goal is to provide a convenient Haskell API for working with
the API.


### Configuration

The default 'configure' function will source credentials configuration as follows:
 - It will check for an environment provided API key in '$SMITH_JWK'.
 - It will fall-back to looking for '$SMITH_HOME/credentials.json' if '$SMITH_HOME' is set.
 - It will fall-back to looking for '$HOME/.smith/credentials.json'.

The default 'configure' function will source endpoint configuration as follows:
 - It will check for an environment provided endpoint in '$SMITH_ENDPOINT'.
 - It will fall-back to the public production endpoint 'https://api.smith.st'.


### Stability

This library is new, and should have the disclaimers that normally
comes with that. It is expected that most people will interact with
`smith-cli` rather than directly with this library, however if you
do use this library directly, please note that whilst compatibility
won't be broken unless necessary there are a few parts of the library
(for example error handling) that will be evolved and polished as an
official release version of the API is available.


### Example

A crude example:

```
import qualified Data.Text.IO as Text

import qualified Smith.Client as Smith
import           Smith.Client.Data.User
import           Smith.Client.Error (SmithError (..))

example :: IO (Either SmithError UserInfo)
example =
  Smith.configure >>= \c -> case c of
    Left err ->
       Text.putStrLn . Smith.renderSmithConfigureError $ err
    Right smith ->
      Smith.runRequest smith Smith.userinfo
```


### Development

The simplest way to develop this library is working against the
[Pact](https://docs.pact.io/) stub server (depends on docker).

```
# Configures and starts a docker container with API stubs.
/bin/ghci-stub
```
