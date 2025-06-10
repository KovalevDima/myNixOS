{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (DRG (..), SystemDRG, getSystemDRG)
import Data.Aeson (FromJSON (..), ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL as Base64 (encode)
import Data.ByteString.Char8 as BS8 (pack)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map as M (delete, empty, insert, lookup)
import Data.Text as Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Blaze (HTML)
import System.Environment (lookupEnv)
import Text.Blaze.Html5 as H hiding (code)
import Text.Blaze.Html5.Attributes as HA (href)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Web.OIDC.Client qualified as O

main :: IO ()
main = do
  !clientPass <-
    maybe (error "Set env variable OIDC_PASS") BS8.pack
      <$> lookupEnv "OIDC_PASS"
  !clientId <-
    maybe (error "Set env variable OIDC_CLIENT_ID") BS8.pack
      <$> lookupEnv "OIDC_CLIENT_ID"

  manager <- newManager tlsManagerSettings
  cprg <- newIORef =<< getSystemDRG
  ssm <- newIORef M.empty
  provider <- O.discover "https://auth.boot.directory/realms/bootDir/" manager

  let oidc = O.setCredentials clientId clientPass "http://localhost:3001/login/cb" (O.newOIDC provider)
      store = \sessionId -> O.SessionStore
        { sessionStoreGenerate = gen cprg
        , sessionStoreSave     = \st nonce -> atomicModifyIORef' ssm $ \m -> (M.insert sessionId (st, nonce) m, ())
        , sessionStoreGet      = \_st -> fmap snd . M.lookup sessionId <$> readIORef ssm
        , sessionStoreDelete   = atomicModifyIORef' ssm $ \m -> (M.delete sessionId m, ())
        }

  let authArgs = MkAuthArgs{..}
  run 3001 . serve (Proxy @API) $
    serveAuthentication authArgs
    :<|> serveHomepage


type API =
  AuthenticationAPI
  :<|> HomepageAPI

-----------------------------------------------------------------------------------------------------------------------
-- * Authentication
-----------------------------------------------------------------------------------------------------------------------

serveAuthentication :: AuthArgs -> Server AuthenticationAPI
serveAuthentication authArgs =
  handleLogin authArgs
  :<|> handleLoggedIn authArgs

type AuthenticationAPI =
  LoginAPI
  :<|> LoggedInAPI

data AuthArgs = MkAuthArgs
  { oidc :: O.OIDC
  , manager  :: Manager
  , cprg :: IORef SystemDRG
  , store :: Text -> O.SessionStore IO
  }


gen :: IORef SystemDRG -> IO ByteString
gen cprg = Base64.encode <$> atomicModifyIORef' cprg (swap . randomBytesGenerate 64)

-------------------
-- ** Login
-------------------

-- | redirect User to the OpenID Provider
type LoginAPI =
  "login"
  :> Get '[JSON] NoContent

handleLogin :: AuthArgs -> Server LoginAPI
handleLogin MkAuthArgs{oidc, cprg, store} = do
  loc <- liftIO $ do
    sessionId <- decodeUtf8Lenient <$> gen cprg
    O.prepareAuthenticationRequestUrl (store sessionId) oidc [O.openId, O.email, O.profile] []
  throwError err302 {errHeaders = [("Location", (BS8.pack . show) loc)]}

-------------------
-- ** LoggedIn
-------------------

-- | render the page that will save the user creds in the user-agent
type LoggedInAPI =
  "login"
  :> "cb"
  :> QueryParam "error" Text
  :> QueryParam "code" Text
  :> QueryParam "state" Text
  :> Get '[HTML] User

data User = User
  { userId      :: Text
  , userSecret  :: Text
  } deriving (Show, Eq, Ord)

handleLoggedIn :: AuthArgs -> Server LoggedInAPI
handleLoggedIn MkAuthArgs{..} merr mcode mstate = do
  maybe (pure ()) forbidden merr
  !code <- maybe (forbidden "no code parameter given") (pure . encodeUtf8) mcode
  !state <- maybe (forbidden "no state parameter given") (pure . encodeUtf8) mstate
  AuthInfo{email, email_verified} <- liftIO $ do
    sessionId <- decodeUtf8Lenient <$> (gen cprg)
    (O.otherClaims . O.idToken) <$> O.getValidTokens (store sessionId) oidc manager state code
  when
    (not $ email_verified)
    (forbidden "Please verify your email")
  let userId = email
      userSecret = "secret!!!"
  pure User{..}
  where
  forbidden :: Text -> Handler a
  forbidden errMsg = throwError err403
    { errHeaders =  [("Content-Type","text/html")]
    , errBody = renderMarkup . toMarkup $
        H.docTypeHtml $ do
          H.head $ do
            H.title "Error"
          H.body $ do
            H.h1 (H.a ! HA.href "/" $ "Home")
            H.h2 (H.toHtml (errReasonPhrase err403))
            H.p (H.toHtml errMsg)
    }

instance ToMarkup User where
  toMarkup User{..} = H.docTypeHtml $ do
    H.head $
      H.title "Logged In"
    H.body $ do
      H.h1 "Logged In"
      H.p (H.toHtml ("Successful login with id " <> userId))
      H.script $
        H.toHtml
          (  "localStorage.setItem('api-key','" <> userSecret <> "');"
          <> "localStorage.setItem('user-id','" <> userId <> "');"
          <> "window.location='/';" -- redirect the user to /
          )

data AuthInfo = AuthInfo
  { email_verified :: Bool
  , email          :: Text
  , name           :: Text
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-----------------------------------------------------------------------------------------------------------------------
-- * Homepage
-----------------------------------------------------------------------------------------------------------------------

type HomepageAPI = Get '[HTML] Homepage

serveHomepage :: Server HomepageAPI
serveHomepage = pure MkHomepage

data Homepage = MkHomepage

instance ToMarkup Homepage where
  toMarkup MkHomepage = H.docTypeHtml $ do
    H.head $ do
      H.title "OpenID Connect Servant Example"
      H.style (H.toHtml ("body { font-family: monospace; font-size: 18px; }" :: Text))
    H.body $ do
      H.h1 "OpenID Connect Servant Example"
      H.div $
        H.a ! HA.href "/login" $ "Click here to login"
      H.ul $ do
        H.li $ do
          H.span "API Key in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('api-key'));" :: Text))
        H.li $ do
          H.span "User ID in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('user-id'));" :: Text))
