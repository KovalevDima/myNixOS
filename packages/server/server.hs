{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Crypto.Random (DRG (..), SystemDRG, getSystemDRG)
import Data.Aeson (FromJSON (..), ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL as Base64 (encode)
import Data.ByteString.Char8 as BS8 (pack)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import Data.Map as M (delete, empty, insert, lookup)
import Data.Text as Text (Text, pack)
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
import Web.OIDC.Client as O

main :: IO ()
main = do
  oidcEnv <- initOIDC
  run 3001 (app oidcEnv)

type API = IdentityRoutes
         :<|> Get '[HTML] Homepage

api :: Proxy API
api = Proxy

server :: OIDCEnv -> Server API
server oidcEnv
  =    serveOIDC oidcEnv
  :<|> return Homepage

-- | Then main app
app :: OIDCEnv -> Application
app oidcEnv = serve (Proxy @API) (server oidcEnv)

genOIDCURL :: OIDCEnv -> IO ByteString
genOIDCURL OIDCEnv {cprg, ssm, oidc} = do
  sid <- genSessionId cprg
  let store = sessionStoreFromSession cprg ssm sid
  loc <- O.prepareAuthenticationRequestUrl store oidc [O.openId, O.email, O.profile] []
  return (BS8.pack $ show loc)

sessionStoreFromSession :: IORef SystemDRG -> IORef SessionStateMap -> Text -> SessionStore IO
sessionStoreFromSession cprg ssm sid =
  O.SessionStore
    { sessionStoreGenerate = gen cprg
    , sessionStoreSave     = \st nonce -> atomicModifyIORef' ssm $ \m -> (M.insert sid (st, nonce) m, ())
    , sessionStoreGet      = \_st -> fmap snd . M.lookup sid <$> readIORef ssm
    , sessionStoreDelete   = atomicModifyIORef' ssm $ \m -> (M.delete sid m, ())
    }

gen :: IORef SystemDRG -> IO ByteString
gen cprg = Base64.encode <$> atomicModifyIORef' cprg (swap . randomBytesGenerate 64)

genSessionId :: IORef SystemDRG -> IO Text
genSessionId cprg = decodeUtf8Lenient <$> gen cprg


handleLogin :: OIDCEnv -> Handler NoContent
handleLogin oidcenv = do
  redirects =<< liftIO (genOIDCURL oidcenv)
  return NoContent
  where
  redirects :: ByteString -> Handler ()
  redirects url = throwError err302 {errHeaders = [("Location", url)]}

-- * OIDC

initOIDC :: IO OIDCEnv
initOIDC  = do
  clientPassword <-
    maybe (error "Set env variable OIDC_PASS") BS8.pack
      <$> lookupEnv "OIDC_PASS"
  clientId <-
    maybe (error "Set env variable OIDC_CLIENT_ID") BS8.pack
      <$> lookupEnv "OIDC_CLIENT_ID"

  mgr  <- newManager tlsManagerSettings
  cprg <- newIORef =<< getSystemDRG
  ssm <- newIORef M.empty
  prov <- O.discover "https://auth.boot.directory/realms/bootDir/" mgr

  let redirectUri = "http://localhost:3001/login/cb"
      oidc = O.setCredentials clientId clientPassword redirectUri (O.newOIDC prov)
  return OIDCEnv {..}


type SessionStateMap = Map Text (O.State, O.Nonce)

data OIDCEnv = OIDCEnv
  { oidc           :: O.OIDC
  , mgr            :: Manager
  , cprg           :: IORef SystemDRG
  , ssm            :: IORef SessionStateMap
  }


-- | @AuthInfo@
data AuthInfo = AuthInfo
  { email_verified :: Bool
  , email          :: Text
  , name           :: Text
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data User = User
  { userId      :: Text
  , userSecret  :: Text
  } deriving (Show, Eq, Ord)

handleLoggedIn :: OIDCEnv -> Maybe Text -> Maybe Text -> Maybe Text -> Handler User
handleLoggedIn OIDCEnv{..} err mcode mstate =
  case err of
    Just errorMsg -> forbidden errorMsg
    Nothing -> case (mcode, mstate) of
      (Just code, Just state) -> do
        let store = sessionStoreFromSession cprg ssm (error "ToDo: session id handling")
        tokens <- liftIO $ O.getValidTokens store oidc mgr (encodeUtf8 state) (encodeUtf8 code)
        let jwt = unJwt . otherClaims . O.idToken $ tokens
            eAuthInfo = decodeClaims jwt :: Either O.JwtError (O.JwtHeader,AuthInfo)
        case eAuthInfo of
          Left jwtErr -> forbidden $ "JWT decode/check problem: " <> Text.pack (show jwtErr)
          Right (_, authInfo) ->
            if email_verified authInfo
            then pure
              User
                { userId     = authInfo.email
                , userSecret = "secret!!!"
                }
            else forbidden "Please verify your email"
      (Nothing, _) -> forbidden "no code parameter given"
      (_, Nothing) -> forbidden "no state parameter given"

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




type IdentityRoutes = "login" :>
  ( -- redirect User to the OpenID Provider
      Get '[JSON] NoContent
    :<|> "cb" -- render the page that will save the user creds in the user-agent
      :> QueryParam "error" Text
      :> QueryParam "code" Text
      :> QueryParam "state" Text
      :> Get '[HTML] User
  )

serveOIDC :: OIDCEnv -> Server IdentityRoutes
serveOIDC oidcenv 
  =    handleLogin oidcenv
  :<|> handleLoggedIn oidcenv

-- * Auth

data Customer = Customer
  { account  :: Text
  , apiKey   :: ByteString
  , fullname :: Maybe Text
  , mail     :: Maybe Text
  }

data Homepage = Homepage

instance ToMarkup Homepage where
  toMarkup Homepage = H.docTypeHtml $ do
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


appToErr :: ServerError -> Text -> ServerError
appToErr err errMsg =
  err
    { errBody = renderMarkup . toMarkup $
        H.docTypeHtml $ do
          H.head $ do
            H.title "Error"
          H.body $ do
            H.h1 (H.a ! HA.href "/" $ "Home")
            H.h2 (H.toHtml (errReasonPhrase err))
            H.p (H.toHtml errMsg)
    , errHeaders =  [("Content-Type","text/html")]
    }

unauthorized :: Text -> Handler a
unauthorized = throwError . appToErr err401

forbidden :: Text -> Handler a
forbidden = throwError . appToErr err403
