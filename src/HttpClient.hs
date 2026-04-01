{-# LANGUAGE TemplateHaskell #-}

module HttpClient (
  HttpClient (..),
  mkHttpClient,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), throwE, withExceptT)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Errors (ApiError (ApiError), IOE)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import Network.HTTP.Req (
  BsResponse,
  DELETE (..),
  FormUrlEncodedParam,
  GET (..),
  HttpConfig (..),
  NoReqBody (..),
  Option,
  PATCH (..),
  POST (..),
  ReqBodyJson (..),
  ReqBodyUrlEnc (..),
  Scheme (Https),
  Url,
  bsResponse,
  defaultHttpConfig,
  header,
  req,
  responseBody,
  runReq,
 )
import Network.TLS (
  ClientHooks (..),
  ClientParams (..),
  Shared (..),
  credentialLoadX509FromMemory,
  defaultParamsClient,
 )
import System.X509 (getSystemCertificateStore)

data HttpClient = HttpClient
  { get :: forall res. (FromJSON res) => Url 'Https -> Option 'Https -> IOE ApiError res
  , delete :: forall res. (FromJSON res) => Url 'Https -> Option 'Https -> IOE ApiError res
  , post ::
      forall res req.
      (FromJSON res, ToJSON req) => req -> Url 'Https -> Option 'Https -> IOE ApiError res
  , patch ::
      forall res req.
      (FromJSON res, ToJSON req) => req -> Url 'Https -> Option 'Https -> IOE ApiError res
  , urlFromEncodedPost ::
      forall res. (FromJSON res) => FormUrlEncodedParam -> Url 'Https -> IOE ApiError res
  , getBytes ::
      Url 'Https -> Option 'Https -> IOE ApiError ByteString
  }

mkHttpClient :: IOE ApiError HttpClient
mkHttpClient = do
  manager <- mTlsManager
  let httpConfig = defaultHttpConfig{httpConfigAltManager = Just manager}
  pure
    HttpClient
      { get = getRewe httpConfig
      , delete = deleteRewe httpConfig
      , post = postRewe httpConfig
      , patch = patchRewe httpConfig
      , urlFromEncodedPost = urlFormEncodeRewe httpConfig
      , getBytes = getReweRaw httpConfig
      }

certPem :: ByteString
certPem = $(embedFile "certs/mobile-clients-api.rewe.de/private.pem")

certKey :: ByteString
certKey = $(embedFile "certs/mobile-clients-api.rewe.de/private.key")

mTlsManager :: IOE ApiError Manager
mTlsManager = do
  credential <-
    withExceptT (ApiError . pack) $
      ExceptT $
        pure (Network.TLS.credentialLoadX509FromMemory certPem certKey)
  let defaultClientParams = Network.TLS.defaultParamsClient "mobile-clients-api.rewe.de" ""
  caStore <- liftIO getSystemCertificateStore
  let clientParams =
        defaultClientParams
          { clientShared = defaultClientParams.clientShared{sharedCAStore = caStore}
          , clientHooks =
              defaultClientParams.clientHooks
                { onCertificateRequest = \_ -> pure (Just credential)
                }
          }
  let tlsSettings = TLSSettings clientParams
  newTlsManagerWith (mkManagerSettings tlsSettings Nothing)

decodeResponse :: (FromJSON res, Show a) => a -> BsResponse -> IOE ApiError res
decodeResponse reqName response =
  case eitherDecodeStrict (responseBody response) of
    Right res -> pure res
    Left err ->
      throwE $
        ApiError
          ( "Parsing Error: "
              <> pack err
              <> " - "
              <> (pack (Prelude.show reqName) <> " - " <> decodeUtf8 (responseBody response))
          )

stealthHeaders :: Option 'Https
stealthHeaders =
  header "user-agent" "REWE-Mobile-Client/6.0.202603161111 iOS/26.2.1 Phone/iPhone_15"
    <> header "rd-is-pickup-station" "false"
    <> header "rd-is-lsfk" "false"
    <> header "rd-user-consent" "{\"conversionOptimization\": 1}"
    <> header "accept-language" "en-GB,en;q=0.9"
    <> header "accept" "*/*"
    <> header "priority" "u=3"

deleteRewe ::
  (FromJSON res) => HttpConfig -> Url 'Https -> Option 'Https -> IOE ApiError res
deleteRewe config url options =
  decodeResponse url
    =<< runReq config (req DELETE url NoReqBody bsResponse (options <> stealthHeaders))

getRewe :: (FromJSON res) => HttpConfig -> Url 'Https -> Option 'Https -> IOE ApiError res
getRewe config url options =
  decodeResponse url
    =<< runReq config (req GET url NoReqBody bsResponse (options <> stealthHeaders))

getReweRaw :: HttpConfig -> Url 'Https -> Option 'Https -> IOE ApiError ByteString
getReweRaw config url options =
  responseBody
    <$> runReq config (req GET url NoReqBody bsResponse (options <> stealthHeaders))

urlFormEncodeRewe ::
  (FromJSON res) => HttpConfig -> FormUrlEncodedParam -> Url 'Https -> IOE ApiError res
urlFormEncodeRewe config body url =
  decodeResponse url
    =<< runReq config (req POST url (ReqBodyUrlEnc body) bsResponse stealthHeaders)

postRewe ::
  (FromJSON res, ToJSON req) =>
  HttpConfig -> req -> Url 'Https -> Option 'Https -> IOE ApiError res
postRewe config body url options =
  decodeResponse url
    =<< runReq config (req POST url (ReqBodyJson body) bsResponse (options <> stealthHeaders))

patchRewe ::
  (FromJSON res, ToJSON req) =>
  HttpConfig -> req -> Url 'Https -> Option 'Https -> IOE ApiError res
patchRewe config body url options =
  decodeResponse url
    =<< runReq config (req PATCH url (ReqBodyJson body) bsResponse (options <> stealthHeaders))
