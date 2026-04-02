{-# LANGUAGE TemplateHaskell #-}

module HttpClient (
  HttpClient (..),
  mkHttpClient,
  Headers,
  QueryParams,
  ApiUrl (..),
) where

import Control.Monad (unless)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Data.Text (Text, intercalate, pack, unpack)
import Data.Text qualified as TIO
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Errors (
  ApiError (ApiError),
  AppError,
  FileError (..),
  IOE,
  LiftError (liftE),
  liftIOE,
 )
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

type Headers = [(Text, Text)]
type QueryParams = [(Text, Text)]
newtype ApiUrl = ApiUrl Text deriving stock (Show)
newtype CertPath = CertPath Text
newtype KeyPath = KeyPath Text
data Method = GET | POST | PATCH | DELETE deriving stock (Show)

data HttpClient = HttpClient
  { get :: forall res. (FromJSON res) => ApiUrl -> Headers -> QueryParams -> IOE ApiError res
  , delete ::
      forall res. (FromJSON res) => ApiUrl -> Headers -> QueryParams -> IOE ApiError res
  , post ::
      forall res req.
      (FromJSON res, ToJSON req) => req -> ApiUrl -> Headers -> QueryParams -> IOE ApiError res
  , patch ::
      forall res req.
      (FromJSON res, ToJSON req) => req -> ApiUrl -> Headers -> QueryParams -> IOE ApiError res
  , urlFromEncodedPost ::
      forall res. (FromJSON res) => QueryParams -> ApiUrl -> IOE ApiError res
  , getBytes ::
      ApiUrl -> Headers -> QueryParams -> IOE ApiError ByteString
  }

certPem :: ByteString
certPem = $(embedFile "certs/mobile-clients-api.rewe.de/private.pem")

certKey :: ByteString
certKey = $(embedFile "certs/mobile-clients-api.rewe.de/private.key")

mkHttpClient :: IOE AppError HttpClient
mkHttpClient = do
  (certPath, keyPath) <- liftE writeCertsToTemp
  pure
    HttpClient
      { get = \url hdrs qps -> curlJson certPath keyPath GET url hdrs qps Nothing
      , delete = \url hdrs qps -> curlJson certPath keyPath DELETE url hdrs qps Nothing
      , post = \body url hdrs qps ->
          curlJson
            certPath
            keyPath
            POST
            url
            hdrs
            qps
            (Just $ decodeUtf8 $ BS.toStrict $ encode body)
      , patch = \body url hdrs qps ->
          curlJson
            certPath
            keyPath
            PATCH
            url
            hdrs
            qps
            (Just $ decodeUtf8 $ BS.toStrict $ encode body)
      , urlFromEncodedPost = curlFormPost
      , getBytes = curlRaw certPath keyPath
      }

writeCertsToTemp :: IOE FileError (CertPath, KeyPath)
writeCertsToTemp = liftIOE FileError $ do
  tmpDir <- getTemporaryDirectory
  let certPath = tmpDir </> "korb-cert.pem"
  let keyPath = tmpDir </> "korb-key.pem"
  certExists <- doesFileExist certPath
  unless certExists $ BS.writeFile certPath certPem
  keyExists <- doesFileExist keyPath
  unless keyExists $ BS.writeFile keyPath certKey
  pure (CertPath $ pack certPath, KeyPath $ pack keyPath)

stealthHeaders :: [(Text, Text)]
stealthHeaders =
  [ ("user-agent", "REWE-Mobile-Client/6.0.202603161111 iOS/26.2.1 Phone/iPhone_15")
  , ("rd-is-pickup-station", "false")
  , ("rd-is-lsfk", "false")
  , ("rd-user-consent", "{\"conversionOptimization\": 1}")
  , ("accept-language", "en-GB,en;q=0.9")
  , ("accept", "*/*")
  , ("priority", "u=3")
  ]

buildUrl :: ApiUrl -> QueryParams -> Text
buildUrl (ApiUrl base) [] = base
buildUrl (ApiUrl base) qps = base <> "?" <> buildParams qps

buildParams :: QueryParams -> Text
buildParams qps = intercalate "&" ((\(k, v) -> k <> "=" <> v) <$> qps)

headersToArgs :: [(Text, Text)] -> [Text]
headersToArgs = concatMap (\(k, v) -> ["-H", k <> ": " <> v])

curlJson ::
  (FromJSON res) =>
  CertPath ->
  KeyPath ->
  Method ->
  ApiUrl ->
  Headers ->
  QueryParams ->
  Maybe Text ->
  IOE ApiError res
curlJson (CertPath certPath) (KeyPath keyPath) method url hdrs qps mBody = do
  let fullUrl = buildUrl url qps
  let bodyArgs = case mBody of
        Nothing -> []
        Just b -> ["-H", "Content-Type: application/json", "-d", b]
  let args =
        ["-s", "-f", "-X", TIO.show method, "--cert", certPath, "--key", keyPath]
          ++ headersToArgs stealthHeaders
          ++ headersToArgs hdrs
          ++ bodyArgs
          ++ [fullUrl]
  runCurl fullUrl args >>= decodeJson fullUrl

curlFormPost :: (FromJSON res) => QueryParams -> ApiUrl -> IOE ApiError res
curlFormPost formBodyParams (ApiUrl url) = do
  let formBody = buildParams formBodyParams
  let args =
        [ "-s"
        , "-f"
        , "-X"
        , TIO.show POST
        , "-H"
        , "Content-Type: application/x-www-form-urlencoded"
        , "-d"
        , formBody
        , url
        ]
  runCurl url args >>= decodeJson url

curlRaw ::
  CertPath -> KeyPath -> ApiUrl -> Headers -> QueryParams -> IOE ApiError ByteString
curlRaw (CertPath certPath) (KeyPath keyPath) url hdrs qps = do
  let fullUrl = buildUrl url qps
  let args =
        ["-s", "-f", "--cert", certPath, "--key", keyPath]
          ++ headersToArgs stealthHeaders
          ++ headersToArgs hdrs
          ++ [fullUrl]
  encodeUtf8 <$> runCurl fullUrl args

runCurl :: Text -> [Text] -> IOE ApiError Text
runCurl url args = do
  (exitCode, stdout, stderr) <-
    liftIOE ApiError $ readProcessWithExitCode "curl" (unpack <$> args) ""
  case exitCode of
    ExitFailure 22 -> throwE $ ApiError ("HTTP error - " <> url <> " - " <> pack stdout)
    ExitFailure code -> throwE $ ApiError ("curl failed (exit " <> pack (show code) <> "): " <> pack stderr)
    ExitSuccess -> pure (pack stdout)

decodeJson :: (FromJSON res) => Text -> Text -> IOE ApiError res
decodeJson url stdout = case eitherDecodeStrict (encodeUtf8 stdout) of
  Right res -> pure res
  Left err -> throwE $ ApiError ("Parsing Error: " <> pack err <> " - " <> url <> " - " <> stdout)
