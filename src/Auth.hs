module Auth (mkAuth, mkTokenFileStore) where

import Auth.Types (
  AccessToken (..),
  Auth (..),
  AuthCode (..),
  Exp (..),
  PKCEVerifier (..),
  RefreshToken (..),
  TokenResponse (..),
  TokenStore (..),
 )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (except)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random.Entropy (getEntropy)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (first)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL (decodeUnpadded, encodeUnpadded)
import Data.Int (Int64)
import Data.Text (Text, pack, splitOn)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Errors (
  AppError,
  AuthError (AuthError),
  FileError (..),
  IOE,
  liftE,
  liftIOE,
 )
import HttpClient (HttpClient (..))
import Network.HTTP.Req (FormUrlEncodedParam, Scheme (Https), Url, https, (/:), (=:))
import System.Directory (createDirectoryIfMissing, getXdgDirectory)
import System.Directory.OsPath (XdgDirectory (..))
import System.FilePath ((</>))
import System.Posix.Files (
  ownerExecuteMode,
  ownerReadMode,
  ownerWriteMode,
  setFileMode,
  unionFileModes,
 )
import Text.Regex.TDFA (AllTextSubmatches (getAllTextSubmatches), (=~))

configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig "korb/tokens"

readToken :: String -> IOE FileError Text
readToken tknKind = do
  liftIOE TokenReadError $
    do
      dir <- configDir
      TIO.readFile (dir </> tknKind)

storeTkn :: Text -> String -> IOE FileError ()
storeTkn tkn tknName =
  liftIOE FileError $
    do
      dir <- configDir
      createDirectoryIfMissing True dir
      setFileMode
        dir
        (ownerReadMode `unionFileModes` ownerWriteMode `unionFileModes` ownerExecuteMode)
      TIO.writeFile (dir </> tknName) tkn

mkTokenFileStore :: TokenStore
mkTokenFileStore =
  TokenStore
    { readAccess = AccessToken <$> readToken "access_token"
    , readRefresh = RefreshToken <$> readToken "refresh_token"
    , storeAccess = \(AccessToken token) -> storeTkn token "access_token"
    , storeRefresh = \(RefreshToken token) -> storeTkn token "refresh_token"
    }

generatePKCE :: IO (ByteString, ByteString)
generatePKCE = do
  bytes <- getEntropy 32
  let verifier = encodeUnpadded bytes
      challenge = encodeUnpadded (convert (hash verifier :: Digest SHA256))
  pure (verifier, challenge)

generateAuthUrl :: IOE AuthError (Text, PKCEVerifier)
generateAuthUrl =
  do
    (verifier, challenge) <- liftIO generatePKCE
    let url =
          "https://account.rewe.de/realms/sso/protocol/openid-connect/auth"
            <> "?client_id=reweios"
            <> "&response_type=code"
            <> "&scope=openid%20email%20customer%20offline_access%20profile"
            <> "&redirect_uri=de.rewe.app%3A%2F%2Fredirect"
            <> "&code_challenge="
            <> decodeUtf8 challenge
            <> "&code_challenge_method=S256"
    pure (url, PKCEVerifier verifier)

extractCodeFromRedirect :: Text -> Either AuthError AuthCode
extractCodeFromRedirect redirect =
  case getAllTextSubmatches (redirect =~ ("code=([^&]+)" :: Text)) of
    [_, code] -> Right (AuthCode code)
    _ ->
      Left $
        AuthError
          "No auth code in redirect found - retry flow - make sure 'de.rewe.app..' is correct"

reweFormEncodedBody :: AuthCode -> PKCEVerifier -> FormUrlEncodedParam
reweFormEncodedBody (AuthCode code) (PKCEVerifier verifier) =
  "grant_type" =: ("authorization_code" :: Text)
    <> "client_id" =: ("reweios" :: Text)
    <> "code" =: code
    <> "redirect_uri" =: ("de.rewe.app://redirect" :: Text)
    <> "code_verifier" =: decodeUtf8 verifier

decodeJWT :: AccessToken -> Either AuthError Int64
decodeJWT (AccessToken tkn) = do
  jwtPayload <- case splitOn "." tkn of
    (_ : token : _) -> first (AuthError . pack) $ decodeUnpadded $ encodeUtf8 token
    _ -> Left $ AuthError "Access token in wrong format - try 'korb login' again"
  case eitherDecodeStrict jwtPayload of
    Right (Exp expiry) -> pure expiry
    Left err -> Left (AuthError $ pack err)

refreshFormBody :: RefreshToken -> FormUrlEncodedParam
refreshFormBody (RefreshToken token) =
  "grant_type" =: ("refresh_token" :: Text)
    <> "client_id" =: ("reweios" :: Text)
    <> "refresh_token" =: token

tokenEndpoint :: Url 'Https
tokenEndpoint =
  https "account.rewe.de" /: "realms" /: "sso" /: "protocol" /: "openid-connect" /: "token"

refreshFlow :: TokenStore -> HttpClient -> IOE AppError AccessToken
refreshFlow store HttpClient{urlFromEncodedPost} = do
  refreshToken <- liftE store.readRefresh
  tknRes :: TokenResponse <-
    liftE $
      urlFromEncodedPost (refreshFormBody refreshToken) tokenEndpoint
  liftE $ store.storeAccess tknRes.access_token
  liftE $ store.storeRefresh tknRes.refresh_token
  pure tknRes.access_token

getValidToken :: TokenStore -> HttpClient -> IOE AppError AccessToken
getValidToken store client = do
  token <- liftE store.readAccess
  expiry <- liftE $ except $ decodeJWT token
  now :: Int64 <- round <$> liftIO getPOSIXTime
  if now < expiry - 30 then pure token else refreshFlow store client

loginFlow :: TokenStore -> HttpClient -> IOE AppError Text
loginFlow store HttpClient{urlFromEncodedPost} = do
  (url, verifier) <- liftE generateAuthUrl
  redirect <- liftIO $ do
    putStrLn "1. Open this URL in Chrome (Firefox won't work):"
    putStrLn ""
    TIO.putStr url
    putStrLn ""
    putStrLn "2. Log in normally (email, password, 2FA)"
    putStrLn "3. After login, Chrome shows a blank page. Open DevTools (Cmd+Option+I)."
    putStrLn
      "   The console or network tab will show a cancelled request to 'de.rewe.app://redirect?...'"
    putStrLn "4. Copy the full 'de.rewe.app://redirect?...' URL and paste it here:"
    TIO.getLine
  code <- liftE $ except (extractCodeFromRedirect redirect)
  tknRes :: TokenResponse <-
    liftE $
      urlFromEncodedPost (reweFormEncodedBody code verifier) tokenEndpoint
  liftE $ store.storeAccess tknRes.access_token
  liftE $ store.storeRefresh tknRes.refresh_token
  pure "Login succeeded, tokens stored"

mkAuth :: TokenStore -> HttpClient -> Auth
mkAuth store client =
  Auth
    { getValidToken = getValidToken store client
    , login = loginFlow store client
    }
