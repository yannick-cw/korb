module Main where

import Auth (mkAuth, mkKeychainTokenStore)
import Auth.Types (Auth (..))
import Cli
import Control.Monad (unless)
import Control.Monad.Trans.Except (runExceptT, throwE, withExceptT)
import Data.Void (absurd)
import Errors
import HttpClient (mkHttpClient)
import ReweApi
import Storage (readSettings, writeSettings)
import StoreApi (printValue, searchForStores, storeExists)

main :: IO ()
main = do
  result <- runExceptT run
  case result of
    Left (AppFileError (FileReadError err)) -> print $ "It seems like you have not yet set a store - run korb store set" ++ show err
    Left (AppAuthError NoTokenError) -> putStr $ "No access token found in keychain - run 'korb login' first"
    Left err -> print err
    Right _ -> pure ()

run :: IOE AppError ()
run = do
  Input{cmd, pretty} <- withExceptT absurd parseInput
  httpClient <- liftE mkHttpClient
  let tokenStore = mkKeychainTokenStore
  let auth = mkAuth tokenStore httpClient
  let publicClient = mkRewePublicClient httpClient
  let authedClient = mkReweAuthedClient httpClient auth
  case cmd of
    Store (StoreSearch zipCode) -> liftE $ searchForStores httpClient zipCode >>= printValue pretty
    Store (StoreSet wwIdent zipCode) -> do
      isStore <- liftE $ storeExists httpClient wwIdent zipCode
      unless isStore $
        throwE (AppApiError (ApiError "Store for given ident and zipcode seems not to exist."))
      liftE $ writeSettings wwIdent zipCode >>= printValue pretty
    Store StoreShow -> liftE $ readSettings >>= printValue pretty
    Login -> auth.login >>= printValue pretty
    needsCurrentStore ->
      liftE readSettings >>= \currentStore -> case needsCurrentStore of
        Search query attributes -> liftE $ searchRewe (publicClient currentStore) query attributes >>= printValue pretty
        needsAuth ->
          authedClient currentStore >>= \client -> liftE $ case needsAuth of
            Basket BasketShow ->
              baskets client >>= printValue pretty
            Basket (BasketAdd item) ->
              basketsAdd client item >>= printValue pretty
            Favorites FavoritesShow ->
              favorites client Nothing >>= printValue pretty
            Favorites (FavoritesFilter query) ->
              favorites client (Just query) >>= printValue pretty
            Favorites (FavoritesAdd listingId productId) ->
              favoritesAdd client listingId productId >>= printValue pretty
            Favorites (FavoritesRemove itemId) ->
              favoritesDelete client itemId >>= printValue pretty
            Slots ->
              slots client >>= printValue pretty
            Checkout (StartCheckout timeslot) ->
              checkoutTimeslot client timeslot >>= printValue pretty
            Checkout GetCheckout ->
              checkout client >>= printValue pretty
            Checkout PlaceOrder ->
              orderCheckout client >>= printValue pretty
            Order (DeleteOrder orderId) ->
              deleteOpenOrder client orderId >>= printValue pretty
            Order (GetOrder orderId) ->
              getOneOrder client orderId >>= printValue pretty
            Order GetOrders ->
              getOpenOrders client >>= printValue pretty
            Order OrdersHistory ->
              getOrderHistory client >>= printValue pretty
