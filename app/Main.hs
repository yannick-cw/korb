module Main (main) where

import Auth (mkAuth, mkTokenFileStore)
import Auth.Types (Auth (..))
import Cli (
  BasketCommand (..),
  CheckoutCommand (..),
  Command (..),
  EbonCommand (..),
  FavoritesCommand (..),
  Input (..),
  OrderCommand (..),
  StoreCommand (..),
  SuggestionCommand (..),
  parseInput,
 )
import Control.Monad (unless)
import Control.Monad.Trans.Except (runExceptT, throwE, withExceptT)
import Data.Void (absurd)
import Errors (
  ApiError (..),
  AppError (..),
  FileError (..),
  IOE,
  LiftError (..),
 )
import HttpClient (mkHttpClient)
import ReweApi (
  basket,
  basketsAdd,
  checkout,
  checkoutTimeslot,
  deleteOpenOrder,
  ebonReceipt,
  ebons,
  favorites,
  favoritesAdd,
  favoritesDelete,
  getCategories,
  getOneOrder,
  getOpenOrders,
  getOrderHistory,
  mkReweAuthedClient,
  mkRewePublicClient,
  orderCheckout,
  searchRewe,
  slots,
  thresholdSuggestion,
 )
import Storage (readSettings, writeSettings)
import StoreApi (printValue, searchForStores, storeExists)

main :: IO ()
main = do
  result <- runExceptT run
  case result of
    Left (AppFileError (FileReadError err)) -> print $ "It seems like you have not yet set a store - run korb store set" ++ show err
    Left (AppFileError (TokenReadError err)) -> print $ "No access token found - run 'korb login' first" ++ show err
    Left err -> print err
    Right _ -> pure ()

run :: IOE AppError ()
run = do
  Input{cmd, pretty} <- withExceptT absurd parseInput
  httpClient <- liftE mkHttpClient
  let tokenStore = mkTokenFileStore
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
        Search query attributes categories ->
          liftE $
            searchRewe (publicClient currentStore) query attributes categories >>= printValue pretty
        needsAuth ->
          authedClient currentStore >>= \client ->
            case needsAuth of
              Ebon (EbonDownload ebonId file) -> ebonReceipt client ebonId file >>= printValue pretty
              noFileAccess -> liftE $ case noFileAccess of
                Basket BasketShow ->
                  basket client >>= printValue pretty
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
                Categories ->
                  getCategories client >>= printValue pretty
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
                Ebon EbonShow ->
                  ebons client >>= printValue pretty
                Suggestion (ThresholdSuggestion num) ->
                  thresholdSuggestion client num >>= printValue pretty
