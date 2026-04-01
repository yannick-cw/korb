module Cli (
  ZipCode (..),
  WwIdent (..),
  Input (..),
  StoreCommand (..),
  FavoritesCommand (..),
  BasketCommand (..),
  EbonCommand (..),
  CheckoutCommand (..),
  OrderCommand (..),
  NumberOfSuggestions (..),
  SuggestionCommand (..),
  Command (..),
  parseInput,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import Data.UUID qualified as UUID
import Data.Version (showVersion)
import Data.Void (Void)
import Errors (IOE)
import Options.Applicative (
  Parser,
  ParserInfo,
  argument,
  auto,
  command,
  eitherReader,
  execParser,
  flag,
  footerDoc,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  infoOption,
  long,
  metavar,
  option,
  optional,
  progDesc,
  short,
  str,
  switch,
  value,
  (<**>),
  (<|>),
 )
import Options.Applicative.Help (Doc, vsep)
import Paths_korb (version)
import ReweApi.Types (
  EbonId (..),
  Item (..),
  ItemId (..),
  ListingId (..),
  OrderId (..),
  ProductId (..),
  Qty (..),
  SearchAttribute (..),
  TimeslotId (..),
 )

newtype ZipCode = ZipCode Text deriving newtype (Show, ToJSON, FromJSON)
newtype WwIdent = WwIdent Text deriving newtype (Show, ToJSON, FromJSON)
data Input = Input {cmd :: Command, pretty :: Bool}
data StoreCommand = StoreShow | StoreSearch ZipCode | StoreSet WwIdent ZipCode
data FavoritesCommand
  = FavoritesShow
  | FavoritesFilter Text
  | FavoritesAdd ListingId ProductId
  | FavoritesRemove ItemId
data BasketCommand = BasketShow | BasketAdd Item
data EbonCommand = EbonShow | EbonDownload EbonId FilePath
data CheckoutCommand = GetCheckout | StartCheckout TimeslotId | PlaceOrder
data OrderCommand = DeleteOrder OrderId | GetOrders | OrdersHistory | GetOrder OrderId
newtype NumberOfSuggestions = NumberOfSuggestions Int deriving newtype (Show)
newtype SuggestionCommand = ThresholdSuggestion NumberOfSuggestions

data Command
  = Store StoreCommand
  | Search Text [SearchAttribute]
  | Login
  | Slots
  | Favorites FavoritesCommand
  | Basket BasketCommand
  | Ebon EbonCommand
  | Checkout CheckoutCommand
  | Order OrderCommand
  | Suggestion SuggestionCommand

favoritesAddParser :: Parser FavoritesCommand
favoritesAddParser =
  FavoritesAdd
    <$> argument (ListingId <$> str) (metavar "LISTING_ID")
    <*> argument (ProductId <$> str) (metavar "PRODUCT_ID")

uuidArg :: forall a. (Text -> a) -> String -> String -> Parser a
uuidArg wrap metaName errMsg = argument (eitherReader parse) (metavar metaName)
  where
    parse :: String -> Either String a
    parse s = case UUID.fromString s of
      Just _ -> Right (wrap (pack s))
      Nothing -> Left errMsg

favoritesRemoveParser :: Parser FavoritesCommand
favoritesRemoveParser =
  FavoritesRemove <$> argument (ItemId <$> str) (metavar "ITEM_ID")

favoritesFilterParser :: Parser FavoritesCommand
favoritesFilterParser = FavoritesFilter <$> argument str (metavar "QUERY")

favoritesParser :: Parser Command
favoritesParser =
  Favorites
    <$> ( hsubparser
            ( command
                "search"
                (info favoritesFilterParser (progDesc "Filter favorites by name"))
                <> command
                  "add"
                  ( info
                      favoritesAddParser
                      (progDesc "Add product to default favorites list by listing ID and product ID")
                  )
                <> command
                  "delete"
                  ( info
                      favoritesRemoveParser
                      (progDesc "Remove item from favorites by item ID")
                  )
            )
            <|> pure FavoritesShow
        )

storeSearchParser :: Parser StoreCommand
storeSearchParser = StoreSearch <$> argument (ZipCode <$> str) (metavar "ZIP")

storeSetParser :: Parser StoreCommand
storeSetParser =
  StoreSet
    <$> argument (WwIdent <$> str) (metavar "wwIdent")
    <*> argument (ZipCode <$> str) (metavar "ZIP")

storeParser :: Parser Command
storeParser =
  Store
    <$> ( hsubparser
            ( command "search" (info storeSearchParser (progDesc "Find pickup stores near ZIP code"))
                <> command
                  "set"
                  (info storeSetParser (progDesc "Set active store by market ID and ZIP"))
            )
            <|> pure StoreShow
        )

searchAttributeParser :: Parser [SearchAttribute]
searchAttributeParser =
  Prelude.concat
    <$> sequenceA
      [ flag [] [Organic] (long "organic" <> help "Filter only organic products")
      , flag [] [Regional] (long "regional" <> help "Filter only regional products")
      , flag [] [Vegan] (long "vegan" <> help "Filter only vegan products")
      , flag [] [Vegetarian] (long "vegetarian" <> help "Filter only vegetarian products")
      ]

searchParser :: Parser Command
searchParser = Search <$> argument str (metavar "QUERY|EAN") <*> searchAttributeParser

loginParser :: Parser Command
loginParser = pure Login

slotsParser :: Parser Command
slotsParser = pure Slots

suggestionParser :: Parser Command
suggestionParser =
  Suggestion . ThresholdSuggestion
    <$> hsubparser
      ( command
          "threshold"
          ( info
              (argument (NumberOfSuggestions <$> auto) (metavar "NUM_SUGGESTIONS"))
              (progDesc "Creates suggestion to add to the basket to reach the threshold for free pickup.")
          )
      )

checkoutStartParser :: Parser CheckoutCommand
checkoutStartParser =
  StartCheckout
    <$> uuidArg
      TimeslotId
      "TIMESLOT_ID"
      "Invalid timeslot ID (expected UUID, get IDs from korb timeslots)"

checkoutParser :: Parser Command
checkoutParser =
  Checkout
    <$> ( hsubparser
            ( command
                "create"
                ( info
                    checkoutStartParser
                    (progDesc "Reserve timeslot, create checkout, and set payment to market pickup")
                )
                <> command
                  "order"
                  ( info
                      (pure PlaceOrder)
                      (progDesc "Confirm and place the order. Timeslot must be attached first")
                  )
            )
            <|> pure GetCheckout
        )

ebonDownloadParser :: Parser EbonCommand
ebonDownloadParser =
  (EbonDownload <$> argument (EbonId <$> str) (metavar "EBON_ID"))
    <*> option
      str
      (long "output" <> help "Output file path" <> metavar "FILE" <> value "ebon.pdf")

ebonParser :: Parser Command
ebonParser =
  Ebon
    <$> ( hsubparser
            ( command
                "download"
                ( info
                    ebonDownloadParser
                    (progDesc "Download an ebon by specified id to the given file-path.")
                )
            )
            <|> pure EbonShow
        )

basketAddParser :: Parser BasketCommand
basketAddParser =
  (\listingId quantity -> BasketAdd Item{listingId, quantity})
    <$> argument (ListingId <$> str) (metavar "LISTING_ID")
    <*> optional
      ( option
          (Qty <$> auto)
          (long "qty" <> help "Absolute quantity (default: 1). Set 0 to remove")
      )

basketParser :: Parser Command
basketParser =
  Basket
    <$> ( hsubparser
            ( command
                "add"
                ( info
                    basketAddParser
                    (progDesc "Set item quantity by listing ID. Adds if new, overwrites if exists")
                )
            )
            <|> pure BasketShow
        )

orderDeleteParser :: Parser OrderCommand
orderDeleteParser = DeleteOrder <$> argument (OrderId <$> str) (metavar "ORDER_ID")

orderGetParser :: Parser OrderCommand
orderGetParser = GetOrder <$> argument (OrderId <$> str) (metavar "ORDER_ID")

orderHistoryParser :: Parser OrderCommand
orderHistoryParser = pure OrdersHistory

orderParser :: Parser Command
orderParser =
  Order
    <$> ( hsubparser
            ( command
                "delete"
                (info orderDeleteParser (progDesc "Cancel an order by order ID"))
                <> command
                  "get"
                  (info orderGetParser (progDesc "Get an order by ID"))
                <> command
                  "history"
                  (info orderHistoryParser (progDesc "Receive the order history"))
            )
            <|> pure GetOrders
        )

commandParser :: Parser Command
commandParser =
  hsubparser $
    command
      "store"
      ( info
          storeParser
          (progDesc "Show current store (no args), or use 'search'/'set' subcommands")
      )
      <> command "search" (info searchParser (progDesc "Search products by name or EAN barcode"))
      <> command
        "favorites"
        ( info
            favoritesParser
            (progDesc "Show all favorites (no args), or use 'search'/'add'/'delete' subcommands")
        )
      <> command "login" (info loginParser (progDesc "Authenticate with your REWE account"))
      <> command
        "timeslots"
        (info slotsParser (progDesc "List available pickup timeslots for current store"))
      <> command
        "basket"
        (info basketParser (progDesc "Show current basket (no args), or use 'add' subcommand"))
      <> command
        "ebons"
        ( info
            ebonParser
            (progDesc "Show the ebons (no args), or use 'download' subcommand to get the pdf.")
        )
      <> command
        "suggestion"
        ( info
            suggestionParser
            (progDesc "Create suggestions.")
        )
      <> command
        "checkout"
        ( info
            checkoutParser
            (progDesc "Show current checkout (no args), or use 'create'/'order' subcommands")
        )
      <> command
        "orders"
        ( info
            orderParser
            (progDesc "Show open orders (no args), or use 'delete'/'history' subcommand")
        )

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ("korb " <> showVersion version)
    (long "version" <> short 'v' <> help "Show version")

prettyOutput :: Parser Bool
prettyOutput = switch (long "pretty" <> short 'p' <> help "Pretty JSON output")

opts :: ParserInfo Input
opts =
  info
    (Input <$> commandParser <*> prettyOutput <**> helper <**> versionOption)
    ( fullDesc
        <> progDesc "REWE Pickup CLI"
        <> header "korb - grocery pickup ordering from the terminal"
        <> footerDoc (Just examples)
    )

examples :: Doc
examples =
  vsep
    [ "Commands:"
    , ""
    , "  korb store                       Show currently selected store"
    , "  korb store search <ZIP>          Find pickup stores near ZIP code"
    , "  korb store set <ID> <ZIP>        Set active store by market ID and ZIP"
    , ""
    , "  korb search <QUERY|EAN>          Search products by name or EAN barcode (use * to browse all)"
    , "  korb search <Q> --organic        Filter by attribute (--organic, --regional, --vegan, --vegetarian)"
    , ""
    , "  korb favorites                   Show all favorite products across all lists"
    , "  korb favorites search <QUERY>    Filter favorites by name (case-insensitive, substring match)"
    , "  korb favorites add <LID> <PID>   Add product to default favorites list (listing ID + product ID)"
    , "  korb favorites delete <ITEM_ID>  Remove item from default favorites list (item ID from favorites)"
    , ""
    , "  korb basket                      Show current basket with items, totals, and free pickup status"
    , "  korb basket add <LISTING_ID>     Set item quantity by listing ID. Overwrites existing quantity."
    , "                                   --qty N sets absolute quantity. Default 1. Use --qty 0 to remove."
    , ""
    , "  korb timeslots                   List available pickup timeslots"
    , ""
    , "  korb checkout                    Show the checkout for current basket. Payment is always in market."
    , "  korb checkout create <TIMESLOT_ID> Create a checkout for a given timeslot id"
    , "  korb checkout order              Place the order. Timeslot must be attached first."
    , ""
    , "  korb orders                      Show open orders."
    , "  korb orders history              Show all orders."
    , "  korb orders get <ORDER_ID>       Get an order (with details) by id."
    , "  korb orders delete <ORDER_ID>    Cancel an order by order ID - will give 200 on successive calls."
    , ""
    , "  korb suggestion threshold <N>    Suggest N items to add to reach free pickup threshold."
    , "                                   Ranks by purchase frequency, excludes items already in basket."
    , ""
    , "  korb ebons                       List digital receipts (eBons)."
    , "  korb ebons download <EBON_ID>    Download eBon PDF. --output FILE (default: ebon.pdf)"
    , ""
    , "  korb login                       Authenticate via REWE PKCE browser flow. Stores tokens in Keychain."
    , ""
    , "All commands support --pretty for formatted JSON output."
    , "Output is JSON for agent consumption."
    ]

parseInput :: IOE Void Input
parseInput = liftIO $ execParser opts
