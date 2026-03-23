## // korb

The `korb` CLI allows using the REWE APIs programmatically to create baskets and order them for pickup to your local REWE market.

The CLI is handwritten in Haskell, but designed to be a CLI mostly used by agents to organise your REWE shopping on your behalf. All output is JSON.

https://github.com/user-attachments/assets/308041f4-4b1f-4db4-9040-0e157247efea

## // Installation

### Binary (recommended)

Download the latest release (currently Mac OS supported) from [GitHub Releases](https://github.com/yannick-cw/korb/releases):

```bash
# macOS (Apple Silicon)
curl -L https://github.com/yannick-cw/korb/releases/latest/download/korb-aarch64-macos -o korb
chmod +x korb
mv korb /usr/local/bin/
```

### From source

Requires GHC 9.12+ and Cabal. You also need the REWE mTLS client certificates.

```bash
git clone https://github.com/yannick-cw/korb.git
cd korb

# Extract mTLS certificates from the REWE app
# See: https://github.com/ByteSizedMarius/rewerse-engineering/tree/main/docs
# Place them at:
#   certs/mobile-clients-api.rewe.de/private.pem
#   certs/mobile-clients-api.rewe.de/private.key

cabal install --install-method=copy
```

## // Getting started

```bash
# 1. Authenticate with your REWE account (one-time, use Chrome to get the link, firefox does not work for this)
korb login --pretty

# 2. Find and set your pickup store - stored locally, needs to be done just once
korb store search 80336 # returns market identifier for zip code
korb store set 420240 80336

# 3. Search for products
korb search "milch" --pretty
korb search "milch" --organic --pretty
# Or browse your favorites
korb favorites
korb favorites search "avo"

# 4. Add items to basket
korb basket add 8-6AJQ3RLX-bd7c2a1e-f1c1-3464-8250-900b12c90f70

# 5. Check your basket
korb basket --pretty

# 6. Pick a timeslot and checkout
korb timeslots # lists available timeslots for pickup
korb checkout create <TIMESLOT_ID>  # create checkout - not ordered yet
korb checkout order # order it with selected timeslot and payment in market

# 7. Cancel an order if needed
korb orders
korb orders delete <ORDER_ID>
```

## // Usage

```
korb COMMAND [-p|--pretty] [-v|--version]

Commands:

  korb store                       Show currently selected store
  korb store search <ZIP>          Find pickup stores near ZIP code
  korb store set <ID> <ZIP>        Set active store by market ID and ZIP

  korb search <QUERY>              Search products in current store (use * to browse all)
  korb search <Q> --organic        Filter by attribute (--organic, --regional, --vegan, --vegetarian)

  korb favorites                   Show all favorite products across all lists
  korb favorites search <QUERY>    Filter favorites by name (case-insensitive, substring match)
  korb favorites add <LID> <PID>   Add product to default favorites list (listing ID + product ID)
  korb favorites delete <ITEM_ID>  Remove item from default favorites list (item ID from favorites)

  korb basket                      Show current basket with items, totals, and free pickup status
  korb basket add <LISTING_ID>     Set item quantity by listing ID. Overwrites existing quantity.
                                   --qty N sets absolute quantity. Default 1. Use --qty 0 to remove.

  korb timeslots                   List available pickup timeslots

  korb checkout                    Show the checkout for current basket. Payment is always in market.
  korb checkout create <TIMESLOT_ID> Reserve timeslot, create checkout, and set payment to market pickup
  korb checkout order              Confirm and place the order. Timeslot must be attached first.

  korb orders                      Show open orders
  korb orders delete <ORDER_ID>    Cancel an order by order ID - will give 200 on successive calls.

  korb login                       Authenticate via REWE PKCE browser flow. Stores tokens in Keychain.

All commands support --pretty for formatted JSON output.
```

## // API documentation

Reverse-engineered OpenAPI specs for the REWE mobile API:

- [REWE Product & Checkout API](api/rewe.openapi.yaml)
- [REWE Account / Keycloak API](api/account-rewe.yaml)

## // Platform support

Currently macOS only (uses macOS Keychain for token storage). Linux support planned.

## // Prior work & attribution

- https://github.com/ByteSizedMarius/rewerse-engineering
- https://github.com/foo-git/rewe-discounts
- https://github.com/torbenpfohl/rewe-discounts

## // Disclaimer

This project is unofficial and not affiliated with REWE. It uses reverse-engineered APIs that may change without notice. Use at your own risk.
