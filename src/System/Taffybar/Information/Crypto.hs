{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.Crypto
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides utility functions for retrieving data about crypto
-- assets.
-----------------------------------------------------------------------------
module System.Taffybar.Information.Crypto where

import           BroadcastChan
import qualified CoinbasePro.Environment as CB
import qualified CoinbasePro.Headers as CB
import qualified CoinbasePro.Request as CB
import qualified CoinbasePro.Types as CB
import qualified CoinbasePro.Unauthenticated.API as CB
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.UTF8 as BS
import           Data.Proxy
import qualified Data.Text
import           Data.Time
import           GHC.TypeLits
import           Network.HTTP.Simple hiding (Proxy)
import           System.Taffybar.Context
import           System.Taffybar.Util
import           Text.Printf

newtype CryptoPriceInfo = CryptoPriceInfo { lastPrice :: Double }

newtype CryptoPriceChannel (a :: Symbol) =
  CryptoPriceChannel (BroadcastChan In CryptoPriceInfo)

getCryptoPriceChannel :: KnownSymbol a => TaffyIO (CryptoPriceChannel a)
getCryptoPriceChannel = getStateDefault $ buildCryptoPriceChannel (60.0 :: Double)

buildCryptoPriceChannel ::
  forall a m d. (KnownSymbol a, MonadIO m, RealFrac d) => d -> m (CryptoPriceChannel a)
buildCryptoPriceChannel delay = do
  chan <- newBroadcastChan
  let symbol = Data.Text.pack $ symbolVal (Proxy :: Proxy a)
  _ <- foreverWithDelay delay $ liftIO $ void $
    getLatestPrice symbol >>= writeBChan chan
  return $ CryptoPriceChannel chan

getLatestPrice :: MonadIO m => Data.Text.Text -> m CryptoPriceInfo
getLatestPrice productString =
  CryptoPriceInfo . CB.unPrice . CB.close . head <$> getDaysCandles productString

getDaysCandles :: MonadIO m => Data.Text.Text -> m [CB.Candle]
getDaysCandles productString = liftIO $ do
  oneDayAgo <- addUTCTime (-60*60*24) <$> getCurrentTime
  let candles =
        CB.candles (CB.ProductId productString)
            (Just oneDayAgo) Nothing CB.Minute
  CB.run CB.Production (candles CB.userAgent)

getCryptoMeta :: MonadIO m => String -> String -> m LBS.ByteString
getCryptoMeta cmcAPIKey symbol = do
  let headers = [("X-CMC_PRO_API_KEY", BS.fromString cmcAPIKey)] :: RequestHeaders
      uri = printf "https://pro-api.coinmarketcap.com/v1/cryptocurrency/info?symbol=%s"
            symbol
      request = setRequestHeaders headers $ parseRequest_ uri
  getResponseBody <$> httpLBS request
