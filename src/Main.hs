{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (replicateM)
import Data.Monoid
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Database.Redis as R
import Web.Scotty

import qualified Data.Text.Lazy as TL
import qualified System.Random as SR

redisConfig = R.defaultConnectInfo
redisConn   = R.checkedConnect redisConfig

alphanum :: String
alphanum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        url    <- param "url"
        key    <- fmap TL.pack $ liftAndCatchIO $ random 16 alphanum
        conn   <- liftAndCatchIO $ redisConn
        result <- liftAndCatchIO $ R.runRedis conn $ R.set (encodeUtf8 (TL.toStrict key)) url

        case result of
          Left reply   -> text $ TL.pack $ show reply
          Right R.Ok   -> text $ "Your shortened URL: http://localhost:3000/" <> key
          Right status -> text $ "Unexpected error: " <> TL.pack (show status)

    get "/:key" $ do
        key    <- param "key"
        conn   <- liftAndCatchIO $ redisConn
        result <- liftAndCatchIO $ R.runRedis conn $ R.get key

        case result of
          Left reply       -> text $ TL.pack $ show reply
          Right (Just url) -> text $ TL.fromStrict $ decodeUtf8 url
          Right Nothing    -> text $ "key " <> TL.fromStrict (decodeUtf8 key) <> " not found"

random :: Int -> [a] -> IO [a]
random n xs = replicateM n (randomElement xs)

randomElement :: [a] -> IO a
randomElement xs = do
    let maxIndex = length xs - 1
    index <- SR.randomRIO (0, maxIndex)
    return (xs !! index)
