{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (replicateM)
import Data.Monoid

import qualified Database.Redis as R
import Web.Scotty

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified System.Random as SR

redisConfig = R.defaultConnectInfo
redisConn   = R.checkedConnect redisConfig

alphanum :: String
alphanum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

main :: IO ()
main = scotty 3000 $ do
    get "/:key" $ do
        key    <- param "key"
        conn   <- liftAndCatchIO $ redisConn
        result <- liftAndCatchIO $ R.runRedis conn $ R.get key

        case result of
          Left reply       -> text $ TL.pack $ show reply
          Right (Just url) -> text $ TL.fromStrict $ TE.decodeUtf8 url
          Right Nothing    -> text $ "key " <> TL.fromStrict (TE.decodeUtf8 key) <> " not found"

    get "/generate/:url" $ do
        url <- param "url"
        key <- fmap TL.pack $ liftAndCatchIO $ random 16 alphanum
        text $ "Storing " <> url <> " in key " <> key

random :: Int -> [a] -> IO [a]
random n xs = replicateM n (randomElement xs)

randomElement :: [a] -> IO a
randomElement xs = do
    let maxIndex = length xs - 1
    index <- SR.randomRIO (0, maxIndex)
    return (xs !! index)
