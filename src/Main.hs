{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (replicateM)
import Data.Monoid
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Database.Redis as R
import Web.Scotty

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as TL
import qualified System.Random as SR

type Key = TL.Text
type URL = TL.Text

alphanum :: String
alphanum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

main :: IO ()
main = do
  redisConn <- R.checkedConnect R.defaultConnectInfo
  scotty 3000 $ shortener redisConn

shortener :: R.Connection -> ScottyM ()
shortener conn = do
    get "/" $ do
        url    <- param "url"
        result <- liftAndCatchIO $ store conn url

        case result of
          Left reply -> text $ TL.pack $ show reply
          Right key  -> html $ "Your shortened URL: <a href=\"" <> shortURL <> "\">" <> shortURL <> "</a>"
            where shortURL = keyToShortURL key

    get "/:key" $ do
        key    <- param "key"
        result <- liftAndCatchIO $ retrieve conn key

        case result of
          Left reply       -> text $ TL.pack $ show reply
          Right (Just url) -> redirect url
          Right Nothing    -> text $ "key " <> key <> " not found"

store :: R.Connection -> URL -> IO (Either R.Reply Key)
store conn url = do
  key    <- random 16 alphanum
  result <- R.runRedis conn $ R.set (BS.pack key) url'
  case result of
    Left reply -> return $ Left reply
    Right _    -> return $ Right (TL.pack key)
  where url' = (encodeUtf8 . TL.toStrict) url

retrieve :: R.Connection -> Key -> IO (Either R.Reply (Maybe URL))
retrieve conn key = do
  result <- R.runRedis conn $ R.get key'
  case result of
    Left reply -> return $ Left reply
    Right url  -> return $ Right $ fmap (TL.fromStrict . decodeUtf8) url
  where key' = encodeUtf8 $ TL.toStrict key

keyToShortURL :: Key -> URL
keyToShortURL key = "http://localhost:3000/" <> key

random :: Int -> [a] -> IO [a]
random n xs = replicateM n (randomElement xs)

randomElement :: [a] -> IO a
randomElement xs = do
    let maxIndex = length xs - 1
    index <- SR.randomRIO (0, maxIndex)
    return (xs !! index)
