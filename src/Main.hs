{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (replicateM)
import Data.Monoid
import Web.Scotty

import qualified Data.Text.Lazy as TL
import qualified System.Random as SR

alphanum :: String
alphanum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

main :: IO ()
main = scotty 3000 $ do
    get "/:key" $ do
        key <- param "key"
        text $ "Requested: " <> key

    get "/generate/:url" $ do
        url <- param "url"
        key <- fmap TL.pack $ liftAndCatchIO $ random 16 alphanum
        text $ "Storing " <> url <> " in key " <> key

random :: Int -> String -> IO String
random n xs = replicateM n (randomElement xs)

randomElement :: String -> IO Char
randomElement xs = do
    let maxIndex = length xs - 1
    index <- SR.randomRIO (0, maxIndex)
    return (xs !! index)
