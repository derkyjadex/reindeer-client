{-# Language OverloadedStrings #-}

module Main where

import Network.Connection
import qualified Data.ByteString as B
import Data.List.Split (splitOn)

connParams = ConnectionParams {  connectionHostname = "10.112.156.136"
                        , connectionPort = 8080
                        , connectionUseSecure = Nothing
                        , connectionUseSocks = Nothing }

data Compass = Compass Int Int Int Int (Maybe Int) deriving (Show, Eq)

parseDirs :: String -> Compass
parseDirs input =
        let [_:n,_:e,_:s,_:w,_:p] = splitOn " " input
            p' = case p of
                     "?" -> Nothing
                     "X" -> Just 0
                     _ -> Just (read p)
        in Compass (read n) (read e) (read s) (read w) p'


--main :: IO String
main = do
        ctx <- initConnectionContext
        conn <- connectTo ctx connParams
        greeting <- connectionGetLine 4096 conn
        putStrLn $ show greeting
        connectionPut conn "haskell\n"
        nextMsg <- connectionGetLine 4096 conn
        putStrLn $ show nextMsg

