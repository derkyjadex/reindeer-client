{-# Language OverloadedStrings #-}

module Main where

import Network.Connection
import qualified Data.ByteString as B
import Data.List.Split (splitOn)
import Data.ByteString.Char8 (unpack, pack)

connParams = ConnectionParams { connectionHostname = "10.112.156.136"
                              , connectionPort = 8080
                              , connectionUseSecure = Nothing
                              , connectionUseSocks = Nothing
                              }

data Compass = Compass { north :: Int
                       , east :: Int
                       , south :: Int
                       , west :: Int
                       , present :: Maybe Move
                       } deriving (Show, Read, Eq)

data Move = N | S | E | W | Done deriving (Show, Read, Eq)

parseMsg :: String -> Compass
parseMsg input =
        let [_:n,_:e,_:s,_:w,_:p] = splitOn " " input
            p' = case p of
                     "?" -> Nothing
                     "X" -> Just Done 
                     _ -> Just (read p)
        in Compass (read n) (read e) (read s) (read w) p'

makeMove :: Move -> Compass -> Maybe Move
makeMove _ (Compass _ _ _ _ (Just Done)) = Nothing
makeMove _ (Compass _ _ _ _ (Just m)) = Just m

makeMove N c
  | west  c > 0 = Just W
  | north c > 0 = Just N
  | east  c > 0 = Just E
  | south c > 0 = Just S

makeMove W c
  | south c > 0 = Just S
  | west  c > 0 = Just W
  | north c > 0 = Just N
  | east  c > 0 = Just E

makeMove S c
  | east  c > 0 = Just E
  | south c > 0 = Just S
  | west  c > 0 = Just W
  | north c > 0 = Just N

makeMove E c
  | north c > 0 = Just N
  | east  c > 0 = Just E
  | south c > 0 = Just S
  | west  c > 0 = Just W

makeMove _ _ = error "Trapped! :("

mainLoop :: (Connection, Move) -> IO (Connection, Move)
mainLoop (conn, oldMove) = do
        msg <- connectionGetLine 4096 conn
        putStr $ (unpack msg) ++ " -> "
        let compass = parseMsg $ unpack msg
        case makeMove oldMove compass of
            Just move -> do
                putStrLn $ show move
                connectionPut conn $ pack $ show move
                connectionPut conn "\n"
                mainLoop (conn, move)
            Nothing -> do
                putStrLn "Done!!!"
                return (conn, Done)

main = do
        ctx <- initConnectionContext
        conn <- connectTo ctx connParams
        greeting <- connectionGetLine 4096 conn
        putStrLn $ unpack greeting
        connectionPut conn "haskell\n"
        mainLoop (conn, N)

