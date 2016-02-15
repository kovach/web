{-# LANGUAGE DeriveGeneric #-} -- needed for json parsing
module Main where

import Data.Aeson (FromJSON, ToJSON, decodeStrict, decode, encode)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Data.ByteString.Lazy (ByteString)

import Network.TextServer

import Types
import Web
import Interpreter

instance FromJSON Ref
instance ToJSON Ref
instance FromJSON Sym
instance ToJSON Sym
instance FromJSON Lit
instance ToJSON Lit
instance FromJSON VE
instance ToJSON VE
instance FromJSON Arrow
instance ToJSON Arrow

data ServerCommand
  = SCGet
  | SCNew [Arrow]
  deriving (Show, Generic)

instance FromJSON ServerCommand
instance ToJSON ServerCommand

type Data = ByteString

parseComm :: Data -> Maybe ServerCommand
parseComm = decode

handler :: Network.TextServer.Handler Web
handler msg web = fromMaybe (return (Nothing, web)) command
  where
    command = do
      c <- parseComm msg
      return $ case c of
        SCGet -> do
          putStrLn "get command"
          return (Just $ encode $ toRows web, web)
        SCNew arrows ->
          let (_, web') = foldl' ne ([], web) arrows
          -- TODO send increment in web (change newEdge to return added edge)
          in do
            putStrLn "put command"
            return (Nothing, web')
    ne (c, w) a = newEdge a c w

main = do
  putStrLn "running"
  mweb <- loadWeb "elements.web"
  case mweb of
    Right (web, "") -> do
      putStrLn "loaded web file"
      runServer web handler
    _ -> do
      putStrLn "failed to load web file"
      runServer emptyWeb handler
