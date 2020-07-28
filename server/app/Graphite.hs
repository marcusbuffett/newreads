module Graphite where

import Network.Socket as Socket
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.UTF8 as BSU
import Data.Map.Strict as Map
import Network.Socket.ByteString (recv, sendAll)
import Data.List (intercalate)

type Tags = Map.Map String String
type NameComponents = [String]

data Metric = Metric NameComponents Tags

sendMetric :: Metric -> Float -> IO ()
sendMetric metric value = runTCPClient "127.0.0.1" "4206" $ \s -> do
    now <- pure 12
    sendAll s $ BSU.fromString $ encodeMetric metric value now
    -- msg <- Socket.recv s 1024
    -- putStr "Received: "
    -- putStr msg

encodeMetric (Metric components tags) value timestamp = do
  let name = intercalate "." components 
  let encodedTags = intercalate ";" $ fmap joinTag $ Map.toList tags
  let fullname = name ++ ";" ++ encodedTags
  fullname ++ " " ++ show value ++ " " ++ show timestamp
    where 
      joinTag (name, value) = name ++ "=" ++ value

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
