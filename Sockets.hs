module Sockets where

import Prelude ()
import Relude
import Network.Socket (Socket)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.Trans.Resource (runResourceT, allocate, ResourceT, ReleaseKey)

import Chunks (repeatUntilIO)
import Control.Exception.Safe (try, IOException)


makeFriend :: S.SockAddr -> IO ()
makeFriend address = runResourceT @IO do
    (_, s) <- allocate 
        (S.socket S.AF_INET S.Stream S.defaultProtocol) -- opening socket
        S.close
    liftIO do 
        S.setSocketOption s S.UserTimeout 1000
        S.connect s address
        S.sendAll s $ T.encodeUtf8 $
            T.pack "Hello, will you be my friend"
        repeatUntilIO (S.recv s 1024) BS.null BS.putStr
        S.gracefulClose s 1000

findHaskellWebsite :: IO S.AddrInfo
findHaskellWebsite = do
    addrInfos <- S.getAddrInfo
        (Just S.defaultHints { S.addrSocketType = S.Stream })
        (Just "www.haskell.org")
        (Just "http")
    case addrInfos of
        [] -> fail "getAddrInfo returned []"
        x : _ -> return x

    
makeFriendAddrInfo :: S.AddrInfo -> IO ()
makeFriendAddrInfo addr = runResourceT @IO do
    (_, s) <- allocate (S.openSocket addr) S.close
    liftIO $ do
        S.setSocketOption s S.UserTimeout 1000
        S.connect s (S.addrAddress addr)
        S.sendAll s $ T.encodeUtf8 $ T.pack "Hello"
        repeatUntilIO (S.recv s 1024) BS.null BS.putStr
        S.gracefulClose s 1000

-- Exercises

--openAndConnect :: S.AddrInfo -> Maybe ResourceT IO (ReleaseKey, Socket)
--openAndConnect addr = allocate setup S.close
--    where
--        setup = do
--            s <- S.openSocket addr
--            S.setSocketOption s S.UserTimeout 1000
--            result <- try (S.connect s (S.addrAddress adrr))
--            case result of
--                Left e -> print e
--                Right c -> print "Connection succeeded"
--
-- Exercise 10
openAndConnect :: S.AddrInfo -> ResourceT IO (ReleaseKey, Maybe Socket)
openAndConnect addressInfo = allocate setup close
    where
        setup = do
            s <- S.openSocket addressInfo
            S.setSocketOption s S.UserTimeout 1000
            result <- try $ S.connect s (S.addrAddress addressInfo) :: IO (Either IOException ())
            case result of
                Left _ -> return Nothing
                Right _ -> return $ Just s

        close x = case x of
            Just s -> S.close s
            Nothing -> print "Connection failed"

findGopherWeb :: IO S.AddrInfo
findGopherWeb = do
    addrInfos <- S.getAddrInfo
        (Just S.defaultHints { S.addrSocketType = S.Stream })
        (Just "quux.org")
        (Just "gopher")
    case addrInfos of
        [] -> fail "getAddrInfo returned []"
        x : _ -> return x

resolve :: S.ServiceName -> S.HostName -> IO S.AddrInfo
resolve servName hostName = do
    addrInfos <- S.getAddrInfo
        (Just S.defaultHints { S.addrSocketType = S.Stream })
        (Just hostName)
        (Just servName)
    case addrInfos of
        [] -> fail ("No address info found for " <> show servName <> " " <> show hostName)
        x : _ -> return x


callGopher :: S.AddrInfo -> IO ()
callGopher info = runResourceT @IO do
    (_, s) <- allocate (S.openSocket info) S.close
    liftIO do
        S.setSocketOption s S.UserTimeout 1000
        S.connect s (S.addrAddress info)
        S.sendAll s $ T.encodeUtf8 $ T.pack "\r\n"
        repeatUntilIO (S.recv s 1024) BS.null BS.putStr
        S.gracefulClose s 1000
