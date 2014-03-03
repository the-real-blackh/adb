{-# LANGUAGE ScopedTypeVariables #-}
module Network.ADB.Socket where

import Control.Exception
import Control.Monad.Error
import Network.ADB.Transport
import Network.Socket (Socket, sClose)
import Network.Socket.ByteString
import System.IO.Error
import Prelude hiding (read)


socketTransport :: Socket -> Transport (ErrorT TransportError IO)
socketTransport s = Transport {
        write = \bs -> do
            --liftIO $ putStrLn $ hexdump 0 (C.unpack bs)   -- ###
            ee <- liftIO . try $ sendAll s bs
            case ee of
                Left (exc :: IOError) -> throwError ClosedByPeer
                Right () -> return (),
        read = \n -> do
            ee <- liftIO . try $ recv s n
            case ee of
                Left (exc :: IOError) -> throwError ClosedByPeer
                Right blk -> do
                    --liftIO $ putStrLn $ hexdump 0 (C.unpack blk)   -- ###
                    return blk,
        close = do
            ee <- liftIO . try $ sClose s
            case ee of
                Left (exc :: IOError) -> throwError ClosedByPeer
                Right blk -> return blk
    }

