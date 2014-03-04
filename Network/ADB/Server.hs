{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings,
        ForeignFunctionInterface, EmptyDataDecls #-}
module Network.ADB.Server (
        Session,
        withServerSession,
        withAccept,
        module Network.ADB.Transport
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Error
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import Network.ADB.Common
import Network.ADB.Transport


withServerSession :: Transport (ErrorT TransportError IO)
                  -> ByteString
                  -> (Session (ErrorT TransportError IO) -> ErrorT TransportError IO a)
                  -> ErrorT TransportError IO a
withServerSession tra descriptor code = do
    negotiate descriptor tra
    withSession tra code

-- | Performs negotiation on a packetTransport
negotiate :: (MonadError TransportError m, Applicative m, MonadIO m) =>
             ByteString
          -> Transport m -> m ()
negotiate descriptor tra = do
        pkt <- readPacket tra
        case pkt of
            Packet CNXN _ _ _ -> write tra $ formatPacket $ Packet CNXN a_VERSION mAX_PAYLOAD (descriptor <> "\0")
            _                 -> negotiate descriptor tra

-- | Loop forever, accepting incoming connections. The specified function returns some
-- code to spawn on a new thread if we should accept a connection on the specified URI.
withAccept :: Session (ErrorT TransportError IO) 
           -> (C.ByteString -> Maybe (Transport (ErrorT TransportError IO) -> ErrorT TransportError IO ()))
           -> ErrorT TransportError IO ()
withAccept session@(Session allocLocalID writePkt readPkt) qCode = forever $ do
    pkt <- readPkt
    case pkt of
        Packet OPEN remoteID 0 uri ->
            if "\0" `C.isSuffixOf` uri
                then case qCode (C.take (C.length uri - 1) uri) of
                         Just code -> do
                             _ <- liftIO $ forkIO $ do
                                 localID <- allocLocalID
                                 _ <- runErrorT $ do
                                     writePkt $ Packet OKAY localID remoteID C.empty
                                     withConversation session localID remoteID code
                                 return ()
                             return ()
                         Nothing -> writePkt $ Packet CLSE 0 remoteID C.empty
                else writePkt $ Packet CLSE 0 remoteID C.empty
        _ -> return ()

