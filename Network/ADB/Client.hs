{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings,
        ForeignFunctionInterface, EmptyDataDecls #-}
module Network.ADB.Client (
        Session,
        withClientSession,
        withConnect,
        module Network.ADB.Transport
    ) where

import Network.ADB.Common
import Network.ADB.Transport

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Error
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as BI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Foreign
import Foreign.C
import Prelude hiding (read, catch)


withClientSession :: Transport (ErrorT TransportError IO)
                  -> (Session (ErrorT TransportError IO) -> ErrorT TransportError IO a)
                  -> ErrorT TransportError IO a
withClientSession tra code = do
    negotiate tra
    withSession tra code

-- | Performs negotiation on a packetTransport
negotiate :: (MonadError TransportError m, Applicative m, MonadIO m) =>
             Transport m -> m ()
negotiate tra = do
    write tra $ formatPacket $ Packet SYNC 1 0 B.empty
    write tra $ formatPacket $ Packet CNXN a_VERSION mAX_PAYLOAD "host::\0"
    auth <- liftIO adbAuthNew
    awaitCNXN auth
  where
    awaitCNXN auth = do
        pkt <- readPacket tra
        case pktCommand pkt of
            CNXN -> return ()
            AUTH -> do
                --liftIO $ putStrLn $ "RECV "++show pkt
                mReply <- liftIO $ adbAuth auth pkt
                case mReply of
                    Just reply -> do
                        --liftIO $ putStrLn $ "SEND "++show reply
                        write tra $ formatPacket reply
                    Nothing -> return ()
                awaitCNXN auth
            _    -> awaitCNXN auth

data ADBAuth_Struct
type ADBAuth = Ptr ADBAuth_Struct

foreign import ccall unsafe "adb_auth_new" adbAuthNew
    :: IO ADBAuth

{- Handle adb's AUTH command. If it wants to reply, it will return
   with 1 and pkt will contain the packet to be sent. -}
foreign import ccall unsafe "adb_auth" _adb_auth
    :: ADBAuth -> Ptr Word8 -> IO CInt

adbAuth :: ADBAuth -> Packet -> IO (Maybe Packet)
adbAuth auth pkt = do
    pkt_c <- B.unsafeUseAsCStringLen (formatPacket pkt) $ \(pkt_c, len) -> do
        p <- mallocBytes (4096 + 24)
        BI.memcpy p (castPtr pkt_c) len
        return p
    ret <-_adb_auth auth pkt_c
    if ret /= 0
        then do
            outLen <- peekElemOff (castPtr pkt_c :: Ptr Word32) 3
            let olen = fromIntegral outLen + 24
            bs' <- BI.create olen $ \p -> BI.memcpy p pkt_c olen
            free pkt_c
            case parsePacket bs' of
                Left err -> fail $ "bad packet from adbAuth: "++err
                Right opkt -> return $ Just opkt
        else do
            free pkt_c
            return Nothing

withConnect :: Session (ErrorT TransportError IO) 
            -> C.ByteString
            -> (Transport (ErrorT TransportError IO) -> ErrorT TransportError IO a)
            -> ErrorT TransportError IO a
withConnect session@(Session allocLocalID writePkt readPkt) uri code = do
    localID <- liftIO allocLocalID
    writePkt $ Packet OPEN localID 0 (uri <> B.singleton 0)
    let awaitResult = do
            pkt <- readPkt
            if pktCommand pkt == OKAY && pktArg1 pkt == localID
                then return $ Just $ pktArg0 pkt
                else if pktCommand pkt == CLSE && pktArg1 pkt == localID
                    then return $ Nothing
                    else awaitResult
    mTransportID <- awaitResult

    case mTransportID of
        Nothing -> do
            throwError $ ConnectionFailure "no response"
        Just remoteID -> do
            withConversation session localID remoteID code

