{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings,
        ForeignFunctionInterface, EmptyDataDecls #-}
module Network.ADB.Client (
        newSession,
        withConnection
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
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as Seq
import Foreign
import Foreign.C
import Prelude hiding (read, catch)


data Session m = Session (Transport m) (IO Word32)

newSession :: Transport (ErrorT TransportError IO) -> ErrorT TransportError IO (Session (ErrorT TransportError IO))
newSession tra = do
    negotiate tra
    nextLocalIDRef <- liftIO $ newMVar 0
    let doAlloc 0      = doAlloc 1
        doAlloc nextID = return $ (nextID+1, nextID)
        allocLocalID = modifyMVar nextLocalIDRef doAlloc
    return $ Session tra allocLocalID

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
                liftIO $ putStrLn $ "RECV "++show pkt
                mReply <- liftIO $ adbAuth auth pkt
                case mReply of
                    Just reply -> do
                        liftIO $ putStrLn $ "SEND "++show reply
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

withConnection :: Session (ErrorT TransportError IO) 
               -> C.ByteString
               -> (Transport (ErrorT TransportError IO) -> ErrorT TransportError IO a)
               -> ErrorT TransportError IO a
withConnection session@(Session tra allocLocalID) uri code = do
    localID <- liftIO allocLocalID
    queueVar <- liftIO $ newMVar Seq.empty

    write tra $ formatPacket $ Packet OPEN localID 0 (uri <> B.singleton 0)
    let awaitResult = do
            pkt <- readPacket tra
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
            (chansVar, ch) <- liftIO $ do
                me <- myThreadId
                ch <- newChan
                chansVar <- newMVar $ M.insert me ch M.empty
                return (chansVar, ch)
            let readPkt = do
                    ee <- liftIO $ do
                        ch <- modifyMVar chansVar $ \chans -> do
                            me <- myThreadId
                            case me `M.lookup` chans of
                                Just ch -> return (chans, ch)
                                Nothing -> do
                                    ch' <- dupChan ch
                                    return (M.insert me ch' chans, ch')
                        readChan ch
                    case ee of
                        Left err -> throwError err
                        Right pkt -> return pkt

            readThr <- liftIO $ forkIO $ do
                forever $ do
                    epkt <- runErrorT $ readPacket tra
                    case epkt of
                        Left err -> writeChan ch (Left err)
                        Right pkt -> do
                            case pktCommand pkt of
                                WRTE | pktArg1 pkt == localID -> do
                                    modifyMVar_ queueVar $ return . (|> pktPayload pkt)
                                    runErrorT $ write tra $ formatPacket $ Packet OKAY localID remoteID B.empty
                                    writeChan ch (Right pkt)
                                _                             -> writeChan ch (Right pkt)

            let traNew = Transport {
                    write =
                        let writeLoop bs = case bs of
                                bs | B.null bs -> return ()
                                bs -> do
                                    let (now, next) = B.splitAt (fromIntegral mAX_PAYLOAD) bs
                                    write tra $ formatPacket $ Packet WRTE localID remoteID now
                                    let awaitResult = do
                                            pkt <- readPkt
                                            case pktCommand pkt of
                                                CLSE | pktArg1 pkt == localID -> throwError ClosedByPeer
                                                OKAY | pktArg0 pkt == remoteID -> writeLoop next
                                                _ -> awaitResult
                                    awaitResult
                        in  \bs -> writeLoop bs,
                    read = \n -> do
                        let readLoop = do
                                q <- liftIO $ readMVar queueVar
                                if Seq.length q == 0 then do
                                        pkt <- readPkt
                                        case pktCommand pkt of
                                            CLSE | pktArg1 pkt == localID -> throwError ClosedByPeer
                                            _ -> readLoop
                                    else do
                                        let hd = q `Seq.index` 0
                                        if B.length hd > n then do
                                            let (mine, notMine) = B.splitAt n hd
                                            liftIO $ modifyMVar_ queueVar $ return . (\q -> notMine <| Seq.drop 1 q)
                                            return mine
                                          else do
                                            liftIO $ modifyMVar_ queueVar $ return . (Seq.drop 1)
                                            return hd
                        readLoop,
                    close = do
                        write tra $ formatPacket $ Packet CLSE localID remoteID B.empty
                        let awaitResult = do
                                pkt <- readPacket tra
                                case pktCommand pkt of
                                    CLSE | pktArg1 pkt == localID -> throwError ClosedByPeer
                                    _ -> awaitResult
                        awaitResult
                  }
            ea <- liftIO $ runErrorT (code traNew) `finally` killThread readThr
            case ea of
                Left err -> throwError err
                Right a  -> return a

