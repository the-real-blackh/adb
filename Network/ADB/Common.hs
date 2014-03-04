{-# LANGUAGE FlexibleContexts #-}
module Network.ADB.Common where

import Network.ADB.Transport

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.Error
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Serialize
import Data.Word
import Prelude hiding (read)


mAX_PAYLOAD :: Word32
mAX_PAYLOAD = 4096

a_VERSION :: Word32
a_VERSION = 0x01000000        -- ADB protocol version

data Command = SYNC | CNXN | AUTH | OPEN | OKAY | CLSE | WRTE
    deriving (Eq, Ord, Show)

fromCommand :: Command -> Word32
fromCommand SYNC = 0x434e5953
fromCommand CNXN = 0x4e584e43
fromCommand AUTH = 0x48545541
fromCommand OPEN = 0x4e45504f
fromCommand OKAY = 0x59414b4f
fromCommand CLSE = 0x45534c43
fromCommand WRTE = 0x45545257

toCommand :: Word32 -> Maybe Command
toCommand 0x434e5953 = Just SYNC
toCommand 0x4e584e43 = Just CNXN
toCommand 0x48545541 = Just AUTH
toCommand 0x4e45504f = Just OPEN 
toCommand 0x59414b4f = Just OKAY 
toCommand 0x45534c43 = Just CLSE 
toCommand 0x45545257 = Just WRTE
toCommand _          = Nothing

data Packet = Packet {
        pktCommand :: Command,
        pktArg0    :: Word32,
        pktArg1    :: Word32,
        pktPayload :: ByteString
    }
    deriving Show

formatPacket :: Packet -> ByteString
formatPacket pkt = runPut $ do
    let cmd = fromCommand . pktCommand $ pkt
    putWord32le $ cmd
    putWord32le . pktArg0 $ pkt
    putWord32le . pktArg1 $ pkt
    putWord32le . fromIntegral . B.length . pktPayload $ pkt
    putWord32le . foldl' (+) 0 . map fromIntegral . B.unpack . pktPayload $ pkt
    putWord32le . complement $ cmd
    putByteString . pktPayload $ pkt

parsePacket :: ByteString -> Either String Packet
parsePacket = runGet $ do
    cmdNo <- getWord32le
    case toCommand cmdNo of
        Just cmd -> do
            arg0 <- getWord32le
            arg1 <- getWord32le
            len <- getWord32le
            _ <- getWord32le
            _ <- getWord32le
            payload <- getByteString (fromIntegral len)
            return $ Packet cmd arg0 arg1 payload
        Nothing  -> fail $ "bad command"

readPacket :: (MonadError TransportError m, Functor m, Applicative m) =>
              Transport m -> m Packet
readPacket rem = do
    Right [cmdNo, arg0, arg1, len, chk, magic] <- runGet (sequence $ replicate 6 getWord32le) <$> readFully rem (6*4)
    when (cmdNo /= complement magic) $ throwError IllegalData
    cmd <- case toCommand cmdNo of
        Just cmd -> return cmd
        Nothing  -> throwError IllegalData
    when (len > fromIntegral mAX_PAYLOAD) $ throwError IllegalData
    payload <- if len == 0 then pure B.empty else readFully rem (fromIntegral len)
    return $ Packet cmd arg0 arg1 payload

data Session m = Session (IO Word32) (Packet -> m ()) (m Packet)

withSession :: Transport (ErrorT TransportError IO)
            -> (Session (ErrorT TransportError IO) -> ErrorT TransportError IO a)
            -> ErrorT TransportError IO a
withSession tra code = do
    nextLocalIDRef <- liftIO $ newMVar 0
    let doAlloc 0      = doAlloc 1
        doAlloc nextID = return $ (nextID+1, nextID)
        allocLocalID = modifyMVar nextLocalIDRef doAlloc

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
                Right pkt -> writeChan ch (Right pkt)

    ee <- liftIO $ runErrorT (code (Session allocLocalID (write tra . formatPacket) readPkt))
      `finally` killThread readThr
    case ee of
        Left err -> throwError err
        Right a  -> return a

withConversation :: Session (ErrorT TransportError IO)
                                  -> Word32
                                  -> Word32
                                  -> (Transport (ErrorT TransportError IO) -> ErrorT TransportError IO a)
                                  -> ErrorT TransportError IO a
withConversation session@(Session allocLocalID writePkt readPkt0) localID remoteID code = do
    queueVar <- liftIO $ newMVar Seq.empty
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
            epkt <- runErrorT $ readPkt0
            case epkt of
                Left err -> writeChan ch (Left err)
                Right pkt -> do
                    case pktCommand pkt of
                        WRTE | pktArg1 pkt == localID -> do
                            modifyMVar_ queueVar $ return . (|> pktPayload pkt)
                            runErrorT $ writePkt $ Packet OKAY localID remoteID B.empty
                            writeChan ch (Right pkt)
                        _                             -> writeChan ch (Right pkt)

    let traNew = Transport {
            write =
                let writeLoop bs = case bs of
                        bs | B.null bs -> return ()
                        bs -> do
                            let (now, next) = B.splitAt (fromIntegral mAX_PAYLOAD) bs
                            writePkt $ Packet WRTE localID remoteID now
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
                writePkt $ Packet CLSE localID remoteID B.empty
                let awaitResult = do
                        pkt <- readPkt
                        case pktCommand pkt of
                            CLSE | pktArg1 pkt == localID -> throwError ClosedByPeer
                            _ -> awaitResult
                awaitResult
          }
    ea <- liftIO $ runErrorT (code traNew) `finally` killThread readThr
    case ea of
        Left err -> throwError err
        Right a  -> return a

