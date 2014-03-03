{-# LANGUAGE FlexibleContexts #-}
module Network.ADB.Common where

import Network.ADB.Transport

import Control.Applicative
import Control.Monad.Error
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
import Data.Serialize
import Data.Word


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

