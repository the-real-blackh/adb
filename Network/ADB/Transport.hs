{-# LANGUAGE ScopedTypeVariables #-}
module Network.ADB.Transport where


import Control.Exception
import Control.Monad.Error
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import Network.Socket.ByteString
import System.IO.Error (IOError(..))
import Prelude hiding (read)


-- | \'m\' is the monad to to run it in, \'h\' is the heuristic value which, where
-- \'e\' is 'ByteString', is the number of bytes to read.  \'e\' is the stream
-- element type.
data Transport m = Transport {
        write :: ByteString -> m (),  -- ^ Write an item.
        read  :: Int -> m ByteString, -- ^ Read an item with 'h' being the heuristic to say what it
                                      -- is we want to read.
        close :: m ()
    }

data TransportError = ADBFailure String
                    | SocketFailure IOError
                    | ConnectionFailure ByteString
                    | ClosedByPeer
                    | IllegalData
    deriving Show

instance Error TransportError where
    noMsg = IllegalData
    strMsg _ = IllegalData

-- | Block until the specified number of bytes has been read.
readFully :: Monad m => Transport m -> Int -> m ByteString
readFully rem n = doRecv C.empty
  where
    doRecv sofar | C.length sofar == n = return sofar
    doRecv sofar = do
        blk <- read rem (n - C.length sofar)
        doRecv (sofar `mappend` blk)

