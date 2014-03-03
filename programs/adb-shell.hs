-- This program talks to a locally running Android Virtual Device, giving you a
-- shell.
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Exception
import Control.Monad.Error
import qualified Data.ByteString.Char8 as B
import Network.ADB.Client
import Network.ADB.Socket
import Network.ADB.Transport
import Network.Socket
import Prelude hiding (read)
import System.IO
import System.Posix.Terminal
import System.Posix.Types

main = do
    putStrLn "connecting to ADB shell on local Android emulator"
    putStrLn "Make sure you enable USB debugging in your emulator, which you'll"
    putStrLn "find under '{ } Developer options'"
    putStrLn "press ctrl-D or 'exit' to quit"
    putStrLn ""
    skt <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr "127.0.0.1"
    connect skt (SockAddrInet 5555 addr)
    let stra = socketTransport skt
    ee <- runErrorT $ do
        ses <- newSession stra
        let uri = "shell:"
        --let uri = "tcp:5000"   -- Connect to a localhost socket on the Android device
        withConnection ses uri $ \tra -> do
            attrs <- liftIO $ do
                --hSetEcho stdin False
                hSetBuffering stdin NoBuffering
                attrs <- getTerminalAttributes (Fd 0)
                let attrs' = attrs `withoutMode` EnableEcho
                                   `withoutMode` ProcessInput
                                   `withoutMode` ProcessOutput
                                   `withoutMode` MapCRtoLF
                                   `withoutMode` IgnoreBreak
                                   `withoutMode` IgnoreCR
                                   `withoutMode` MapLFtoCR
                                   `withoutMode` EchoLF
                                   `withoutMode` ExtendedFunctions
                                   `withoutMode` KeyboardInterrupts
                setTerminalAttributes (Fd 0) attrs' Immediately
                return attrs
            writeThr <- liftIO $ forkIO $ do
                runErrorT $ forever $ do
                    l <- liftIO $ B.hGetSome stdin 1024
                    write tra l
                return ()
            -- Need to lift and unlift to use finally, if we don't want to
            -- use some fancy package like 'exceptions' to do this for us.
            ee <- liftIO $ runErrorT (forever $ do
                    x <- read tra 1024
                    liftIO $ do
                        B.hPutStr stdout x
                        hFlush stdout
                ) `finally` do
                    killThread writeThr
                    setTerminalAttributes (Fd 0) attrs Immediately
            case ee of
                Left err -> throwError err
                Right () -> return ()
    case ee of
        Left err -> putStrLn $ "FAILED: "++show err
        Right () -> return ()

