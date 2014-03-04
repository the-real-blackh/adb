-- This program talks to a locally running Android Virtual Device, giving you a
-- shell.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.Error
import qualified Data.ByteString.Char8 as B
import Data.List
import Network.ADB.Client
import Network.ADB.Server
import Network.ADB.Socket
import Network.Socket
import Prelude hiding (read)
import qualified Prelude
import System.Environment
import System.Exit
import System.IO
import System.Posix.Terminal
import System.Posix.Types


main = withSocketsDo $ do
    args <- getArgs
    let def_host = "localhost"
        def_port = "5555"
        def_uri = "shell:"
        (host, port, uri) = case args of
            [] -> (def_host, def_port, def_uri)
            [host] -> (host, def_port, def_uri)
            [host, port] -> (host, port, def_uri)
            (host:port:uri:_) -> (host, port, B.pack uri)
    putStrLn $ "  Usage is: adb-shell {host {port {uri}}}      (client connection)"
    putStrLn $ "            adb-shell --server port            (test server)"
    putStrLn $ "    where { } means 'optional'"
    putStrLn ""
    if host == "--server" then do
        let localAddresses = ["*"]   -- On Windows this should be ["", "localhost"]
        ais <- concat <$> forM localAddresses (\addr ->
            getAddrInfo (Just $ AddrInfo {
                addrFlags = [AI_PASSIVE],
                addrFamily = AF_UNSPEC,
                addrSocketType = Stream,
                addrProtocol = 0,
                addrAddress = SockAddrInet 0 0,
                addrCanonName = Nothing
              }) (Just addr) (Just port)
          )
        forM_ (nub ais) $ \ai -> do
            ss <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
            setSocketOption ss ReusePort 1
              `catch` \(exc :: IOException) ->
                return ()  -- ignore failure on Windows
            when (addrFamily ai == AF_INET6) $ setSocketOption ss IPv6Only 1
            bind ss (addrAddress ai)
            listen ss 50
            putStrLn $ "listening on port "++port++" and uri "++show uri
            _ <- forkIO $ forever $ do
                (s, _) <- accept ss
                _ <- forkIO $ do
                    let tra = socketTransport s
                    _ <- runErrorT $ withServerSession tra "device::" $ \ses -> do
                        withAccept ses $ \descriptor ->
                            if descriptor /= uri
                                then Nothing
                                else Just $ \tra -> do
                                    liftIO $ do
                                        hPutStrLn stdout "incoming connection started"
                                        hFlush stdout
                                    terminal tra
                    return ()
                  `finally` sClose s
                return ()
            return ()
        forever $ threadDelay 10000000
      else do
        putStrLn $ "connecting to "++host++":"++port++" and uri "++show uri
        putStrLn ""
        putStrLn "Default port 5555 is used by a local Android emulator:"
        putStrLn "  Make sure you enable USB debugging in your emulator, which you'll"
        putStrLn "  find in settings under '{ } Developer options'"
        putStrLn "  press ctrl-D or 'exit' to quit"
        putStrLn ""
        let hints = defaultHints { addrSocketType = Stream }
        ais <- getAddrInfo (Just hints) (Just host) (Just port)
        mSocket <- foldM (\mSocket ai -> do
                case mSocket of
                    Nothing -> do
                        s <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
                        do
                            connect s (addrAddress ai)
                            return (Just s)
                          `catch` \(exc :: IOException) -> do
                            sClose s
                            return Nothing
                    Just _ -> return mSocket
            ) Nothing ais
        case mSocket of
            Nothing -> do
                hPutStrLn stderr $ "failed to connect to "++host++":"++port
                exitFailure
            Just skt -> do
                let stra = socketTransport skt
                ee <- runErrorT $ do
                    withClientSession stra $ \ses -> do
                        --let uri = "tcp:5000"   -- Connect to a localhost socket on the Android device
                        withConnect ses uri $ \tra -> terminal tra
                case ee of
                    Left err -> putStrLn $ "FAILED: "++show err
                    Right () -> return ()

terminal :: Transport (ErrorT TransportError IO) -> ErrorT TransportError IO ()
terminal tra = do
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

