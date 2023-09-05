module Book where

import Prelude ()
import Relude
import System.FilePath ((</>))
import qualified System.IO as IO
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import qualified Control.Exception.Safe as Ex

import FileHelpers ( getDataDir, fileResource )

writeGreetingFile :: IO ()
writeGreetingFile = do
    dir <- getDataDir
    h <- IO.openFile (dir </> "greeting.txt") WriteMode
    IO.hPutStrLn h "hello"
    IO.hPutStrLn h "World"
    IO.hClose h -- hClose is idempotent

handleHelloWorld :: IO ()
handleHelloWorld = IO.hPutStrLn stdout "Hello World"

writeGreetingSafe :: IO ()
writeGreetingSafe = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <- fileResource (dir </> "greetings.txt") WriteMode
    liftIO (IO.hPutStrLn h "hello") 
    liftIO (IO.hPutStrLn h "new world") 

handlePrintTest :: IO ()
handlePrintTest = runResourceT @IO do
    (_, handle) <- fileResource "test/handle.txt" WriteMode
    liftIO $ putStrLn $ show handle
    liftIO $ IO.print handle
    str <- liftIO $ IO.hShow handle
    liftIO $ putStrLn str
    liftIO $ IO.hPutStrLn handle "testData"

howManyHandles :: IO ()
howManyHandles = runResourceT @IO do
    hs <- openManyHandles
    putStrLn ("Opened " <> show (length hs) <> " handles")

_openManyHandles :: Integer -> ResourceT IO [Handle]
_openManyHandles n = do
    mH <- fileResourceMaybe n
    case mH of
        Nothing -> return []
        Just h -> do
            hs <- _openManyHandles (n+1)
            return (h : hs)

openManyHandles :: ResourceT IO [Handle]
openManyHandles = _openManyHandles 0

printLenHandles :: IO ()
printLenHandles = runResourceT @IO do
    hs <- openManyHandles
    liftIO $ IO.print (length hs)

fileResourceMaybe :: Integer -> ResourceT IO (Maybe Handle)
fileResourceMaybe n = do
    dir <- liftIO getDataDir
    result <- Ex.tryIO do
        fileResource (dir </> ("greeting" <> show n)) WriteMode
    case result of 
        Right x -> return $ Just $ snd x
        Left e -> do
            print (displayException e)
            return Nothing

-- Chapetr 2 begins