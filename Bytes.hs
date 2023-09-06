module Bytes where

import Prelude ()
import Relude
import qualified Data.ByteString as BS 
import Control.Monad.Trans.Resource
    ( ResourceT, ReleaseKey, allocate, runResourceT )

import System.FilePath ( (</>) ) 
import FileHelpers (getDataDir)
import qualified System.IO as IO
import Chunks (repeatUntilIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T


binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode = allocate (IO.openBinaryFile path mode) IO.hClose

copyGreetingFile :: IO ()
copyGreetingFile = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h1) <- binaryFileResource (dir </> "greeting.txt") ReadMode
    (_, h2) <- binaryFileResource (dir </> "greeting_copy.txt") WriteMode
    liftIO $ repeatUntilIO (BS.hGetSome h1 2048) BS.null (BS.hPutStr h2)

    
helloBytes :: [Word8]
helloBytes = [104, 101, 108, 108, 111, 10]

helloByteString :: IO ()
helloByteString = do
    IO.hSetBinaryMode stdout True
    BS.hPut stdout (BS.pack helloBytes)

helloUtf8 :: IO ()
helloUtf8 = do
    IO.hSetBinaryMode stdout True
    BS.hPutStr stdout (T.encodeUtf8 (T.pack "hello world\n"))


-- Exercises
greet :: BS.ByteString -> IO ()
greet nameBS = case T.decodeUtf8' nameBS of
    Left _ -> putStrLn "Invalid byte string"
    Right name -> T.putStrLn (T.pack "Hello, " <> name)

asciiUpper :: BS.ByteString -> BS.ByteString
asciiUpper = BS.map toUpper

toUpper :: Word8 -> Word8
toUpper x = if x > 96 && x < 123 then x - 32 else x 