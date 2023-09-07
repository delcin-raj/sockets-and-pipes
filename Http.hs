module Http where

import Prelude ()
import Relude
import qualified ASCII as A
import qualified ASCII.Char as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Simple.TCP as Net
import Network.Simple.TCP (HostPreference(HostAny), serve)
import Sockets (resolve, openAndConnect)
import Control.Monad.Trans.Resource (runResourceT)
import ASCII.Decimal (Digit (..))

line :: BS.ByteString -> BS.ByteString
line x = x <> A.lift crlf

helloRequestString :: ByteString
helloRequestString =
    line [A.string|Get /hello.txt HTTP/1.1|] <>
    line [A.string|User-Agent: curl/7.16.3|] <>
    line [A.string|Accept-Language: en, mi|] <>
    line [A.string||]

crlf :: [A.Char]
crlf = [A.CarriageReturn, A.LineFeed]

helloResponseString :: ByteString
helloResponseString =
    line [A.string|HTTP/1.1 200 OK|] <>
    line [A.string|Content-Type: text/plain; charset=us-ascii|] <>
    line [A.string|Content-Length: 6|] <>
    line [A.string||] <>
    [A.string|Hello|]

ourFirstServer :: IO a
ourFirstServer = serve @IO HostAny "8000" \(s,a) -> do
    putStrLn ("New connection from " <> show a)
    Net.send s helloResponseString

-- Exercises
repeatUntilNothing :: Monad m => m (Maybe chunk) -> (chunk -> m x) -> m ()
repeatUntilNothing mChunk f = do
    chunk <- mChunk
    case chunk of
        Nothing -> return ()
        Just c -> do
            _ <- f c
            repeatUntilNothing mChunk f

{-
repeatUntilNothing is polymorphic and it can work on any monad,
where repeatUntilIO works only on IO. So the latter can be implemented
using the former
-}
repeatUntilIO :: IO chunk -> (chunk -> Bool) -> (chunk -> IO x) -> IO ()
repeatUntilIO ioChunk isEnd = repeatUntilNothing $ do
    chunk <- ioChunk
    return (if isEnd chunk then Nothing else Just chunk)

getHaskell :: IO ()
getHaskell = runResourceT @IO do
    addrInfo <- liftIO $ resolve "http" "haskell.org" 
    (_, s) <- openAndConnect addrInfo 
    liftIO $ do
        Net.send s haskellRequest
    liftIO $ repeatUntilNothing (Net.recv s 1024) BS.putStr
    where
        haskellRequest =
            line [A.string|Get / HTTP/1.1|] <>
            line [A.string|Host: haskell.org|] <>
            line [A.string|Connection: close|] <>
            line [A.string||]

data Request = Request RequestLine [HeaderField] (Maybe MessageBody)
data Response = Response StatusLine [HeaderField] (Maybe MessageBody)

data RequestLine = RequestLine Method RequestTarget HttpVersion
data Method = Get | Post | Put | Delete | Connect | Options | Trace
-- data Method = Method BS.ByteString

newtype RequestTarget = RequestTarget BS.ByteString

data StatusLine = StatusLine HttpVersion StatusCode ReasonPhrase

data StatusCode = StatusCode A.Digit A.Digit A.Digit

newtype ReasonPhrase = ReasonPhrase BS.ByteString

newtype MessageBody = MessageBody LBS.ByteString

data HttpVersion = HttpVersion A.Digit A.Digit

data HeaderField = HeaderField FieldName FieldValue

newtype FieldName = FieldName BS.ByteString
newtype FieldValue = FieldValue BS.ByteString

exRequest :: Request
exRequest = Request reqLine headers Nothing
    where
        reqLine = RequestLine
            Get
            (RequestTarget [A.string|/hello.txt|])
            (HttpVersion Digit1 Digit1)
        
        headers = [
            HeaderField (FieldName [A.string|Host|]) $ FieldValue [A.string|www.example.com|],
            HeaderField (FieldName [A.string|Accept-Language|]) $ FieldValue [A.string|en, mi|]
            ]

exResponse :: Response
exResponse = Response status headers message
    where
        status = StatusLine
            (HttpVersion Digit1 Digit1)
            (StatusCode Digit2 Digit0 Digit0)
            (ReasonPhrase [A.string|OK|])
        
        headers = [
            HeaderField (FieldName [A.string|Content-Type|]) (FieldValue [A.string|charset=us-ascii|]),
            HeaderField (FieldName [A.string|Content-Length|]) (FieldValue [A.string|6|])
            ]

        message = Just $ MessageBody [A.string|Hello!|]