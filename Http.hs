module Http where

import Prelude ()
import Relude
import qualified ASCII as A
import qualified ASCII.Char as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Network.Simple.TCP as Net
import Network.Socket (Socket)
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
data Method = Get | Post | Put | Delete | Connect | Options | Trace deriving Show
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
exResponse = Response statusLine headers message
    where
        statusLine = StatusLine
            (HttpVersion Digit1 Digit1)
            (StatusCode Digit2 Digit0 Digit0)
            (ReasonPhrase [A.string|OK|])
        
        headers = [
            HeaderField (FieldName [A.string|Content-Type|]) (FieldValue [A.string|charset=us-ascii|]),
            HeaderField (FieldName [A.string|Content-Length|]) (FieldValue [A.string|6|])
            ]

        message = Just $ MessageBody [A.string|Hello!|]

-- Chapter 7
encodeLineEnd :: BSB.Builder
encodeLineEnd = A.lift crlf

encodeRequest :: Request -> BSB.Builder
encodeRequest (Request reqLine headers bodyMaybe) =
    encodeRequestLine reqLine <>
    repeatedlyEncode
        (\x -> encodeHeaderField x <> encodeLineEnd)
        headers <>
    encodeLineEnd <>
    optionallyEncode encodeMessageBody bodyMaybe

encodeResponse :: Response -> BSB.Builder
encodeResponse (Response statusLine headers bodyMaybe) =
    encodeStatusLine statusLine <>
    repeatedlyEncode
        (\x -> encodeHeaderField x <> encodeLineEnd)
        headers <>
    encodeLineEnd <>
    optionallyEncode encodeMessageBody bodyMaybe

repeatedlyEncode :: (a -> BSB.Builder) -> [a] -> BSB.Builder
repeatedlyEncode = foldMap

optionallyEncode :: (a -> BSB.Builder) -> Maybe a -> BSB.Builder
optionallyEncode = foldMap

encodeMethod :: Method -> BSB.Builder
encodeMethod method = BSB.string8 (show method)

encodeRequestLine :: RequestLine -> BSB.Builder
encodeRequestLine (RequestLine method target httpVersion) =
    encodeMethod method <> A.lift [A.Space] <>
    encodeRequestTarget target <> A.lift [A.Space] <>
    encodeHttpVersion httpVersion <> encodeLineEnd

encodeRequestTarget :: RequestTarget -> BSB.Builder
encodeRequestTarget (RequestTarget t) = BSB.byteString t

encodeHttpVersion :: HttpVersion -> BSB.Builder
encodeHttpVersion (HttpVersion x y) =
    [A.string|Http/|] <> A.lift [x]
    <> [A.string|.|] <> A.lift [y]

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) = A.lift [x, y, z]

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine httpVersion statusCode reason) =
    encodeHttpVersion httpVersion <> A.lift [A.Space] <>
    encodeStatusCode statusCode <> A.lift [A.Space] <>
    encodeReasonPhrase reason <> encodeLineEnd

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase reason) = BSB.byteString reason

{-
data HeaderField = HeaderField FieldName FieldValue

newtype FieldName = FieldName BS.ByteString
newtype FieldValue = FieldValue BS.ByteString
line [A.string|Content-Type: text/plain; charset=us-ascii|] <>
-}

encodeHeaderField :: HeaderField -> BSB.Builder
encodeHeaderField (HeaderField fieldName fieldValue) =
    encodeFieldName fieldName <> [A.string|: |] <> encodeFieldValue fieldValue 

encodeFieldName :: FieldName -> BSB.Builder
encodeFieldName (FieldName x) = BSB.byteString x

encodeFieldValue :: FieldValue -> BSB.Builder
encodeFieldValue (FieldValue x) = BSB.byteString x

encodeMessageBody :: MessageBody -> BSB.Builder
encodeMessageBody (MessageBody message) = BSB.lazyByteString message

-- Chapter 8
countHelloAscii :: Natural -> LBS.ByteString
countHelloAscii count = BSB.toLazyByteString $
    [A.string|Hello!|] <> encodeLineEnd <> case count of
        0 -> [A.string|This page has never been viewed.|]
        1 -> [A.string|This page has been viewed once.|]
        _ -> [A.string|This page has beem viewed|] <> A.showIntegralDecimal count <> [A.string|times.|]

data Status = Status StatusCode ReasonPhrase

ok :: Status
ok = Status
    (StatusCode Digit2 Digit0 Digit0)
    (ReasonPhrase [A.string|ok|])

http_1_1 :: HttpVersion
http_1_1 = HttpVersion Digit1 Digit1

status :: Status -> StatusLine
status (Status code phrase) =  StatusLine http_1_1 code phrase

contentType :: FieldName
contentType = FieldName [A.string|Content-Type|]

plainAscii :: FieldValue
plainAscii = FieldValue [A.string|text/plain; charset=us=ascii|]

contentLenght :: FieldName
contentLenght = FieldName [A.string|Content-Length|]


asciiOk :: LBS.ByteString -> Response
asciiOk str = Response (status ok) [typ, len] (Just body)
    where
        typ = HeaderField contentType plainAscii
        len = HeaderField contentLenght (bodyLengthValue body)
        body = MessageBody str

bodyLengthValue :: MessageBody -> FieldValue
bodyLengthValue (MessageBody x) = FieldValue (A.showIntegralDecimal $ LBS.length x)

sendResponse :: Socket -> Response -> IO ()
sendResponse s r = Net.sendLazy s $ BSB.toLazyByteString $ encodeResponse r

stuckCountingServer :: IO a
stuckCountingServer = serve @IO HostAny "8000" \(s, _) -> do
    let count = 0
    sendResponse s (asciiOk (countHelloAscii count))

-- Solution to Exercise 22

mid :: Word8 -> Word8 -> Word8
mid x y = fromInteger $ (toInteger x * toInteger y) `div` 2