{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{- |Event stream encoding provides bidirectional communication using messages between a client and a server.
Data frames sent to the Amazon Transcribe streaming service are encoded in this format. The response from Amazon Transcribe also uses this encoding.

Each message consists of two sections: the prelude and the data. The prelude consists of:

    The total byte length of the message

    The combined byte length of all of the headers

The data section consists of:

    The headers

    A payload

Each section ends with a 4-byte big-endian integer CRC checksum.
The message CRC checksum is for both the prelude section and the data section.
Amazon Transcribe uses CRC32 (often referred to as GZIP CRC32) to calculate both CRCs.
For more information about CRC32, see GZIP file format specification version 4.3

.

Total message overhead, including the prelude and both checksums, is 16 bytes.

documentation source: https://docs.aws.amazon.com/transcribe/latest/dg/event-stream.html
-}
module AWS.Transcribe.EventStream where

import Control.Lens (makeLenses, (^.))
import Data.Binary (Binary (get, put), getWord8)
import Data.Binary.Get (Get, getByteString, getInt16be, getInt32be, skip)
import Data.Binary.Put (putBuilder)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.CRC32 as CRC32
import Data.Int (Int16, Int32)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word8)

-- A message used in event stream encoding
data Message = MkMessage
    { _hContentType :: !Header
    , _hEventType :: !Header
    , _hMessageType :: !Header
    , _payload :: !BS.ByteString
    }
    deriving (Show, Eq)

-- | Event stram encoding header
data Header = MkHeader
    { _hNameLength :: !Word8
    , _hName :: !T.Text
    , _hValueType :: !Word8
    , _hValueStringLength :: !Int16
    , _hValueString :: !T.Text
    }
    deriving (Show, Eq)

makeLenses ''Message
makeLenses ''Header

instance Binary Header where
    put = putBuilder . headerBuilder
    get = do
        hnl <- get
        hn <- getByteString (fromIntegral hnl)
        hvt <- get
        hvsl <- getInt16be
        hvs <- getByteString (fromIntegral hvsl)
        pure $ MkHeader hnl (T.decodeUtf8 hn) hvt hvsl (T.decodeUtf8 hvs)

instance Binary Message where
    put msg = do
        putBuilder totalMsg
        putBuilder msgCrc
      where
        prelude = tbl' <> hbl'
        tbl' = BS.int32BE $ tbl msg
        hbl' = BS.int32BE hbl
        preludeCrc = crcBuilder prelude
        pb = BS.byteString (msg ^. payload)
        totalMsg = prelude <> preludeCrc <> requestMessageHeaders <> pb
        msgCrc = crcBuilder totalMsg

    get = do
        tbl' <- getInt32be
        hbl' <- getInt32be
        -- Skip crc check
        skip 4
        h1 <- get
        h2 <- get
        h3 <- get
        let (h1', h2', h3') = classifyHeaders h2 h1 h3
        pl <- getByteString (fromIntegral tbl' - fromIntegral hbl' - 16)
        -- Skip crc check
        pure $ MkMessage h1' h2' h3' pl

classifyHeaders :: Header -> Header -> Header -> (Header, Header, Header)
classifyHeaders h1 h2 h3
    | h1 ^. hNameLength == 13 = classifyHeaders h2 h3 h1
    | h2 ^. hName == "content-type" = (h2, h1, h3)
    | otherwise = (h3, h1, h2)

-- The total byte length of a message
-- tbl: 16 Bytes overhead + headers length + payload length
tbl :: Message -> Int32
tbl msg = 16 + hbl + pbl msg

-- A message's headers byte length
hbl :: Int32
hbl = hContentTypeRequestBL + hEventTypeRequestBL + hMessageTypeRequestBL

-- A message's payload byte length
pbl :: Message -> Int32
pbl msg = fromIntegral (BS.length $ msg ^. payload)

-- A strict ByteString builder for `Header`
headerBuilder :: Header -> BS.Builder
headerBuilder (MkHeader nl n vt vsl vl) =
    BS.word8 nl
        <> BS.byteString (T.encodeUtf8 n)
        <> BS.word8 vt
        <> BS.int16BE vsl
        <> BS.byteString (T.encodeUtf8 vl)

getHeader :: Get Header
getHeader = do
    hnbl <- getWord8
    hn <- getByteString (fromIntegral hnbl)
    hvt <- getWord8
    vsbl <- getInt16be
    vs <- getByteString (fromIntegral vsbl)
    pure $ MkHeader hnbl (T.decodeUtf8 hn) hvt vsbl (T.decodeUtf8 vs)

-- A CRC32 builder for strict ByteStrings
crcBuilder :: BS.Builder -> BS.Builder
crcBuilder = BS.word32BE . CRC32.crc32 . CRC32.digest . BL.toStrict . BS.toLazyByteString

-- The standard content-type request header
hContentTypeRequest :: Header
hContentTypeRequest = MkHeader 13 ":content-type" 7 24 "application/octet-stream"

-- Request content-type header byte length
hContentTypeRequestBL :: Int32
hContentTypeRequestBL = 41

-- The standard event-type request header
hEventTypeRequest :: Header
hEventTypeRequest = MkHeader 11 ":event-type" 7 10 "AudioEvent"

-- Request event-type header byte length
hEventTypeRequestBL :: Int32
hEventTypeRequestBL = 25

-- The standard message-type request header
hMessageTypeRequest :: Header
hMessageTypeRequest = MkHeader 13 ":message-type" 7 5 "event"

-- Request message-type header byte length
hMessageTypeRequestBL :: Int32
hMessageTypeRequestBL = 22

requestMessageHeaders :: BS.Builder
requestMessageHeaders =
    headerBuilder hContentTypeRequest
        <> headerBuilder hEventTypeRequest
        <> headerBuilder hMessageTypeRequest

-- | Make a streaming message, using the default request headers
mkStreamingMessage :: BS.ByteString -> Message
mkStreamingMessage pl = MkMessage hContentTypeRequest hEventTypeRequest hMessageTypeRequest pl
