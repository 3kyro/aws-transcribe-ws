{-# LANGUAGE OverloadedStrings #-}

module AWS.Transcribe.PreSignedUrl (host, path) where

import AWS.Credentials (Credentials (accessKey, secretKey))
import AWS.Transcribe.Settings (Region, Settings (..), langCode, meToText, region, rgToText, srToText)
import qualified Crypto.Hash.SHA256 as SH
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)

-- Http verb
method :: T.Text
method = "GET"

-- Service name
service :: T.Text
service = "transcribe"

-- # Host
host :: Region -> T.Text
host rg = "transcribestreaming." <> rgToText rg <> ".amazonaws.com"

-- Date and time of the signature's creation
amzDate :: UTCTime -> T.Text
amzDate = T.pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

-- Date without time for credential scope
datestamp :: UTCTime -> T.Text
datestamp = T.pack . formatTime defaultTimeLocale "%Y%m%d"

-- The canonical URI
canonicalUri :: T.Text
canonicalUri = "/stream-transcription-websocket"

-- The canonical headers
canonicalHeaders :: Region -> T.Text
canonicalHeaders rg = "host:" <> host rg <> "\n"

-- The signed headers
signedHeaders :: T.Text
signedHeaders = "host"

-- The hashing algorithm
algorithm :: T.Text
algorithm = "AWS4-HMAC-SHA256"

-- The credential scope, scopes the derived key to the date, Region and service
credentialScope :: Region -> UTCTime -> T.Text
credentialScope rg now = datestamp now <> "%2F" <> rgToText rg <> "%2F" <> service <> "%2F" <> "aws4_request"

-- | Similar to `credentialScope`, but with no url encoding for '/'
credentialScopeNoEncoding :: Region -> UTCTime -> T.Text
credentialScopeNoEncoding rg now = datestamp now <> "/" <> rgToText rg <> "/" <> service <> "/" <> "aws4_request"

canonicalQueryString :: Credentials -> Settings -> UTCTime -> T.Text
canonicalQueryString creds settings now =
    "X-Amz-Algorithm=" <> algorithm
        <> "&X-Amz-Credential="
        <> accessKey creds
        <> "%2F"
        <> credentialScope (region settings) now
        <> "&X-Amz-Date="
        <> amzDate now
        <> "&X-Amz-Expires=300"
        <> "&X-Amz-SignedHeaders="
        <> signedHeaders
        <> "&language-code="
        <> langCode (languageCode settings)
        <> "&media-encoding="
        <> meToText (mediaEncoding settings)
        <> "&sample-rate="
        <> srToText (sampleRate settings)

payloadHash :: BS.ByteString
payloadHash = hexHash $ T.encodeUtf8 ""

canonicalRequest :: Credentials -> Settings -> UTCTime -> BS.ByteString
canonicalRequest creds settings now =
    T.encodeUtf8 method
        <> "\n"
        <> T.encodeUtf8 canonicalUri
        <> "\n"
        <> T.encodeUtf8 (canonicalQueryString creds settings now)
        <> "\n"
        <> T.encodeUtf8 (canonicalHeaders $ region settings)
        <> "\n"
        <> T.encodeUtf8 signedHeaders
        <> "\n"
        <> payloadHash

stringToSign :: Credentials -> Settings -> UTCTime -> BS.ByteString
stringToSign creds settings now =
    T.encodeUtf8 algorithm
        <> "\n"
        <> T.encodeUtf8 (amzDate now)
        <> "\n"
        <> T.encodeUtf8 (credentialScopeNoEncoding (region settings) now)
        <> "\n"
        <> hexHash (canonicalRequest creds settings now)

signingKey :: Credentials -> Region -> UTCTime -> BS.ByteString
signingKey creds rg now =
    hmacSHA256 kService "aws4_request"
  where
    kDate = hmacSHA256 ("AWS4" <> T.encodeUtf8 (secretKey creds)) (T.encodeUtf8 $ datestamp now)
    kRegion = hmacSHA256 kDate $ T.encodeUtf8 $ rgToText rg
    kService = hmacSHA256 kRegion $ T.encodeUtf8 service

signature :: Credentials -> Settings -> UTCTime -> BS.ByteString
signature creds settings now = v4Signature (signingKey creds (region settings) now) (stringToSign creds settings now)

finalQueryString :: Credentials -> Settings -> UTCTime -> BS.ByteString
finalQueryString creds settings now =
    T.encodeUtf8 (canonicalQueryString creds settings now) <> "&X-Amz-Signature=" <> signature creds settings now

path :: Credentials -> Settings -> UTCTime -> BS.ByteString
path creds settings now = T.encodeUtf8 canonicalUri <> "?" <> finalQueryString creds settings now

v4Signature :: BS.ByteString -> BS.ByteString -> BS.ByteString
v4Signature derivedKey payLoad = B16.encode $ hmacSHA256 derivedKey payLoad

hexHash :: BS.ByteString -> BS.ByteString
hexHash = B16.encode . SH.hash

hmacSHA256 :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacSHA256 key msg = SH.hmac key msg
