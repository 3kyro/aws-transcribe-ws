{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module AWS.Transcribe.Client (runClient) where

import AWS.Credentials (Credentials)
import AWS.Transcribe.Channel (Channel (MkChannel))
import AWS.Transcribe.EventStream (hEventType, hValueString, mkStreamingMessage, payload)
import qualified AWS.Transcribe.PreSignedUrl as PS
import AWS.Transcribe.Settings
import AWS.Transcribe.StreamingResponse (StreamingError (..), StreamingResponse (EndOfStream, Error, Event))
import Control.Concurrent.Async (Async, cancel, poll, withAsync)
import Control.Concurrent.STM (TQueue, atomically, readTQueue, writeTQueue)
import Control.Lens ((^.))
import qualified Data.Aeson as AE
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (getCurrentTime)
import qualified Network.WebSockets as WS
import qualified Wuss as WS

{- | Start a @WebSocket@ client.
 The client will probe the channel for incoming `BL.ByteString`, encode it in
 the expected @Event Stream@ and transmit them to the AWS Transcribe endpoint.
 When a response is received it will be written in the channel.

 Sending an empty `BL.ByteString` will close the @WebSocket@ connection. Receiving any kind
 of exception from @AWS Transcribe@ will also close the connection.
-}
runClient ::
    -- | AWS Credentials
    Credentials ->
    -- | Session settings
    Settings ->
    -- | Transcribe channel
    Channel ->
    IO ()
runClient creds settings (MkChannel wQ rQ) = do
    putStrLn "start client"
    now <- getCurrentTime
    WS.runSecureClient
        (T.unpack $ PS.host $ region settings)
        8443
        (T.unpack $ T.decodeUtf8 $ PS.path creds settings now)
        $ \conn ->
            withAsync (send conn wQ) $ \handle ->
                receive conn rQ handle

send :: WS.Connection -> TQueue BL.ByteString -> IO ()
send conn wQ = go
  where
    go = do
        pl <- atomically $ readTQueue wQ
        WS.sendBinaryData conn $ encode $ mkStreamingMessage $ BL.toStrict pl
        -- Sending an empty message will shut down the socket
        if BL.null pl then pure () else go

receive :: WS.Connection -> TQueue StreamingResponse -> Async () -> IO ()
receive conn rQ handle = go
  where
    go = withRunningSend rQ handle $ do
        message <- WS.receive conn
        case message of
            WS.DataMessage _ _ _ (WS.Binary msg) -> do
                -- TODO: Use decodeOrFail
                let decoded = decode msg
                let response = case decoded ^. hEventType . hValueString of
                        "BadRequestException" -> Error BadRequestException
                        "InternalFailureException" -> Error InternalFailureException
                        "LimitExceededException" -> Error LimitExceededException
                        "UnrecognizedClientException" -> Error UnrecognizedClientException
                        "TranscriptEvent" ->
                            case AE.eitherDecodeStrict $ decoded ^. payload of
                                Left err -> Error $ TranscriptEventError err
                                Right trans -> Event trans
                        hOther -> Error $ OtherStreamingError decoded $ "Unknown header: " <> T.unpack hOther

                atomically $ writeTQueue rQ response
                go
            -- On close, clean up the send async action and close the connection
            WS.ControlMessage (WS.Close _ _) -> cancel handle >> atomically (writeTQueue rQ EndOfStream) >> pure ()
            -- Ignore other messages
            _ -> go

-- Perform an action while the async send has not raised an exception. If an
-- exception is raised in the send thread, a `EndOfStream` message is sent
-- to the stream and the action is not run.
withRunningSend :: TQueue StreamingResponse -> Async () -> IO () -> IO ()
withRunningSend rQ handle action = do
    status <- poll handle
    case status of
        -- If the sendthread returns, we still run the receive action
        -- to catch any in-flight messages
        Just (Right _) -> action
        Just (Left _) -> atomically (writeTQueue rQ EndOfStream) >> pure ()
        Nothing -> action
