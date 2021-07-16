{- | Use the WebSocket protocol to stream audio to the AWS Transcribe service

 For general information regarding the service see the relevant AWS documentation
 (<https://docs.aws.amazon.com/transcribe/latest/dg/websocket.html>)
-}
module AWS.Transcribe (
    -- ** Credentials
    Credentials,
    newCredentials,
    lookupCredentials,
    runClient,

    -- ** Channel
    Channel,
    newChannel,
    readChannel,
    writeChannel,
    tryReadChannel,
    isEmpty,

    -- ** Settings
    Settings (..),
    LanguageCode (..),
    MediaEncoding (..),
    Region (..),

    -- ** StreamingResponse
    StreamingResponse (..),
    StreamingError (..),
    TranscriptEvent,
    transcript,
    Transcript,
    results,
    Result,
    alternatives,
    channelId,
    endTime,
    isPartial,
    resultId,
    startTime,
    Alternative,
    items,
    altTranscript,
    Item,
    confidence,
    content,
    iEndTime,
    speaker,
    stable,
    iStartTime,
    itemType,
    vocabularyFilterMatch,
    ItemType (..),
) where

import AWS.Credentials
import AWS.Transcribe.Channel
import AWS.Transcribe.Client
import AWS.Transcribe.Settings
import AWS.Transcribe.StreamingResponse
