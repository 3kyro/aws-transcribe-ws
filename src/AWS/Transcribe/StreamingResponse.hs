{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AWS.Transcribe.StreamingResponse (
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

import AWS.Transcribe.Alternative
import AWS.Transcribe.EventStream (Message)
import AWS.Transcribe.Item
import Control.Lens (makeLenses, (^.))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.:?), (.=))
import qualified Data.Text as T

newtype TranscriptEvent = MkTranscriptEvent
    {_transcript :: Transcript}
    deriving (Eq, Show)

instance FromJSON TranscriptEvent where
    parseJSON (Object o) =
        MkTranscriptEvent
            <$> o .: "Transcript"
    parseJSON _ = fail " Not a Transcribed"

newtype Transcript = MkTranscript
    {_results :: [Result]}
    deriving (Eq, Show)

instance FromJSON Transcript where
    parseJSON (Object o) =
        MkTranscript
            <$> o .: "Results"
    parseJSON _ = fail " Not a Transcript"

data Result = MkResult
    { _alternatives :: ![Alternative]
    , _channelId :: !(Maybe T.Text)
    , _endTime :: !Double
    , _isPartial :: !Bool
    , _resultId :: !T.Text
    , _startTime :: !Double
    }
    deriving (Eq, Show)

makeLenses ''TranscriptEvent
makeLenses ''Transcript
makeLenses ''Result

instance FromJSON Result where
    parseJSON (Object o) = do
        alt <- o .: "Alternatives"
        cid <- o .:? "ChannelId"
        endT <- o .: "EndTime"
        partial <- o .: "IsPartial"
        rid <- o .: "ResultId"
        startT <- o .: "StartTime"
        pure $ MkResult alt cid endT partial rid startT
    parseJSON _ = fail " Not a Result"

instance ToJSON Result where
    toJSON r =
        object
            [ "Alternatives" .= (r ^. alternatives)
            , "IsPartial" .= (r ^. isPartial)
            , "ResultId" .= (r ^. resultId)
            ]

-- |
data StreamingError
    = BadRequestException
    | InternalFailureException
    | LimitExceededException
    | UnrecognizedClientException
    | -- | An error in decoding a `TranscriptEvent`
      TranscriptEventError String
    | -- | An unrecognised exception type or a binary decoding
      -- error. The original message is returned along with a
      -- possible description of the error
      OtherStreamingError Message String
    deriving (Eq, Show)

data StreamingResponse
    = Event TranscriptEvent
    | Error StreamingError
    | EndOfStream
    deriving (Eq, Show)
