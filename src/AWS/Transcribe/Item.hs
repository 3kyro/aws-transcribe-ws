{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AWS.Transcribe.Item where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), Value (..), (.:?))
import qualified Data.Text as T

{- | A word, phrase, or punctuation mark that is transcribed from the input audio.
 https://docs.aws.amazon.com/transcribe/latest/dg/API_streaming_Item.html
-}
data Item = MkItem
    { -- | A value between 0 and 1 for an `Item` that is a confidence
      -- score that Amazon Transcribe assigns to each word or phrase that it transcribes.
      _confidence :: !(Maybe Double)
    , -- | The word or punctuation that was recognized in the input audio.
      _content :: !(Maybe T.Text)
    , -- | The offset from the beginning of the audio stream to the end of the
      -- audio that resulted in the item.
      _iEndTime :: !(Maybe Double)
    , -- | If speaker identification is enabled, shows the speakers identified
      -- in the real-time stream.
      _speaker :: !(Maybe T.Text)
    , -- | If partial result stabilization has been enabled,
      -- indicates whether the word or phrase in the `Item` is stable.
      -- If Stable is true, the result is stable.
      _stable :: !(Maybe Bool)
    , -- | The offset from the beginning of the audio stream to the beginning
      -- of the audio that resulted in the item.
      _iStartTime :: !(Maybe Double)
    , -- | The type of the `Item`
      _itemType :: !(Maybe ItemType)
    , -- | Indicates whether a word in the item matches a word in the vocabulary
      -- filter you've chosen for your real-time stream. If true then a word in
      -- the item matches your vocabulary filter.
      _vocabularyFilterMatch :: !(Maybe Bool)
    }
    deriving (Show, Eq)

-- | The type of the `Item`
data ItemType
    = -- | Indicates that the `Item` is a word that was recognized in the input audio
      Pronunciation
    | -- | Indicates that the `Item` was interpreted as a pause in the input audio
      Punctuation
    deriving (Show, Eq)

makeLenses ''Item

instance FromJSON ItemType where
    parseJSON (String s) =
        case s of
            "pronunciation" -> pure Pronunciation
            "punctuation" -> pure Punctuation
            _ -> fail "Not an ItemType"
    parseJSON _ = fail "Not an ItemType"

instance FromJSON Item where
    parseJSON (Object o) = do
        cnf <- o .:? "Confidence"
        cnt <- o .:? "Content"
        endT <- o .:? "EndTime"
        spk <- o .:? "Speaker"
        stb <- o .:? "Stable"
        startT <- o .:? "StartTime"
        tp <- o .:? "Type"
        voc <- o .:? "VocabularyFilterMatch"
        pure $ MkItem cnf cnt endT spk stb startT tp voc
    parseJSON _ = fail "Not an Item"
