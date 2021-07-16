{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AWS.Transcribe.Alternative where

import AWS.Transcribe.Item (Item)
import Control.Lens (makeLenses, (^.))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), object, (.:), (.:?), (.=))
import qualified Data.Text as T

data Alternative = MkAlternative
    { _items :: ![Item]
    , _altTranscript :: !(Maybe T.Text)
    }
    deriving (Eq, Show)
makeLenses ''Alternative

instance FromJSON Alternative where
    parseJSON (Object o) =
        MkAlternative
            <$> o .: "Items"
            <*> o .:? "Transcript"
    parseJSON _ = fail "Not an Alternative"

-- For now only send the transcription field and ignore the
-- `Item`s
instance ToJSON Alternative where
    toJSON alt =
        object
            ["Transcript" .= (alt ^. altTranscript)]
