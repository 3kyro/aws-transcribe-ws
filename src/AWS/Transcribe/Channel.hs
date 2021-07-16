module AWS.Transcribe.Channel where

import AWS.Transcribe.StreamingResponse (StreamingResponse)
import Control.Concurrent.STM (TQueue, atomically, isEmptyTQueue, newTQueue, readTQueue, tryReadTQueue, writeTQueue)
import qualified Data.ByteString.Lazy as BL

{- | An AWS Transcribe channel abstraction.
 Audio `BL.Bytestring`s are written to the channel to be transcribed.
 Transcribed `StreamingResponse`s are read from the channel.
-}
data Channel = MkChannel
    { audioQueue :: !(TQueue BL.ByteString)
    , responseQueue :: !(TQueue StreamingResponse)
    }

-- | Create a new `Channel`.
newChannel :: IO Channel
newChannel = atomically $ MkChannel <$> newTQueue <*> newTQueue

-- | Read a `StreamingResponse` from the `Channel`
readChannel :: Channel -> IO StreamingResponse
readChannel (MkChannel _ rQ) = atomically $ readTQueue rQ

-- | Read a `StreamingResponse` from the `Channel` if one is available
tryReadChannel :: Channel -> IO (Maybe StreamingResponse)
tryReadChannel (MkChannel _ rQ) = atomically $ tryReadTQueue rQ

-- | Write an audio `BL.ByteString` to the channel
writeChannel :: Channel -> BL.ByteString -> IO ()
writeChannel (MkChannel wQ _) = atomically . writeTQueue wQ

-- | Is the reading side of the channel empty?
isEmpty :: Channel -> IO Bool
isEmpty (MkChannel _ rQ) = atomically $ isEmptyTQueue rQ
