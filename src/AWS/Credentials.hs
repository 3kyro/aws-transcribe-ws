{-# LANGUAGE OverloadedStrings #-}

module AWS.Credentials (Credentials, accessKey, secretKey, newCredentials, lookupCredentials) where

import Data.Aeson (FromJSON (..), Value (..), (.:))
import qualified Data.Text as T
import System.Environment (lookupEnv)

{- | AWS Security credentials comprising of an access key ID and a secret key.
 Temporary security tokens are not supported
-}
data Credentials = MkCredentials
    { accessKey :: !T.Text
    , secretKey :: !T.Text
    }

newCredentials ::
    -- | Access Key
    T.Text ->
    -- | Secret Key
    T.Text ->
    Credentials
newCredentials = MkCredentials

{- | Use the provided environment variables to create
 `Credentials`
-}
lookupCredentials ::
    -- | Access Key environment variable
    String ->
    -- | Secret Key environment variable
    String ->
    IO (Maybe Credentials)
lookupCredentials akVar skVar = do
    ak <- fmap T.pack <$> lookupEnv akVar
    sk <- fmap T.pack <$> lookupEnv skVar
    pure $ MkCredentials <$> ak <*> sk

instance FromJSON Credentials where
    parseJSON (Object o) =
        MkCredentials
            <$> o .: "access-key"
            <*> o .: "secret-key"
    parseJSON _ = fail "Not a Credentials"
