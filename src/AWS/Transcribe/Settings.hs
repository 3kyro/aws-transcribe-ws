{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AWS.Transcribe.Settings where

import qualified Data.Text as T

-- | Transcribe session specific settings
data Settings = MkSettings
    { languageCode :: LanguageCode
    , mediaEncoding :: MediaEncoding
    , sampleRate :: Word
    , region :: Region
    }

-- | The language code for the input audio.
data LanguageCode
    = EnAU
    | EnGB
    | EnUS
    | EsUS
    | FrCA
    | FrFR
    | DeDE
    | JaJP
    | KoKR
    | PtBR
    | ZhCN
    | ItIT

langCode :: LanguageCode -> T.Text
langCode = \case
    EnAU -> "en-AU"
    EnGB -> "en-GB"
    EnUS -> "en-US"
    EsUS -> "es-US"
    FrCA -> "fr-CA"
    FrFR -> "fr-FR"
    DeDE -> "de-DE"
    JaJP -> "ja-JP"
    KoKR -> "ko-KR"
    PtBR -> "pt-BR"
    ZhCN -> "zh-CN"
    ItIT -> "it-IT"

-- | The encoding used for the input audio
data MediaEncoding
    = PCM
    | OggOpus
    | Flac

meToText :: MediaEncoding -> T.Text
meToText = \case
    PCM -> "pcm"
    OggOpus -> "ogg-opus"
    Flac -> "flac"

-- | Amazon Transcribe Streaming regions
data Region
    = USEast1
    | USEast2
    | USWest2
    | APNorthEast1
    | APNorthEast2
    | APSouthEast2
    | CACentral1
    | EUCentral1
    | EUWest1
    | EUWest2
    | SAEast1

rgToText :: Region -> T.Text
rgToText = \case
    USEast1 -> "us-east-1"
    USEast2 -> "us-east-2"
    USWest2 -> "us-west-2"
    APNorthEast1 -> "ap-northeast-1"
    APNorthEast2 -> "ap-northeast-2"
    APSouthEast2 -> "ap-southeast-2"
    CACentral1 -> "ca-central-1"
    EUCentral1 -> "eu-central-1"
    EUWest1 -> "eu-west-1"
    EUWest2 -> "eu-west-2"
    SAEast1 -> "sa-east-1"

srToText :: Word -> T.Text
srToText = T.pack . show
