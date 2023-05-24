module DuckBot.Effects.Radio (
  Metadata (..),
  RadioEff (..),
  getMetadata,
  getRemainingTime,
  skip,
  setVarispeed,
  setVarispeedRandomly,
  downloadTrack,
  uploadTrack,
  requestTrack,
) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Polysemy (makeSem)

data Metadata = Metadata
  { artist :: Text
  , title :: Text
  , onAirSince :: Text
  , initialUri :: Text
  }

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \v ->
    Metadata
      <$> v
      .: "artist"
      <*> v
      .: "title"
      <*> v
      .: "on_air_timestamp"
      <*> v
      .: "initial_uri"

data RadioEff m a where
  GetMetadata :: RadioEff m Metadata
  GetRemainingTime :: RadioEff m Float
  Skip :: RadioEff m ()
  SetVarispeed :: Float -> RadioEff m ()
  SetVarispeedRandomly :: Bool -> RadioEff m ()
  DownloadTrack :: Text -> RadioEff m ByteString
  UploadTrack :: Text -> ByteString -> RadioEff m ()
  RequestTrack :: Text -> RadioEff m ()

makeSem ''RadioEff
