module DuckBot.Effects.Radio (
  Metadata (..),
  getMetadata,
  getRemainingTime,
  skip,
  setVarispeed,
  setVarispeedRandomly,
  requestTrack,
  RadioError (..),
  RadioC,
) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import DuckBot.Effects.HTTP (HttpEff, request)
import Network.HTTP.Req ((/:))
import Network.HTTP.Req qualified as R
import Polysemy (Sem)
import Polysemy qualified as P
import Polysemy.Error qualified as P

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

data RadioError = DecodingFailed | ParsingFailed

liq ::
  ( R.HttpBodyAllowed (R.AllowsBody method) (R.ProvidesBody body)
  , P.Member HttpEff r
  , R.HttpMethod method
  , R.HttpBody body
  , R.HttpResponse response
  ) =>
  method ->
  Text ->
  body ->
  Proxy response ->
  Sem r response
liq method path body response =
  request method (R.http "skipnix" /: path) body response (R.port 8898)

type RadioC r = P.Members '[HttpEff, P.Error RadioError] r

getMetadata :: RadioC r => Sem r Metadata
getMetadata = do
  resp <- liq R.GET "metadata" R.NoReqBody R.jsonResponse
  pure $ R.responseBody resp

getRemainingTime :: RadioC r => Sem r Float
getRemainingTime = do
  respBs <- liq R.GET "remaining" R.NoReqBody R.bsResponse
  respText <- P.fromEither . first (const DecodingFailed) . decodeUtf8' . R.responseBody $ respBs
  P.fromEither . maybeToRight ParsingFailed . readMaybe . toString $ respText

skip :: RadioC r => Sem r ()
skip = void $ liq R.POST "skip" R.NoReqBody R.ignoreResponse

setVarispeed :: RadioC r => Float -> Sem r ()
setVarispeed speed = void $ liq R.POST "vari" (textBody . show $ speed) R.ignoreResponse

setVarispeedRandomly :: RadioC r => Bool -> Sem r ()
setVarispeedRandomly enabled = void $ liq R.POST "varirand" (textBody body) R.ignoreResponse
 where
  body = if enabled then "true" else "false"

requestTrack :: RadioC r => Text -> Sem r ()
requestTrack uri = void $ liq R.POST "request" (textBody uri) R.ignoreResponse

textBody :: Text -> R.ReqBodyBs
textBody = R.ReqBodyBs . encodeUtf8
