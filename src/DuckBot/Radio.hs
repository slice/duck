module DuckBot.Radio (
  RadioError,
  runRadioLiq,
  registerRadioCommands,
  RadioBotEff (..),
  updateNowPlaying,
  formatMetadata,
  formatMetadata',
)
where

import Calamity (reply)
import Calamity.Client (BotC)
import Calamity.Commands
import Calamity.Commands.Context (FullContext)
import Calamity.Types.Model.Channel ()
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import DuckBot.Commands
import DuckBot.Config (BotConfig (..))
import DuckBot.Effects.HTTP
import DuckBot.Effects.Radio
import Network.HTTP.Req (GET (..), NoReqBody (..), POST (..), ReqBodyBs (..), bsResponse, http, ignoreResponse, jsonResponse, port, responseBody, (/:))
import Polysemy (Member, Members, Sem, makeSem)
import Polysemy qualified as P
import Polysemy.Error qualified as P
import Polysemy.Reader qualified as P
import Text.Read (readMaybe)
import TextShow (TextShow (..))

data RadioBotEff m a where
  UpdateNowPlaying :: RadioBotEff m ()

makeSem ''RadioBotEff

data RadioError = DecodingFailed | ParsingFailed

runRadioLiq :: Members '[HttpEff, P.Error RadioError] r => Sem (RadioEff ': r) a -> Sem r a
runRadioLiq = P.interpret \case
  GetMetadata -> fetchMetadata
  GetRemainingTime -> do
    respBs <- liq GET "remaining" NoReqBody bsResponse
    respText <- P.fromEither . first (const DecodingFailed) . decodeUtf8' . responseBody $ respBs
    P.fromEither . maybeToEither ParsingFailed . readMaybe . unpack $ respText
  Skip -> do
    void $ liq POST "skip" NoReqBody ignoreResponse
  SetVarispeed speed -> do
    void $ liq POST "vari" (textBody . showt $ speed) ignoreResponse
  SetVarispeedRandomly enabled ->
    let body = if enabled then "true" else "false"
     in void $ liq POST "varirand" (textBody body) ignoreResponse
  DownloadTrack _ -> undefined
  UploadTrack _ _ -> undefined
  RequestTrack uri ->
    void $ liq POST "request" (textBody uri) ignoreResponse
 where
  fetchMetadata :: Member HttpEff r => Sem r Metadata
  fetchMetadata = do
    resp <- liq GET "metadata" NoReqBody jsonResponse
    pure $ responseBody resp

  textBody = ReqBodyBs . encodeUtf8
  maybeToEither a = maybe (Left a) Right

  liq method path body response =
    req method (http "skipnix" /: path) body response (port 8898)

registerRadioCommands :: (BotC r, c ~ FullContext, DSLC c r, Members '[RadioEff, RadioBotEff, P.Final IO, P.Reader BotConfig] r) => Sem r ()
registerRadioCommands = do
  helpC "skips the current song" . commandA @'[] "skip" ["s"] $ \ctx -> do
    skip
    sleep 1
    nowPlaying <- formatMetadata <$> getMetadata
    void . reply @Text ctx $ "ok, now playing: " <> nowPlaying
    updateNowPlaying

  helpC "what's on the radio?" . commandA @'[] "nowplaying" ["np"] $ \ctx -> do
    metadata <- getMetadata
    void . reply @Text ctx $ formatMetadata metadata

  helpC "how long until this song is over?" . commandA @'[] "remaining" ["rm"] $ \ctx -> do
    remaining <- getRemainingTime
    void . reply @Text ctx $ "remaining time in this song: " <> showt remaining <> "s (this will be formatted better in the future I PROMISE)"

  helpC "turns varirand on or off" . commandA @'[Named "enabled" DuckBool] "varirand" ["vr"] $ \ctx (DuckBool enabled) -> do
    setVarispeedRandomly enabled
    let copy =
          if enabled
            then "**ON**, will take effect from the next track"
            else "**OFF** (current varispeed will be kept, type `!vari 1` to turn off varispeed)"
    void . reply @Text ctx $ "turned varirand " <> copy

  helpC "requests a song" . command @'[Named "uri" (KleenePlusConcat Text)] "req" $ \ctx path -> do
    requestTrack path
    void . reply @Text ctx $ "ok, tried to request that (lol)"

  helpC "forceupdates the bot's status " . command @'[] "unp" $ \ctx -> do
    updateNowPlaying
    void . reply @Text ctx $ "ok, updated now playing status (hopefully)"

  commandA @'[Named "speed" Float] "vari" ["v"] \ctx speed -> do
    if speed < 0.5 || speed > 2
      then void . reply @Text ctx $ "speed outside of acceptable range ;P"
      else do
        setVarispeed speed
        updateNowPlaying
        ok ctx

  pure ()

formatMetadata :: Metadata -> Text
formatMetadata m =
  artist m <> " \8212 " <> title m

formatMetadata' :: Metadata -> Text
formatMetadata' m =
  title m <> " by " <> artist m
