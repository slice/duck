module DuckBot.Radio (
  RadioError,
  radioCommands,
  formatMetadata,
  formatMetadata',
)
where

import DuckBot.Prelude

import Calamity (CDNAsset (..), Embed, User (avatar), embedField, embedImage, embedThumbnail)
import Calamity.Cache.Eff (getUser)
import Calamity.Commands
import Calamity.Commands.Context (FullContext)
import Calamity.Types.Model.Channel ()
import Data.Colour.SRGB (sRGB24read)
import Data.Default (Default (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import DuckBot.Commands
import DuckBot.Config (BotConfig, artistMappings, radio)
import DuckBot.Effects.Radio
import DuckBot.Parsing
import Network.HTTP.Req (Scheme (..), Url, renderUrl)
import Optics ((.~), (?~))
import Polysemy.Fail qualified as P
import Polysemy.Reader qualified as P

artistAvatar :: (BotC r, Member (P.Reader BotConfig) r) => Text -> Sem r (Maybe (Url 'Https))
artistAvatar artist = do
  mappings <- artistMappings <$> P.asks radio
  maybeUser <- traverse getUser $ Map.lookup artist mappings
  pure do
    user <- join maybeUser
    pure . assetURL . avatar $ user

radioCommands ::
  (BotC r, RadioC r, c ~ FullContext, DSLC c r) =>
  Sem (P.Fail ': r) () ->
  Sem r ()
radioCommands updateNowPlaying = do
  helpC "skips the current song" . commandA @'[] "skip" ["s"] $ \ctx -> do
    skip
    sleep 1
    nowPlaying <- formatMetadata <$> getMetadata
    reply_ ctx $ "ok, now playing: " <> nowPlaying
    updateNowPlaying

  helpC "what's on the radio?" . commandA @'[] "nowplaying" ["np"] $ \ctx -> do
    metadata <- getMetadata
    avatar <- artistAvatar (artist metadata)
    let
      formatTimestamp timestamp = "<t:" <> timestamp <> ":t>"
      -- remove .00 at the end
      wrestleTimestamp = T.reverse . T.drop 3 . T.reverse
      formatted = formatTimestamp . wrestleTimestamp . onAirSince $ metadata
      embed =
        def
          & #title ?~ title metadata
          & #description ?~ artist metadata
          & #fields .~ [embedField "Playing" ("Since " <> formatted)]
          & #thumbnail .~ (embedThumbnail . renderUrl <$> avatar)
          & #color ?~ sRGB24read "#14f852"
    reply_ @Embed ctx embed

  helpC "how long until this song is over?" . commandA @'[] "remaining" ["rm"] $ \ctx -> do
    remaining <- getRemainingTime
    reply_ @Text ctx $ "remaining time in this song: " <> show remaining <> " second(s)"

  helpC "turns varirand on or off" . commandA @'[Named "enabled" DuckBool] "varirand" ["vr"] $ \ctx (DuckBool enabled) -> do
    setVarispeedRandomly enabled
    let status =
          if enabled
            then "**on**; will take effect from the next track"
            else "**off** (current varispeed will be kept, type `!vari 1` to turn off varispeed)"
    reply_ @Text ctx $ "turned varirand " <> status

  helpC "requests a song" . command @'[Named "uri" (KleenePlusConcat Text)] "req" $ \ctx path -> do
    requestTrack path
    reply_ @Text ctx "ok, tried to request that."

  helpC "forceupdates the bot's status " . command @'[] "unp" $ \ctx -> do
    updateNowPlaying
    reply_ @Text ctx "ok, updated now playing status (hopefully)"

  commandA @'[Named "speed" Varispeed] "vari" ["v"] \ctx (Varispeed speed) -> do
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
