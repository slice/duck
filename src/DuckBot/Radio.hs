module DuckBot.Radio (
  RadioError,
  radioCommands,
  formatMetadata,
  formatMetadata',
)
where

import Calamity (reply)
import Calamity.Client (BotC)
import Calamity.Commands
import Calamity.Commands.Context (FullContext)
import Calamity.Types.Model.Channel ()
import DuckBot.Commands
import DuckBot.Config (BotConfig (..))
import DuckBot.Effects.Radio
import Polysemy qualified as P
import Polysemy.Fail qualified as P
import Polysemy.Reader qualified as P

radioCommands ::
  ( BotC r
  , RadioC r
  , c ~ FullContext
  , DSLC c r
  , P.Member (P.Reader BotConfig) r
  ) =>
  P.Sem (P.Fail ': r) () ->
  P.Sem r ()
radioCommands updateNowPlaying = do
  helpC "skips the current song" . commandA @'[] "skip" ["s"] $ \ctx -> do
    skip
    sleep 1
    nowPlaying <- formatMetadata <$> getMetadata
    reply_ ctx $ "ok, now playing: " <> nowPlaying
    updateNowPlaying

  helpC "what's on the radio?" . commandA @'[] "nowplaying" ["np"] $ \ctx -> do
    metadata <- getMetadata
    reply_ @Text ctx $ formatMetadata metadata

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
