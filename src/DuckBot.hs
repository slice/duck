module DuckBot (runBot) where

import Calamity hiding (status)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands
import Calamity.Commands.Context (FullContext, useFullContext)
import Calamity.Gateway.Types (StatusUpdateData (..))
import Calamity.Metrics.Noop (runMetricsNoop)
import Calamity.Types.Model.Presence.Activity (activity)
import CalamityCommands.Check (buildCheckPure)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.QSem
import Control.Monad
import Data.Aeson (eitherDecodeFileStrict')
import Data.Colour.SRGB (sRGB24read)
import Data.Default (def)
import Di qualified
import DiPolysemy (info)
import DiPolysemy qualified as DiP
import DuckBot.Commands
import DuckBot.Config (BotConfig (..))
import DuckBot.Effects (RadioC, interpretHttp)
import DuckBot.Effects.Radio (getMetadata, getRemainingTime)
import DuckBot.Radio (RadioError, formatMetadata', radioCommands)
import Optics
import Polysemy qualified as P
import Polysemy.Async qualified as P
import Polysemy.Error qualified as P
import Polysemy.Reader qualified as P
import Polysemy.Time (interpretTimeGhc)
import Prelude hiding (group)

runBot :: IO ()
runBot = do
  config <- eitherDecodeFileStrict' @BotConfig "./config.json" >>= either (fail @IO) pure
  Di.new $ \di ->
    void
      . P.runFinal
      . P.embedToFinal @IO
      . DiP.runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . useFullContext
      . useConstantPrefix (commandPrefix config)
      . P.runReader config
      . interpretTimeGhc
      . interpretHttp
      . runBotIO' (BotToken $ DuckBot.Config.token config) defaultIntents Nothing
      $ do
        info @Text "setting things up >:3"

        forceUpdateStatusSem <- P.embed $ newQSem 0
        let forceUpdateStatus = signalQSem forceUpdateStatusSem
            interpretRadio :: P.Sem (P.Error RadioError ': r) a -> P.Sem r (Either RadioError a)
            interpretRadio = P.runError

        react @'ReadyEvt $ \ready -> do
          let user = ready ^. #user
          info @Text $ "*** bot is READY as " <> displayUser user <> " (" <> show (getID @User user) <> ")"
          info @Text $ "    gateway version: " <> show (ready ^. #v)
          info @Text $ "    session ID: " <> ready ^. #sessionID
          info @Text $ "    #guilds: " <> show (length $ ready ^. #guilds)

          void $ P.async do
            info @Text "continuously setting up now playing"
            interpretRadio updateNowPlaying
            interpretRadio $ continuouslyUpdateNowPlaying forceUpdateStatusSem

        react @('CustomEvt (CtxCommandError FullContext)) \(CtxCommandError ctx err) -> do
          case err of
            Calamity.Commands.ParseError parserType errorMessage ->
              void . reply @Text ctx $ "not sure what that means for " <> codeline parserType <> ":\n" <> codeblock' Nothing errorMessage
            Calamity.Commands.CheckError _ checkMessage ->
              void . reply @Text ctx $ "you can't do that, because: " <> checkMessage
            Calamity.Commands.InvokeError commandName errorReason -> do
              DiP.error @Text ("command error: " <> show err)
              void . reply ctx $
                intoMsg @Embed
                  ( def
                      & #title ?~ "couldn't run " <> codeline commandName
                      & #description ?~ "reason: " <> codeline errorReason
                      & #footer ?~ EmbedFooter "maybe try again later?" Nothing Nothing
                      & #color ?~ sRGB24read "#f09090"
                  )

        addCommands do
          helpCommand

          interpretRadio $ radioCommands (P.embed forceUpdateStatus)

          helpC "what ducks do best" . command @'[] "quack" $ \ctx -> do
            void $ tell @Text ctx "quack!"

          adminC <- admin
          requires [adminC] do
            helpC "stops the bot" . commandA @'[] "shutdown" ["die", "quit", "poweroff", "stop"] $ \ctx -> do
              void . tell @Text ctx $ "goodbye world!!"
              stopBot

            group "test" do
              command @'[] "explode" \_ctx -> do
                Just _ <- pure Nothing
                DiP.debug @Text "unreachable!"

            helpC "forces the bot to say something" . command @'[Named "words" (KleenePlusConcat Text)] "say" $ \ctx w -> do
              void $ tell ctx w

          helpC "goes \"pong\"" . command @'[] "ping" $ \ctx -> do
            void $ reply @Text ctx "pong"

          helpC "poke someone unsuspectingly" . command @'[Named "victim" (Snowflake Member)] "poke" $ \ctx victim -> do
            void $ tell @Text ctx $ mention victim <> " poke!"
            ok ctx

          pure ()
 where
  admin :: (P.Member (P.Reader BotConfig) r) => P.Sem r (Check FullContext)
  admin = do
    admins <- P.asks (^. #administrators)
    pure $ buildCheckPure "admin" \ctx ->
      if (ctx ^. #user % #id) `elem` admins
        then Nothing
        else Just "you're not an admin"

  updateNowPlaying :: (BotC r, RadioC r) => P.Sem r ()
  updateNowPlaying = DiP.push "duck-radio-integration" do
    metadata <- getMetadata
    let listening = Calamity.Types.Model.Presence.Activity.activity (formatMetadata' metadata) Listening
    info @Text $ "sending song presence NOW: " <> show listening
    sendPresence $ StatusUpdateData Nothing [listening] Online False

  continuouslyUpdateNowPlaying :: (BotC r, RadioC r) => QSem -> P.Sem r ()
  continuouslyUpdateNowPlaying manuallyUpdateSem = forever $ DiP.push "duck-radio-integration" do
    remaining <- getRemainingTime
    info @Text $ "remaining time in this song: " <> show remaining
    let songOver = threadDelay $ 1_000_000 * round (remaining + 0.5)
    P.embed $ race songOver (waitQSem manuallyUpdateSem)
    info @Text "song ended or requested to manually update, updating now playing now"
    updateNowPlaying
