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
import Data.Text (Text)
import Di qualified
import DiPolysemy qualified as DiP
import DuckBot.Commands
import DuckBot.Config (BotConfig (..))
import DuckBot.Effects (HttpEff, interpretHttp)
import DuckBot.Effects.Radio (RadioEff (..), getMetadata, getRemainingTime)
import DuckBot.Radio (RadioBotEff (..), RadioError, formatMetadata', registerRadioCommands, runRadioLiq)
import Network.HTTP.Req qualified as Req
import Optics
import Polysemy (Members, Sem)
import Polysemy qualified as P
import Polysemy.Async qualified as P
import Polysemy.Error qualified as P
import Polysemy.Reader qualified as P
import Polysemy.Time (interpretTimeGhc)
import TextShow (showt)

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
        DiP.info @Text "setting things up >:3"

        forceUpdateStatusSem <- P.embed $ newQSem 0
        let forceUpdateStatus = signalQSem forceUpdateStatusSem

        let runRadioBot :: (P.Member (P.Embed IO) r) => Sem (RadioBotEff ': r) a -> Sem r a
            runRadioBot = P.interpret \case
              UpdateNowPlaying -> P.embed forceUpdateStatus

            runRadio :: (Members '[HttpEff, P.Embed IO] r) => Sem (RadioEff ': RadioBotEff ': P.Error RadioError ': r) a -> Sem r (Either RadioError a)
            runRadio = P.runError . runRadioBot . runRadioLiq

        react @'ReadyEvt $ \ready -> do
          let user = ready ^. #user
          DiP.info $ "*** bot is READY as " <> displayUser user <> " (" <> showt (getID @User user) <> ")"
          DiP.info $ "    gateway version: " <> showt (ready ^. #v)
          DiP.info $ "    session ID: " <> ready ^. #sessionID
          DiP.info $ "    #guilds: " <> showt (length $ ready ^. #guilds)

          void $ P.async do
            DiP.info @Text "continuously setting up now playing"
            runRadio updateNowPlaying
            runRadio $ continuouslyUpdateNowPlaying forceUpdateStatusSem

        react @('CustomEvt (CtxCommandError FullContext)) \(CtxCommandError ctx err) -> do
          case err of
            Calamity.Commands.ParseError parserType errorMessage ->
              void . reply @Text ctx $ "not sure what that means for " <> codeline parserType <> ":\n" <> codeblock' Nothing errorMessage
            Calamity.Commands.CheckError _ checkMessage ->
              void . reply @Text ctx $ "you can't do that, because: " <> checkMessage
            Calamity.Commands.InvokeError commandName errorReason -> do
              DiP.error @Text ("command error: " <> showt err)
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

          runRadio registerRadioCommands

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
  admin :: (P.Member (P.Reader BotConfig) r) => Sem r (Check FullContext)
  admin = do
    admins <- P.asks (^. #administrators)
    pure $ buildCheckPure "admin" \ctx ->
      if (ctx ^. #user % #id) `elem` admins
        then Nothing
        else Just "you're not an admin"

  updateNowPlaying :: (BotC r, P.Member RadioEff r) => Sem r ()
  updateNowPlaying = DiP.push "duck-radio-integration" do
    metadata <- getMetadata
    let listening = Calamity.Types.Model.Presence.Activity.activity (formatMetadata' metadata) Listening
    DiP.info $ "sending song presence NOW: " <> showt listening
    sendPresence $ StatusUpdateData Nothing [listening] Online False

  continuouslyUpdateNowPlaying :: (BotC r, P.Member RadioEff r) => QSem -> Sem r ()
  continuouslyUpdateNowPlaying manuallyUpdateSem = forever $ DiP.push "duck-radio-integration" do
    remaining <- getRemainingTime
    DiP.info $ "remaining time in this song: " <> showt remaining
    let songOver = threadDelay $ 1_000_000 * round (remaining + 0.5)
    P.embed $ race songOver (waitQSem manuallyUpdateSem)
    DiP.info @Text "song ended or requested to manually update, updating now playing now"
    updateNowPlaying
