module DuckBot (runBot) where

import Calamity hiding (status)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands
import Calamity.Commands.Context (FullContext, useFullContext)
import Calamity.Gateway.Types (StatusUpdateData (..))
import Calamity.Metrics.Noop (runMetricsNoop)
import Calamity.Types.Model.Presence.Activity (activity)
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad qualified as M
import Data.Aeson (eitherDecodeFileStrict')
import Data.Colour.SRGB (sRGB24read)
import Data.Default (def)
import Data.Text (Text)
import Di qualified
import DiPolysemy qualified as DiP
import DuckBot.Config (BotConfig (..))
import Optics
import Polysemy qualified as P
import Polysemy.Async qualified as P
import Polysemy.Reader qualified as P
import Polysemy.Req (interpretReq)
import Polysemy.Time (interpretTimeGhc)
import TextShow (showt)

ok :: (HasID Channel ctx, HasID Message ctx, BotC r, P.Member (P.Reader BotConfig) r) => ctx -> P.Sem r ()
ok ctx = P.asks feedbackEmoji >>= \d -> void . invoke $ CreateReaction ctx ctx (UnicodeEmoji d)

initialActivity :: Activity
initialActivity = Calamity.Types.Model.Presence.Activity.activity "quacking about" Game

initialStatusUpdate :: StatusUpdateData
initialStatusUpdate = StatusUpdateData Nothing [initialActivity] Online False

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
      . interpretReq
      . runBotIO' (BotToken $ DuckBot.Config.token config) defaultIntents (Just initialStatusUpdate)
      $ do
        DiP.info @Text "setting things up >:3"

        P.async . M.replicateM 5 $ do
          DiP.debug @Text "this is going to be printed 5 times!"
          P.embed . threadDelay $ 5 * 1000 * 1000

        react @'ReadyEvt $ \ready -> do
          let user = ready ^. #user
          DiP.info $ "*** bot is READY as " <> displayUser user <> " (" <> showt (getID @User user) <> ")"
          DiP.info $ "    gateway version: " <> showt (ready ^. #v)
          DiP.info $ "    session ID: " <> ready ^. #sessionID
          DiP.info $ "    #guilds: " <> showt (length $ ready ^. #guilds)

        react @('CustomEvt (CtxCommandError FullContext)) \(CtxCommandError ctx err) -> do
          DiP.error @Text ("command error: " <> showt err)
          void . reply ctx $
            intoMsg @Embed
              ( def
                  & #title ?~ "fatal error occurred while executing a command"
                  & #description ?~ "maybe try again later?"
                  & #color ?~ sRGB24read "#f09090"
              )

        addCommands do
          helpCommand

          help (const "what ducks do best") . command @'[] "quack" $ \ctx -> do
            void $ tell @Text ctx "quack"

          hide do
            group "test" do
              command @'[] "explode" \_ctx -> do
                Just _ <- pure Nothing
                DiP.debug @Text "unreachable!"
            -- command @'[] "interactions" $ \ctx -> do
            --   let modalView = I.textInput TextInputShort "a"
            --   I.runView modalView (void . I.pushModal "a modal") $ \a -> do
            --     void $ I.followUp @Text ("you typed: " <> a)
            --     I.endView ()
            command @'[] "ping" \ctx -> do
              void $ reply @Text ctx "pong"
            command @'[Named "text" (KleenePlusConcat Text)] "say" \ctx text -> do
              let puppeteer = ctx ^. #user % #username
              void . tell @Text ctx $ puppeteer <> " told me to say: " <> text

          help (const "poke someone unsuspectingly") . command @'[Named "victim" (Snowflake Member)] "poke" $ \ctx victim -> do
            void $ tell @Text ctx $ mention victim <> " poke!"
            ok ctx
