module DuckBot (runBot) where

import Calamity hiding (status)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands
import Calamity.Commands.Context (FullContext, useFullContext)
import Calamity.Gateway.Types (StatusUpdateData (..))
import Calamity.Metrics.Noop (runMetricsNoop)
import Calamity.Types.Model.Presence.Activity (activity)
import qualified Carnage.Discovery as C
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as A
import Control.Lens
import Control.Monad
import qualified Control.Monad as M
import Data.Aeson (eitherDecodeFileStrict')
import Data.Colour.SRGB (sRGB24read)
import Data.Default (def)
import Data.Generics.Labels ()
import qualified Data.HashSet as HS
import Data.Text (Text)
import Data.Text.Lens (packed)
import qualified Di
import qualified DiPolysemy as DiP
import DuckBot.Config (BotConfig (..))
import DuckBot.Dbree (DbreeException, runDbreeSearchReq)
import qualified Polysemy as P
import qualified Polysemy.Async as P
import qualified Polysemy.Error as P
import qualified Polysemy.Reader as P
import Polysemy.Req (interpretReq)
import Polysemy.Time (interpretTimeGhc)
import TextShow (showt)

ok :: (HasID Channel ctx, HasID Message ctx, BotC r, P.Member (P.Reader BotConfig) r) => ctx -> P.Sem r ()
ok ctx = P.asks feedbackEmoji >>= \d -> void . invoke $ CreateReaction ctx ctx (UnicodeEmoji d)

initialActivity :: Activity
initialActivity = Calamity.Types.Model.Presence.Activity.activity "quacking about" Game

initialStatusUpdate :: StatusUpdateData
initialStatusUpdate = StatusUpdateData Nothing (Just initialActivity) Online False

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

        scraper <- P.async . M.forever $ do
          DiP.info @Text "searching..."

          results <-
            P.runError @DbreeException
              . runDbreeSearchReq
              $ C.discoverNewFiles HS.empty "magic"
          case results of
            Left exc -> DiP.error @Text $ "failed to search: " <> show exc ^. packed
            Right r -> DiP.info @Text $ "got results: " <> show r ^. packed

          let second = 1 * 1000 * 1000
          P.embed . threadDelay $ 20 * second

        P.async . M.replicateM 5 $ do
          DiP.debug @Text "this is going to be printed 5 times!"
          P.embed . threadDelay $ 5 * 1000 * 1000

        react @'ReadyEvt $ \ready -> do
          let user = ready ^. #user
          DiP.info $ "*** bot is ready as " <> displayUser user <> " (" <> showt (getID @User user) <> ")"
          DiP.info $ "    gateway version: " <> showt (ready ^. #v)
          DiP.info $ "    session id: " <> ready ^. #sessionID
          DiP.info $ "    n guilds: " <> showt (length $ ready ^. #guilds)

        react @('CustomEvt (CtxCommandError FullContext)) $ \(CtxCommandError ctx err) -> do
          DiP.error @Text ("command error: " <> showt err)
          void . reply ctx $
            intoMsg @Embed
              ( def
                  & #description ?~ "oops! something went wrong"
                  & #color ?~ sRGB24read "#f09090"
              )

        addCommands $ do
          helpCommand

          help (const "what ducks do best") . command @'[] "quack" $ \ctx -> do
            void $ tell @Text ctx "quack"

          help (const "carnage related commands") . group "c" $ do
            help (const "inspect the state of the searcher") . command @'[] "status" $ \ctx -> do
              result <- P.embed $ A.poll scraper
              let status =
                    case result of
                      Nothing -> "working \128170"
                      Just (Left exception) -> bold "stopped" <> " \128165\n" <> codeblock' Nothing (showt exception)
                      Just _ -> bold "stopped...? \129300"
              void . reply ctx $ "searcher status: " <> status

          hide $ do
            group "test" $ do
              command @'[] "explode" $ \_ctx -> do
                Just _ <- pure Nothing
                DiP.debug @Text "unreachable!"
            command @'[] "ping" $ \ctx -> do
              void $ reply @Text ctx "pong"
            command @'[Named "text" (KleenePlusConcat Text)] "say" $ \ctx text -> do
              let puppeteer = ctx ^. #user . #username
              void . tell @Text ctx $ puppeteer <> " told me to say: " <> text

          help (const "poke someone unsuspectingly") . command @'[Named "victim" (Snowflake Member)] "poke" $ \ctx victim -> do
            void $ tell @Text ctx $ mention victim <> " poke!"
            ok ctx
