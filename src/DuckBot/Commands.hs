module DuckBot.Commands (helpC, ok, sleep, reply_) where

import Calamity (HasID, Message, RawEmoji (..), ToMessage, invoke, reply)
import Calamity.Client (BotC)
import Calamity.Commands
import Calamity.HTTP (ChannelRequest (..))
import Calamity.Types (Channel)
import Data.Text
import DuckBot.Config (BotConfig (..))
import GHC.Conc (threadDelay)
import Polysemy
import Polysemy.Reader
import Prelude hiding (Reader, asks)

helpC :: Member (Reader (c -> Text)) r => Text -> Sem r a -> Sem r a
helpC = help . const

ok :: (HasID Channel ctx, HasID Message ctx, BotC r, Member (Reader BotConfig) r) => ctx -> Sem r ()
ok ctx = do
  emoji <- asks feedbackEmoji
  void . invoke $ CreateReaction ctx ctx (UnicodeEmoji emoji)

sleep :: (Member (Embed IO) r) => Int -> Sem r ()
sleep seconds = embed . threadDelay $ 1_000_000 * seconds

reply_ :: (ToMessage msg, HasID Channel ctx, HasID Message ctx, BotC r) => ctx -> msg -> Sem r ()
reply_ ctx msg = void $ reply ctx msg
