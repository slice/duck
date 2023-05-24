module DuckBot.Commands (DuckBool (..), helpC, ok, sleep) where

import Calamity (HasID, Message, RawEmoji (..), invoke)
import Calamity.Client (BotC)
import Calamity.Commands
import Calamity.HTTP (ChannelRequest (..))
import Calamity.Types (Channel)
import Control.Monad (void)
import Data.Text
import DuckBot.Config (BotConfig (..))
import GHC.Conc (threadDelay)
import Polysemy
import Polysemy.Reader
import Text.Megaparsec
import Text.Megaparsec.Char

newtype DuckBool = DuckBool {unDuckBool :: Bool}

instance ParameterParser DuckBool c a where
  parse = parseMP "boolean" $ parser <* eof
    where
      true = ["y", "yes", "on", "1", "t", "true"]
      false = ["n", "no", "off", "0", "f", "false"]
      strings value = ((value <$) . string' <$>)
      parser = choice $ strings (DuckBool True) true ++ strings (DuckBool False) false
  parameterDescription = "yes or no"

helpC :: Member (Reader (c -> Text)) r => Text -> Sem r a -> Sem r a
helpC = help . const

ok :: (HasID Channel ctx, HasID Message ctx, BotC r, Member (Reader BotConfig) r) => ctx -> Sem r ()
ok ctx = do
  emoji <- asks feedbackEmoji
  void . invoke $ CreateReaction ctx ctx (UnicodeEmoji emoji)

sleep :: (Member (Embed IO) r) => Int -> Sem r ()
sleep seconds = embed . threadDelay $ 1_000_000 * seconds
