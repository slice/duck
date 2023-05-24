module DuckBot.Config (BotConfig (..)) where

import Calamity (Snowflake)
import Calamity.Types (User)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics
import Optics (makeFieldLabelsNoPrefix)

data BotConfig = BotConfig
  { token :: Text
  , feedbackEmoji :: Text
  , commandPrefix :: Text
  , administrators :: [Snowflake User]
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''BotConfig

instance FromJSON BotConfig
