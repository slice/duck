module DuckBot.Config (BotConfig (..), LiquidsoapConfig (..)) where

import Calamity (Snowflake)
import Calamity.Types (User)
import Data.Aeson (FromJSON)
import Optics (makeFieldLabelsNoPrefix)

data LiquidsoapConfig = LiquidsoapConfig
  { host :: Text
  , port :: Int
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''LiquidsoapConfig

instance FromJSON LiquidsoapConfig

data BotConfig = BotConfig
  { token :: Text
  , feedbackEmoji :: Text
  , commandPrefix :: Text
  , administrators :: [Snowflake User]
  , liquidsoap :: LiquidsoapConfig
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''BotConfig

instance FromJSON BotConfig
