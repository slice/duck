module DuckBot.Config (BotConfig (..), RadioConfig (..), LiquidsoapConfig (..)) where

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

data RadioConfig = RadioConfig
  { artistMappings :: Map Text (Snowflake User)
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''RadioConfig

instance FromJSON RadioConfig

data BotConfig = BotConfig
  { token :: Text
  , feedbackEmoji :: Text
  , commandPrefix :: Text
  , administrators :: [Snowflake User]
  , liquidsoap :: LiquidsoapConfig
  , radio :: RadioConfig
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''BotConfig

instance FromJSON BotConfig
