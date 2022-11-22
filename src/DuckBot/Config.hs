module DuckBot.Config (BotConfig (..), DbreeConfig (..)) where

import Data.Aeson (FromJSON)
import qualified Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics

data DbreeConfig = DbreeConfig
  { cookies :: Data.HashMap.Strict.HashMap Text Text,
    userAgent :: Text
  }
  deriving (Generic)

instance FromJSON DbreeConfig

data BotConfig = BotConfig
  { token :: Text,
    feedbackEmoji :: Text,
    commandPrefix :: Text,
    dbree :: DbreeConfig
  }
  deriving (Generic)

instance FromJSON BotConfig
