module DuckBot.Parsing (DuckBool (..), Varispeed (..)) where

import Calamity.Commands (ParameterParser (..), parseMP)
import Data.Scientific (toRealFloat)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

newtype DuckBool = DuckBool {unDuckBool :: Bool}

instance ParameterParser DuckBool c a where
  parse = parseMP "boolean" $ parser <* eof
   where
    true = ["y", "yes", "on", "1", "t", "true"]
    false = ["n", "no", "off", "0", "f", "false"]
    strings value = ((value <$) . string' <$>)
    parser = choice $ strings (DuckBool True) true ++ strings (DuckBool False) false
  parameterDescription = "yes or no"

newtype Varispeed = Varispeed {unVarispeed :: Float}

instance ParameterParser Varispeed c a where
  parse = parseMP "varispeed" $ do
    speed <- scientific
    when (speed < 0.5 || speed > 2) $ fail "speed outside of acceptable range"
    pure $ Varispeed (toRealFloat speed)
  parameterDescription = "varispeed (between 0.5 and 2)"
