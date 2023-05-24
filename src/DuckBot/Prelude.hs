module DuckBot.Prelude (
  module Calamity.Client,
  module Polysemy,
  module Data.Aeson,
) where

import Calamity.Client (BotC)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Polysemy (Member, Members, Sem)
