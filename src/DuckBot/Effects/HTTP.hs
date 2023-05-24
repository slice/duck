module DuckBot.Effects.HTTP (HttpEff (..), req, interpretHttp) where

import Data.Data (Proxy)
import Network.HTTP.Req qualified as R
import Polysemy qualified as P

data HttpEff m a where
  Req ::
    ( R.HttpMethod method
    , R.HttpBody body
    , R.HttpResponse response
    , R.HttpBodyAllowed (R.AllowsBody method) (R.ProvidesBody body)
    ) =>
    method ->
    R.Url scheme ->
    body ->
    Proxy response ->
    R.Option scheme ->
    HttpEff m response

P.makeSem ''HttpEff

interpretHttp :: (P.Member (P.Embed IO) r) => P.Sem (HttpEff ': r) a -> P.Sem r a
interpretHttp = P.interpret \case
  Req method scheme body response params ->
    let
      request = R.req method scheme body response params
     in
      P.embed $ R.runReq R.defaultHttpConfig request
