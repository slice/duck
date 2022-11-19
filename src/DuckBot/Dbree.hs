module DuckBot.Dbree (dbreeHost, DbreeException, runDbreeSearchReq) where

import Calamity.Types.LogEff (LogEff)
import qualified Carnage.Discovery as C
import qualified Carnage.Models as C
import qualified Carnage.Search as C
import Control.Exception (Exception)
import Control.Lens
import Data.ByteString (ByteString)
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as BS
import qualified Data.Text.Encoding.Error as T
import Data.Time (Day, UTCTime)
import qualified Data.Time.Clock as Clock
import qualified DiPolysemy as DiP
import DuckBot.Config (BotConfig)
import Network.HTTP.Client (Cookie (Cookie), CookieJar, createCookieJar)
import Network.HTTP.Req
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.Reader as P
import qualified Polysemy.Req as P
import qualified Polysemy.Time as P
import TextShow (showt)

dbreeHost :: Text
dbreeHost = "dbree.org"

data DbreeException = FailedToDecode T.UnicodeException | FailedToSearch HttpException deriving (Show)

instance Exception DbreeException

runDbreeSearchReq ::
  P.Members '[P.Final IO, P.Reader BotConfig, LogEff, P.Req, P.Error DbreeException, P.Time UTCTime Day] r =>
  P.Sem (C.DbreeSearch ': r) a ->
  P.Sem r a
runDbreeSearchReq = P.interpret $ \case
  C.SearchDbree query (C.SearchOffset offset) -> do
    _ <- DiP.info $ "requested to search dbree for \"" <> query <> "\" @ offset " <> showt offset
    config <- P.ask

    now <- P.now
    let makeCookie :: ByteString -> ByteString -> Cookie
        makeCookie name value =
          Cookie name value (Clock.addUTCTime (Clock.nominalDay * 30) now) (encodeUtf8 dbreeHost) "/" now now True True False False

        dbreeCookieJar :: CookieJar
        dbreeCookieJar =
          createCookieJar $
            fmap (\(name, value) -> makeCookie (encodeUtf8 name) (encodeUtf8 value))
              . HashMap.toList
              $ config ^. #dbree . #cookies

    -- We can't just standard query options into `req` because DBREE _needs_
    -- the search offset to be specified with a `&` instead of a `?`, even
    -- though it's the first and only query parameter.
    let url = https dbreeHost /: "s" /: (query <> "&page=" <> showt offset)
        options =
          header (encodeUtf8 "User-Agent") (encodeUtf8 $ config ^. #dbree . #userAgent)
            <> cookieJar dbreeCookieJar

    response <-
      P.fromExceptionSemVia @HttpException FailedToSearch $
        P.req GET url NoReqBody bsResponse options

    fmap C.parseSearchResults
      . P.fromEither
      . over _Left FailedToDecode
      . BS.decodeUtf8'
      $ responseBody response
