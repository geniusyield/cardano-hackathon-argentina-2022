module GeniusYield.Providers.SubmitApi
    ( SubmitApiEnv
    , SubmitTxException (..)
    , newSubmitApiEnv
    , submitApiSubmitTxDefault
    , submitApiSubmitTxCore
    , TxSubmitApi
    ) where

import qualified Cardano.Api                  as Api
import           Control.Exception            (Exception, throwIO)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Proxy                   (Proxy (..))
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as LT
import           Network.HTTP.Media           ((//))
import           Network.HTTP.Types           (status400)
import qualified Servant.API                  as Servant
import           Servant.Client               (ClientEnv,
                                               ClientError (FailureResponse),
                                               ClientM,
                                               ResponseF (Response, responseBody, responseStatusCode),
                                               client, runClientM)
import           Servant.Client.Core          (RequestF (Request, requestPath))

import           GeniusYield.Imports          (decodeUtf8Lenient)
import           GeniusYield.Providers.Common (newServantClientEnv)
import           GeniusYield.Types.Providers  (GYSubmitTx)
import           GeniusYield.Types.Tx         (txIdFromApi, txToApi)


newtype SubmitTxException = SubmitTxException Text
  deriving stock    (Show)
  deriving anyclass (Exception)

txSubmitPost :: BS.ByteString -> ClientM Api.TxId
txSubmitPost = client @TxSubmitApi Proxy

submitApiSubmitTxDefault :: SubmitApiEnv -> GYSubmitTx
submitApiSubmitTxDefault (SubmitApiEnv env) = submitApiSubmitTxCore txSubmitPost env

submitApiSubmitTxCore :: (BS.ByteString -> ClientM Api.TxId) -> ClientEnv -> GYSubmitTx
submitApiSubmitTxCore txSubmitPostCommon env tx = do
    let cbor = Api.serialiseToCBOR $ txToApi tx
    e <- runClientM (txSubmitPostCommon cbor) env
    case e of
        Left (FailureResponse
            Request {requestPath = (_, path)}
            Response{responseStatusCode = statCode, responseBody=err}
            ) | statCode == status400 && path == "api/submit/tx"
                -> throwIO . SubmitTxException . LT.toStrict $ decodeUtf8Lenient err
        Left clientErr -> throwIO clientErr -- Unknown client error, shouldn't happen.
        Right tid -> return (txIdFromApi tid)

-- | submit-api environment
newtype SubmitApiEnv = SubmitApiEnv ClientEnv

newSubmitApiEnv :: String -> IO SubmitApiEnv
newSubmitApiEnv baseUrl = SubmitApiEnv <$> newServantClientEnv baseUrl

----------------------------------------------------------------------------------------------------------------
-- Copied from https://github.com/input-output-hk/cardano-node/cardano-submit-api/src/Cardano/TxSubmit/Types.hs,
-- because TxSubmitApi and CBORStream are unfortunately not exported.
----------------------------------------------------------------------------------------------------------------

type TxSubmitApi =
               "api"
    Servant.:> "submit"
    Servant.:> "tx"
    Servant.:> Servant.ReqBody '[CBORStream] BS.ByteString
    Servant.:> Servant.PostAccepted '[Servant.JSON] Api.TxId

data CBORStream

instance Servant.Accept CBORStream where
  contentType _ = "application" // "cbor"

instance Servant.MimeRender CBORStream BS.ByteString where
    mimeRender _ = LBS.fromStrict

instance Servant.MimeRender CBORStream LBS.ByteString where
    mimeRender _ = id

instance Servant.MimeUnrender CBORStream BS.ByteString where
    mimeUnrender _ = Right . LBS.toStrict

instance Servant.MimeUnrender CBORStream LBS.ByteString where
    mimeUnrender _ = Right
