module GeniusYield.Types.TxBody (
    -- * Transaction body
    GYTxBody,
    -- * Conversions
    txBodyFromApi,
    txBodyToApi,
    -- * Transaction creation
    signTx,
    multiSignTx,
    unsignedTx,
    -- * Functions
    txBodyFee,
    txBodyFeeValue,
    txBodyUTxOs,
    txBodyTxIns,
    txBodyTxId,
    getTxBody
) where


import qualified Cardano.Api                as Api

import GeniusYield.Imports
import GeniusYield.Types.Key
import GeniusYield.Types.Tx
import GeniusYield.Types.UTxO
import GeniusYield.Types.TxOutRef
import GeniusYield.Types.Value

-- | Transaction body: the part which is then signed.
newtype GYTxBody = GYTxBody (Api.TxBody Api.AlonzoEra)
  deriving Show

txBodyFromApi :: Api.TxBody Api.AlonzoEra -> GYTxBody
txBodyFromApi = coerce

txBodyToApi :: GYTxBody -> Api.TxBody Api.AlonzoEra
txBodyToApi = coerce

-- | Sign transaction (body)
--
signTx :: GYTxBody -> GYPaymentSigningKey -> GYTx
signTx (GYTxBody txBody) skey =
    txFromApi $ Api.signShelleyTransaction txBody [Api.WitnessPaymentKey $ paymentSigningKeyToApi skey]

-- | Sign a transaction body with multiple keys.
multiSignTx :: GYTxBody -> [GYPaymentSigningKey] -> GYTx
multiSignTx (GYTxBody txBody) skeys =
    txFromApi $ Api.signShelleyTransaction txBody $ map (Api.WitnessPaymentKey . paymentSigningKeyToApi) skeys

-- | Create an unsigned transaction from the body.
unsignedTx :: GYTxBody -> GYTx
unsignedTx (GYTxBody body) = txFromApi (Api.Tx body [])

-- | Return the fees in lovelace.
txBodyFee :: GYTxBody -> Integer
txBodyFee (GYTxBody (Api.TxBody Api.TxBodyContent { Api.txFee = fee })) =
    case fee of
        Api.TxFeeImplicit x                       -> case x of {}
        Api.TxFeeExplicit _ (Api.Lovelace actual) -> actual

-- | Return the fees as 'GYValue'.
txBodyFeeValue :: GYTxBody -> GYValue
txBodyFeeValue = valueFromLovelace . txBodyFee

-- | Return utxos created by tx (body).
txBodyUTxOs :: GYTxBody -> GYUTxOs
txBodyUTxOs (GYTxBody body@(Api.TxBody Api.TxBodyContent {txOuts})) =
    utxosFromList $ zipWith f [0..] txOuts
  where
    txId = Api.getTxId body

    f :: Word -> Api.TxOut Api.CtxTx Api.AlonzoEra -> GYUTxO
    f i out = utxoFromApi (Api.TxIn txId (Api.TxIx i)) out

-- | Returns the 'GYTxOutRef' consumed by the tx.
txBodyTxIns :: GYTxBody -> [GYTxOutRef]
txBodyTxIns (GYTxBody (Api.TxBody Api.TxBodyContent {txIns})) = map (txOutRefFromApi . fst) txIns

-- | Returns the 'GYTxId' of the given 'GYTxBody'.
txBodyTxId :: GYTxBody -> GYTxId
txBodyTxId = txIdFromApi . Api.getTxId . txBodyToApi

getTxBody :: GYTx -> GYTxBody
getTxBody = txBodyFromApi . Api.getTxBody . txToApi
