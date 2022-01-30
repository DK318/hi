{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module HW3.Evaluator (eval) where

import Codec.Compression.Zlib (CompressParams (compressLevel), bestCompression, compressWith,
                               decompressWith, defaultCompressParams, defaultDecompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Applicative (Alternative ((<|>)))
import Control.Lens (makePrisms, (^?!), (^?))
import Control.Lens.Extras (is)
import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift), foldM, runExceptT,
                             when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bitraversable (bimapM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (Semigroup (stimes))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word8)
import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..))
import HW3.Class (HiAddable (..), HiDivable (..), HiList (..), HiListable (..), HiMulable (..),
                  HiPackable (..), HiSubbable (..), hiReturn)
import HW3.TH (generateFunN)
import qualified Text.Read as T

makePrisms ''HiValue

-- | Lifts given value to 'MaybeT'
hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

-- | Lifts given value to corresponding transformer
hoistToTrans :: HiMonad m => Maybe (Either HiError HiValue) -> MaybeT (ExceptT HiError m) HiValue
hoistToTrans Nothing = hoistMaybe Nothing
hoistToTrans (Just e) = case e of
  Left err  -> throwError err
  Right res -> return res

-- | Evaluates strict functions
evalHiFun :: HiMonad m => HiFun -> [HiValue] -> MaybeT (ExceptT HiError m) HiValue
evalHiFun HiFunDiv [a, b] = $(generateFunN 2 'hiDiv [ ['_HiValueNumber, '_HiValueNumber]
                                                    , ['_HiValueString, '_HiValueString] ]) a b
evalHiFun HiFunMul [a, b] = $(generateFunN 2 'hiMul [ ['_HiValueNumber, '_HiValueNumber]
                                                    , ['_HiValueString, '_HiValueNumber]
                                                    , ['_HiValueList, '_HiValueNumber]
                                                    , ['_HiValueBytes, '_HiValueNumber] ]) a b
evalHiFun HiFunAdd [a, b] = $(generateFunN 2 'hiAdd [ ['_HiValueNumber, '_HiValueNumber]
                                                    , ['_HiValueString, '_HiValueString]
                                                    , ['_HiValueList, '_HiValueList]
                                                    , ['_HiValueBytes, '_HiValueBytes]
                                                    , ['_HiValueTime, '_HiValueNumber] ]) a b
evalHiFun HiFunSub [a, b] = $(generateFunN 2 'hiSub [ ['_HiValueNumber, '_HiValueNumber]
                                                    , ['_HiValueTime, '_HiValueTime] ]) a b
evalHiFun HiFunNot [a] =
  let f = Right . hiPack . not
  in $(generateFunN 1 'f [['_HiValueBool]]) a
evalHiFun HiFunLessThan [a, b] = hiReturn $ a < b
evalHiFun HiFunGreaterThan [a, b] = hiReturn $ a > b
evalHiFun HiFunEquals [a, b] = hiReturn $ a == b
evalHiFun HiFunNotLessThan [a, b] = hiReturn $ a >= b
evalHiFun HiFunNotGreaterThan [a, b] = hiReturn $ a <= b
evalHiFun HiFunNotEquals [a, b] = hiReturn $ a /= b
evalHiFun HiFunLength [a] = $(generateFunN 1 'hiLen [ ['_HiValueString]
                                                    , ['_HiValueList]
                                                    , ['_HiValueBytes] ]) a
evalHiFun HiFunReverse [a] = $(generateFunN 1 'hiRev [ ['_HiValueString]
                                                     , ['_HiValueList]
                                                     , ['_HiValueBytes] ]) a
evalHiFun HiFunToUpper [a] =
  let f = Right . hiPack . T.toUpper
  in $(generateFunN 1 'f [['_HiValueString]]) a
evalHiFun HiFunToLower [a] =
  let f = Right . hiPack . T.toLower
  in $(generateFunN 1 'f [['_HiValueString]]) a
evalHiFun HiFunTrim [a] =
  let f = Right . hiPack . T.strip
  in $(generateFunN 1 'f [['_HiValueString]]) a
evalHiFun HiFunList args = hiReturn $ Seq.fromList args
evalHiFun HiFunRange [a, b] =
  let f a b = (Right . (hiPack @(Seq.Seq HiValue))) $ hiPack <$> [a..b]
  in $(generateFunN 2 'f [['_HiValueNumber, '_HiValueNumber]]) a b
evalHiFun HiFunFold [a, b]
  | _HiValueFunction `is` a && _HiValueList `is` b = do
    let func = a^?!_HiValueFunction
        seq = b^?!_HiValueList
    if Seq.length seq == 0 then
      return HiValueNull
    else do
      let (start Seq.:< rest) = Seq.viewl seq
      lift $ foldM (\acc cur -> evalLazyHiFun func [HiExprValue acc, HiExprValue cur]) start rest
  | otherwise = throwError HiErrorInvalidFunction
evalHiFun HiFunPackBytes [a] =
  let f a = (hiPack . BS.pack . toList) <$> mapM unpackHiValueToWord8 a
  in $(generateFunN 1 'f [['_HiValueList]]) a
evalHiFun HiFunUnpackBytes [a] =
  let f a = (Right . hiPack . Seq.fromList) $ (hiPack . toRational) <$> BS.unpack a
  in $(generateFunN 1 'f [['_HiValueBytes]]) a
evalHiFun HiFunEncodeUtf8 [a] =
  let f = Right . hiPack . T.encodeUtf8
  in $(generateFunN 1 'f [['_HiValueString]]) a
evalHiFun HiFunDecodeUtf8 [a] =
  let f a = Right $ either (const HiValueNull) hiPack (T.decodeUtf8' a)
  in $(generateFunN 1 'f [['_HiValueBytes]]) a
evalHiFun HiFunZip [a] =
  let f a = (Right . hiPack . BSL.toStrict) $ compressWith defaultCompressParams { compressLevel = bestCompression } (BSL.fromStrict a)
  in $(generateFunN 1 'f [['_HiValueBytes]]) a
evalHiFun HiFunUnzip [a] =
  let f a = (Right . hiPack . BSL.toStrict) $ decompressWith defaultDecompressParams (BSL.fromStrict a)
  in $(generateFunN 1 'f [['_HiValueBytes]]) a
evalHiFun HiFunSerialise [a] = hiReturn $ BSL.toStrict $ serialise a
evalHiFun HiFunDeserialise [a] =
  let f a = either (const $ Right HiValueNull) Right (deserialiseOrFail (BSL.fromStrict a))
  in $(generateFunN 1 'f [['_HiValueBytes]]) a
evalHiFun HiFunRead [a] =
  let f = Right . hiPack . HiActionRead . T.unpack
  in $(generateFunN 1 'f [['_HiValueString]]) a
evalHiFun HiFunWrite [a, b] =
  let f filePath txt = (Right . hiPack) $ HiActionWrite (T.unpack filePath) (T.encodeUtf8 txt)
  in $(generateFunN 2 'f [['_HiValueString, '_HiValueString]]) a b
evalHiFun HiFunMkDir [a] =
  let f = Right . hiPack . HiActionMkDir . T.unpack
  in $(generateFunN 1 'f [['_HiValueString]]) a
evalHiFun HiFunChDir [a] =
  let f = Right . hiPack . HiActionChDir . T.unpack
  in $(generateFunN 1 'f [['_HiValueString]]) a
evalHiFun HiFunParseTime [a] =
  let f a = Right $ fromMaybe HiValueNull (hiPack <$> (T.readMaybe @UTCTime (T.unpack a)))
  in $(generateFunN 1 'f [['_HiValueString]]) a
evalHiFun HiFunRand [a, b] =
  let checkOutOfBounds a = a < fromIntegral (minBound :: Int) || a > fromIntegral (maxBound :: Int)
      f a b
        | denominator a /= 1 || denominator b /= 1 || checkOutOfBounds a || checkOutOfBounds b = Left HiErrorInvalidArgument
        | otherwise = (Right . hiPack) $ HiActionRand (fromIntegral $ numerator a) (fromIntegral $ numerator b)
  in $(generateFunN 2 'f [['_HiValueNumber, '_HiValueNumber]]) a b
evalHiFun HiFunEcho [a] =
  let f = Right . hiPack . HiActionEcho
  in $(generateFunN 1 'f [['_HiValueString]]) a
evalHiFun HiFunKeys [a] =
  let f = Right . hiPack . Seq.fromList . Map.keys
  in $(generateFunN 1 'f [['_HiValueDict]]) a
evalHiFun HiFunValues [a] =
  let f = Right . hiPack . Seq.fromList . Map.elems
  in $(generateFunN 1 'f [['_HiValueDict]]) a
evalHiFun HiFunInvert [a] =
  let folder :: Map.Map HiValue [HiValue] -> (HiValue, HiValue) -> Map.Map HiValue [HiValue]
      folder acc (k, v) = Map.insertWith (++) v [k] acc
      f a = (Right . hiPack) $ Map.map (\lst -> hiPack $ Seq.fromList lst) (foldl folder Map.empty (Map.assocs a))
  in $(generateFunN 1 'f [['_HiValueDict]]) a
evalHiFun HiFunCount [a] = $(generateFunN 1 'countHelper [ ['_HiValueList]
                                                         , ['_HiValueBytes]
                                                         , ['_HiValueString] ]) a
evalHiFun _ _ = throwError HiErrorArityMismatch

-- | Helper for "count" function
countHelper :: forall a b. (HiListable a b) => a -> Either HiError HiValue
countHelper listable =
  let folder :: Map.Map HiValue Int -> b -> Map.Map HiValue Int
      folder acc cur = Map.insertWith (+) (mkHiValueSingle cur) 1 acc
  in (Right . hiPack) $ Map.map (hiPack . toRational) (hiFoldl folder Map.empty listable)

-- | Tries to evaluate lazy functions (e.g. if). If given function is not lazy, then tries to evaluate it strictly
evalLazyHiFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalLazyHiFun HiFunIf [a, b, c] = do
  aEvaled <- eval' a
  case aEvaled of
    HiValueBool True  -> eval' b
    HiValueBool False -> eval' c
    _                 -> throwE HiErrorInvalidArgument
evalLazyHiFun HiFunAnd [a, b] = do
  aEvaled <- eval' a
  case aEvaled of
    HiValueNull       -> return HiValueNull
    HiValueBool False -> hiReturn $ False
    _                 -> eval' b
evalLazyHiFun HiFunOr [a, b] = do
  aEvaled <- eval' a
  case aEvaled of
    HiValueNull       -> eval' b
    HiValueBool False -> eval' b
    val               -> return val
evalLazyHiFun hiFun args = do
  evaledArgs <- mapM eval' args
  mRes <- runMaybeT (evalHiFun hiFun evaledArgs)
  case mRes of
    Just res -> return res
    Nothing  -> throwE HiErrorInvalidArgument

-- | Helper function for unpacking numbers from 'HiValueNumber'. Checks that given number is a valid 'Word8'
unpackHiValueToWord8 :: HiValue -> Either HiError Word8
unpackHiValueToWord8 (HiValueNumber num)
  | num >= 0 && num <= 255               = return $ fromIntegral $ numerator num
  | otherwise                            = Left HiErrorInvalidArgument
unpackHiValueToWord8 _                   = Left HiErrorInvalidArgument

-- | Generic function for slices
evalListableAction :: (HiMonad m, HiListable a b, HiPackable a) => a -> [HiValue] -> ExceptT HiError m HiValue
evalListableAction listable [HiValueNumber a]
  | denominator a /= 1 = throwE HiErrorInvalidArgument
  | a < 0 || (fromIntegral $ numerator a) >= hiLength listable = return HiValueNull
  | otherwise = return $ mkHiValueSingle $ listable `hiIndex` fromIntegral (numerator a)
evalListableAction listable [HiValueNumber a, HiValueNumber b]
  | denominator a /= 1 || denominator b /= 1 = throwE HiErrorInvalidArgument
  | otherwise =
  let from' = fromIntegral $ numerator a
      to' = fromIntegral $ numerator b
      len = hiLength listable
      from
        | from' < 0 = max 0 (len + from')
        | otherwise = from'
      to
        | to' < 0   = max 0 (len + to')
        | otherwise = to'
  in hiReturn $ (hiTake (to - from) . hiDrop from) listable
evalListableAction listable [a, HiValueNull] = evalListableAction listable [a, HiValueNumber (toRational $ hiLength listable)]
evalListableAction listable [HiValueNull, b] = evalListableAction listable [HiValueNumber 0, b]
evalListableAction _ _ = throwE HiErrorInvalidArgument

-- | Helper for 'evalSliceAction'
evalSliceAction' :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
evalSliceAction' (HiValueList seq) args    = evalListableAction seq args
evalSliceAction' (HiValueString text) args = evalListableAction text args
evalSliceAction' (HiValueBytes bytes) args = evalListableAction bytes args
evalSliceAction' _ _                       = throwE HiErrorInvalidArgument

-- | Evaluates slice operations for lists, strings and bytes
evalSliceAction :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalSliceAction value args = do
  let len = length args
  when (len > 2 || len == 0) $ throwE HiErrorArityMismatch
  evaledArgs <- mapM eval' args
  evalSliceAction' value evaledArgs

-- | Helper for 'eval'
eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprValue value) = return value
eval' (HiExprApply expr exprs) = do
  evaled <- eval' expr
  case evaled of
    HiValueString text -> evalSliceAction (HiValueString text) exprs
    HiValueList lst -> evalSliceAction (HiValueList lst) exprs
    HiValueBytes bytes -> evalSliceAction (HiValueBytes bytes) exprs
    HiValueDict dict -> do
      when (length exprs /= 1) $ throwE HiErrorInvalidArgument
      arg <- eval' (head exprs)
      return $ fromMaybe HiValueNull (Map.lookup arg dict)
    HiValueFunction hf -> evalLazyHiFun hf exprs
    _ -> throwE HiErrorInvalidFunction
eval' (HiExprRun expr) = do
  evaled <- eval' expr
  case evaled of
    HiValueAction ha -> lift $ runAction ha
    _                -> throwE HiErrorInvalidArgument
eval' (HiExprDict vals) = do
  evaled <- mapM (bimapM eval' eval') vals
  return $ HiValueDict $ Map.fromList evaled

-- | Evaluates expression in specific HiMonad
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (eval' expr)
