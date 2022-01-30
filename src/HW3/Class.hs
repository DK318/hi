{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE UndecidableInstances   #-}
module HW3.Class
  ( -- * Type classes
    HiListable (..)
  , HiPackable (..)
  , HiDivable (..)
  , HiMulable (..)
  , HiAddable (..)
  , HiSubbable (..)
  , HiList (..)
  -- * Aux function
  , hiReturn ) where

import qualified Data.ByteString as BS
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map
import Data.Ratio (denominator, numerator)
import Data.Semigroup (Semigroup (stimes))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word8)
import HW3.Base (HiAction (HiActionWrite), HiError (HiErrorDivideByZero, HiErrorInvalidArgument),
                 HiFun, HiValue (..))

-- | Type class for listable structures (e.g. text or sequence)
class HiListable a b | a -> b, b -> a where
  hiLength :: a -> Int                    -- ^ length of structure
  hiIndex :: a -> Int -> b                -- ^ UNSAFE element at specific position in structure
  hiTake :: Int -> a -> a                 -- ^ take elements from beggining of structure
  hiDrop :: Int -> a -> a                 -- ^ drop elements from beggining of structure
  mkHiValueSingle :: b -> HiValue         -- ^ creates HiValue from element of the structure
  hiFoldl :: (c -> b -> c) -> c -> a -> c -- ^ left fold function for structure
  hiReverse :: a -> a                     -- ^ reverse structure

instance HiListable (Seq.Seq HiValue) HiValue where
  hiLength = Seq.length
  hiIndex = Seq.index
  hiTake = Seq.take
  hiDrop = Seq.drop
  mkHiValueSingle = id
  hiFoldl = foldl
  hiReverse = Seq.reverse

instance HiListable T.Text Char where
  hiLength = T.length
  hiIndex = T.index
  hiTake = T.take
  hiDrop = T.drop
  mkHiValueSingle = HiValueString . T.singleton
  hiFoldl = T.foldl
  hiReverse = T.reverse

instance HiListable BS.ByteString Word8 where
  hiLength = BS.length
  hiIndex = BS.index
  hiTake = BS.take
  hiDrop = BS.drop
  mkHiValueSingle = HiValueNumber . toRational
  hiFoldl = BS.foldl
  hiReverse = BS.reverse

-- | Type class for packing values to HiValue
class HiPackable a where
  hiPack :: a -> HiValue -- ^ Pack value to HiValue

instance HiPackable Bool where
  hiPack = HiValueBool

instance HiPackable HiAction where
  hiPack = HiValueAction

instance HiPackable Rational where
  hiPack = HiValueNumber

instance HiPackable UTCTime where
  hiPack = HiValueTime

instance HiPackable T.Text where
  hiPack = HiValueString

instance HiPackable BS.ByteString where
  hiPack = HiValueBytes

instance HiPackable (Seq.Seq HiValue) where
  hiPack = HiValueList

instance HiPackable (Map.Map HiValue HiValue) where
  hiPack = HiValueDict

instance HiPackable HiFun where
  hiPack = HiValueFunction

-- | Packs an element and returns it in specific monad
hiReturn :: (HiPackable a, Monad m) => a -> m HiValue
hiReturn = return . hiPack

-- | Type class for types for which division operation is defined
class HiDivable a b where
  hiDiv :: a -> b -> Either HiError HiValue

instance HiDivable Rational Rational where
  hiDiv _ 0 = Left HiErrorDivideByZero
  hiDiv a b = (Right . hiPack) $ a / b

instance HiDivable T.Text T.Text where
  hiDiv a b = (Right . hiPack) $ a <> "/" <> b

-- | Type class for types for which multiply operation is defined
class HiMulable a b where
  hiMul :: a -> b -> Either HiError HiValue

instance {-# OVERLAPPING #-} HiMulable Rational Rational where
  hiMul a b = (Right . hiPack) $ a * b

instance {-# OVERLAPPABLE #-} (HiPackable a, Semigroup a) => HiMulable a Rational where
  hiMul a b
    | denominator b /= 1 || b <= 0 = Left HiErrorInvalidArgument
    | otherwise = (Right . hiPack) $ stimes (numerator b) a

-- | Type class for types for which add operation is defined
class HiAddable a b where
  hiAdd :: a -> b -> Either HiError HiValue

instance {-# OVERLAPPABLE #-} (Semigroup a, HiPackable a) => HiAddable a a where
  hiAdd a b = (Right . hiPack) $ a <> b

instance {-# OVERLAPPING #-} HiAddable Rational Rational where
  hiAdd a b = (Right . hiPack) $ a + b

instance {-# OVERLAPPING #-} HiAddable UTCTime Rational where
  hiAdd a b = (Right . hiPack) $ addUTCTime (realToFrac b) a

-- | Type class for types for which subtract operation is defined
class HiSubbable a b where
  hiSub :: a -> b -> Either HiError HiValue

instance HiSubbable Rational Rational where
  hiSub a b = (Right . hiPack) $ a - b

instance HiSubbable UTCTime UTCTime where
  hiSub a b = (Right . hiPack) $ toRational $ diffUTCTime a b

-- | Type class for listable types
class HiList a where
  hiLen, hiRev :: a -> Either HiError HiValue

instance (HiListable a b, HiPackable a) => HiList a where
  hiLen = Right . hiPack . toRational . hiLength
  hiRev = Right . hiPack . hiReverse
