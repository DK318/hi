{-# LANGUAGE DeriveGeneric #-}
module HW3.Base
  ( -- * Functions
    HiFun (..)
    -- * Values
  , HiValue (..)
    -- * Expressions
  , HiExpr (..)
    -- * Errors
  , HiError (..)
    -- * Actions
  , HiAction (..)
    -- * HIMonad
  , HiMonad (..) ) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data HiFun
  = HiFunDiv            -- ^ divide function
  | HiFunMul            -- ^ multiply function
  | HiFunAdd            -- ^ add function
  | HiFunSub            -- ^ subtract function
  | HiFunNot            -- ^ not function
  | HiFunAnd            -- ^ and function
  | HiFunOr             -- ^ or function
  | HiFunLessThan       -- ^ less than function
  | HiFunGreaterThan    -- ^ greater than function
  | HiFunEquals         -- ^ equals function
  | HiFunNotLessThan    -- ^ not less than function
  | HiFunNotGreaterThan -- ^ not greater than function
  | HiFunNotEquals      -- ^ not equals function
  | HiFunIf             -- ^ if function
  | HiFunLength         -- ^ length function
  | HiFunToUpper        -- ^ to upper function
  | HiFunToLower        -- ^ to lower function
  | HiFunReverse        -- ^ reverse function
  | HiFunTrim           -- ^ trim function
  | HiFunList           -- ^ list function
  | HiFunRange          -- ^ range function
  | HiFunFold           -- ^ fold function
  | HiFunPackBytes      -- ^ pack bytes function
  | HiFunUnpackBytes    -- ^ unpack bytes function
  | HiFunEncodeUtf8     -- ^ encode utf8 function
  | HiFunDecodeUtf8     -- ^ decode utf8 function
  | HiFunZip            -- ^ zip function
  | HiFunUnzip          -- ^ unzip function
  | HiFunSerialise      -- ^ serialise function
  | HiFunDeserialise    -- ^ deserialise function
  | HiFunRead           -- ^ read function
  | HiFunWrite          -- ^ write function
  | HiFunMkDir          -- ^ make directory function
  | HiFunChDir          -- ^ change directory function
  | HiFunParseTime      -- ^ parse time function
  | HiFunRand           -- ^ random function
  | HiFunEcho           -- ^ echo function
  | HiFunCount          -- ^ count function
  | HiFunKeys           -- ^ keys function
  | HiFunValues         -- ^ values function
  | HiFunInvert         -- ^ invert function
  deriving (Eq, Ord, Generic)

data HiValue
  = HiValueNull                           -- ^ null value
  | HiValueBool     Bool                  -- ^ boolean value
  | HiValueAction   HiAction              -- ^ action value
  | HiValueNumber   Rational              -- ^ number value
  | HiValueTime     UTCTime               -- ^ time value
  | HiValueString   T.Text                -- ^ string value
  | HiValueBytes    ByteString            -- ^ bytes value
  | HiValueList     (Seq HiValue)         -- ^ list value
  | HiValueDict     (Map HiValue HiValue) -- ^ dictionary value
  | HiValueFunction HiFun                 -- ^ function value
  deriving (Eq, Show, Ord, Generic)

data HiExpr
  = HiExprValue HiValue            -- ^ expression value
  | HiExprApply HiExpr [HiExpr]    -- ^ apply value
  | HiExprRun   HiExpr             -- ^ run value
  | HiExprDict  [(HiExpr, HiExpr)] -- ^ dictionary value
  deriving (Eq, Show)

data HiError
  = HiErrorInvalidArgument -- ^ invalid argument error
  | HiErrorInvalidFunction -- ^ invalid function error
  | HiErrorArityMismatch   -- ^ arity mismatch error
  | HiErrorDivideByZero    -- ^ divide by zero error
  deriving (Eq)

data HiAction
  = HiActionRead  FilePath            -- ^ read action
  | HiActionWrite FilePath ByteString -- ^ write action
  | HiActionMkDir FilePath            -- ^ make directory action
  | HiActionChDir FilePath            -- ^ change directory action
  | HiActionCwd                       -- ^ current working directory action
  | HiActionNow                       -- ^ current time action
  | HiActionRand  Int Int             -- ^ random action
  | HiActionEcho  T.Text              -- ^ echo action
  deriving (Eq, Ord, Show, Generic)

instance Show HiError where
  show HiErrorInvalidArgument = "Invalid argument"
  show HiErrorInvalidFunction = "Invalid function"
  show HiErrorArityMismatch   = "Arity mismatch"
  show HiErrorDivideByZero    = "Divide by zero"

instance Show HiFun where
  show HiFunDiv            = "div"
  show HiFunMul            = "mul"
  show HiFunAdd            = "add"
  show HiFunSub            = "sub"
  show HiFunNot            = "not"
  show HiFunAnd            = "and"
  show HiFunOr             = "or"
  show HiFunLessThan       = "less-than"
  show HiFunGreaterThan    = "greater-than"
  show HiFunEquals         = "equals"
  show HiFunNotLessThan    = "not-less-than"
  show HiFunNotGreaterThan = "not-greater-than"
  show HiFunNotEquals      = "not-equals"
  show HiFunIf             = "if"
  show HiFunLength         = "length"
  show HiFunToUpper        = "to-upper"
  show HiFunToLower        = "to-lower"
  show HiFunReverse        = "reverse"
  show HiFunTrim           = "trim"
  show HiFunList           = "list"
  show HiFunRange          = "range"
  show HiFunFold           = "fold"
  show HiFunPackBytes      = "pack-bytes"
  show HiFunUnpackBytes    = "unpack-bytes"
  show HiFunEncodeUtf8     = "encode-utf8"
  show HiFunDecodeUtf8     = "decode-utf8"
  show HiFunZip            = "zip"
  show HiFunUnzip          = "unzip"
  show HiFunSerialise      = "serialise"
  show HiFunDeserialise    = "deserialise"
  show HiFunRead           = "read"
  show HiFunWrite          = "write"
  show HiFunMkDir          = "mkdir"
  show HiFunChDir          = "cd"
  show HiFunParseTime      = "parse-time"
  show HiFunRand           = "rand"
  show HiFunEcho           = "echo"
  show HiFunCount          = "count"
  show HiFunKeys           = "keys"
  show HiFunValues         = "values"
  show HiFunInvert         = "invert"

instance Serialise HiFun
instance Serialise HiValue
instance Serialise HiAction

class Monad m => HiMonad m where
  -- | Runs action in spectified monad
  runAction :: HiAction -> m HiValue
