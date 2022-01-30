module HW3.HaskelineCompletion (hiComplete) where

import Control.Monad.IO.Class (MonadIO)
import Data.List (isPrefixOf)
import HW3.Base (HiFun (..))
import System.Console.Haskeline.Completion (CompletionFunc, completeFilename, completeWord,
                                            fallbackCompletion, simpleCompletion)

-- | List of possible function names
completeNames :: [String]
completeNames = (:) "quit" $ show <$>
  [ HiFunDiv
  , HiFunMul
  , HiFunAdd
  , HiFunSub
  , HiFunNot
  , HiFunAnd
  , HiFunOr
  , HiFunLessThan
  , HiFunGreaterThan
  , HiFunEquals
  , HiFunNotLessThan
  , HiFunNotGreaterThan
  , HiFunNotEquals
  , HiFunIf
  , HiFunLength
  , HiFunToUpper
  , HiFunToLower
  , HiFunReverse
  , HiFunTrim
  , HiFunList
  , HiFunRange
  , HiFunFold
  , HiFunPackBytes
  , HiFunUnpackBytes
  , HiFunEncodeUtf8
  , HiFunDecodeUtf8
  , HiFunZip
  , HiFunUnzip
  , HiFunSerialise
  , HiFunDeserialise
  , HiFunRead
  , HiFunWrite
  , HiFunMkDir
  , HiFunChDir
  , HiFunParseTime
  , HiFunRand
  , HiFunEcho
  , HiFunCount
  , HiFunKeys
  , HiFunValues
  , HiFunInvert ]

-- | Completion function for 'haskeline' that completes function names
completeHiFunName :: Monad m => CompletionFunc m
completeHiFunName = completeWord (Just '"') [' '] compl
  where compl str = return $ simpleCompletion <$> filter (isPrefixOf str) completeNames

-- | Completion function for 'haskeline' (firstly completes function names, then filenames)
hiComplete :: MonadIO m => CompletionFunc m
hiComplete = fallbackCompletion completeHiFunName completeFilename
