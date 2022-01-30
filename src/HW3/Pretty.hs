{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HW3.Pretty (prettyValue) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Ratio (denominator, numerator)
import qualified Data.Scientific as S
import qualified Data.Text.Encoding as T
import Data.Word (Word8)
import HW3.Base (HiAction (..), HiValue (..))
import Prettyprinter (Doc, Pretty (pretty), dquotes, encloseSep, flatAlt, list, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Printf (printf)

-- | Prettifies given 'HiValue'
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction fun) = viaShow fun
prettyValue (HiValueBool True) = "true"
prettyValue (HiValueBool False) = "false"
prettyValue HiValueNull = "null"
prettyValue (HiValueString text) = viaShow text
prettyValue (HiValueList lst) = list (prettyValue <$> toList lst)
prettyValue (HiValueBytes bts) = encloseSep "[# " " #]" " " ((pretty . printf @(Word8 -> String) "%02x") <$> BS.unpack bts)
prettyValue (HiValueAction act) = case act of
  HiActionRead s       -> "read(" <> viaShow s <> ")"
  HiActionWrite s bs   -> "write(" <> viaShow s <> "," <+> prettyValue (HiValueBytes bs) <> ")"
  HiActionMkDir s      -> "mkdir(" <> viaShow s <> ")"
  HiActionChDir s      -> "cd(" <> viaShow s <> ")"
  HiActionCwd          -> "cwd"
  HiActionNow          -> "now"
  HiActionRand from to -> "rand(" <> pretty from <> "," <+> pretty to <> ")"
  HiActionEcho str     -> "echo(" <> viaShow str <> ")"
prettyValue (HiValueDict dict) =
  let lst = Map.toList dict
      docs = (\(a, b) -> prettyValue a <> ":" <+> prettyValue b) <$> lst
  in encloseSep "{ " " }" ", " docs
prettyValue (HiValueTime time) = "parse-time(\"" <> viaShow time <> "\")"
prettyValue (HiValueNumber val) =
  let (s, repedent) = S.fromRationalRepetendUnlimited val
      num = numerator val
      dem = denominator val
      (q, r) = quotRem num dem
      sign = if r < 0 then "-" else "+"
      fraction = if q == 0 then pretty num <> "/" <> pretty dem else pretty q <+> sign <+> pretty (abs r) <> "/" <> pretty dem
  in
    if isJust repedent then
      fraction
    else if S.isInteger s then
      pretty num
    else
      pretty $ S.formatScientific S.Fixed Nothing s
