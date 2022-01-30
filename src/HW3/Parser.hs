module HW3.Parser (parse) where

import Control.Applicative (Applicative (liftA2), (<**>))
import Control.Monad (when)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR), makeExprParser)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as S
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), ParseErrorBundle, Parsec, between,
                        choice, empty, many, manyTill, runParser, satisfy, sepBy, sepBy1, some,
                        (<?>), (<|>))
import Text.Megaparsec.Char (char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)
import Text.Read (readMaybe)

type Parser = Parsec Void String

-- | Space consumer
sc :: Parser ()
sc = L.space space1 empty empty

-- | Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol wrapper
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parses value between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses value between brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parses value between braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parses value between "[#" and "#]"
bytesBrackets :: Parser a -> Parser a
bytesBrackets = between (symbol "[#") (symbol "#]")

-- | Parses functions
pHiValueFunction :: Parser HiValue
pHiValueFunction = HiValueFunction <$> choice
  ((\hiFun -> hiFun <$ string (show hiFun)) <$>
   [ HiFunDiv
   , HiFunMul
   , HiFunAdd
   , HiFunSub
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
   , HiFunInvert
   , HiFunNot ]) <?> "function"

-- | Parses 'Word8' that represented by two character hexadecimal
pWord8 :: Parser Word8
pWord8 = do
  w <- lexeme $ many (satisfy isAlphaNum)
  when (length w /= 2) $ fail "Parsed value should have length 2"
  let mWord8 = readMaybe ("0x" ++ w) :: Maybe Word8
  case mWord8 of
    Nothing    -> fail "Parsed value is not a valid word8"
    Just word8 -> return word8

-- | Parses bytes
pHiValueBytes :: Parser HiValue
pHiValueBytes = (HiValueBytes . BS.pack) <$> bytesBrackets (many pWord8) <?> "bytes"

-- | Parses number
pHiValueNumber :: Parser HiValue
pHiValueNumber = (HiValueNumber . toRational) <$> L.signed sc (lexeme L.scientific) <?> "number"

-- | Parses action
pHiValueAction :: Parser HiValue
pHiValueAction = HiValueAction <$> choice
 [ HiActionCwd <$ string "cwd"
 , HiActionNow <$ string "now" ] <?> "action"

-- | Parses boolean value
pHiValueBool :: Parser HiValue
pHiValueBool = HiValueBool <$> choice
           [ True <$ string "true"
           , False <$ string "false" ] <?> "bool"

-- | Parses "null"
pHiValueNull :: Parser HiValue
pHiValueNull = HiValueNull <$ string "null"

-- | Parses string literal (value between quotes)
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"') <?> "string"

-- | Parses list
pList :: Parser HiExpr
pList = (HiExprApply (HiExprValue $ HiValueFunction HiFunList)) <$> brackets (pInfix `sepBy` symbol ",") <?> "list"

-- | Parses string
pHiValueString :: Parser HiValue
pHiValueString = (HiValueString . T.pack) <$> stringLiteral

-- | Parses 'HiValue'
pHiValue :: Parser HiValue
pHiValue = choice
 [ pHiValueBool
 , pHiValueFunction
 , pHiValueNumber
 , pHiValueNull
 , pHiValueString
 , pHiValueBytes
 , pHiValueAction ]

-- | Parses HiExprValue
pHiExprValue :: Parser HiExpr
pHiExprValue = HiExprValue <$> lexeme pHiValue <|> pList <|> pDictionary

-- | Parses partially applied HiExprRun
pAction :: Parser (HiExpr -> HiExpr)
pAction = HiExprRun <$ symbol "!"

-- | Parses [a-zA-Z][a-zA-Z0-9]* separated by "-" character
pDotName :: Parser [String]
pDotName = ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'

-- | Parses partially applied dot access
pDotAccess :: Parser (HiExpr -> HiExpr)
pDotAccess = lexeme $ (\str expr -> HiExprApply expr [HiExprValue $ HiValueString (T.pack (intercalate "-" str))]) <$> (char '.' *> pDotName)

-- | Parses an item in dictionary
pDictionaryItem :: Parser (HiExpr, HiExpr)
pDictionaryItem = (,) <$> (pInfix <* symbol ":") <*> pInfix

-- | Parses dictionary
pDictionary :: Parser HiExpr
pDictionary = HiExprDict <$> braces (pDictionaryItem `sepBy` symbol ",")

-- | Parses partially applied arguments (values in parenthesis separated by comma)
pArguments' :: Parser (HiExpr -> HiExpr)
pArguments' = pure (flip HiExprApply) <*> parens (pInfix `sepBy` symbol ",")

-- | Parses partially applied arguments, runs and doc accesses
pArguments :: Parser (HiExpr -> HiExpr)
pArguments = do
  lst <- many (pArguments' <|> pAction <|> pDotAccess)
  return $ (flip mkHiExpr) lst

-- | Makes HiExpr with given HiExpr and list of partially applied arguments
mkHiExpr :: HiExpr -> [HiExpr -> HiExpr] -> HiExpr
mkHiExpr = foldl (&)

-- | Parses HiExpr
pHiExpr :: Parser HiExpr
pHiExpr = (parens pHiExpr <|> pHiExprValue) <**> pArguments

-- | Parses given string to expression
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (sc *> pInfix <* eof) ""

-- | Infix operators
operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binaryL "*" HiFunMul
    , binary' InfixL (op "/" "=") HiFunDiv ]

  , [ binaryL "+" HiFunAdd
    , binaryL "-" HiFunSub ]

  , [ binary "<=" HiFunNotGreaterThan
    , binary ">=" HiFunNotLessThan
    , binary "<" HiFunLessThan
    , binary ">" HiFunGreaterThan
    , binary "==" HiFunEquals
    , binary "/=" HiFunNotEquals ]

  , [ binaryR "&&" HiFunAnd ]

  , [ binaryR "||" HiFunOr ]
  ]

-- | Operator parser
op :: String -> String -> Parser String
op n next = (lexeme . try) (string n <* notFollowedBy (symbol next))

-- | Helper for binary operator constructor
binary'
  :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -- ^ operator constructor
  -> Parser String                                                   -- ^ infix operator parser
  -> HiFun                                                           -- ^ function
  -> Operator Parser HiExpr
binary' ctor p f = ctor ((\a b -> HiExprApply (HiExprValue $ HiValueFunction f) [a, b]) <$ p)

-- | Constructs InfixL operator
binaryL :: String -> HiFun -> Operator Parser HiExpr
binaryL name f = binary' InfixL (symbol name) f

-- | Constructs InfixR operator
binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryR name f = binary' InfixR (symbol name) f

-- | Constructs InfixN operator
binary :: String -> HiFun -> Operator Parser HiExpr
binary name f = binary' InfixN (symbol name) f

-- | Parses term in infix form
pTerm :: Parser HiExpr
pTerm = try pHiExpr <|> parens pInfix

-- | Parses infix expressions
pInfix :: Parser HiExpr
pInfix = makeExprParser pTerm operatorTable
