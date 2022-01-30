{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception (catch)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Set (Set)
import Data.Void (Void)
import HW3.Action (HIO (runHIO), HiPermission, PermissionException)
import HW3.Base (HiError)
import HW3.Evaluator (eval)
import HW3.HaskelineCompletion (hiComplete)
import HW3.OptionsParser (parseOptions)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))
import Prettyprinter (Doc, LayoutOptions (layoutPageWidth), PageWidth (Unbounded),
                      defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Console.Haskeline (InputT, defaultSettings, getExternalPrint, getInputLine, runInputT,
                                 setComplete)
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Error (ParseErrorBundle)

processLine :: Set HiPermission -> String -> IO (Either (ParseErrorBundle String Void) (Either HiError (Doc AnsiStyle)))
processLine permissions str = do
  let eExpr = parse str
  evaled <- mapM (\expr -> runHIO (eval expr) permissions) eExpr
  return $ (fmap . fmap) prettyValue evaled

showResult :: IO (Either (ParseErrorBundle String Void) (Either HiError (Doc AnsiStyle))) -> IO String
showResult eIO = do
  ee <- eIO
  case ee of
    Left parseErrorBundle -> return $ errorBundlePretty parseErrorBundle
    Right e -> case e of
      Left he -> return $ show he
      Right doc -> return $ renderString $ layoutPretty defaultLayoutOptions { layoutPageWidth = Unbounded } doc

main :: IO ()
main = process =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      (  fullDesc
      <> progDesc "Made with love"
      <> header "hi language interpreter" )

process :: Set HiPermission -> IO ()
process permissions = runInputT (setComplete hiComplete defaultSettings) loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do
                 printer <- getExternalPrint
                 lift (catch @PermissionException ((showResult . processLine permissions) input >>= printer) (\e -> print e))
                 loop
