{-# LANGUAGE TypeApplications #-}
module HW3.Action
  ( -- * Permissions
    HiPermission (..)
    -- * Exceptions
  , PermissionException (..)
    -- * HIO monad
  , HIO (..) ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (ap, liftM, unless)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Set (Set, member)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time (getCurrentTime)
import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))
import HW3.Class (HiPackable (hiPack), hiReturn)
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, listDirectory,
                         setCurrentDirectory)
import System.Random (getStdRandom, uniformR)

data HiPermission
  = AllowRead  -- ^ Read permission
  | AllowWrite -- ^ Write permission
  | AllowTime  -- ^ Time permission
  deriving (Eq, Show, Ord)

data PermissionException =
  PermissionRequired HiPermission -- ^ Permission required exception
  deriving (Eq, Show)

instance Exception PermissionException

newtype HIO a = HIO
  {
    runHIO :: Set HiPermission -> IO a -- ^ Runs 'HIO' action
  }

instance Monad HIO where
  return a = HIO $ \_ -> return a
  (HIO hio) >>= k = HIO $ \set -> hio set >>= \a -> runHIO (k a) set

instance Applicative HIO where
  pure = return
  (<*>) = ap

instance Functor HIO where
  fmap = liftM

instance MonadIO HIO where
  liftIO io = HIO $ \set -> io


-- | Lifts 'IO' monad to 'HIO' checking given permission
liftIOPermissions
  :: IO a         -- ^ 'IO' monad to lift
  -> HiPermission -- ^ permission to check
  -> HIO a
liftIOPermissions io permission = liftIOAction (const io) permission ()

-- | Lifts 'IO' action to 'HIO' checking given permission
liftIOAction
  :: (a -> IO b)   -- ^ 'IO' action to lift
  -> HiPermission  -- ^ permission to check
  -> a             -- ^ 'IO' action argument
  -> HIO b
liftIOAction f permission a = HIO $ \set -> do
  unless (permission `member` set) $ throwIO (PermissionRequired permission)
  f a

instance HiMonad HIO where
  runAction (HiActionRead from) = do
    fileExist <- liftIOAction doesFileExist AllowRead from
    if fileExist then do
      contents <- liftIOAction BS.readFile AllowRead from
      let result = either (const (hiPack contents)) hiPack (T.decodeUtf8' contents)
      return result
    else do
      lst <- liftIOAction listDirectory AllowRead from
      let result = Seq.fromList $ (hiPack . T.pack) <$> lst
      hiReturn result
  runAction (HiActionWrite to bytes) = do
    liftIOAction (BS.writeFile to) AllowWrite bytes
    return HiValueNull
  runAction (HiActionMkDir filepath) = do
    liftIOAction createDirectory AllowWrite filepath
    return HiValueNull
  runAction (HiActionChDir filepath) = do
    liftIOAction setCurrentDirectory AllowRead filepath
    return HiValueNull
  runAction HiActionCwd = do
    current <- liftIOPermissions getCurrentDirectory AllowRead
    hiReturn $ T.pack current
  runAction HiActionNow = do
    current <- liftIOPermissions getCurrentTime AllowTime
    hiReturn current
  runAction (HiActionRand from to) = do
    val <- getStdRandom (uniformR (from, to))
    hiReturn $ toRational val
  runAction (HiActionEcho str) = do
    liftIOAction T.putStrLn AllowWrite str
    return HiValueNull
