{-# LANGUAGE ApplicativeDo #-}
module HW3.OptionsParser (parseOptions) where

import qualified Data.Set as Set
import HW3.Action (HiPermission (..))
import Options.Applicative (Parser, flag, help, long, short)

-- | Command line parser for permissions
parseOptions :: Parser (Set.Set HiPermission)
parseOptions = do
    r <- flag Set.empty (Set.singleton AllowRead)
          (  long "read"
          <> short 'r'
          <> help "Allow read permissions"  )
    w <- flag Set.empty (Set.singleton AllowWrite)
          (  long "write"
          <> short 'w'
          <> help "Allow write permissions"  )
    t <- flag Set.empty (Set.singleton AllowTime)
          (  long "time"
          <> short 't'
          <> help "Allow time permissions"  )
    return $ Set.unions [r, w, t]
