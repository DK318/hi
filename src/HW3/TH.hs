{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module HW3.TH (generateFunN) where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((^?))
import Control.Monad (replicateM)
import Language.Haskell.TH (Exp (AppE, UInfixE, VarE), Name, Q, appE, lamE, newName, varE, varP)

-- For code extensibility

-- | Generates liftAn :: (Applicative f) => (a_1 -> a_2 -> ... -> a_n -> a_{n+1}) -> f a_1 -> f a_2 -> ... -> f a_{n+1}
liftAN :: Int -> Q Exp
liftAN n
  | n <= 0 = error "Argument should be positive"
  | otherwise = do
      operation <- varE '(<*>)
      f <- newName "f"
      args <- replicateM n (newName "a")
      start <- [| pure $(varE f) |]
      let expr = foldl (`UInfixE` operation) start (VarE <$> args)
      lamE (varP <$> f : args) (return expr)

-- | Generates function, that tries to unpack it's values and calculates them
generateFunN
  :: Int      -- ^ number of arguments (n)
  -> Name     -- ^ function with type (...) => a1 -> a2 -> ... -> an -> Either HiError HiValue
  -> [[Name]] -- ^ prisms for HiValues
  -> Q Exp
generateFunN n f prisms
  | n <= 0 = error "n should be positive"
  | not $ all (\xs -> length xs == n) prisms = error $ "All sublists should have length " ++ show n
  | otherwise = do
      vars <- replicateM n (newName "xs")
      liftFunc <- liftAN n
      let generateArgs :: [Name] -> [Exp]
          generateArgs prisms = [UInfixE (VarE a) (VarE '(^?)) (VarE prism) | (a, prism) <- zip vars prisms]

          generateClause :: [Name] -> Exp
          generateClause prisms = foldl AppE (AppE liftFunc (VarE f)) (generateArgs prisms)

          clauses = generateClause <$> prisms
          expr = foldl1 (`UInfixE` (VarE '(<|>))) clauses
      lamE (varP <$> vars) (appE [| hoistToTrans |] $ return expr)
