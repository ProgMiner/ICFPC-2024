{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad.Except
import Control.Monad.Writer

import Prelude hiding (showString, showChar)

import ICFP


runErrorIO :: (forall m. MonadError String m => m a) -> IO a
runErrorIO (Right x)  = pure x
runErrorIO (Left err) = error err

evalParser :: MonadError String m => Parser m t a -> [t] -> m a
evalParser p toks = do (x, toks') <- runParser p toks
                       when (not $ null toks') $ throwError "not fully parsed"
                       pure x

main = do code <- getContents

          toks <- runErrorIO $ parseTokens code
          putStrLn "Parsed tokens: "
          mapM_ print toks
          putStrLn ""

          expr <- runErrorIO $ evalParser exprParser toks
          putStrLn $ "Parsed expr: " ++ show expr
          putStrLn ""

          res <- runErrorIO $ eval expr >>= showValue

          putStrLn "Evaluation result:"
          putStrLn res
