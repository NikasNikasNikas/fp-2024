{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.String.Conversions (cs)
import Data.Maybe (fromMaybe)
import Web.Scotty
import qualified Lib2
import qualified Lib3

main :: IO ()
main = do
    chan <- newChan :: IO (Chan Lib3.StorageOp)
    state <- newTVarIO Lib2.emptyState
    _ <- forkIO $ Lib3.storageOpLoop chan
    scotty 3000 $
        post "/" $ do
            b <- body
            liftIO $ putStrLn ("Request was: " ++ cs b)
            response <- liftIO $ parseInput state chan $ cs b
            text $ cs response

parseInput :: TVar Lib2.State -> Chan Lib3.StorageOp -> String -> IO String
parseInput state storageChan input = 
    case Lib2.parse Lib3.parseCommand (strip input) of
        (Left e, _) -> return e
        (Right cmd, "") -> do
            info <- Lib3.stateTransition state cmd storageChan
            case info of
                Left e -> return e
                Right mb -> return $ fromMaybe "Success" mb
        (Right _, rest) -> return $ "Could not parse fully: " ++ rest
    where 
        strip = Lib2.strip 