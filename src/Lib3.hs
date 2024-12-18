{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using infix" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use concatMap" #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Command(..),
    Statements(..),
    parseLoad,
    parseSave,
    parseStatementsCommand,
    renderQuery,
    renderItem,
    applyQueries,
    getQueries,
    ) where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)
import Control.Concurrent.STM.TVar
import Data.Maybe (fromMaybe)
import Lib2 (Query(..), State(..), Storage(..), ItemWithId(..), Item(..), parseQuery, strip, emptyState)
import qualified Lib2

import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)

data StorageOp = Save String (Chan ()) | Load (Chan String)

fileName :: String
fileName = "stateBackup.txt"

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of 
    Save content replyChan -> do
      writeFile fileName content
      writeChan replyChan ()
    Load replyChan -> do
      content <- readFile fileName
      writeChan replyChan content
  storageOpLoop chan

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)


-- | Parses user's input.
parseCommand ::  Lib2.Parser Command
parseCommand =
  Lib2.or3'
    parseLoad
    parseSave
    parseStatementsCommand

parseLoad :: Lib2.Parser Command
parseLoad = do
  _ <- Lib2.parseLiteral "load"
  return LoadCommand

parseSave :: Lib2.Parser Command
parseSave = do
  _ <- Lib2.parseLiteral "save"
  return SaveCommand

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
-- | Parses Statements.
-- Handles both single and batch commands.
-- | Parses Statements.
parseStatements :: Lib2.Parser Statements
parseStatements = do
  queries <- Lib2.many Lib2.parseQuery
  return (Batch queries)

parseStatementsCommand :: Lib2.Parser Command
parseStatementsCommand = do
  StatementCommand <$> parseStatements

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> [Query]
marshallState state =
    map (AddStorage . Storage . (:[]) . item) (inventory state)

-- Modified to render each command on a new line
-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: [Query] -> String
renderStatements queries = 
    concat $ map renderQuery queries

--getStringFromComponent :: Lib2.Component -> String
--getStringFromComponent (Lib2.Storage items) = 
--getStringFromComponent (Lib2.Storage a b) = "storage(" ++ show a ++ ", " ++ show b ++ ")"
--getStringFromComponent (Lib2.ProductionUnit a b) = "production_unit(" ++ show a ++ ", " ++ show b ++ ")"
--getStringFromComponent (Lib2.Subsystem components) = "subsystem(" ++ unwords (map getStringFromComponent components) ++ ")"

-- Helper function to render individual queries
renderQuery :: Query -> String
renderQuery query = case query of
    AddStorage (Storage items) -> 
        concat $ map (\item -> "add_storage " ++ renderItem item ++ "\n") items
    SellItem name qty -> 
        "sell " ++ name ++ " " ++ show qty ++ "\n"
    ShowInventory -> 
        "show_store\n"
    RestockItems name qty -> 
        "restock " ++ name ++ " " ++ show qty ++ "\n"
    RemoveItem name -> 
        "remove_item " ++ name ++ "\n"

renderItem :: Item -> String
renderItem (Food cat name qty) = show cat ++ " " ++ name ++ " " ++ show qty
renderItem (Beverage name qty) = "Beverage " ++ name ++ " " ++ show qty
renderItem (HouseholdSupplies cat name qty) = show cat ++ " " ++ name ++ " " ++ show qty


-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable    
stateTransition ::
  TVar Lib2.State ->
  Command ->
  Chan StorageOp ->
  IO (Either String (Maybe String))
stateTransition st command ioChan = case command of
  LoadCommand -> do
    replyChan <- newChan
    writeChan ioChan (Load replyChan)
    content <- readChan replyChan
    case Lib2.parse parseStatements content of
      (Left err, _) -> return $ Left err
      (Right statements, _) -> atomically $ do
        let (_, newState) = applyQueries Lib2.emptyState (getQueries statements)
        writeTVar st newState
        return $ Right $ Just "State loaded successfully."
  SaveCommand -> do
    currentState <- readTVarIO st
    result <- saveStateToFile currentState ioChan
    return $ case result of
      Left err -> Left err
      Right () -> Right $ Just "State saved successfully."
  StatementCommand statements -> atomically $ do
    currentState <- readTVar st
    let (msg, newState) = applyQueries currentState (getQueries statements)
    writeTVar st newState
    return $ Right $ Just ("Commands parsed\nLog:\n" ++ msg)

saveStateToFile :: Lib2.State -> Chan StorageOp -> IO (Either String ())
saveStateToFile state ioChan = do
  let serializedState = renderStatements (marshallState state)
  replyChan <- newChan
  writeChan ioChan (Save serializedState replyChan)
  result <- readChan replyChan
  return $ Right result

getQueries :: Statements -> [Lib2.Query]
getQueries (Single query) = [query]
getQueries (Batch queries) = queries

applyQueries :: Lib2.State -> [Lib2.Query] -> (String, Lib2.State)
applyQueries st [] = ("", st)
applyQueries st (q : qs) =
  let (msg, newState) = applyQuery st q
      (otherMsg, finalState) = applyQueries newState qs
   in (msg ++ "\n" ++ otherMsg, finalState)


applyQuery :: Lib2.State -> Lib2.Query -> (String, Lib2.State)
applyQuery state query = unsafePerformIO $ do
    putStrLn $ "applyQuery called with query: " ++ show query
    case Lib2.stateTransition state query of
      Right (msg, newState) -> do
          putStrLn $ "State after query: " ++ show (inventory newState)
          return (fromMaybe "" msg, newState)
      Left e -> do
          putStrLn $ "Error in query: " ++ e
          return (e, state)