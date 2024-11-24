{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using infix" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,  -- kept for compatibility
    marshallState,
    renderStatements
    ) where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)
import Control.Concurrent.STM.TVar
import Data.Maybe (fromMaybe)
import Lib2 (Query(..), State(..), Storage(..), ItemWithId(..), Item(..), parseQuery, strip, emptyState)
import qualified Lib2

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

-- Simplified to just handle single commands
data Command = StatementCommand Query |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- Modified to convert an ItemWithId into a storage command string
itemToStorageCommand :: ItemWithId -> String
itemToStorageCommand (ItemWithId _ item) = case item of
    Food cat name qty -> 
        "add_storage " ++ show cat ++ " " ++ name ++ " " ++ show qty
    Beverage name qty -> 
        "add_storage Beverage " ++ name ++ " " ++ show qty
    HouseholdSupplies cat name qty -> 
        "add_storage " ++ show cat ++ " " ++ name ++ " " ++ show qty

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
parseLoad :: String -> Either String (Command, String)
parseLoad input =
  let strippedInput = Lib2.strip input
  in case Lib2.parseLiteral "load" strippedInput of
       Right (_, rest) -> Right (LoadCommand, rest)
       Left _ -> Left "Failed to parse 'load'"

parseSave :: String -> Either String (Command, String)
parseSave input =
  case Lib2.parseLiteral "save" (Lib2.strip input) of
    Right (_, rest) -> Right (SaveCommand, rest)
    Left _ -> Left ""

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input = 
    case Lib2.or3' parseStatementCommand parseLoad parseSave input of
        Right result -> Right result
        Left err -> Left $ "Failed to parse command: " ++ err
    where
        parseStatementCommand input = 
            case lines input of
                [line] -> case parseQuery line of
                    Right (query, rest) -> Right (StatementCommand query, rest)
                    Left err -> Left err
                _ -> case parseStatements input of
                    Right (queries, rest) -> Right (StatementCommand (head queries), rest)
                    Left err -> Left err

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String ([Query], String)
parseStatements input = do
    let lines' = filter (not . null) $ lines input
    queries <- parseLines lines'
    return (queries, "")
  where
    parseLines :: [String] -> Either String [Query]
    parseLines [] = Right []
    parseLines (line:rest) = do
        (query, remaining) <- parseQuery line
        if null (Lib2.strip remaining)
            then do
                rest_queries <- parseLines rest
                return (query : rest_queries)
            else Left $ "Unexpected content after query: " ++ remaining

-- You might also need to ensure strip is available:
strip :: String -> String
strip = Lib2.strip

-- Modified to convert state into a list of commands

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
    case parseStatements content of
      Left err -> return $ Left err
      Right (queries, _) -> atomically $ do
        let (_, newState) = applyQueries Lib2.emptyState queries
        writeTVar st newState
        return $ Right $ Just "State loaded successfully."
  SaveCommand -> do
    currentState <- readTVarIO st
    result <- saveStateToFile currentState ioChan
    return $ case result of
      Left err -> Left err
      Right () -> Right $ Just "State saved successfully."
  StatementCommand query -> atomically $ do
    currentState <- readTVar st
    let (msg, newState) = applyQuery currentState query
    writeTVar st newState
    return $ Right $ Just ("Command parsed\nLog:\n" ++ msg)

saveStateToFile :: Lib2.State -> Chan StorageOp -> IO (Either String ())
saveStateToFile state ioChan = do
  let serializedState = renderStatements (marshallState state)
  replyChan <- newChan
  writeChan ioChan (Save serializedState replyChan)
  result <- readChan replyChan
  return $ Right result

applyQueries :: Lib2.State -> [Lib2.Query] -> (String, Lib2.State)
applyQueries st [] = ("", st)
applyQueries st (q : qs) =
  let (msg, newState) = applyQuery st q
      (otherMsg, finalState) = applyQueries newState qs
   in (msg ++ "\n" ++ otherMsg, finalState)

applyQuery :: Lib2.State -> Lib2.Query -> (String, Lib2.State)
applyQuery state query =
  case Lib2.stateTransition state query of
    Right (msg, newState) -> (fromMaybe "" msg, newState)
    Left e -> (e, state)