{-# LANGUAGE DeriveFunctor #-}

module Main (main) where
import Control.Monad.State (StateT, get, put, runStateT, MonadState)
import Control.Lens hiding (view)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.State
import Data.String.Conversions (cs)
import Network.Wreq hiding (put, get) 
import Data.ByteString.Lazy.Char8 (ByteString)
import System.Environment (getArgs)
import qualified Lib2
import qualified Lib3
import Control.Monad.State.Lazy (State, runState, modify, get)
import System.IO (readFile)


--import qualified Control.Monad.Trans.State as S
data Command next
  = 
 AddStorage Lib2.Storage next
  | SellItem String Int next
  | ShowInventory (String -> next)
  | RestockItems String Int next
  | RemoveItem String next
  | LoadState (String -> next)
  | SaveState (Bool -> next) 
  deriving (Functor)

type StoreDSL = Free Command

loadState :: StoreDSL String
loadState = liftF $ LoadState id

saveState :: StoreDSL Bool
saveState = liftF $ SaveState id

addStorage :: Lib2.Storage -> StoreDSL ()
addStorage storage = liftF $ AddStorage storage ()

sellItem :: String -> Int -> StoreDSL ()
sellItem itemName quantity = liftF $ SellItem itemName quantity ()

showInventory :: StoreDSL String
showInventory = liftF $ ShowInventory id

restockItems :: String -> Int -> StoreDSL ()
restockItems itemName quantity = liftF $ RestockItems itemName quantity ()

removeItem :: String -> StoreDSL ()
removeItem itemName = liftF $ RemoveItem itemName ()

renderStorage :: Lib2.Storage -> String
renderStorage (Lib2.Storage items) = unwords (Prelude.map renderItem items)

renderItem :: Lib2.Item -> String
renderItem (Lib2.Food category name qty) = show category ++ " " ++ name ++ " " ++ show qty
renderItem (Lib2.Beverage name qty) = "Beverage " ++ name ++ " " ++ show qty
renderItem (Lib2.HouseholdSupplies category name qty) = show category ++ " " ++ name ++ " " ++ show qty

runHttpSingle :: StoreDSL a -> IO a
runHttpSingle (Pure a) = return a

runHttpSingle (Free (AddStorage storage next)) = do
  putStrLn $ "Sending request: add_storage " ++ show storage
  _ <- post "http://localhost:3000" (cs ("add_storage " ++ (renderStorage storage)) :: ByteString)
  runHttpSingle next
runHttpSingle (Free (SellItem itemName quantity next)) = do
  putStrLn $ "Sending request: Sell " ++ itemName ++ " " ++ show quantity
  _ <- post "http://localhost:3000" (cs ("sell " ++ itemName ++ " " ++ show quantity) :: ByteString)
  runHttpSingle next
runHttpSingle (Free (RestockItems itemName quantity next)) = do
  putStrLn $ "Sending request: restock " ++ itemName ++ " " ++ show quantity
  _ <- post "http://localhost:3000" (cs  ("restock " ++ itemName ++ " " ++ show quantity) :: ByteString)
  runHttpSingle next
runHttpSingle (Free (RemoveItem itemName next)) = do
  putStrLn $ "Sending request: remove_item " ++ itemName
  _ <- post "http://localhost:3000" (cs ("remove_item " ++ itemName) :: ByteString)
  runHttpSingle next
runHttpSingle (Free (ShowInventory next)) = do
  resp <- post "http://localhost:3000" (cs "show_store" :: ByteString)
  let inventoryStr = cs $ resp ^. responseBody
  putStrLn "Current Inventory:"
  putStrLn inventoryStr
  runHttpSingle (next inventoryStr)

runHttpSingle (Free (LoadState next)) = do
  putStrLn "Sending request: load"
  resp <- post "http://localhost:3000" (cs "load" :: ByteString)
  let loadedState = cs $ resp ^. responseBody
  putStrLn "State loaded:"
  putStrLn loadedState
  runHttpSingle (next loadedState)
runHttpSingle (Free (SaveState next)) = do
  putStrLn "Sending request: save"
  resp <- post "http://localhost:3000" (cs "save" :: ByteString)
  let saveSuccess = cs (resp ^. responseBody) == "State saved successfully."
  putStrLn $ if saveSuccess then "State saved" else "Save failed"
  runHttpSingle (next saveSuccess)

runHttpBatch :: StoreDSL a -> IO a
runHttpBatch = runHttpBatch' []

runHttpBatch' :: [String] -> StoreDSL a -> IO a
runHttpBatch' acc (Pure a) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  return a
runHttpBatch' acc (Free (AddStorage storage next)) =
  runHttpBatch' (acc ++ ["add_storage " ++ (renderStorage storage)]) next
runHttpBatch' acc (Free (SellItem itemName quantity next)) =
  runHttpBatch' (acc ++ ["sell " ++ itemName ++ " " ++ show quantity]) next
runHttpBatch' acc (Free (RestockItems itemName quantity next)) =
  runHttpBatch' (acc ++ ["restock " ++ itemName ++ " " ++ show quantity]) next
runHttpBatch' acc (Free (RemoveItem itemName next)) =
  runHttpBatch' (acc ++ ["remove_item " ++ itemName]) next
runHttpBatch' acc (Free (ShowInventory next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  resp <- post "http://localhost:3000" (cs "show_store" :: ByteString)
  let inventoryStr = cs $ resp ^. responseBody
  putStrLn "Current Inventory:"
  putStrLn inventoryStr
  runHttpBatch' [] (next inventoryStr)
runHttpBatch' acc (Free (LoadState next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  resp <- post "http://localhost:3000" (cs "load" :: ByteString)
  let loadedState = cs $ resp ^. responseBody
  putStrLn "State loaded:"
  putStrLn loadedState
  runHttpBatch' [] (next loadedState)
runHttpBatch' acc (Free (SaveState next)) = do
  resp <- post "http://localhost:3000" (cs "save" :: ByteString)
  let saveSuccess = cs (resp ^. responseBody) == "State saved successfully."
  putStrLn $ if saveSuccess then "State saved" else "Save failed"
  runHttpBatch' acc (next saveSuccess)

-- In-Memory
type InMemoryState = StateT Lib2.State IO

runInMemory :: StoreDSL a -> InMemoryState a
runInMemory (Pure a) = return a
runInMemory (Free (AddStorage storage next)) = do
  modify (\state -> 
    case Lib2.stateTransition state (Lib2.AddStorage storage) of
      Right (_, newState) -> newState
      Left err -> error $ "Failed to add storage: " ++ err
    )
  runInMemory next
runInMemory (Free (SellItem itemName quantity next)) = do
  modify (\state -> 
    case Lib2.stateTransition state (Lib2.SellItem itemName quantity) of
      Right (_, newState) -> newState
      Left _ -> state
    )
  runInMemory next
runInMemory (Free (ShowInventory next)) = do
  state <- get
  let inventoryList = Lib2.inventory state
  let inventoryStr = Lib2.showInventory inventoryList
  runInMemory (next inventoryStr)
runInMemory (Free (RestockItems itemName quantity next)) = do
  modify (\state -> 
    case Lib2.stateTransition state (Lib2.RestockItems itemName quantity) of
      Right (_, newState) -> newState
      Left _ -> state
    )
  runInMemory next
runInMemory (Free (RemoveItem itemName next)) = do
  modify (\state -> 
    case Lib2.stateTransition state (Lib2.RemoveItem itemName) of
      Right (_, newState) -> newState
      Left _ -> state
    )
  runInMemory next
runInMemory (Free (LoadState next)) = do
  let fileName = "stateBackup.txt"
  fileContent <- liftIO $ readFile fileName
  case Lib2.parse Lib3.parseStatements fileContent of
    (Left err, _) -> error $ "Failed to parse state: " ++ err
    (Right statements, _) -> do
      currentState <- get
      let queries = Lib3.getQueries statements
      let (_, newState) = Lib3.applyQueries currentState queries
      put newState
      runInMemory (next fileContent)
runInMemory (Free (SaveState next)) = do
  currentState <- get
  let serializedState = Lib3.renderStatements (Lib3.marshallState currentState)
  let fileName = "stateBackup.txt"
  liftIO $ writeFile fileName serializedState
  runInMemory (next True)

main :: IO ()
main = do
  args <- getArgs

  let program = do
        addStorage (Lib2.Storage [Lib2.Food Lib2.Fruits "Apples" 100])
        addStorage (Lib2.Storage [Lib2.Food Lib2.Fruits "Bananans" 100])
        sellItem "Apples" 20
        restockItems "Apples" 5
        addStorage (Lib2.Storage [Lib2.Food Lib2.Vegetables "Carrots" 50])
        restockItems "Carrots" 30
        removeItem "Carrots"
        addStorage (Lib2.Storage [Lib2.HouseholdSupplies Lib2.CleaningProducts "Soap" 5])
        _ <- saveState
        _ <- loadState
        showInventory

  case args of
    ["single"] -> do
      putStrLn "Running with HTTP single request per command:"
      _ <- runHttpSingle program
      return ()
    ["batch"] -> do
      putStrLn "Running with HTTP batch requests:"
      _ <- runHttpBatch program
      return ()
    ["memory"] -> do
      putStrLn "Running with in-memory interpreter for testing:"
      finalState <- execStateT (runInMemory program) Lib2.emptyState
      print finalState
    _ -> putStrLn "Usage: stack exec fp2024-four-client [single|batch|memory]"