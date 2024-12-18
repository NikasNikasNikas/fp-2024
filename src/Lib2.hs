{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
module Lib2
  ( Query(..),
    State(..),
    Item(..),
    Category(..),
    Storage(..),
    ItemWithId(..),
    emptyState,
    stateTransition,
    parseItem,
    parseStorage,
    parseAddStorage,
    parseSellItem,
    parseQuery,
    parseRestock,
    parseRemove,
    parseView,
    parseAddInt,
    parseString,
    parseLiteral,
    strip,
    or3',
    Parser,
    parse,
    many,
    ItemWithId,
    showInventory,
  ) where

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
import Data.Char (isDigit)
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Except (ExceptT, runExceptT, catchE, throwE)
import Control.Monad.Trans.Class
import Control.Applicative hiding (many)
import qualified Parsers

-- | Data types for Queries and State
data Query
  = AddStorage Storage
  | SellItem String Int
  | ShowInventory
  | RestockItems String Int
  | RemoveItem String
  deriving (Eq, Show)

-- | Categories of items in the store
data Category = Fruits | Vegetables | Grains | Dairy | Meats | Beverages | CleaningProducts | PaperGoods
  deriving (Eq, Show)

-- | Items in the grocery store
data Item
  = Food Category String Int
  | Beverage String Int
  | HouseholdSupplies Category String Int
  deriving (Eq, Show)

data Storage = Storage [Item]
  deriving (Eq, Show)

-- <GroceryStore> ::= [<Item>]
data ItemWithId = ItemWithId
  { itemId :: Int,
    item :: Item
  }
  deriving (Eq, Show)

-- | Program state representing the inventory
data State = State
  { inventory :: [ItemWithId],
    nextId :: Int
  }
  deriving (Eq, Show)

-- | Initial program state
emptyState :: State
emptyState = State {inventory = [], nextId = 1}

-- | Basic parsers
type Parser a = ExceptT String (S.State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = S.runState (runExceptT parser)

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f p1 p2 = do
  v1 <- p1
  f v1 <$> p2

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f p1 p2 p3 = do
  v1 <- p1
  v2 <- p2
  f v1 v2 <$> p3

or2' :: Parser a -> Parser a -> Parser a
or2' a b = do
  inputBefore <- lift S.get
  a `catchE` \e1 -> do
    lift (S.put inputBefore)
    b `catchE` \e2 -> do
      lift (S.put inputBefore)
      throwE (e1 ++ "\n" ++ e2)

or3' :: Parser a -> Parser a -> Parser a -> Parser a
or3' a b c = do
  inputBefore <- lift S.get
  a `catchE` \e1 -> do
    lift (S.put inputBefore)
    b `catchE` \e2 -> do
      lift (S.put inputBefore)
      c `catchE` \e3 -> do
        lift (S.put inputBefore)
        throwE (e1 ++ "\n" ++ e2 ++ "\n" ++ e3)

or5' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or5' a b c d e = do
  or3' a b c `catchE` \e1 -> do
    resultB <- or2' d e
    return resultB `catchE` \_ -> throwE e1

many :: Parser a -> Parser [a]
many p =
  ( do
      x <- p
      xs <- many p
      return (x : xs)
  )
    `catchE` \_ -> return []

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip s = case s of
  [] -> []
  (x : xs) ->
    if x `elem` " \t\r\n"
      then lstrip xs
      else s

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

parseChar :: Char -> Parser Char
parseChar c = do
  input <- lift S.get
  case input of
    [] -> throwE ("Cannot find " ++ [c] ++ " in an empty input")
    s@(h : t) -> if c == h then lift $ S.put t >> return h else throwE (c : " is not found in " ++ s ++ "\n")

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  input <- lift S.get
  let strippedInput = strip input
  lift $ S.put strippedInput
  _ <- parseChar x
  rest <- parseLiteral xs
  return (x : rest)

-- <FoodItems> ::= <Fruits> | <Vegetables> | <Grains> | <Dairy> | <Meats>
parseFoodCategory :: Parser Category
parseFoodCategory = 
  or5' 
    (parseLiteral "Fruits" >> return Fruits)
    (parseLiteral "Vegetables" >> return Vegetables)
    (parseLiteral "Grains" >> return Grains)
    (parseLiteral "Dairy" >> return Dairy)
    (parseLiteral "Meats" >> return Meats)

-- <HouseholdSupplies> ::= <CleaningProducts> | <PaperGoods>
parseHouseholdCategory :: Parser Category
parseHouseholdCategory = do
  or2' 
    (parseLiteral "CleaningProducts" >> return CleaningProducts)
    (parseLiteral "PaperGoods" >> return PaperGoods)

-- <Fruits> ::= <Apples> | <Bananas> | <Oranges>
parseFruits :: Parser String
parseFruits = do
  name <- parseString
  if name `elem` ["Apples", "Bananas", "Oranges"]
    then return name
    else throwE "No such fruit allowed"

-- <Vegetables> ::= <Carrots> | <Potatoes> | <Spinach>
parseVegetables :: Parser String
parseVegetables = do
  name <- parseString
  if name `elem` ["Carrots", "Potatoes", "Spinach"]
    then return name
    else throwE "No such vegetable allowed"

-- <Grains> ::= <Rice> | <Bread> | <Pasta>
parseGrains :: Parser String
parseGrains = do
  name <- parseString
  if name `elem` ["Rice", "Bread", "Pasta"]
    then return name
    else throwE "No such grain allowed"

-- <Dairy> ::= <Milk> | <Cheese> | <Yogurt>
parseDairy :: Parser String
parseDairy = do
  name <- parseString
  if name `elem` ["Milk", "Cheese", "Yogurt"]
    then return name
    else throwE "No such dairy allowed"

-- <Meats> ::= <Chicken> | <Beef> | <Fish>
parseMeat :: Parser String
parseMeat = do
  name <- parseString
  if name `elem` ["Chicken", "Beef", "Fish"]
    then return name
    else throwE "No such meat allowed"

-- <Beverages> ::= <Soda> | <Juice> | <Water>
parseSpecificBeverage :: Parser String
parseSpecificBeverage = do
  name <- parseString
  if name `elem` ["Soda", "Juice", "Water"]
    then return name
    else throwE "No such beverage allowed"

-- <CleaningProducts> ::= <Detergent> | <Soap>
parseCleaningProducts :: Parser String
parseCleaningProducts = do
  name <- parseString
  if name `elem` ["Detergent", "Soap"]
    then return name
    else throwE "No such cleaning product allowed"

-- <PaperGoods> ::= <PaperTowels> | <ToiletPaper>
parsePaperGoods :: Parser String
parsePaperGoods = do
  name <- parseString
  if name `elem` ["PaperTowels", "ToiletPaper"]
    then return name
    else throwE "No such paper goods allowed"

parseBeverage :: Parser Item
parseBeverage = do
  _ <- parseLiteral "Beverage"
  name <- parseSpecificBeverage
  qty <- parseAddInt
  return (Beverage name qty)

parseHousehold :: Parser Item
parseHousehold = do
  cat <- parseHouseholdCategory
  name <- or2' parseCleaningProducts parsePaperGoods
  qty <- parseAddInt
  return (HouseholdSupplies cat name qty)



parseString :: Parser String
parseString = do
  input <- lift S.get
  let input' = strip input
  case input' of
    ('"' : xs) -> parseQuotedString xs
    _ -> do
      let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
      lift (S.put rest)
      return str
  where
    parseQuotedString [] = throwE "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = lift (S.put rest) >> return ""
    parseQuotedString (x : rest) = do
      str <- parseQuotedString rest
      return (x : str)


parseAddInt :: Parser Int
parseAddInt = do
  input <- lift S.get
  let strippedInput = strip input
  let baseValue = 0
  let (digits, rest) = span isDigit strippedInput
  if null digits
    then return baseValue
    else do
      lift (S.put rest)
      return (read digits)

parseInt :: Parser Int
parseInt = do
  input <- lift S.get
  let strippedInput = strip input
  let (digits, rest) = span isDigit strippedInput
  if null digits
    then throwE "Expected an integer"
    else do
      lift (S.put rest)
      return (read digits)

-- <Item> ::= <FoodItems> | <Beverages> | <HouseholdSupplies>
parseItem :: Parser Item
parseItem =
  or3'
    (and3' (\cat name number -> Food cat name number) parseFoodCategory (or5' parseFruits parseVegetables parseGrains parseDairy parseMeat) parseAddInt) 
    parseBeverage
    parseHousehold

parseStorage :: Parser Storage
parseStorage = do
  _ <- parseLiteral "Storage"
  items <- parseMultipleItems
  return (Storage items)

-- <Storage> ::= <Item> | <Storage> <Item>
parseMultipleItems :: Parser [Item]
parseMultipleItems = do
  items <- many parseItem
  return items
  where
    many p = ((:) <$> p <*> many p) <|> return []


parseView :: Parser Query
parseView = do
 _ <- parseLiteral "show_store"
 return ShowInventory


parseRestockInventory :: [ItemWithId] -> String -> Int -> [ItemWithId]
parseRestockInventory [] _ _ = []
parseRestockInventory (x:xs) search number =
  if searchMatches x search
    then restockItem x number : parseRestockInventory xs search number
    else x : parseRestockInventory xs search number

parseSellInventory :: [ItemWithId] -> String -> Int -> Either String [ItemWithId]
parseSellInventory [] _ _ = Right []
parseSellInventory (x:xs) search number = 
  if searchMatches x search
    then case sellItem x number of
      Right updatedItem -> Right (updatedItem : xs)
      Left err -> Left err
    else case parseSellInventory xs search number of
      Left err -> Left err
      Right updatedRest -> Right (x : updatedRest)


parseRestock :: Parser Query
parseRestock = 
  and3' (\_ name number -> RestockItems name number) (parseLiteral "restock") parseString parseInt


parseSellItem :: Parser Query
parseSellItem = 
  and3' (\_ name number -> SellItem name number) (parseLiteral "sell") parseString parseInt

parseRemove :: Parser Query
parseRemove  =
  and2' (\_ itemName -> RemoveItem itemName) (parseLiteral "remove_item") parseString



parseAddStorage :: Parser Query
parseAddStorage = and2' (\_ storage -> AddStorage storage) (parseLiteral "add_storage") parseStorage


searchMatches :: ItemWithId -> String -> Bool
searchMatches (ItemWithId _ item) search = itemNameMatches item search

itemNameMatches :: Item -> String -> Bool
itemNameMatches (Food _ name _) search = name == search
itemNameMatches (Beverage name _) search = name == search
itemNameMatches (HouseholdSupplies _ name _) search = name == search

restockItem :: ItemWithId -> Int -> ItemWithId
restockItem (ItemWithId id (Food cat name qty)) quantity =
    ItemWithId id (Food cat name (qty + quantity))
restockItem (ItemWithId id (Beverage name qty)) quantity =
    ItemWithId id (Beverage name (qty + quantity))
restockItem (ItemWithId id (HouseholdSupplies cat name qty)) quantity =
    ItemWithId id (HouseholdSupplies cat name (qty + quantity))



sellItem :: ItemWithId -> Int -> Either String ItemWithId
sellItem (ItemWithId id (Food cat name qty)) quantityToSell =
    let newQty = qty - quantityToSell
    in if newQty >= 0 
        then Right (ItemWithId id (Food cat name newQty)) 
        else Left ("Insufficient stock for item: " ++ name)
sellItem (ItemWithId id (Beverage name qty)) quantityToSell =
    let newQty = qty - quantityToSell
    in if newQty >= 0 
        then Right (ItemWithId id (Beverage name newQty)) 
        else Left ("Insufficient stock for item: " ++ name)
sellItem (ItemWithId id (HouseholdSupplies cat name qty)) quantityToSell =
    let newQty = qty - quantityToSell
    in if newQty >= 0 
        then Right (ItemWithId id (HouseholdSupplies cat name newQty)) 
        else Left ("Insufficient stock for item: " ++ name)


parseQuery :: Parser Query
parseQuery =
    or5' parseView parseAddStorage parseSellItem parseRestock parseRemove


stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  AddStorage (Storage items) ->
    let
      itemsAreEquivalent :: Item -> Item -> Bool
      itemsAreEquivalent (Food cat1 name1 _) (Food cat2 name2 _) = cat1 == cat2 && name1 == name2
      itemsAreEquivalent (Beverage name1 _) (Beverage name2 _) = name1 == name2
      itemsAreEquivalent (HouseholdSupplies _ name1 _) (HouseholdSupplies _ name2 _) = name1 == name2
      itemsAreEquivalent _ _ = False

      itemExists :: Item -> [ItemWithId] -> Bool
      itemExists _ [] = False
      itemExists itemToCheck (ItemWithId _ invItem : rest) =
        itemsAreEquivalent itemToCheck invItem || itemExists itemToCheck rest

      filterNewItems :: [Item] -> [ItemWithId] -> [Item]
      filterNewItems [] _ = []
      filterNewItems (item:rest) inventory =
        if itemExists item inventory
          then filterNewItems rest inventory
          else item : filterNewItems rest inventory

      assignIds :: [Item] -> Int -> [ItemWithId]
      assignIds [] _ = []
      assignIds (item:rest) currentId =
        ItemWithId currentId item : assignIds rest (currentId + 1)

      uniqueItems = filterNewItems items (inventory st)
      newItems = assignIds uniqueItems (nextId st)      
      newInventory = inventory st ++ newItems
      newState = st { inventory = newInventory, nextId = nextId st + length uniqueItems }
    in
      if null uniqueItems
      then Right (Just "No new items to add. All items already in inventory.", st)
      else Right (Just ("Added to inventory: " ++ show uniqueItems), newState)

  SellItem item' qty' -> 
    case parseSellInventory (inventory st) item' qty' of
      Right updatedInventory -> 
        let newState = st { inventory = updatedInventory }
        in Right (Just ("Sold item: " ++ item' ++ " with quantity " ++ show qty'), newState)
      Left err -> Right (Just err, st)

  RestockItems item' number' ->
    let updatedInventory = parseRestockInventory (inventory st) item' number'
        newState = st { inventory = updatedInventory }
    in if updatedInventory == inventory st
       then Right (Just ("Item not found: " ++ item'), st)
       else Right (Just ("Restocked item: " ++ item' ++ " with quantity " ++ show number'), newState)

  RemoveItem itemName  ->
    let
      newInventory = filter (\(ItemWithId _ item) -> not (itemNameMatches item itemName)) (inventory st)
    in
      if length newInventory == length (inventory st)
      then Right (Just ("Item not found: " ++ itemName), st)
      else
        let newState = st { inventory = newInventory }
        in Right (Just ("Removed item with name: " ++ itemName), newState)
  ShowInventory ->
    let inventoryStr = showInventory (inventory st)
    in Right (Just ("Inventory list:\n" ++ inventoryStr ++ "\n"), st)

showInventory :: [ItemWithId] -> String
showInventory [] = "No more items in inventory."
showInventory (x:xs) = "Item ID: " ++ show (itemId x) ++ ", Item: " ++ show (item x) ++ "\n" ++ showInventory xs