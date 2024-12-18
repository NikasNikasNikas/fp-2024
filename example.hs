{-# OPTIONS_GHC -Wno-name-shadowing #-}
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
  ) where
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.State as S
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
import Data.Char (isDigit)

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

many :: Parser a -> Parser [a]
many p =
  ( do
      x <- p
      xs <- many p
      return (x : xs)
  )
    `catchE` \_ -> return []

parse :: Parser a -> String -> (Either String a, String)
parse parser input = 
  let cleanInput = strip input
  in S.runState (runExceptT parser) cleanInput

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
  inputBefore <- lift get
  a `catchE` \e1 -> do
    lift (put inputBefore)
    b `catchE` \e2 -> do
      lift (put inputBefore)
      throwE (e1 ++ "\n" ++ e2)

or3' :: Parser a -> Parser a -> Parser a -> Parser a
or3' a b c = do
  inputBefore <- lift get
  a `catchE` \e1 -> do
    lift (put inputBefore)
    b `catchE` \e2 -> do
      lift (put inputBefore)
      c `catchE` \e3 -> do
        lift (put inputBefore)
        throwE (e1 ++ "\n" ++ e2 ++ "\n" ++ e3)

or5' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or5' a b c d e = do
  or3' a b c `catchE` \e1 -> do
    resultB <- or2' d e
    return resultB `catchE` \_ -> throwE e1

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

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
  input <- lift get
  case input of
    [] -> throwE ("Cannot find " ++ [c] ++ " in an empty input")
    s@(h : t) -> if c == h then lift $ put t >> return h else throwE (c : " is not found in " ++ s ++ "\n")

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  input <- lift get
  let strippedInput = strip input
  lift $ put strippedInput
  _ <- parseChar x
  rest <- parseLiteral xs
  return (x : rest)

-- <FoodItems> ::= <Fruits> | <Vegetables> | <Grains> | <Dairy> | <Meats>
parseFoodCategory :: Parser Category
parseFoodCategory input =
  case parseLiteral "Fruits" input of
    Right (_, rest) -> Right (Fruits, rest)
    Left _ -> case parseLiteral "Vegetables" input of
      Right (_, rest) -> Right (Vegetables, rest)
      Left _ -> case parseLiteral "Grains" input of
        Right (_, rest) -> Right (Grains, rest)
        Left _ -> case parseLiteral "Dairy" input of
          Right (_, rest) -> Right (Dairy, rest)
          Left _ -> case parseLiteral "Meats" input of
            Right (_, rest) -> Right (Meats, rest)
            Left _ -> Left "Failed to parse food category"

-- <HouseholdSupplies> ::= <CleaningProducts> | <PaperGoods>
parseHouseholdCategory :: Parser Category
parseHouseholdCategory input =
    case parseLiteral "CleaningProducts" input of
      Right(_, rest) -> Right (CleaningProducts, rest)
      Left _ -> case parseLiteral "PaperGoods" input of
        Right (_, rest) -> Right (PaperGoods, rest)
        Left _ -> Left "Failed to parse household supplies category"

-- <Fruits> ::= <Apples> | <Bananas> | <Oranges>
parseFruits :: Parser String
parseFruits input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Apples" || name == "Bananas" || name == "Oranges"
       then Right (name, skipSpaces rest)
       else Left "No such fruit allowed"

-- <Vegetables> ::= <Carrots> | <Potatoes> | <Spinach>
parseVegetables :: Parser String
parseVegetables input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Carrots" || name == "Potatoes" || name == "Spinach"
       then Right (name, skipSpaces rest)
       else Left "No such vegetable allowed"

-- <Grains> ::= <Rice> | <Bread> | <Pasta>
parseGrains :: Parser String
parseGrains input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Rice" || name == "Bread" || name == "Pasta"
       then Right (name, skipSpaces rest)
       else Left "No such grain allowed"

-- <Dairy> ::= <Milk> | <Cheese> | <Yogurt>
parseDairy :: Parser String
parseDairy input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Milk" || name == "Cheese" || name == "Yogurt"
       then Right (name, skipSpaces rest)
       else Left "No such dairy allowed"

-- <Meats> ::= <Chicken> | <Beef> | <Fish>
parseMeat :: Parser String
parseMeat input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Chicken" || name == "Beef" || name == "Fish"
       then Right (name, skipSpaces rest)
       else Left "No such meat allowed"

-- <Beverages> ::= <Soda> | <Juice> | <Water>
parseSpecificBeverage :: Parser String
parseSpecificBeverage input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Soda" || name == "Juice" || name == "Water"
       then Right (name, skipSpaces rest)
       else Left "No such fruit allowed"

-- <CleaningProducts> ::= <Detergent> | <Soap>
parseCleaningProducts :: Parser String
parseCleaningProducts input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Detergent" || name == "Soap"
       then Right (name, skipSpaces rest)
       else Left "No such cleaning product allowed"

-- <PaperGoods> ::= <PaperTowels> | <ToiletPaper>
parsePaperGoods :: Parser String
parsePaperGoods input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "PaperTowels" || name == "ToiletPaper"
       then Right (name, skipSpaces rest)
       else Left "No such paper goods allowed"

parseBeverage :: Parser Item
parseBeverage input =
  and3' (\_ name number -> Beverage name number) (parseLiteral "Beverage") parseSpecificBeverage parseAddInt input

parseHousehold :: Parser Item
parseHousehold input =
  and3' (\cat name number -> HouseholdSupplies cat name number) parseHouseholdCategory (or2' parseCleaningProducts parsePaperGoods) parseAddInt input

-- <Item> ::= <FoodItems> | <Beverages> | <HouseholdSupplies>
parseItem :: Parser Item
parseItem =
  or3'
    (and3' (\cat name number -> Food cat name number) parseFoodCategory (or5' parseFruits parseVegetables parseGrains parseDairy parseMeat) parseAddInt) 
    parseBeverage
    parseHousehold


parseString :: Parser String
parseString = do
  input <- lift get
  let input' = strip input
  case input' of
    ('"' : xs) -> parseQuotedString xs
    _ -> do
      let (str, rest) = span (\c -> not (isSpace c)) input'
      if null str
        then throwE "Failed to parse string"
        else do
          lift (put rest)
          return str
  where
    parseQuotedString [] = throwE "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = do
      lift (put rest)
      return ""
    parseQuotedString (x : rest) = do
      str <- parseQuotedString rest
      return (x : str)

parseAddInt :: Parser Int
parseAddInt input =
    let (digits, rest) = span isDigit (skipSpaces input)
        baseValue = 0
    in if null digits
        then Right(baseValue, rest)
        else Right(read digits, rest)

parseStorage :: Parser Storage
parseStorage input = 
      case parseMultipleItems input of
        Right (items, rest') -> Right (Storage items, rest')
        Left _-> Left  "Likely case of mistyping"

-- <Storage> ::= <Item> | <Storage> <Item>
parseMultipleItems :: Parser [Item]
parseMultipleItems input =
  case parseItem input of
    Right (item, rest) ->
      case parseMultipleItems rest of
        Right (items, rest') -> Right (item : items, rest')
        Left _ -> Right ([item], rest)
    Left _ -> Left "Failed to parse items in storage"


parseAddStorage :: Parser Query
parseAddStorage = and2' (\_ storage -> AddStorage storage) (parseLiteral "add_storage") parseStorage

parseInt :: Parser Int
parseInt input =
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected an integer that is positive"
        else Right (read digits, rest)

parseView :: Parser Query
parseView = do
  _ <- parseLiteral "show_store" (skipSpaces input)
  return ShowFacility


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


parseRestock :: Parser Query
parseRestock input = 
  and3' (\_ name number -> RestockItems name number) (parseLiteral "restock") parseString parseInt input


parseSellItem :: Parser Query
parseSellItem input = 
  and3' (\_ name number -> SellItem name number) (parseLiteral "sell") parseString parseInt input

parseRemove :: Parser Query
parseRemove input =
  and2' (\_ itemName -> RemoveItem itemName) (parseLiteral "remove_item") parseString input
parseQuery :: String -> Either String (Query, String)
parseQuery input = 
  let cleanInput = strip input
  in case or5' parseView parseAddStorage parseSellItem parseRestock parseRemove cleanInput of
       Right (query, rest) -> Right (query, strip rest)
       Left err -> Left $ "Failed to parse query: " ++ err


stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  AddStorage (Storage items) ->
    let 
      -- More robust item equivalence check
      itemsAreEquivalent :: Item -> Item -> Bool
      itemsAreEquivalent (Food cat1 name1 _) (Food cat2 name2 _) = cat1 == cat2 && name1 == name2
      itemsAreEquivalent (Beverage name1 _) (Beverage name2 _) = name1 == name2
      itemsAreEquivalent (HouseholdSupplies cat1 name1 _) (HouseholdSupplies cat2 name2 _) = 
        cat1 == cat2 && name1 == name2
      itemsAreEquivalent _ _ = False

      -- Find unique items, preventing duplicates
      uniqueItems = nub items

      -- More intelligent item addition
      addItemWithCheck :: [ItemWithId] -> Item -> Int -> ([ItemWithId], Int)
      addItemWithCheck inventory item currentId =
        if any (\(ItemWithId _ existingItem) -> itemsAreEquivalent item existingItem) inventory
          then (inventory, currentId)
          else (inventory ++ [ItemWithId currentId item], currentId + 1)

      (newInventory, newNextId) = 
        foldl (\(inv, nextId') item -> addItemWithCheck inv item nextId') 
              (inventory st, nextId st) 
              uniqueItems
      
      newState = st { inventory = newInventory, nextId = newNextId }
    in
      if null uniqueItems
      then Right (Just "No new items to add.", st)
      else Right (Just ("Added to inventory: " ++ show uniqueItems), newState)

  SellItem itemName qty' -> 
    case parseSellInventory (inventory st) itemName qty' of
      Right updatedInventory -> 
        let newState = st { inventory = updatedInventory }
        in Right (Just ("Sold " ++ show qty' ++ " of " ++ itemName), newState)
      Left err -> Right (Just err, st)

  RestockItems itemName number' ->
    let updatedInventory = parseRestockInventory (inventory st) itemName number'
        newState = st { inventory = updatedInventory }
    in if updatedInventory == inventory st
       then Right (Just ("Item not found: " ++ itemName), st)
       else Right (Just ("Restocked " ++ show number' ++ " of " ++ itemName), newState)

  RemoveItem itemName  ->
    let 
      newInventory = filter (\(ItemWithId _ item) -> not (itemNameMatches item itemName)) (inventory st)
    in
      if length newInventory == length (inventory st)
      then Right (Just ("Item not found: " ++ itemName), st)
      else
        let newState = st { inventory = newInventory }
        in Right (Just ("Removed item: " ++ itemName), newState)

  ShowInventory ->
    let inventoryStr = showInventory (inventory st)
    in Right (Just inventoryStr, st)


showInventory :: [ItemWithId] -> String
showInventory [] = "No more items in inventory."
showInventory (x:xs) = "Item ID: " ++ show (itemId x) ++ ", Item: " ++ show (item x) ++ "\n" ++ showInventory xs