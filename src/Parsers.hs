module Parsers
(
    Parser,
    parse,
    and2',
    and3',
    or2',
    or3',
    or5',
    many,
    parseChar,
    parseLiteral,
    parseFoodCategory,
    parseHouseholdCategory,
    parseFruits,
    parseVegetables,
    parseGrains,
    parseDairy,
    parseMeat,
    parseSpecificBeverage,
    parseCleaningProducts,
    parsePaperGoods,
    parseBeverage,
    parseHousehold,
    parseString,
    parseAddInt,
    parseInt,
)
where

import Data.Char (isDigit)
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Except (ExceptT, runExceptT, catchE, throwE)
import Control.Monad.Trans.Class
import Control.Applicative hiding (many)

data Category = Fruits | Vegetables | Grains | Dairy | Meats | Beverages | CleaningProducts | PaperGoods
  deriving (Eq, Show)

data Query
  = AddStorage Storage
  | SellItem String Int
  | ShowInventory
  | RestockItems String Int
  | RemoveItem String
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

-- | Basic parsers
-- type Parser a = String -> Either String (a, String)
type Parser a = ExceptT String (S.State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = S.runState (runExceptT parser)

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