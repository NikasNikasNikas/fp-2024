{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC

import Lib2 qualified
import Lib3 qualified
import Control.Concurrent
import Data.Maybe

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

-- Unit Tests
unitTests :: TestTree
unitTests = testGroup
  "Lib2 tests"
  [ 
    testCase "State Transition - AddStorage" $
      let initialState = Lib2.emptyState
          query = Lib2.AddStorage (Lib2.Storage [Lib2.Food Lib2.Fruits "Apples" 10])
          expectedState = initialState { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 10)], Lib2.nextId = 2 }
      in Lib2.stateTransition initialState query @?= Right (Just "Added to inventory: [Food Fruits \"Apples\" 10]", expectedState),

    testCase "State Transition - SellItem" $
      let initialState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 10)], Lib2.nextId = 2 }
          query = Lib2.SellItem "Apples" 5
          expectedState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 5)], Lib2.nextId = 2 }
      in Lib2.stateTransition initialState query @?= Right (Just "Sold item: Apples with quantity 5", expectedState),

    testCase "State Transition - RestockItems" $
      let initialState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 5)], Lib2.nextId = 2 }
          query = Lib2.RestockItems "Apples" 10
          expectedState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 15)], Lib2.nextId = 2 }
      in Lib2.stateTransition initialState query @?= Right (Just "Restocked item: Apples with quantity 10", expectedState),

    testCase "State Transition - RemoveItem" $
      let initialState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 5)], Lib2.nextId = 2 }
          query = Lib2.RemoveItem "Apples"
          expectedState = Lib2.State { Lib2.inventory = [], Lib2.nextId = 2 }
      in Lib2.stateTransition initialState query @?= Right (Just "Removed item with name: Apples", expectedState),

    testCase "State Transition - ShowInventory" $
      let initialState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 5)], Lib2.nextId = 2 }
      in Lib2.stateTransition initialState Lib2.ShowInventory @?=
         Right (Just "Inventory list:\nItem ID: 1, Item: Food Fruits \"Apples\" 5\n", initialState)
  ]

-- Property Tests
propertyTests :: TestTree
propertyTests = testGroup
  "Lib3 property tests"
  [
    QC.testProperty "Save-then-load preserves state" prop_saveThenLoad,
    QC.testProperty "Saved queries reproduce original state" prop_savedQueriesReproduceState
  ]

-- Arbitrary Instances
instance Arbitrary Lib2.Query where
  arbitrary = oneof
    [ Lib2.AddStorage <$> arbitrary
    , Lib2.SellItem <$> validItemName <*> validQuantity
    , Lib2.RestockItems <$> validItemName <*> validQuantity
    , Lib2.RemoveItem <$> validItemName
    , pure Lib2.ShowInventory
    ]
    where 
      validItemName = elements ["Apples", "Bananas", "Oranges", "Carrots", "Potatoes", "Spinach", 
                                "Rice", "Bread", "Pasta", "Milk", "Cheese", "Yogurt", 
                                "Chicken", "Beef", "Fish", "Soda", "Juice", "Water", 
                                "Detergent", "Soap", "PaperTowels", "ToiletPaper"]
      validQuantity = choose (0, 100)

instance Arbitrary Lib2.Storage where
  arbitrary = Lib2.Storage <$> listOf1 arbitrary

instance Arbitrary Lib2.Category where
  arbitrary = elements [Lib2.Fruits, Lib2.Vegetables, Lib2.Grains, Lib2.Dairy, Lib2.Meats, 
                        Lib2.CleaningProducts, Lib2.PaperGoods]

instance Arbitrary Lib2.Item where
  arbitrary = oneof
    [ Lib2.Food <$> arbitrary <*> validFoodName <*> validQuantity
    , Lib2.Beverage <$> validBeverageName <*> validQuantity
    , Lib2.HouseholdSupplies <$> arbitrary <*> validHouseholdName <*> validQuantity
    ]
    where 
      validFoodName = elements ["Apples", "Bananas", "Oranges", "Carrots", "Potatoes", "Spinach", 
                                "Rice", "Bread", "Pasta", "Milk", "Cheese", "Yogurt", 
                                "Chicken", "Beef", "Fish"]
      validBeverageName = elements ["Soda", "Juice", "Water"]
      validHouseholdName = elements ["Detergent", "Soap", "PaperTowels", "ToiletPaper"]
      validQuantity = choose (0, 100)

instance Arbitrary Lib2.State where
    arbitrary = do
        queries <- listOf1 arbitrary
        let (_, finalState) = applyQueries Lib2.emptyState queries
        return finalState
      where
        applyQueries :: Lib2.State -> [Lib2.Query] -> (String, Lib2.State)
        applyQueries state [] = ("", state)
        applyQueries state (q:qs) =
            case Lib2.stateTransition state q of
                Right (_, newState) -> applyQueries newState qs
                Left _ -> ("", state)

-- Property Implementations
prop_saveThenLoad :: Lib2.State -> Property
prop_saveThenLoad originalState = ioProperty $ do
    chan <- newChan
    let serializedState = Lib3.renderStatements (Lib3.marshallState originalState)
    saveReply <- newChan
    writeChan chan (Lib3.Save serializedState saveReply)
    _ <- readChan saveReply

    loadReply <- newChan
    writeChan chan (Lib3.Load loadReply)
    loadedState <- readChan loadReply

    pure $ serializedState == loadedState

prop_savedQueriesReproduceState :: Lib2.State -> Property
prop_savedQueriesReproduceState originalState =
  let serialized = Lib3.renderStatements (Lib3.marshallState originalState)
      (parsed, _) = Lib2.parse Lib3.parseStatements serialized
  in case parsed of
       Right statements -> 
         let reconstructedQueries = Lib3.getQueries statements
             originalQueries = Lib3.marshallState originalState
         in reconstructedQueries === originalQueries
       Left err -> counterexample ("Parsing failed: " ++ err) False