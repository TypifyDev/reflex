{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.Map.Monoidal as Map
import Data.MonoidMap (MonoidMap (..), monoidMap)
import Data.Set (Set)
import qualified Data.Set as Set
import Reflex.Query.Class (Query (..))

newtype Selection = Selection (Set Int)

instance Query Selection where
  type QueryResult Selection = Set Int
  crop (Selection selected) = Set.intersection selected

query :: MonoidMap Int Selection
query = MonoidMap $ Map.fromList
  [ (1, Selection $ Set.fromList [10, 11])
  , (2, Selection $ Set.singleton 20)
  ]

results :: MonoidMap Int (Set Int)
results = monoidMap $ Map.fromList
  [ (1, Set.fromList [11, 12])
  , (2, Set.singleton 21)
  , (3, Set.singleton 30)
  ]

main :: IO ()
main = do
  let expected = monoidMap $ Map.singleton 1 $ Set.singleton 11
      cropped = crop query results
  assertEqual "crop filters both levels and prunes empty results" expected cropped
  assertEqual "crop is idempotent" cropped (crop query cropped)
  assertEqual "empty query selects no results" mempty $
    crop (MonoidMap Map.empty :: MonoidMap Int Selection) results

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (actual == expected) $
    ioError $ userError $ label <> ": expected " <> show expected <> ", got " <> show actual
