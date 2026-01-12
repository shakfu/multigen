{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      : MultiGenRuntime
Description : MultiGen Haskell runtime providing Python-like operations using only Haskell standard library
Copyright   : (c) MultiGen Project, 2024
License     : MIT
Maintainer  : multigen-dev@example.com

This module provides Python-like operations and data structures using only
Haskell's standard library and necessary language extensions. It supports
string operations, comprehensions, built-in functions, and container operations
that mirror Python's behavior while leveraging Haskell's type safety and
functional programming paradigms.
-}

module MultiGenRuntime
    ( -- * String Operations
      StrOps(..)
    , upper, lower, strip, find, replace, split
      -- * Built-in Functions
    , Builtins(..)
    , abs', bool', Len(..), min', max', sum'
      -- * Range and Iteration
    , Range(..)
    , range, range2, range3, rangeList
      -- * Comprehensions
    , Comprehensions(..)
    , listComprehension, listComprehensionWithFilter
    , dictComprehension, dictComprehensionWithFilter
    , setComprehension, setComprehensionWithFilter
      -- * Utility Functions
    , toString, printValue
      -- * Container Types
    , Dict, Set
      -- * Dictionary Operations
    , items, values, keys
    ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)


-- | Type synonym for Python-like dictionaries
type Dict k v = Map k v

-- | String operations module providing Python-like string methods
data StrOps = StrOps

-- | Convert string to uppercase (Python's str.upper())
upper :: String -> String
upper = map Char.toUpper

-- | Convert string to lowercase (Python's str.lower())
lower :: String -> String
lower = map Char.toLower

-- | Strip whitespace from both ends (Python's str.strip())
strip :: String -> String
strip = reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace

-- | Find substring index (Python's str.find()) - returns -1 if not found
find :: String -> String -> Int
find haystack needle =
    case List.findIndex (List.isPrefixOf needle) (List.tails haystack) of
        Just i -> i
        Nothing -> -1

-- | Replace all occurrences (Python's str.replace())
replace :: String -> String -> String -> String
replace [] _ replacement = replacement
replace original@(x:xs) pattern replacement
    | List.isPrefixOf pattern original =
        replacement ++ replace (drop (length pattern) original) pattern replacement
    | otherwise = x : replace xs pattern replacement

-- | Split string by delimiter (Python's str.split())
split :: String -> String -> [String]
split [] _ = [""]
split str delimiter =
    case List.findIndex (List.isPrefixOf delimiter) (List.tails str) of
        Nothing -> [str]
        Just i ->
            let (before, after) = splitAt i str
                remaining = drop (length delimiter) after
            in before : split remaining delimiter


-- | Built-in functions module providing Python-like built-ins
data Builtins = Builtins

-- | Absolute value (Python's abs())
abs' :: (Num a, Ord a) => a -> a
abs' x = if x < 0 then -x else x

-- | Boolean conversion (Python's bool())
bool' :: (Eq a, Num a) => a -> Bool
bool' x = x /= 0

-- | Length function (Python's len()) - works for lists, maps, and sets
class Len a where
    len' :: a -> Int

instance Len [a] where
    len' = length

instance Len (Map k v) where
    len' = Map.size

instance Len (Set a) where
    len' = Set.size

-- | Minimum function (Python's min())
min' :: (Ord a) => [a] -> a
min' [] = error "min() arg is an empty sequence"
min' xs = minimum xs

-- | Maximum function (Python's max())
max' :: (Ord a) => [a] -> a
max' [] = error "max() arg is an empty sequence"
max' xs = maximum xs

-- | Sum function (Python's sum())
sum' :: (Num a) => [a] -> a
sum' = sum


-- | Range type for Python-like range iteration
data Range = Range Int Int Int  -- start, stop, step

-- | Create range with just stop (start=0, step=1)
range :: Int -> Range
range stop = Range 0 stop 1

-- | Create range with start and stop (step=1)
range2 :: Int -> Int -> Range
range2 start stop = Range start stop 1

-- | Create range with start, stop, and step
range3 :: Int -> Int -> Int -> Range
range3 = Range

-- | Convert range to list for iteration
rangeList :: Range -> [Int]
rangeList (Range start stop step)
    | step > 0 && start < stop = start : rangeList (Range (start + step) stop step)
    | step < 0 && start > stop = start : rangeList (Range (start + step) stop step)
    | otherwise = []


-- | Comprehensions module providing Python-like comprehensions
data Comprehensions = Comprehensions

-- | List comprehension (Python's [expr for item in iterable])
listComprehension :: [a] -> (a -> b) -> [b]
listComprehension iterable transform = map transform iterable

-- | List comprehension with filter (Python's [expr for item in iterable if condition])
listComprehensionWithFilter :: [a] -> (a -> Bool) -> (a -> b) -> [b]
listComprehensionWithFilter iterable predicate transform =
    map transform (filter predicate iterable)

-- | Dictionary comprehension (Python's {key_expr: value_expr for item in iterable})
dictComprehension :: (Ord k) => [a] -> (a -> k) -> (a -> v) -> Dict k v
dictComprehension iterable keyFunc valueFunc =
    Map.fromList [(keyFunc item, valueFunc item) | item <- iterable]

-- | Dictionary comprehension with filter
dictComprehensionWithFilter :: (Ord k) => [a] -> (a -> Bool) -> (a -> k) -> (a -> v) -> Dict k v
dictComprehensionWithFilter iterable predicate keyFunc valueFunc =
    Map.fromList [(keyFunc item, valueFunc item) | item <- iterable, predicate item]

-- | Helper type class for converting to list (for comprehensions)
class ToListCompat t where
    toListCompat :: t a -> [a]

instance ToListCompat [] where
    toListCompat = id

instance ToListCompat Set where
    toListCompat = Set.toList

-- | Set comprehension (Python's {expr for item in iterable})
setComprehension :: (ToListCompat t, Ord b) => t a -> (a -> b) -> Set b
setComprehension iterable transform = Set.fromList (map transform (toListCompat iterable))

-- | Set comprehension with filter
setComprehensionWithFilter :: (ToListCompat t, Ord b) => t a -> (a -> Bool) -> (a -> b) -> Set b
setComprehensionWithFilter iterable predicate transform =
    Set.fromList [transform item | item <- toListCompat iterable, predicate item]


-- | Utility function for converting values to strings (Python's str())
class ToString a where
    toString :: a -> String

instance ToString Int where
    toString = show

instance ToString Integer where
    toString = show

instance ToString Double where
    toString = show

instance ToString Float where
    toString = show

instance ToString Bool where
    toString True = "True"
    toString False = "False"

instance {-# OVERLAPPING #-} ToString String where
    toString = id

instance (ToString a) => ToString [a] where
    toString xs = "[" ++ List.intercalate ", " (map toString xs) ++ "]"

instance (ToString k, ToString v) => ToString (Dict k v) where
    toString dict = "{" ++ List.intercalate ", "
        [toString k ++ ": " ++ toString v | (k, v) <- Map.toList dict] ++ "}"

instance (ToString a) => ToString (Set a) where
    toString set = "{" ++ List.intercalate ", " (map toString (Set.toList set)) ++ "}"


-- | Print function (Python's print())
printValue :: (ToString a) => a -> IO ()
printValue x = putStrLn (toString x)


-- | Dictionary Operations

-- | Get key-value pairs from dictionary (Python's dict.items())
items :: Dict k v -> [(k, v)]
items = Map.toList

-- | Get values from dictionary (Python's dict.values())
values :: Dict k v -> [v]
values = Map.elems

-- | Get keys from dictionary (Python's dict.keys())
keys :: Dict k v -> [k]
keys = Map.keys