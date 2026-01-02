{-# LANGUAGE GADTs #-}

{- | This module provides a flexible and type-safe library for parsing, validating,
and constructing LTSV (Labeled Tab-Separated Values) labels.
-}
module Ltsv.Label (
    -- * Types
    LtsvError (..),
    InvalidCharReason (..),
    Alph (..),
    AlphNum (..),
    StrictLabel (..),
    Label (..),
    PermissiveLabel (..),

    -- * Parsers
    parseAlph,
    parseAlphNum,
    parseStrict,
    parse,
    parsePermissive,

    -- * Builders
    Delimiter (..),
    combine,

    -- * Promotions
    alphToAlphNum,
    alphNumToStrict,
    strictToLabel,
    labelToPermissive,
) where

import Data.List (find)
import Ltsv.Label.Internal
import Ltsv.Label.Types

-- | Converts an 'Alph' label to an 'AlphNum' label. This conversion is always safe.
alphToAlphNum :: Alph -> AlphNum
alphToAlphNum (Alph s) = AlphNum s

-- | Converts an 'AlphNum' label to a 'StrictLabel'. This conversion is always safe.
alphNumToStrict :: AlphNum -> StrictLabel
alphNumToStrict (AlphNum s) = StrictLabel s

-- | Converts a 'StrictLabel' to a 'Label'. This conversion is always safe.
strictToLabel :: StrictLabel -> Label
strictToLabel (StrictLabel s) = Label s

-- | Converts a 'Label' to a 'PermissiveLabel'. This conversion is always safe.
labelToPermissive :: Label -> PermissiveLabel
labelToPermissive (Label s) = PermissiveLabel s

-- | Combines two labels using a specified 'Delimiter'.
combine :: Delimiter from1 from2 to -> from1 -> from2 -> to
combine Underscore (AlphNum s1) (AlphNum s2) = StrictLabel (s1 ++ "_" ++ s2)
combine Hyphen (StrictLabel s1) (StrictLabel s2) = Label (s1 ++ "-" ++ s2)
combine Dot (StrictLabel s1) (StrictLabel s2) = Label (s1 ++ "." ++ s2)

-- | Parses a string into an 'Alph' label.
parseAlph :: String -> Either LtsvError Alph
parseAlph [] = Left EmptyLabel
parseAlph s =
    case find (not . isAlphChar) s of
        Just c -> Left (InvalidCharacter c s NotAllowedInAlph)
        Nothing -> Right (Alph s)

-- | Parses a string into an 'AlphNum' label.
parseAlphNum :: String -> Either LtsvError AlphNum
parseAlphNum [] = Left EmptyLabel
parseAlphNum s =
    case find (not . isAlphNumChar) s of
        Just c -> Left (InvalidCharacter c s NotAllowedInAlphNum)
        Nothing -> Right (AlphNum s)

-- | Parses a string into a 'StrictLabel'.
parseStrict :: String -> Either LtsvError StrictLabel
parseStrict [] = Left EmptyLabel
parseStrict s =
    case find (not . isStrictChar) s of
        Just c -> Left (InvalidCharacter c s NotAllowedInStrict)
        Nothing -> Right (StrictLabel s)

-- | Parses a string into a 'Label' (default strictness).
parse :: String -> Either LtsvError Label
parse [] = Left EmptyLabel
parse s =
    case find (not . isChar) s of
        Just c -> Left (InvalidCharacter c s NotAllowedInLabel)
        Nothing -> Right (Label s)

-- | Parses a string into a 'PermissiveLabel'.
parsePermissive :: String -> Either LtsvError PermissiveLabel
parsePermissive [] = Left EmptyLabel
parsePermissive s =
    case find (not . isPermissiveChar) s of
        Just c -> Left (InvalidCharacter c s NotAllowedInPermissive)
        Nothing -> Right (PermissiveLabel s)
