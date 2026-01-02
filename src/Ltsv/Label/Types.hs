{-# LANGUAGE GADTs #-}

{- | This module defines the core data types for the LTSV Label parsing library.
It includes the error type, various strictness levels for labels,
and the delimiter type for combining labels.
-}
module Ltsv.Label.Types (
    -- * Error Type
    LtsvError (..),
    InvalidCharReason (..),

    -- * Label Types
    Alph (..),
    AlphNum (..),
    StrictLabel (..),
    Label (..),
    PermissiveLabel (..),

    -- * Delimiter Type
    Delimiter (..),
) where

-- | Represents the reason a character is considered invalid for a given label type.
data InvalidCharReason
    = NotAllowedInAlph
    | NotAllowedInAlphNum
    | NotAllowedInStrict
    | NotAllowedInLabel
    | NotAllowedInPermissive
    deriving (Eq, Show)

-- | Represents errors that can occur during label parsing.
data LtsvError
    = -- | The input string was empty.
      EmptyLabel
    | -- | An invalid character was found.
      InvalidCharacter Char String InvalidCharReason
    deriving (Eq, Show)

-- | A label consisting only of alphabetic characters (a-z, A-Z).
newtype Alph = Alph {getAlph :: String} deriving (Eq, Ord, Show)

-- | A label consisting only of alphanumeric characters (a-z, A-Z, 0-9).
newtype AlphNum = AlphNum {getAlphNum :: String} deriving (Eq, Ord, Show)

{- | A strict label allowing alphanumeric characters and underscore.
This corresponds to typical variable naming conventions.
-}
newtype StrictLabel = StrictLabel {getStrictLabel :: String} deriving (Eq, Ord, Show)

-- | A general purpose label allowing alphanumeric characters, underscore, hyphen, and dot.
newtype Label = Label {getLabel :: String} deriving (Eq, Ord, Show)

{- | A permissive label allowing any ASCII printable character except
tab, carriage return, newline, and colon.
-}
newtype PermissiveLabel = PermissiveLabel {getPermissiveLabel :: String} deriving (Eq, Ord, Show)

{- | A GADT that defines type-safe delimiters for combining labels.
Each constructor specifies the input label types and the resulting label type.
-}
data Delimiter from1 from2 to where
    -- | Combines two AlphNum labels with an underscore to form a StrictLabel.
    Underscore :: Delimiter AlphNum AlphNum StrictLabel
    -- | Combines two StrictLabels with a hyphen to form a Label.
    Hyphen :: Delimiter StrictLabel StrictLabel Label
    -- | Combines two StrictLabels with a dot to form a Label.
    Dot :: Delimiter StrictLabel StrictLabel Label
