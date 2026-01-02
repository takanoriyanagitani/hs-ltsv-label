module Ltsv.Label.Internal where

import Data.Char (isAscii)
import qualified Data.Set as Set

{- | Internal character sets used for validating labels.
These sets are constructed compositionally to reflect strictness levels.
-}

-- | Set of lowercase alphabetic characters (a-z).
alphLowerSet :: Set.Set Char
alphLowerSet = Set.fromList ['a' .. 'z']

-- | Set of uppercase alphabetic characters (A-Z).
alphUpperSet :: Set.Set Char
alphUpperSet = Set.fromList ['A' .. 'Z']

-- | Set of digit characters (0-9).
digitSet :: Set.Set Char
digitSet = Set.fromList ['0' .. '9']

-- | Set of all alphabetic characters (a-z, A-Z).
alphSet :: Set.Set Char
alphSet = alphLowerSet `Set.union` alphUpperSet

-- | Set of all alphanumeric characters (a-z, A-Z, 0-9).
alphNumSet :: Set.Set Char
alphNumSet = alphSet `Set.union` digitSet

-- | Set of characters allowed in StrictLabels (alphanumeric and underscore).
strictCharSet :: Set.Set Char
strictCharSet = alphNumSet `Set.union` Set.singleton '_'

-- | Set of characters allowed in Labels (alphanumeric, underscore, dot, and hyphen).
labelCharSet :: Set.Set Char
labelCharSet = strictCharSet `Set.union` Set.fromList ".-"

-- | Set of characters explicitly forbidden in PermissiveLabels (tab, CR, LF, colon).
forbiddenSet :: Set.Set Char
forbiddenSet = Set.fromList ['\t', '\r', '\n', ':']

-- | Predicate functions to check if a character belongs to a specific set.

-- | Checks if a character is an alphabetic character.
isAlphChar :: Char -> Bool
isAlphChar c = c `Set.member` alphSet

-- | Checks if a character is an alphanumeric character.
isAlphNumChar :: Char -> Bool
isAlphNumChar c = c `Set.member` alphNumSet

-- | Checks if a character is allowed in a StrictLabel.
isStrictChar :: Char -> Bool
isStrictChar c = c `Set.member` strictCharSet

-- | Checks if a character is allowed in a Label.
isChar :: Char -> Bool
isChar c = c `Set.member` labelCharSet

-- | Checks if a character is one of the explicitly forbidden characters.
isForbiddenChar :: Char -> Bool
isForbiddenChar c = c `Set.member` forbiddenSet

{- | Checks if a character is allowed in a PermissiveLabel.
A permissive character must be ASCII and not in the forbiddenSet.
-}
isPermissiveChar :: Char -> Bool
isPermissiveChar c = isAscii c && not (isForbiddenChar c)
