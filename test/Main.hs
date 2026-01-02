module Main (main) where

import Ltsv.Label
import System.Exit (exitFailure, exitSuccess)

-- Helper to make tests more readable
fromRight' :: (Show a) => Either a b -> b
fromRight' (Right b) = b
fromRight' (Left a) = error $ "Test setup failed: expected Right, got Left: " ++ show a

main :: IO ()
main = do
    testPromotions
    testCombine
    testAlphParsing
    testAlphNumParsing
    testStrictParsing
    testDefaultParsing
    testPermissiveParsing
    exitSuccess

testPromotions :: IO ()
testPromotions = do
    putStrLn "--- Testing Promotions ---"
    let alph = fromRight' (parseAlph "abc")
    let alphNum = alphToAlphNum alph
    let strict = alphNumToStrict alphNum
    let label = strictToLabel strict
    let permissive = labelToPermissive label
    if getPermissiveLabel permissive == "abc"
        then putStrLn "[promotion] Test passed"
        else exitFailure

testCombine :: IO ()
testCombine = do
    putStrLn "--- Testing Combine ---"
    let an1 = fromRight' (parseAlphNum "http")
    let an2 = fromRight' (parseAlphNum "status")
    let sl1 = combine Underscore an1 an2
    if getStrictLabel sl1 == "http_status"
        then putStrLn "[combine] Test passed for Underscore"
        else exitFailure

    let sl2 = fromRight' (parseStrict "ab_c")
    let l1 = combine Hyphen sl1 sl2
    if getLabel l1 == "http_status-ab_c"
        then putStrLn "[combine] Test passed for Hyphen"
        else exitFailure

    let l2 = combine Dot sl1 sl2
    if getLabel l2 == "http_status.ab_c"
        then putStrLn "[combine] Test passed for Dot"
        else exitFailure

testAlphParsing :: IO ()
testAlphParsing = do
    putStrLn "--- Testing Alph Parsing ---"
    testParse "alph" parseAlph "abc" (Right (Alph "abc"))
    testParse "alph" parseAlph "ab1" (Left (InvalidCharacter '1' "ab1" NotAllowedInAlph))
    testParse "alph" parseAlph "" (Left EmptyLabel)

testAlphNumParsing :: IO ()
testAlphNumParsing = do
    putStrLn "--- Testing AlphNum Parsing ---"
    testParse "alphnum" parseAlphNum "ab1" (Right (AlphNum "ab1"))
    testParse "alphnum" parseAlphNum "ab_" (Left (InvalidCharacter '_' "ab_" NotAllowedInAlphNum))
    testParse "alphnum" parseAlphNum "" (Left EmptyLabel)

testStrictParsing :: IO ()
testStrictParsing = do
    putStrLn "--- Testing Strict Parsing ---"
    testParse "strict" parseStrict "ab_" (Right (StrictLabel "ab_"))
    testParse "strict" parseStrict "ab-" (Left (InvalidCharacter '-' "ab-" NotAllowedInStrict))
    testParse "strict" parseStrict "" (Left EmptyLabel)

testDefaultParsing :: IO ()
testDefaultParsing = do
    putStrLn "--- Testing Default Parsing ---"
    testParse "default" parse "ab-" (Right (Label "ab-"))
    testParse "default" parse "ab$" (Left (InvalidCharacter '$' "ab$" NotAllowedInLabel))
    testParse "default" parse "" (Left EmptyLabel)

testPermissiveParsing :: IO ()
testPermissiveParsing = do
    putStrLn "--- Testing Permissive Parsing ---"
    testParse "permissive" parsePermissive "ab$" (Right (PermissiveLabel "ab$"))
    testParse "permissive" parsePermissive "ab:" (Left (InvalidCharacter ':' "ab:" NotAllowedInPermissive))
    testParse "permissive" parsePermissive "" (Left EmptyLabel)

testParse :: (Eq a, Show a) => String -> (String -> Either LtsvError a) -> String -> Either LtsvError a -> IO ()
testParse testGroupName parseFunc input expected =
    let result = parseFunc input
     in if result == expected
            then putStrLn $ "[" ++ testGroupName ++ "] Test passed for input: " ++ input
            else do
                putStrLn $ "[" ++ testGroupName ++ "] Test FAILED for input: " ++ input
                putStrLn $ "  Expected: " ++ show expected
                putStrLn $ "  Got: " ++ show result
                exitFailure
