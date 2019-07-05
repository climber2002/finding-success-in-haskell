module Main where

import Data.Char

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  case (passwordLength > 20 || passwordLength < 10 ) of
    True -> Left "Your password cannot be longer \
                  \than 20 or less than 10 characters"
    False -> Right password
  where passwordLength = length password

-- Exercise 13
requireAlphaNum :: String -> Either String String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Left "All charachers must be alphabet or number"
    True -> Right xs

cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left "Cannot be empty string"
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Right (x : xs)

-- Excercise 8
-- validatePassword' :: String -> Maybe String
-- validatePassword' password =
--   case (cleanWhitespace password) of
--     Nothing -> Nothing
--     Just password ->
--       case (checkPasswordLength password) of
--         Nothing -> Nothing
--         Just password ->
--           case (requireAlphaNum password) of
--             Nothing -> Nothing
--             Just password -> Just password

main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (validatePassword password)

-- Exercise 9 returns Just ""

validatePassword :: String -> Either String String
validatePassword password =
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength

-- Exercise 10
reverseLine :: IO ()
reverseLine = getLine >>= (print . reverse)

reverseLine' :: IO ()
reverseLine' = do
  line <- getLine
  print (reverse line)

-- Exercise 11
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma f = 
  case ma of
    Nothing -> Nothing
    Just a -> f a

-- Exercise 12
data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue :: StringOrValue a
                  -> (a -> StringOrValue b)
                  -> StringOrValue b
bindStringOrValue sorv f =
  case sorv of
    Str s -> Str s
    Val a -> f a


-- Exercise 15
printTestResult :: Either String () -> IO ()
printTestResult r = 
  case r of
    Left err -> putStrLn err
    Right () -> putStrLn "All tests passed."

eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected =
  case (actual == expected) of
    True -> Right ()
    False -> Left (unlines
              [ "Test " ++ show n
              , "  Expected:  " ++ show expected
              , "  But got:   " ++ show actual
              ])

test :: IO ()
test = printTestResult $
  do
    eq 1 (checkPasswordLength "") (Right "")
    eq 2 (checkPasswordLength "julielovesbooks")
        (Right "julielovesbooks")
