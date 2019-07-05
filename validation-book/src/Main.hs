module Main where

import Data.Char

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (passwordLength > 20 || passwordLength < 10 ) of
    True -> Nothing
    False -> Just password
  where passwordLength = length password

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Nothing
    True -> Just xs

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Just (x : xs)

-- Excercise 8
validatePassword' :: String -> Maybe String
validatePassword' password =
  case (cleanWhitespace password) of
    Nothing -> Nothing
    Just password ->
      case (checkPasswordLength password) of
        Nothing -> Nothing
        Just password ->
          case (requireAlphaNum password) of
            Nothing -> Nothing
            Just password -> Just password

main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (validatePassword password)

-- Exercise 9 returns Just ""

validatePassword :: String -> Maybe String
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
