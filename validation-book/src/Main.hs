module Main where

import Data.Char

newtype Password = Password String
  deriving (Show, Eq)

newtype Error = Error String
  deriving (Show, Eq)

newtype Username = Username String
  deriving (Show, Eq)

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
  Password <$> (checkLength 20 password)

checkUsernameLength :: String -> Either Error Username
checkUsernameLength name =
  Username <$> (checkLength 15 name)

-- Exercise 13
requireAlphaNum :: String -> Either Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Left (Error "All charachers must be alphabet or number")
    True -> Right xs

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Cannot be empty string")
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
  putStr "Please enter a username. \n> "
  username <- Username <$> getLine
  putStr "Please enter a password\n> "
  password <- Password <$> getLine
  print (makeUser username password)

-- Exercise 9 returns Just ""

validatePassword :: Password -> Either Error Password
validatePassword (Password password) =
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
    eq 1 (checkPasswordLength "") (Right $ Password "")
    eq 2 (checkPasswordLength "julielovesbooks")
        (Right $ Password "julielovesbooks")


-- Exercise 17
validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
  cleanWhitespace(username)
    >>= requireAlphaNum
    >>= checkUsernameLength

-- Exercise 18
checkLength :: Int -> String -> Either Error String
checkLength maxLength str =
  case (length str > maxLength) of
    True -> Left (Error ("Cannot be longer than " ++ show(maxLength) ++ " characters."))
    False -> Right str

-- Exercise 19
main' :: IO ()
main' =
  putStr "Please enter a password\n> "
    >>= (\_ -> Password <$> getLine)
    >>= (\password -> print (validatePassword password))

main'' :: IO ()
main'' =
  putStr "Please enter a password\n> "
    >> (Password <$> getLine)
    >>= (\password -> print (validatePassword password))

-- Exercise 20
validatePassword' :: Password -> Either Error Password
validatePassword' (Password password) = do
  password' <- cleanWhitespace password
  password'' <- requireAlphaNum password'
  checkPasswordLength password''

data User = User Username Password deriving Show

makeUser :: Username -> Password -> Either Error User
makeUser name password =
  User <$> validateUsername name
       <*> validatePassword password

-- Exercise 21
makeUserTmpPassword :: Username -> Either Error User
makeUserTmpPassword name =
    User <$> validateUsername name
         <*> Right (Password "temporaryPassword")

-- Exercise 22
pureMaybe :: a -> Maybe a
pureMaybe a = Just a

pureEither :: a -> Either l a
pureEither a = Right a

-- Exercise 23
-- main :: IO ()
-- main =
-- do
--     result <- checkAnagram <$> promptWord1 <*> promptWord2
--     print result
