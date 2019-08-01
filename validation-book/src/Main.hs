{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Char
import Data.Validation
import Data.Coerce

newtype Password = Password String
  deriving (Show, Eq)

newtype Error = Error [String]
  deriving (Show, Eq, Semigroup)

newtype Username = Username String
  deriving (Show, Eq)

checkPasswordLength :: String -> Validation Error Password
checkPasswordLength password =
  Password <$> (checkLength 20 password)

checkUsernameLength :: String -> Validation Error Username
checkUsernameLength name =
  Username <$> (checkLength 15 name)

-- Exercise 13
requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Failure (Error ["All charachers must be alphabet or number"])
    True -> Success xs

cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (Error ["Cannot be empty string"])
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Success (x : xs)

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
  display username password

-- Exercise 9 returns Just ""

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  case (cleanWhitespace password) of
    Failure err -> Failure err
    Success password2 -> requireAlphaNum password2 *>
                         checkPasswordLength password2

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
    eq 1 (checkPasswordLength "") (Success $ Password "")
    eq 2 (checkPasswordLength "julielovesbooks")
        (Success $ Password "julielovesbooks")


-- Exercise 17
validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case cleanWhitespace(username) of
    Failure err -> Failure err
    Success username2 -> requireAlphaNum username2
                      *> checkUsernameLength username2

-- Exercise 18
checkLength :: Int -> String -> Validation Error String
checkLength maxLength str =
  case (length str > maxLength) of
    True -> Failure (Error [("Cannot be longer than " ++ show(maxLength) ++ " characters.")])
    False -> Success str

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
-- validatePassword' :: Password -> Either Error Password
-- validatePassword' (Password password) = do
--   password' <- cleanWhitespace password
--   password'' <- requireAlphaNum password'
--   checkPasswordLength password''

data User = User Username Password deriving Show

makeUser :: Username -> Password -> Validation Error User
makeUser name password =
  User <$> usernameErrors name
       <*> passwordErrors password

-- Exercise 21
-- makeUserTmpPassword :: Username -> Either Error User
-- makeUserTmpPassword name =
--     User <$> validateUsername name
--          <*> Right (Password "temporaryPassword")

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

-- Exercise 24 

-- Exercise 25
promptWord1 :: IO String
promptWord1 =
  putStr "Please enter a word.\n> " *>
  getLine

passwordErrors :: Password -> Validation Error Password
passwordErrors password =
  case validatePassword password of 
    Failure err -> Failure (Error ["Invalid password:"] <> err)
    Success password2 -> Success password2

usernameErrors :: Username -> Validation Error Username
usernameErrors username =
  case validateUsername username of
    Failure err -> Failure (Error ["Invalid username:"]
                                <> err)
    Success username2 -> Success username2

errorCoerce :: Error -> [String]
errorCoerce (Error err) = err

display :: Username -> Password -> IO ()
display name password =
  case makeUser name password of
    Failure err -> putStr (unlines (coerce err))
    Success (User username password) -> putStr ("Welcome! " ++ coerce username)

-- Exercise 28
newtype Error' = Error' String deriving Show
instance Semigroup (Error') where
  (Error' err1) <> (Error' err2) = Error' (err1 ++ "\n" ++ err2)

-- Exercise 29
createError :: String -> Error'
createError msg = Error' msg

newtype UserPW = UserPW Password deriving (Show)

userPasswordCoerce' :: UserPW -> String
userPasswordCoerce' = coerce

type Rule a = (a -> Validation Error a)

validatePassword'' :: Rule Password
validatePassword'' password =
  case (coerce cleanWhitespace :: Rule Password) password of 
    Failure err -> Failure err
    Success password2 ->
      (coerce requireAlphaNum :: Rule Password) password2 *>
      checkPasswordLength (coerce password2)

-- Ch11
class LiftAB f where 
  liftA :: a -> f a b
  liftB :: b -> f a b

instance LiftAB Validation where 
  liftA = Failure
  liftB = Success

instance LiftAB Either where 
  liftA = Left
  liftB = Right

class MaybeAB f where
  maybeA :: f a b -> Maybe a
  maybeB :: f a b -> Maybe b

instance MaybeAB Validation where 
  maybeA (Failure a) = Just a 
  maybeA (Success _b) = Nothing 
  maybeB (Failure _a) = Nothing 
  maybeB (Success b) = Just b

instance MaybeAB Either where 
  maybeA (Left a) = Just a 
  maybeA (Right _b) = Nothing 
  maybeB (Left _a) = Nothing 
  maybeB (Right b) = Just b


