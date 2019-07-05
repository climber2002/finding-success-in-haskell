-- Exercise 1

absVal :: (Num a, Ord a) => a -> a
absVal x = if (x < 0) then (negate x) else x

absVal' :: (Num a, Ord a) => a -> a
absVal' x = 
  case (x < 0) of
    True -> negate x
    False -> x

-- Exercise 2
validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password = 
  if null username
  then (if null password
        then "Empty username and password"
        else "Empty username")
  else (if null password
        then "Empty password"
        else "Okey")

validateUsernamePassword' :: String -> String -> String
validateUsernamePassword' username password =
  case (null username, null password) of
    (True, True) -> "Empty username and password"
    (True, False) -> "Empty username"
    (False, True) -> "Empty password"
    (False, False) -> "Okey"

-- Exercise 3
-- safeHead [] = [] returns a list instead of a value of a

-- Exercse 4
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
