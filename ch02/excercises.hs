import Data.Char

-- Exercise 5

isPalindromes :: String -> Bool
isPalindromes word = word == (reverse word)

isWord :: String -> Maybe String
isWord word = 
  case (null word) of
    True -> Nothing
    False ->
      case (all isAlpha word) of
        False -> Nothing
        True -> Just word


checkPalindromes :: String -> String
checkPalindromes word =
  case (isWord word) of
    Nothing -> "The first word is invalid."
    Just word ->
      case (isPalindromes word) of
        False -> "The word is not palindromes."
        True -> "The word is palindromes."
          
-- Excercise 6 : Hackers voice
substituteChar :: Char -> Char
substituteChar c = 
  case c of
    'e' -> '3'
    _   -> c

translateWord :: String -> String
translateWord s = map substituteChar s

mainExcercise6 :: IO ()
mainExcercise6 = 
  do
    putStr "Please enter a word.\n> "
    word <- getLine
    print (translateWord word)


