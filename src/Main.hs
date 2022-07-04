module Main where

import Data.Char (isAlpha, toLower)
import Data.List (elemIndex, (!!))
import GHC.IO.Handle (hSetEcho)
import Main.Utf8 qualified as Utf8
import System.IO (getChar)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    _ <- gameLoop NotStarted
    pass

limit :: Attempts
limit = Attempts 6

attemptsToInt :: Attempts -> Int
attemptsToInt (Attempts int) = int

newtype Secret = Secret String
newtype Guesses = Guesses [(Char, Bool)]
newtype Attempts = Attempts Int

data GameState = NotStarted | Started Secret Guesses | Won Secret | Lost Secret

secretToString :: Secret -> String
secretToString (Secret secret) = secret

getSecret :: IO Text
getSecret = do
  _ <- hSetEcho stdin False
  secret <- getLine
  _ <- hSetEcho stdin True
  return secret

displaySecret :: Secret -> String -> Text
displaySecret (Secret secret) guessed =
  toText $
    map
      ( \x -> case elemIndex (toLower x) (toString guessed) of
          Nothing -> if not . isAlpha $ x then x else '_'
          Just i -> toString guessed !! i
      )
      $ toString secret

normalizingSecret :: Text -> Text
normalizingSecret secret = toText $ map toLower $ toString secret

displayStarted :: Secret -> Guesses -> IO String
displayStarted secret (Guesses guessed) = do
  putStrLn $ toString $ displaySecret secret (map fst guessed)
  putStrLn "Guessed letters:"
  putStrLn $ intersperse ' ' (map fst guessed)
  putStrLn "Enter your guess:"
  char <- getChar
  if not . isAlpha $ char
    then displayStarted secret (Guesses guessed)
    else return [toLower char]

displayNotStarted :: IO String
displayNotStarted = do
  putStrLn "Enter the secret to guess 🤐"
  secret <- getSecret
  if not $ any isAlpha (toString secret)
    then displayNotStarted
    else return $ toString secret

displayGameWon :: Secret -> IO ()
displayGameWon (Secret secret) = do
  putStrLn $ "The secret was: " ++ secret
  putStrLn "Congrats ! You won :)"

displayGameLost :: Secret -> IO ()
displayGameLost (Secret secret) = do
  putStrLn $ "Oh noes :( The answer was: " ++ secret

isFiniteState :: GameState -> Bool
isFiniteState (Won _) = True
isFiniteState (Lost _) = True
isFiniteState _ = False

gameLoop :: GameState -> IO ()
gameLoop gameState =
  case gameState of
    NotStarted -> do
      secret <- displayNotStarted
      gameLoop $ Started (Secret $ toString secret) (Guesses [])
    Won secret -> displayGameWon secret
    Lost secret -> displayGameLost secret
    (Started secret (Guesses guesses)) -> do
      charS <- displayStarted secret (Guesses guesses)
      let char = charS !! 0
      let alreadyPlayed = toLower char `elem` map fst guesses
      let match = toLower char `elem` map toLower (secretToString secret)
      let newGuesses = (toLower char, match) : guesses
      let errors = filter (\(_, y) -> not y) guesses
      let lettersToGuess = filter isAlpha $ map toLower (secretToString secret)
      let guessed = map fst $ filter snd newGuesses
      let foundWord = length lettersToGuess == length (filter (`elem` guessed) lettersToGuess)
      if foundWord
        then gameLoop (Won secret)
        else
          if alreadyPlayed
            then gameLoop (Started secret (Guesses guesses))
            else
              if length errors == attemptsToInt limit
                then gameLoop (Lost secret)
                else gameLoop (Started secret (Guesses newGuesses))