module Bob (responseFor) where

import Data.Char
import Data.List
import Control.Monad

data Tone = Statement | Shouting | Question | ShoutingQuestion | Insult | Silence
  deriving (Eq, Show)

-- Utils

fAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd = liftM2 (&&)

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

isLastChar :: Char -> String -> Bool
isLastChar c xs = last xs == c

-- Textual Checks

noLowerCase :: String -> Bool
noLowerCase = not . any isLower

hasUpperCase :: String -> Bool
hasUpperCase = any isUpper

hasTerminatingQuestionMark :: String -> Bool
hasTerminatingQuestionMark = isLastChar '?'

-- Tone Checks

isShouting :: String -> Bool
isShouting = noLowerCase `fAnd` hasUpperCase

isQuestion :: String -> Bool
isQuestion = hasTerminatingQuestionMark

isShoutingQuestion :: String -> Bool
isShoutingQuestion = isShouting `fAnd` isQuestion

isSilence :: String -> Bool
isSilence = (== "")

isInsult :: String -> Bool
isInsult = (isInfixOf "mum") . (map toLower)

tone :: String -> Tone
tone xs | isSilence xs = Silence
        | isInsult xs = Insult
        | isShoutingQuestion xs = ShoutingQuestion
        | isShouting xs = Shouting
        | isQuestion xs = Question
        | otherwise = Statement

-- Bob's responses

bobsRiposte :: Tone -> String
bobsRiposte Silence = "Fine. Be that way!"
bobsRiposte ShoutingQuestion = "Calm down, I know what I'm doing!"
bobsRiposte Shouting = "Whoa, chill out!"
bobsRiposte Question = "Sure."
bobsRiposte Insult = "And your mama, too!"
bobsRiposte _ = "Whatever."

-- Entry function

responseFor :: String -> String
responseFor = bobsRiposte . tone . trimEnd
