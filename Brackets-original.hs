-- Brackets.hs
-- Function arePaired checks to see if the brackets in a string are correctly nested and closed.
-- For example "{ ( [ ] ) }" will return True.
-- and "{ )" will return False.
-- Ignores all non-bracket characters.

module Brackets (arePaired) where

data BracketType = Curly | Square | Paren | UnknownBracketType deriving (Show, Eq)
data BracketDir = Opening | Closing deriving (Show, Eq)
data Bracket = Bracket {
  bType :: BracketType,
  bDir :: BracketDir
  } deriving (Show, Eq)
type Stack = [Bracket]

-- Stack-like operations (using lists)

push :: Bracket -> Stack -> Stack
push = (:)

pop :: Stack -> Stack
pop = tail

topOf :: Stack -> Bracket
topOf = head

-- Bracket

makeBracket :: Char -> Bracket
makeBracket c = Bracket { bType = bracketType c, bDir = bracketDir c}

bracketDir :: Char -> BracketDir
bracketDir c | c `elem` "{[(" = Opening
             | otherwise = Closing

isClosing :: Bracket -> Bool
isClosing = ( == Closing ) . bDir

bracketType :: Char -> BracketType
bracketType c | c `elem` "{}" = Curly
              | c `elem` "[]" = Square
              | c `elem` "()" = Paren
              | otherwise = UnknownBracketType

areMismatched :: Bracket -> Bracket -> Bool
areMismatched a b =
  (bType a /= bType b)
  && bDir a == Opening
  && bDir b == Closing

-- Parser

parse :: String -> Stack -> Bool
parse [] [] = True
parse [] _ = False
parse (x:xs) [] = parse xs [makeBracket x]
parse (x:xs) stack = let b = makeBracket x in
  if isClosing b then
    if areMismatched (topOf stack) b then
      False
      else
      parse xs (pop stack)
    else
    parse xs (push b stack)

arePaired :: String -> Bool
-- arePaired xs = parse (onlyBrackets xs) []
arePaired = check

onlyBrackets :: String -> String
onlyBrackets = filter (`elem` "{[()]}")

-- glguy

check :: String -> Bool
check = go []
  where
    go ('[':xs) (']':ys) = go xs ys
    go ('{':xs) ('}':ys) = go xs ys
    go ('(':xs) (')':ys) = go xs ys
    go xs (y:ys)
      | y `elem` "[{(" = go (y:xs) ys
      | otherwise      = y `notElem` "]})" && go xs ys
    go xs [] = null xs

