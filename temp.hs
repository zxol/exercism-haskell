module LinkedList
  where

data LinkedList a = Node a (LinkedList a) | Empty deriving (Eq, Show)

instance Foldable LinkedList where
  foldr f z = go
    where
      go Empty = z
      go (Node v n) = v `f` go n
  foldl f z = go
    where
      go Empty = z
      go (Node v n) = let z' = z `f` v in foldl f z' n

headOr :: a -> LinkedList a -> a
headOr = foldr const

tailOr :: LinkedList a -> LinkedList a -> LinkedList a
tailOr x Empty = x
tailOr _ (Node _ next') = next'

koncat :: LinkedList a -> LinkedList a -> LinkedList a
koncat = flip (foldr Node)

insert :: a -> LinkedList a -> LinkedList a
insert = Node

new :: a -> LinkedList a
new x = Node x Empty

nil :: LinkedList a
nil = Empty

isNil :: LinkedList a -> Bool
isNil Empty = True
isNil _ = False

datum :: LinkedList a -> a
datum Empty = error "Empty"
datum (Node v _) = v

next :: LinkedList a -> LinkedList a
next Empty = error "Empty"
next (Node _ n) = n

append :: a -> LinkedList a -> LinkedList a
append = flip koncat . new

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList xs = foldl (flip Node) Empty xs

toList :: LinkedList a -> [a]
toList = foldr (:) []

fromList :: [a] -> LinkedList a
fromList = foldr Node Empty

vt = (Node "a" (Node "b" (Node "c" (Node "d" Empty))))
qt = (Node "w" (Node "x" (Node "y" (Node "z" Empty))))
