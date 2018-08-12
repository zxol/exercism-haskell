module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

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

new :: a -> LinkedList a -> LinkedList a
new = Node

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

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl (flip Node) Empty

toList :: LinkedList a -> [a]
toList = foldr (:) []

fromList :: [a] -> LinkedList a
fromList = foldr Node Empty
