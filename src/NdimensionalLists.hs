module NdimensionalLists where

-- https://stackoverflow.com/questions/30204950/is-it-possible-to-define-a-function-that-return-a-n-dimensional-list-in-haskell

data Nlist vals = Leaf vals | Node (Nlist [vals])
  deriving (Show)


initNlist :: Int -> Int -> a -> Nlist a
initNlist n sz val
  | n == 1 = Node $ Leaf (replicate sz val)
  | n  > 1 = Node $ initNlist (n-1) sz (replicate sz val)
  | otherwise = error "cannot have negative nlist"

initNlist' :: Int -> Int -> [a] -> Nlist a
initNlist' n sz val
  | n == 1 = Node $ Leaf (val)
  | n  > 1 = Node $ initNlist (n-1) sz (val)
  | otherwise = error "cannot have negative nlist"




initNlist'' sz val
  | sz == 1 = Leaf val
  | sz > 1  = Node (initNlist'' (sz-1) val)
  | otherwise = error "cannot have negative nList"


{-
initNlist''' :: [Int] -> a -> Nlist a
initNlist''' sizes val =
  case sizes of
    [] -> error "empty sizes error"
    [last] -> Node $ Leaf (replicate last val)
    (hd:tl) -> Node $ replicate hd (initNlist''' tl val)
-}
