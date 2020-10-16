-- sort out header, import into cabal
module LCA where

import Tree

path2Leaf :: (Ord a) => (Tree a) -> a -> [a]
path2Leaf Nil _ = []
path2Leaf (Node t1 v t2) x 
    | x == v = x:[]
    | contains t1 x = v:path2Leaf t1 x   --wait, we need to figure if sub tree contains it, then add path
    | contains t2 x = v:path2Leaf t2 x
    | otherwise = error "Prelude.!!:elementNotInTree"

-- To get LCA, generate both paths, then find what element is the last shared one
lcaGet :: (Ord a) => (Tree a) -> a -> a -> a
lcaGet Nil _ _ = error "Prelude.!!:noTreePresent"
lcaGet (Node t1 v t2) x y = pathCompare (path2Leaf (Node t1 v t2) x) (path2Leaf (Node t1 v t2) y) v


pathCompare :: Eq a => [a] -> [a] -> a -> a
pathCompare (x:xs) (y:ys) z 
        | x == y = pathCompare xs ys x
        | otherwise = z