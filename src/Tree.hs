--source - https://gist.github.com/Kedrigern/1239141/5ee8f5f45facdf4f48785fc92a78ad4104f16537#file-tree-hs, with some edits
{- Implementation of BST (binary search tree)
Script is absolutly free/libre, but with no guarantee.
Author: Ondrej Profant -}
module Tree where

import qualified Data.List


{- DEF data structure -}
data (Ord a, Eq a) => Tree a = Nil | Node (Tree a) a (Tree a) 
    deriving Show

{- BASIC Information -}
empty :: (Ord a) => Tree a -> Bool
empty Nil = True
empty  _  = False

contains :: (Ord a) => (Tree a) -> a -> Bool
contains Nil _ = False
contains (Node t1 v t2) x 
    | x == v = True
    | x  < v = contains t1 x 
    | x  > v = contains t2 x

{- BASIC Manipulation -}
insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node t1 v t2) x 
    | v == x = Node t1 v t2
    | v  < x = Node t1 v (insert t2 x)
    | v  > x = Node (insert t1 x) v t2

delete :: (Ord a) => Tree a -> a -> Tree a
delete Nil _ = Nil
delete (Node t1 v t2) x  
    | x == v = deleteX (Node t1 v t2)
    | x  < v = Node (delete t1 x) v t2
    | x  > v = Node t1 v (delete t2 x)

-- Delete root (is used on subtree)
deleteX :: (Ord a) => Tree a -> Tree a 
deleteX (Node Nil v t2) = t2
deleteX (Node t1 v Nil) = t1
deleteX (Node t1 v t2) = (Node left v2 t2)   --(Node t1 v2 t3)                            
    where                 --   where 
        (v2, left) = (maxElement(t1),(deleteMax t1))                --       (t3,v2) = leftistElement t2


maxElement :: (Ord a) => Tree a -> a
maxElement (Node t1 v Nil) = v           -- need to get right tree of leftest
maxElement (Node t1 v t2) = maxElement t2

deleteMax :: (Ord a) => Tree a -> Tree a
deleteMax (Node t1 v Nil) = t1
deleteMax (Node t1 v t2) = (Node t1 v (deleteMax t2))



-- Create tree from list of elemtents
ctree :: (Ord a) => [a] -> Tree a
ctree [] = Nil
ctree (h:t) = ctree2 (Node Nil h Nil) t
    where
        ctree2 tr [] = tr
        ctree2 tr (h:t) = ctree2 (insert tr h) t

-- Create perfect balance BST
ctreePB :: (Ord a) => [a] -> Tree a
ctreePB [] = Nil
ctreePB s = cpb Nil (qsort s) 

cpb :: (Ord a) => Tree a -> [a] -> Tree a
cpb tr [] = tr
cpb tr t = cpb (insert tr e) t2
    where
        e = middleEl t
        t2 = Data.List.delete e t

-- Element in middle
middleEl :: (Ord a) => [a] -> a
middleEl s = mEl s s 

mEl :: (Ord a) => [a] ->  [a] -> a
mEl    []    (h:s2) = h
mEl (_:[])   (h:s2) = h
mEl (_:_:s1) (_:s2) = mEl s1 s2

{- PRINT -}
inorder :: (Ord a) => Tree a -> [a]
inorder Nil = []
inorder (Node t1 v t2) = inorder t1 ++ [v] ++ inorder t2

preorder :: (Ord a) => Tree a -> [a]
preorder Nil = []
preorder (Node t1 v t2) = [v] ++ preorder t1 ++ preorder t2

postorder :: (Ord a) => Tree a -> [a]
postorder Nil = []
postorder (Node t1 v t2) = postorder t1 ++ postorder t2 ++ [v]

-- from wiki
levelorder :: (Ord a) => Tree a -> [a]
levelorder t = step [t]
    where
        step [] = []
        step ts = concatMap elements ts ++ step (concatMap subtrees ts)
        elements Nil = []
        elements (Node left x right) = [x]
        subtrees Nil = []
        subtrees (Node left x right) = [left,right]

qsort :: (Ord a) => [a] -> [a] 
qsort [] = []
qsort (h:t) = (qsort [x| x<-t, x < h]) ++ [h] ++ (qsort [x| x<-t, x>=h ])