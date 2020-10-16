module Main where

import Tree
import LCA
--bst :: Tree
--path1, path2 :: [Integer]



main = putStrLn "Hello World :-)"


{-
ctree :: (Ord a) => [a] -> Tree a
ctree [] = Nil
ctree (h:t) = ctree2 (Node Nil h Nil) t
    where
        ctree2 tr [] = tr
        ctree2 tr (h:t) = ctree2 (insert tr h) t
-}