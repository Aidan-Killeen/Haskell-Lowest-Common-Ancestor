{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Tree
import LCA


main = defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "\n\nLCA Tests\n"
            [ part1Tests
            , part2Tests
            ]
        ]

--BST testing
part1Tests :: TF.Test
part1Tests
 = testGroup "\nPart 1 - BST\n"
    [ testTree1
    , testTree2
    , testTree3
    , testTree4
    ]

--LCA testing
part2Tests :: TF.Test
part2Tests
 = testGroup "\nPart 2 - LCA\n"
    [ testLCA1
    , testLCA2
    ]

--Diagram of BST used
--       _7_
--     /     \
--   _3_      8
-- /     \
--1       6
-- \     /
--  2   4
--       \
--        5

treeToTest = ctree [7,8,3,1,2,6,4,5]
orderTopDown = preorder treeToTest
testTree1 = testCase "ctree [7,8,3,1,2,6,4,5]," (orderTopDown @?= [7,3,1,2,6,4,5,8])
insert9 = insert treeToTest 9
printInsert9 = preorder insert9
testTree2 = testCase "insert treeToTest 9," (printInsert9 @?= [7,3,1,2,6,4,5,8,9])

delete1 = preorder (delete treeToTest 1)
testTree3 = testCase "delete treeToTest 1," (delete1 @?= [7,3,2,6,4,5,8])

delete3 = preorder (delete treeToTest 3)
testTree4 = testCase "delete treeToTest 3," (delete3 @?= [7,2,1,6,4,5,8])

treeToTest2 = ctree [7,8,3,1,2,6,4,5]
testLCA1 = testCase "path2Leaf treeToTest2 5," (path2Leaf treeToTest2 5 @?= [7,3,6,4,5])
testLCA2 = testCase "lcaGet treeToTest2 5 2," (lcaGet treeToTest2 5 2 @?= 3)
