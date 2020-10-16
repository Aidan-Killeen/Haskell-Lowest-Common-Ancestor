{-# LANGUAGE StandaloneDeriving #-}
--renaming file requires cabal changes
module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Tree
import LCA


{- HUnit Tests -}



{- QuickCheck Tests -}


main = defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "\n\nLCA Tests\n"
            [ part1Tests
            , part2Tests
            --, part3Tests
            --, part4Tests
            ]
        ]

part1Tests :: TF.Test
part1Tests
 = testGroup "\nPart 1 - BST\n"
    [ test1
    --, testCase "raise null [2 marks]" (raise "" @?= "")
    --, testCase "raise numbers [2 marks]" (raise "1234" @?= "1234")
    --, testCase "raise single [2 marks]" (raise "a" @?= "A")
    --, testCase "raise mixed [2 marks]" (raise "1aB2cD" @?= "1AB2CD")
    ]

part2Tests :: TF.Test
part2Tests
 = testGroup "\nPart 2 - LCA\n"
    [ testLCA1
    , testLCA2
    --, testCase "raise numbers [2 marks]" (raise "1234" @?= "1234")
    --, testCase "raise single [2 marks]" (raise "a" @?= "A")
    --, testCase "raise mixed [2 marks]" (raise "1aB2cD" @?= "1AB2CD")
    ]


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
test1 = testCase "ctree [7,8,3,1,2,6,4,5]," (orderTopDown @?= [7,3,1,2,6,4,5,8])


treeToTest2 = ctree [7,8,3,1,2,6,4,5]
testLCA1 = testCase "path2Leaf treeToTest2 5," (path2Leaf treeToTest2 5 @?= [7,3,6,4,5])
testLCA2 = testCase "lcaGet treeToTest2 5 2," (lcaGet treeToTest2 5 2 @?= 3)
