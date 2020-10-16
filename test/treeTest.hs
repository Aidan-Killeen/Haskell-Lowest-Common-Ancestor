{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Tree


{- HUnit Tests -}



{- QuickCheck Tests -}


main = defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "\n\nLCA Tests\n"
            [ part1Tests
            --, part2Tests
            --, part3Tests
            --, part4Tests
            ]
        ]

part1Tests :: TF.Test
part1Tests
 = testGroup "\nPart 1 - ctree\n"
    [ test1
    --, testCase "raise null [2 marks]" (raise "" @?= "")
    --, testCase "raise numbers [2 marks]" (raise "1234" @?= "1234")
    --, testCase "raise single [2 marks]" (raise "a" @?= "A")
    --, testCase "raise mixed [2 marks]" (raise "1aB2cD" @?= "1AB2CD")
    ]

treeToTest = ctree [7,8,3,1,2,6,4,5]
orderTopDown = preorder treeToTest
test1 = testCase "ctree [7,8,3,1,2,6,4,5]," (orderTopDown @?= [7,3,1,2,6,4,5,8])
--test1 = testCase "ctree [7,8,3,1,2,6,4,5]," (ctree [7,8,3,1,2,6,4,5] @?= (Node (Node (Node Nil 1 (Node Nil 2 Nil)) 3 (Node (Node Nil 4 (Node Nil 5 Nil)) 6 Nil)) 7 (Node Nil 8 Nil)))
    -- (ctree [7,8,3,1,2,6,4,5]) 'shouldBe' (Node ((Node (Node Nil 1 (Node Nil 2 Nil)) 3 (Node (Node Nil 4 (Node Nil 5 Nil))) 6 Nil) 7 (Node Nil 8 Nil)))  --(((()1(()2()))3((()4(()5()))6()))7(()8()))