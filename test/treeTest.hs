{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Tree


{- HUnit Tests -}

test1 = TestCase (assertEqual "ctree [7,8,3,1,2,6,4,5]," (ctree [7,8,3,1,2,6,4,5]) (Node (Node (Node Nil 1 (Node Nil 2 Nil)) 3 (Node (Node Nil 4 (Node Nil 5 Nil)) 6 Nil)) 7 (Node Nil 8 Nil)))

{- QuickCheck Tests -}


main = defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "\nDeclaration" [
            testCase declaration (1+1 @?= 2)
          ]
        , testGroup "\nBST Tests" [
            testCase "Create Tree" test1
          ]
        ]


    -- (ctree [7,8,3,1,2,6,4,5]) 'shouldBe' (Node ((Node (Node Nil 1 (Node Nil 2 Nil)) 3 (Node (Node Nil 4 (Node Nil 5 Nil))) 6 Nil) 7 (Node Nil 8 Nil)))  --(((()1(()2()))3((()4(()5()))6()))7(()8()))