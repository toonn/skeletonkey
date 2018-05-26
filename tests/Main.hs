module Main where

import Test.Tasty

import qualified MPW.Test as MPW

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ MPW.tests ]
