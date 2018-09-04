module Main where

import Test.Tasty
import Test.Tasty.Ingredients.FailFast

import qualified MPW.Test as MPW

main = tests >>= defaultMainWithIngredients (map failFast defaultIngredients)

tests :: IO TestTree
tests = do
  mpwTs <- MPW.tests
  return $ testGroup "Tests" [ mpwTs ]
