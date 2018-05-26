module MPW.Test where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "MPW" [ properties, unitTests ]

properties :: TestTree
properties = testGroup "Properties" []

unitTests = testGroup "Unit tests"
  []

