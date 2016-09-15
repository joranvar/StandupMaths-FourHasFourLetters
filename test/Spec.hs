import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [englishTests]

englishTests :: TestTree
englishTests = testGroup "English Tests"
  [ testCase "0 is zero" $ inEnglish 0 @?= "zero"
  , testCase "100 in one hundred" $ inEnglish 100 @?= "one hundred"
  ]
