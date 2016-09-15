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
  , testCase "100 is one hundred" $ inEnglish 100 @?= "one hundred"
  , testCase "1000 is one thousand" $ inEnglish 1000 @?= "one thousand"
  , testCase "65461987 is sixty-five million four hundred sixty-one thousand nine hundred eighty-seven" $ inEnglish 65461987 @?= "sixty-five million four hundred sixty-one thousand nine hundred eighty-seven"
  ]
