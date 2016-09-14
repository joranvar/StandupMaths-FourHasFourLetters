module Lib
    ( inEnglish
    ) where

import Data.List (nubBy)
import Data.Ord (comparing)

inEnglish :: Int -> String
inEnglish 0 = "zero"
inEnglish 1 = "one"
inEnglish 2 = "two"
inEnglish 3 = "three"
inEnglish 4 = "four"
inEnglish 5 = "five"
inEnglish 6 = "six"
inEnglish 7 = "seven"
inEnglish 8 = "eight"
inEnglish 9 = "nine"
inEnglish 10 = "ten"
inEnglish 11 = "eleven"
inEnglish 12 = "twelve"
inEnglish 13 = "thirteen"
inEnglish 14 = "fourteen"
inEnglish 15 = "fifteen"
inEnglish 16 = "sixteen"
inEnglish 17 = "seventeen"
inEnglish 18 = "eighteen"
inEnglish 19 = "nineteen"
inEnglish i | i < 100 =
              let inEnglish' 0 = ""
                  inEnglish' i = inEnglish i in
              case i `divMod` 10 of
                (2, j) -> "twenty" ++ inEnglish' j
                (3, j) -> "thirty" ++ inEnglish' j
                (4, j) -> "forty" ++ inEnglish' j
                (5, j) -> "fifty" ++ inEnglish' j
                (6, j) -> "sixty" ++ inEnglish' j
                (7, j) -> "seventy" ++ inEnglish' j
                (8, j) -> "eighty" ++ inEnglish' j
                (9, j) -> "ninety" ++ inEnglish' j
                _ -> ""
inEnglish i | i < 1000 =
                let inEnglish' 0 = ""
                    inEnglish' i = inEnglish i in
                  case i `divMod` 100 of
                    (k, j) -> inEnglish' k ++ " hundred " ++ inEnglish' j
inEnglish i | i < 1000000 =
                let inEnglish' 0 = ""
                    inEnglish' i = inEnglish i in
                  case i `divMod` 1000 of
                    (k, j) -> inEnglish' k ++ " thousand " ++ inEnglish' j
inEnglish i | i < 1000000000 =
                let inEnglish' 0 = ""
                    inEnglish' i = inEnglish i in
                  case i `divMod` 1000000 of
                    (k, j) -> inEnglish' k ++ " million " ++ inEnglish' j
inEnglish i | i < 1000000000000 =
                let inEnglish' 0 = ""
                    inEnglish' i = inEnglish i in
                  case i `divMod` 1000000000 of
                    (k, j) -> inEnglish' k ++ " billion " ++ inEnglish' j

letterCount :: String -> Int
letterCount = length . filter (/= ' ')

chain :: Int -> [Int]
chain = chain' []
  where chain' cs i
          | i `elem` cs = cs
          | otherwise = chain' (i : cs) $ nextInt i

nextInt :: Int -> Int
nextInt = letterCount . inEnglish

longest :: [Int] -> [[(Int, String)]]
longest = (map.map) (\i -> (i, inEnglish i)) . nubBy (\a b -> length a == length b) . map chain
