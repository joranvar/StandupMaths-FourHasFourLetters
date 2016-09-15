module Lib
    ( inEnglish
    , chain
    , longest
    ) where

import Data.List (nubBy)
import Data.Ord (comparing)

inEnglish :: Integer -> String
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
        inEnglish' i = "-" ++ inEnglish i in
      case i `divMod` 10 of
         (2, j) -> "twenty"  ++ inEnglish' j
         (3, j) -> "thirty"  ++ inEnglish' j
         (4, j) -> "forty"   ++ inEnglish' j
         (5, j) -> "fifty"   ++ inEnglish' j
         (6, j) -> "sixty"   ++ inEnglish' j
         (7, j) -> "seventy" ++ inEnglish' j
         (8, j) -> "eighty"  ++ inEnglish' j
         (9, j) -> "ninety"  ++ inEnglish' j
         _      -> ""
inEnglish i | i < 1000 = splitOff inEnglish " " 100 " hundred" i
inEnglish i | i < 1000000 = splitOff inEnglish " " 1000 " thousand" i
inEnglish i =
  case (subtract 1) $ (`div` (3::Int)) $ fromIntegral $ truncate $ logBase 10 $ fromIntegral i of
    1  -> splitOff inEnglish " " (1000^(1+1))  " million" i
    2  -> splitOff inEnglish " " (1000^(2+1))  " billion" i
    3  -> splitOff inEnglish " " (1000^(3+1))  " trillion" i
    4  -> splitOff inEnglish " " (1000^(4+1))  " quadrillion" i
    5  -> splitOff inEnglish " " (1000^(5+1))  " quintillion" i
    6  -> splitOff inEnglish " " (1000^(6+1))  " sextillion" i
    7  -> splitOff inEnglish " " (1000^(7+1))  " septillion" i
    8  -> splitOff inEnglish " " (1000^(8+1))  " octillion" i
    9  -> splitOff inEnglish " " (1000^(9+1))  " nonillion" i
    10 -> splitOff inEnglish " " (1000^(10+1)) " decillion" i

splitOff f combineNonZero factor stringFactor i =
  let f' 0 = ""
      f' other = combineNonZero ++ f other
      combine (div, mod) = f div ++ stringFactor ++ f' mod
  in combine $ i `divMod` factor


letterCount :: String -> Integer
letterCount = fromIntegral . length . filter (/= ' ')

chain :: Integer -> [Integer]
chain = chain' []
  where chain' cs i
          | i `elem` cs = cs
          | otherwise = chain' (i : cs) $ nextInteger i

nextInteger :: Integer -> Integer
nextInteger = letterCount . inEnglish

longest :: [Integer] -> [[(Integer, String)]]
longest = (map.map) (\i -> (i, inEnglish i)) . nubBy (\a b -> length a == length b) . map chain
