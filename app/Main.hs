module Main where

import Data.Array
import Data.Char

main :: IO ()
main = undefined


sample :: [String]
sample = [ "3  21 3"
         , "  3    "
         , "1 3 23 "
         , "2     3"
         , " 03 2 2"
         , "    3  "
         , "0 13  3"
         ]

problem :: [String]
problem = [ " 3 12  212  31 "
          , "3   3     1   0"
          , "  3   3  1  2  "
          , " 2    1  2  0  "
          , "3  32  1  31  1"
          , "  1  0  0    3 "
          , "  1  2  3   3  "
          , "3   2     3   3"
          , " 01  233  20 2 "
          ]

hints :: [String] -> Array (Int, Int) (Maybe Int)
hints p = array cellRange $ zip cells $ map conv $ concat p
  where
    (row, col) = (length problem, maximum $ map length problem)
    cellRange = ((0,0),(row-1,col-1))
    cells = range cellRange
    conv c | isDigit c = Just (ord c - ord '0')
           | otherwise = Nothing
