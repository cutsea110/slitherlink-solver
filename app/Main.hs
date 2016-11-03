module Main where

import Prelude hiding (any)
import Control.Arrow ((***), (&&&))
import Control.Monad.State (MonadState)
import Data.Array
import Data.Char (isDigit, ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (sequence)

import Ersatz

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

type Row = Int
type Col = Int
type Point = (Row, Col)
type Cell = (Row, Col)
type Line = (Point,Point)
type Problem = [String]
type Hint = [(Cell, Int)]

defineVariables :: (Variable a, HasSAT s, MonadState s m) => Problem -> m (Map Line a)
defineVariables p = sequence $ Map.fromList [(line, exists) | line <- vLines ++ hLines]
  where
    (row, col) = (length p, maximum $ map length p)
    vLines   = [((r, c), (r+1, c)) | r <- [0..row-1], c <- [0..col]]
    hLines = [((r, c), (r, c+1)) | r <- [0..row], c <- [0..col-1]]

hints :: Problem -> [(Cell, Int)]
hints p = map (id *** charToInt) $ filter (isDigit.snd) $ zip cells $ concat p
  where
    (row, col) = (length problem, maximum $ map length problem)
    cellRange = ((0,0),(row-1,col-1))
    cells = range cellRange
    charToInt ch = ord ch - ord '0'
    
validWith :: Boolean a => Map Line a -> Hint -> a
validWith = undefined
  where

slitherlink :: (MonadState s m, HasSAT s) => Problem -> m (Map Line Bit)
slitherlink p = do
  v <- defineVariables p
  let hint = hints p
  assert (v `validWith` hint)
  return v

roundedBy :: Int -> (Bit, Bit, Bit, Bit) -> Bit
roundedBy n (a,b,c,d) = any (===(Bit4 a b c d)) $ toBit4s n

toBit4s :: Int -> [Bit4]
toBit4s n = maybe [] id $ lookup n $ map (id &&& conv) [0..3]
  where
    conv :: Int -> [Bit4]
    conv = map bit4 . bin
    bin :: Int -> [(Int, Int, Int, Int)]
    bin i = [(a,b,c,d)| a<-[0,1], b<-[0,1], c<-[0,1], d<-[0,1], a+b+c+d == i]
    bit4 :: (Int, Int, Int, Int) -> Bit4
    bit4 (a,b,c,d) = Bit4 (toBit a) (toBit b) (toBit c) (toBit d)
    toBit :: Int -> Bit
    toBit = encode . toEnum
