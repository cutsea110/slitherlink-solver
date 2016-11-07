module Main where

import Prelude hiding (all, any, and, not, or, (&&), (||))
import Control.Arrow ((***))
import Control.Monad (forM_)
import Control.Monad.State (MonadState)
import Data.Array
import Data.Char (isDigit, ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (sequence)

import Ersatz

main :: IO ()
main = runSlitherlink sample

runSlitherlink :: Problem -> IO ()
runSlitherlink p = do
  (Satisfied, Just solution) <- minisat `solveWith` (slitherlink p)
  showBoard p $ fst solution
  return ()

showBoard :: Problem -> Map Line Bool -> IO ()
showBoard p sol = do
  forM_ (range (0,row-1)) $ \r -> do
    putHorizontalLine r
    putHorizontalArea r
  putHorizontalLine row
  where
    (row, col) = (length p, maximum $ map length p)
    putHorizontalLine :: Row -> IO ()
    putHorizontalLine r = do
      forM_ (range (0,col-1)) $ \c -> do
        putCorner $ cross sol (r,c)
        putNorthBorder (r,c)
      putCorner $ cross sol (r, col)
      putChar '\n'
    putHorizontalArea :: Row -> IO ()
    putHorizontalArea r = do
      forM_ (range (0,col-1)) $ \c -> do
        putWestBorder (r,c)
        putChar $ p !! r !! c
      putWestBorder (r,col)
      putChar '\n'
    putNorthBorder :: Cell -> IO ()
    putNorthBorder (r,c) = putChar $ if b then '─' else ' '
      where
        Just b = Map.lookup ((r,c), (r,c+1)) sol
    putWestBorder :: Cell -> IO ()
    putWestBorder (r,c) = putChar $ if b then '│' else ' '
      where
        Just b = Map.lookup ((r,c), (r+1,c)) sol
    putCorner :: (Bool, Bool, Bool, Bool) -> IO ()
    putCorner (n, e, s, w) = putChar ch
      where
        ch | n && e = '└'
           | e && s = '┌'
           | s && w = '┐'
           | w && n = '┘'
           | n && s = '│'
           | e && w = '─'
           | otherwise = ' '

triv :: Problem
triv = [ "  "
       , " 4"
       ]
sample :: Problem
sample = [ "3  21 3"
         , "  3    "
         , "1 3 23 "
         , "2     3"
         , " 03 2 2"
         , "    3  "
         , "0 13  3"
         ]

problem :: Problem
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

-- Slitherlink Puzzle
-- http://www.nikoli.co.jp/en/puzzles/slitherlink/
-- size 8 8
problem8x8 :: Problem
problem8x8=[ " 0 1  1 "
           , " 3  23 2"
           , "  0    0"
           , " 3  0   "
           , "   3  0 "
           , "1    3  "
           , "3 13  3 "
           , " 0  3 3 "
           ]
             
-- Slitherlink Puzzle
-- http://www.nikoli.co.jp/en/puzzles/slitherlink/
-- size 36 20
problem36x20 :: [String]
problem36x20 = [ "   3    0 122           21 01     20"
               , " 303 01      3321  30 2             "
               , " 3      2            3 3 3  3  3  23"
               , " 3  3 0   0 3  33  3 2   01 3  0    "
               , " 3 0   32 2 0    3     3    3   20 3"
               , "    32 3  3 2  22   0   13 1  33  3 "
               , "131        2 2      3 1    2    1 2 "
               , "     3 2 0   3 2  3 2  3            "
               , "  30 3  1 31  3   0    3 20 3 20  31"
               , "2  3 2      3  2  3         0    0  "
               , "  1    2         2  0  2      2 1  1"
               , "32  03 2 33 2    1   2  22 3  0 33  "
               , "            0  2 3  2 2   3 3 1     "
               , " 3 2    3    3 2      0 2        320"
               , " 3  21  3 01   0   21  2 3  0 22    "
               , "2 03   1    3     2    1 0 12   3 2 "
               , "    2  3 13   2 0  20  2 3   2 2  3 "
               , "13  2  3  2 0 1            3      0 "
               , "             3 33  2333      33 333 "
               , "10     01 21           101 1    2   "
               ]
               
type Row = Int
type Col = Int
type Point = (Row, Col)
type Cell = (Row, Col)
type Line = (Point,Point)
type Problem = [String]
type Hint = [(Cell, Int)]

defineVariables :: (Variable a, HasSAT s, MonadState s m) =>
  Problem -> m ((Map Line a), (Map Cell a))
defineVariables p = do
  ls <- sequence $ Map.fromList [(line, exists) | line <- vLines ++ hLines]
  cs <- sequence $ Map.fromList [(cell, exists) | cell <- range ((0,0),(row-1,col-1))]
  return (ls, cs)
  where
    (row, col) = (length p, maximum $ map length p)
    vLines   = [((r, c), (r+1, c)) | r <- [0..row-1], c <- [0..col]]
    hLines = [((r, c), (r, c+1)) | r <- [0..row], c <- [0..col-1]]

hints :: Problem -> [(Cell, Int)]
hints p = map (id *** charToInt) $ filter (isDigit.snd) $ zip cells $ concat p
  where
    (row, col) = (length p, maximum $ map length p)
    cellRange = ((0,0),(row-1,col-1))
    cells = range cellRange
    charToInt ch = ord ch - ord '0'

validWith :: Boolean a => Map Line a -> [(Cell, Int)] -> a
validWith p hs = and $ map (validAssignment p) hs

slitherlink :: (HasSAT s, MonadState s m) =>
  Problem -> m ((Map Line Bit), (Map Cell Bit))
slitherlink p = do
  (ls, cs) <- defineVariables p
  assert $ ls `validWith` (hints p)
  assert $ formIsland ls
  assert $ cs `divideBy` ls
  assert $ singleIsland cs
  return (ls, cs)

singleIsland :: Map Cell Bit -> Bit
singleIsland cs = cape === bay
  where
    count p base = sumBit (base:(map p $ Map.keys cs))
    cape = count (fst.isCapeBay) false
    bay = count (snd.isCapeBay) true
    isCapeBay c@(x, y) = (here && not north && not west, not here && south && east)
      where
        safe = maybe false id
        Just here = Map.lookup c cs
        (north, west) = (safe *** safe) (Map.lookup (x-1,y) cs, Map.lookup (x,y-1) cs)
        (south, east) = (safe *** safe) (Map.lookup (x+1,y) cs, Map.lookup (x,y+1) cs)

divideBy :: (Boolean a, Equatable a) => Map Cell a -> Map Line a -> Bit
cs `divideBy` ls =  and $ map (`isIslandOrOcean` (ls, cs)) (Map.keys cs)

isIslandOrOcean :: (Boolean a, Equatable a) => Cell -> (Map Line a, Map Cell a) -> Bit
c@(x, y) `isIslandOrOcean` (ls, cs) = here === (west `xor` left)
  where
    Just here = Map.lookup c cs
    west = maybe false id $ Map.lookup (x, y-1) cs
    Just left = Map.lookup ((x,y), (x+1,y)) ls

validAssignment :: Boolean a => Map Line a -> ((Row, Col), Int) -> a
validAssignment p ((x,y), n) = n `roundedBy` (a, b, c, d)
  where
    vl, vr, hu, hl :: Line
    (vl,vr,hu,hl) = (((x,y),(x+1,y)), ((x,y+1),(x+1,y+1)), ((x,y),(x,y+1)), ((x+1,y),(x+1,y+1)))
    (Just a, Just b, Just c, Just d)
      = (Map.lookup vl p, Map.lookup vr p, Map.lookup hu p, Map.lookup hl p)

formIsland :: Boolean a => Map Line a -> a
formIsland p = foldr (\k b -> k `connectWith` p && b) true (Map.keys p)

connectWith :: Boolean a => Line -> Map Line a -> a
l `connectWith` p = l `isActiveOn` p ==> (singleton p1s && singleton p2s)
  where
    p1s, p2s :: [Line]
    (p1s, p2s) = connectable l
    isActiveOn :: Boolean a => Line -> Map Line a -> a
    isActiveOn x = maybe false id . Map.lookup x
    singleton xs = trueCountEq 1 (map (`isActiveOn` p) xs)

trueCountEq :: Boolean a => Int -> [a] -> a
trueCountEq 0 xs = nor xs
trueCountEq _ [] = false
trueCountEq n (x:xs) = (x && trueCountEq (n-1) xs) || (not x && trueCountEq n xs)

cross :: Boolean a => Map Line a -> Point -> (a, a, a, a)
cross p (r,c) = (safe north, safe east, safe south, safe west)
  where
    safe = maybe false id
    north = Map.lookup ((r-1,c), (r,c)) p
    west  = Map.lookup ((r,c-1), (r,c)) p
    south = Map.lookup ((r,c), (r+1,c)) p
    east  = Map.lookup ((r,c), (r,c+1)) p

connectable :: Line -> ([Line], [Line])
connectable l@(p1@(r1, c1), p2@(r2, c2))
  | r1 == r2 = (map ($ p1) [north, west, south], map ($ p2) [north, east, south])
  | c1 == c2 = (map ($ p1) [west, north, east], map ($ p2) [west, south, east])
  | otherwise = error $ "illegal line: " ++ show l
  where
    north (r,c) = ((r-1,c), (r,c))
    west  (r,c) = ((r,c-1), (r,c))
    south (r,c) = ((r,c), (r+1,c))
    east  (r,c) = ((r,c), (r,c+1))

roundedBy :: Boolean a => Int -> (a, a, a, a) -> a
roundedBy n (a,b,c,d) = trueCountEq n [a,b,c,d]
