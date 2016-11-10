module Main where

import Prelude hiding (all, any, and, not, or, (&&), (||))
import Control.Arrow ((***))
import Control.Monad (forM_, replicateM, liftM)
import Control.Monad.State (MonadState)
import Data.Array
import Data.Char (isDigit, ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (sequence)

import Ersatz

sizeLimit :: Int
sizeLimit = 6

main :: IO ()
main = runSlitherlink sample

runSlitherlink :: Problem -> IO ()
runSlitherlink p = do
  (Satisfied, Just solution) <- minisat `solveWith` (slitherlink p)
  showBoard p $ fst solution
  paintBoard p $ snd solution
  return ()

paintBoard :: Problem -> Map Cell (Bool, (Integer, Integer)) -> IO ()
paintBoard p cs = do
  putStrLn "------------------------------------"
  forM_ (range (0,row-1)) $ \r -> do
    forM_ (range (0,col-1)) $ \c -> do
      let Just (b, _) = Map.lookup (r,c) cs
      putChar $ if b then 'X' else ' '
    putStrLn "+"
  putStrLn "------------------------------------"
  forM_ (range (0,row-1)) $ \r -> do
    forM_ (range (0,col-1)) $ \c -> do
      let Just (_, lvl) = Map.lookup (r,c) cs
      putStr $ show lvl
    putStrLn "+"
    where
      (row, col) = (length p, maximum $ map length p)

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

nikoli8x8 :: Problem
nikoli8x8=[ " 0 1  1 "
           , " 3  23 2"
           , "  0    0"
           , " 3  0   "
           , "   3  0 "
           , "1    3  "
           , "3 13  3 "
           , " 0  3 3 "
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

nikoli01 :: Problem
nikoli01 = [ "    02    "
           , "230    223"
           , "   3  3   "
           , "3   22   1"
           , " 2 2  0 2 "
           , " 2 3  3 3 "
           , "3   10   2"
           , "   2  3   "
           , "303    331"
           , "    02    "
           ]

nikoli02 :: Problem
nikoli02 = [ "1 220     "
           , "0     0 3 "
           , "1     1  3"
           , "1 333 0   "
           , "0   1  1  "
           , "  2  1   3"
           , "   2 201 2"
           , "2  3     3"
           , " 3 0     3"
           , "     010 1"
           ]

nikoli03 :: Problem
nikoli03 = [ " 3 3 31  2"
           , "30   1  3 "
           , "  33   3  "
           , "0 1 2 1  2"
           , "   3 2  23"
           , "33  2 1   "
           , "2  3 0 3 3"
           , "  2   13  "
           , " 2  3   32"
           , "2  13 3 2 "
           ]

nikoli04 :: Problem
nikoli04 = [ " 3 3  0    3  3 1 "
           , "1   2 3 31 0 3   1"
           , "1   1 2    1 2   0"
           , " 3 3   1  3   2 2 "
           , "3 2 3  2  3  3 3 1"
           , "2 3 3  0  3  0 2 2"
           , " 3 1   2  2   3 1 "
           , "2   0 1    3 2   3"
           , "1   3 0 32 3 1   3"
           , " 3 1  2    2  1 2 "
           ]

nikoli05 :: Problem
nikoli05 = [ " 01 1 0 33 2 2 12 "
           , "1   2  3  2  0   2"
           , "   1 2      2 3   "
           , " 2 2  3 03 3  3 1 "
           , "2  0 1 1  2 2 1  1"
           , "2  3 2 1  1 1 2  1"
           , " 3 2  2 11 3  0 1 "
           , "   1 2      1 2   "
           , "3   1  1  3  2   3"
           , " 02 1 3 10 3 0 10 "
           ]

nikoli06 :: Problem
nikoli06 = [ "3 1  3  22  3  2 1"
           , "3 1102      3133 2"
           , "       3  2       "
           , " 03 11 1322 21 31 "
           , " 3   3      3   1 "
           , " 1   1      0   3 "
           , " 32 32 2323 33 23 "
           , "       1  3       "
           , "3 1230      0212 0"
           , "3 2  2  31  2  1 1"
           ]

nikoli07 :: Problem
nikoli07 = [ "1 0 0  2 2   3   0  1112"
           , "       2 2 3   1    1111"
           , " 23133 1 0   3   1      "
           , "       3 2 2   0  2 3  1"
           , " 11131 3 2        0 1   "
           , "      0     11312 3 0  3"
           , "0  3 1 20223      1 2   "
           , "   3 1      11112 2 3  0"
           , "3  1 2 01111     0      "
           , "   2 3        2 1 11122 "
           , "0  2 2  0   0 1 2       "
           , "      3   0   0 3 22333 "
           , "2222    0   0 1 1       "
           , "2122  3   0   0 1  2 1 1"
           ]

nikoli08 :: Problem
nikoli08 = [ " 11  2  1  11  1  0  12 "
           , "3  0  30 13  22 21  3  1"
           , "3  1                3  1"
           , " 13  3  3  32  0  3  22 "
           , "       3  2  3  2       "
           , "2   3    2    3    2   1"
           , " 3 2  11   31   02  0 2 "
           , " 1 0  11   13   21  1 1 "
           , "2   3    3    1    2   0"
           , "       3  2  3  0       "
           , " 13  0  3  02  3  0  31 "
           , "1  0                2  3"
           , "1  1  23 03  11 20  0  1"
           , " 22  3  3  21  0  3  22 "
           ]

nikoli09 :: [String]
nikoli09 = [ "   3    0 122           21 01     20"
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

nikoli10 :: Problem
nikoli10 = [ " 20  01        3201    2    0122    "
           , "    2    1223        33  30 1  2  11"
           , "  03  22 2122  22   1   02  0  2 30 "
           , " 1   31  3222 32  33        1122    "
           , "2        2123    1    3210        23"
           , "   2232        13  01 2  3  13   2  "
           , "11 0  2  20   3   11  2  2 33  13   "
           , "0  1  3 22  21        2021    1    3"
           , "   2322    2    2223        21  22 3"
           , "1        33  02 1222  23   2   22  1"
           , "1  31   1   33  2322 22  13        1"
           , "2 13  23        0222    1    3032   "
           , "3    3    3032        33  31 3  2  1"
           , "   12  32 2  1  12   1   12  1  1 21"
           , "  0   03  3  1 21  23        1111   "
           , "33        2111    2    2120        0"
           , "    3222        13  21 1133  32   1 "
           , " 22 0  2  11   3   12  1111 12  31  "
           , "22  1  1 01  21        2210    2    "
           , "    3231    1    1312        22  13 "
           ]

type Row = Int
type Col = Int
type Point = (Row, Col)
type Cell = (Row, Col)
type Line = (Point,Point)
type Problem = [String]
type Hint = [(Cell, Int)]

existsPos :: (HasSAT s, MonadState s m) => m (Bits, Bits)
existsPos = (,) <$> existsBits <*> existsBits
  where
    existsBits = liftM Bits $ replicateM sizeLimit exists
      
defineVariables :: (Variable a, HasSAT s, MonadState s m) =>
  Problem -> m ((Map Line a), (Map Cell (a, (Bits, Bits))))
defineVariables p = do
  ls <- sequence $ Map.fromList [(line, exists) | line <- vLines ++ hLines]
  cs <- sequence $ Map.fromList [(cell, exists') | cell <- range ((0,0),(row-1,col-1))]
  return (ls, cs)
  where
    exists' = (,) <$> exists <*> existsPos
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
  Problem -> m ((Map Line Bit), (Map Cell (Bit, (Bits, Bits))))
slitherlink p = do
  (ls, cs) <- defineVariables p
  assert $ ls `validWith` (hints p)
  assert $ formIsland ls
  assert $ cs `divideBy` ls
  assert $ singleIsland (ls, cs)
  return (ls, cs)

singleIsland :: (Map Line Bit, Map Cell (Bit, (Bits, Bits))) -> Bit
singleIsland (ls, cs) = cape === bayPlus1
  where
    count p base = sumBit (base:(map p $ Map.keys cs))
    cape = count (fst.isCapeBay) false
    bayPlus1 = count (snd.isCapeBay) true
    isCapeBay c@(x, y) = (here && above && left, not here && below && right)
      where
        Just (here, _) = Map.lookup c cs
        above = let Just b = Map.lookup ((x,y),(x,y+1)) ls in b
        left = let Just b = Map.lookup ((x,y),(x+1,y)) ls in b
        below = let Just b = Map.lookup ((x+1,y),(x+1,y+1)) ls in b
        right = let Just b = Map.lookup ((x,y+1),(x+1,y+1)) ls in b
        

divideBy :: (Boolean a, Equatable a) => Map Cell (a, (Bits, Bits)) -> Map Line Bit -> Bit
cs `divideBy` ls =  and $ map (`contour` (ls, cs)) (Map.keys cs)

contour :: Cell -> (Map Line Bit, Map Cell (a, (Bits, Bits))) -> Bit
contour c@(x, y) (ls, cs) =
  (
    (not above ==> here === north) &&
    (not right ==> here === east) &&
    (not below ==> here === south) &&
    (not left  ==> here === west) &&
    (above ==> here /== north) &&
    (right ==> here /== east) &&
    (below ==> here /== south) &&
    (left  ==> here /== west)
  )
  where
    seaLevel :: (Bits, Bits)
    seaLevel = (intToBits 100, intToBits 100)
    here :: (Bits, Bits)
    Just (_, here) = Map.lookup c cs
    (Just above, Just right) = (Map.lookup ((x,y),(x,y+1)) ls, Map.lookup ((x,y+1),(x+1,y+1)) ls)
    (Just left, Just below) = (Map.lookup ((x,y),(x+1,y)) ls, Map.lookup ((x+1,y),(x+1,y+1)) ls)
    safe :: Maybe (a, (Bits, Bits)) -> (Bits, Bits)
    safe = maybe seaLevel snd
    intToBits :: Int -> Bits
    intToBits = encode . fromIntegral
    north, east, south, west :: (Bits, Bits)
    (north, east) = (safe *** safe) (Map.lookup (x-1,y) cs, Map.lookup (x,y+1) cs)
    (south, west) = (safe *** safe) (Map.lookup (x+1,y) cs, Map.lookup (x,y-1) cs)

isIslandOrOcean :: (Boolean a, Equatable a) =>
  Cell -> (Map Line a, Map Cell (a, (Bits, Bits))) -> Bit
c@(x, y) `isIslandOrOcean` (ls, cs) = undefined -- here === west `xor` left

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
