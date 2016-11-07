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
        putChar' $ cross sol (r,c)
        let (Just b) = Map.lookup ((r,c),(r,c+1)) sol
        putChar $ if b then '─' else ' '
      putChar' $ cross sol (r, col)
      putChar '\n'
    putHorizontalArea :: Row -> IO ()
    putHorizontalArea r = do
      forM_ (range (0,col-1)) $ \c -> do
        let (Just b) = Map.lookup ((r,c),(r+1,c)) sol
        putChar $ if b then '│' else ' '
        putChar $ p !! r !! c
      let (Just b) = Map.lookup ((r,col),(r+1,col)) sol
      putChar $ if b then '│' else ' '
      putChar '\n'
      
    putChar' ( Just n,  Just e,  Just s,  Just w) -- c
      | n && e = putChar '└'
      | n && s = putChar '│'
      | n && w = putChar '┘'
      | e && s = putChar '┌'
      | e && w = putChar '─'
      | s && w = putChar '┐'
      | otherwise = putChar ' '
    putChar' ( Nothing, Just e,  Just s, Nothing) -- lu
      | e && s = putChar '┌'
      | otherwise = putChar '+'
    putChar' ( Nothing, Just e,  Just s,  Just w) -- u
      | e && s = putChar '┌'
      | e && w = putChar '─'
      | s && w = putChar '┐'
      | otherwise = putChar ' '
    putChar' ( Nothing, Nothing, Just s,  Just w) -- ru
      | s && w = putChar '┐'
      | otherwise = putChar '+'
    putChar' ( Just n,  Just e,  Just s, Nothing) -- l
      | n && e = putChar '└'
      | n && s = putChar '│'
      | e && s = putChar '┌'
      | otherwise = putChar ' '
    putChar' ( Just n, Nothing,  Just s,  Just w) -- r
      | n && s = putChar '│'
      | n && w = putChar '┘'
      | s && w = putChar '┐'
      | otherwise = putChar ' '
    putChar' ( Just n,  Just e, Nothing, Nothing) -- ld
      | n && e = putChar '└'
      | otherwise = putChar '+'
    putChar' ( Just n,  Just e, Nothing,  Just w) -- d
      | n && e = putChar '└'
      | n && w = putChar '┘'
      | e && w = putChar '─'
      | otherwise = putChar ' '
    putChar' ( Just n, Nothing, Nothing,  Just w) -- rd
      | n && w = putChar '┘'
      | otherwise = putChar '+'
    putChar' (Nothing, Nothing, Nothing, Nothing) -- rd
      = putChar ' '
    putChar' _ = error "illegal pattern found"

triv :: [String]
triv = [ "  "
       , " 4"
       ]
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
    count :: (Cell -> Bit) -> [Bit] -> [Bit]
    count p base = foldr (\c bs -> p c `add` bs) base $ Map.keys cs
    cape, bay :: [Bit]
    cape = count isCape [false]
    bay = count isBay [true]
    safe = maybe false id
    isCape c@(x, y) = here && not (safe north) && not (safe west)
      where
        Just here = Map.lookup c cs
        (north, west) = (Map.lookup (x-1,y) cs, Map.lookup (x,y-1) cs)
    isBay c@(x, y) = not here && safe south && safe east
      where
        Just here = Map.lookup c cs
        (south, east) = (Map.lookup (x+1,y) cs, Map.lookup (x,y+1) cs)

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

cross :: Map Line a -> Point -> (Maybe a, Maybe a, Maybe a, Maybe a)
cross p (r,c) = (north, east, south, west)
  where
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

-- Arithmetic
add :: Bit -> [Bit] -> [Bit]
add x [] = [x]
add x (y:ys) = (x `xor` y):add (x && y) ys
