module Maze where

import Rando

import Optics
import Data.Bits
import Control.Monad.Trans.State

-- [left,down,right,up]

adjacent = [(-1,0),(0,-1),(1,0),(0,1)]
oppositeOf x = [2,3,0,1] !! x

addPair (x0,y0) (x1,y1) = (x0+x1,y0+y1)

indexOf' (w,h) (x,y) = w * (y `mod` h) + (x `mod` w)
locationOf' (w,h) i = (i `mod` w,i `div` w `mod` h)

adjacentTo' (w,h) i =
  let
    locationOf = locationOf' (w,h);
    indexOf = indexOf' (w,h)
  in map (indexOf . (addPair $ locationOf i)) adjacent

newMaze' (w,h) = take (w * h) $ repeat 0

tunnel' :: (Int,Int) -> [Int] -> Int -> Int -> ([Int], Int)
tunnel' (w,h) maze from direction =
  let
    adjacent = adjacentTo' (w,h) from
    to = adjacent !! direction
    oppositeDirection = oppositeOf direction
  in
    ( maze
      & (ix from) %~ (1 `shiftL` direction .|.)
      & (ix to) %~ (1 `shiftL` oppositeDirection .|.)
    , to
    )

path' :: (Int,Int) -> [Int] -> Int -> State Int (Maybe ([Int], Int))
path' (w,h) maze from =
  let
    adjacent = adjacentTo' (w,h) from
    options = filter (\x -> 0 == maze !! (adjacent !! x)) [0..3]
  in do
    direction <- pickOne options
    return $ fmap (tunnel' (w,h) maze from) direction

-- mazeDimensions = (4,4)
-- newMaze = newMaze' mazeDimensions
-- tunnel = tunnel' mazeDimensions
-- maze = newMaze
-- tunnel maze 0 2
