module Rando where

import Data.Bits
import Control.Monad.Trans.State

-- an implementation of a (bad) Psuedo-Random Number Generator
-- this PRNG was designed for Gameboy hardware

-- a four-bit shift register with a cycle of 15

rand_f x =
  let b1 = (x `shiftR` 3) `xor` (x `shiftR` 2) .&. 1
  in (x `shiftL` 1) .&. 15 .|. b1

-- a relatively-prime walk of Z16

rand_g x = (x + 9) .&. 15

-- trivial combination of rand_f and rand_g for a cycle of 240
-- excludes values in the range 0..15

rand x =
  let
    a = rand_f (x `shiftR` 4) `shiftL` 4
    b = rand_g (x .&. 15)
  in a .|. b

rand0 = 16

-- filters for `rand` with "good" PRNG characteristics
-- good for an eight-bit PRNG, anyway

d2 x = 1 .&. ((x `shiftR` 4) `xor` (x `shiftR` 3))
d3 x = x `mod` 3
d4 x = 3 .&. ((x `shiftR` 4) `xor` (x `shiftR` 2))
d5 x = x `mod` 5
d6 x = ((x `shiftR` 4) `xor` x) `mod` 6
d8 x = 7 .&. ((x `shiftR` 4) `xor` (x `shiftR` 1))

-- State Monad variants

roll :: State Int Int
roll = state $ \s -> (rand s, rand s)

rollD2 = fmap d2 roll
rollD3 = fmap d3 roll
rollD4 = fmap d4 roll
rollD5 = fmap d5 roll
rollD6 = fmap d6 roll
rollD8 = fmap d8 roll

-- simple random selection for small arrays

pickOne :: [t] -> State Int (Maybe t)
pickOne [] = return Nothing
pickOne [a] = return (Just a)
pickOne [a,b] = fmap (Just . ([a,b] !!)) rollD2
pickOne [a,b,c] = fmap (Just . ([a,b,c] !!)) rollD3
pickOne [a,b,c,d] = fmap (Just . ([a,b,c,d] !!)) rollD4

-- scratch notes for reference

-- q = [16, 41, 66, 155, 52, 109, 214, 175, 88, 177, 122, 243, 236, 197, 142, 23]
-- [1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1] == fmap d2 q
-- [1, 2, 0, 2, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 1, 2] == fmap d3 q
-- [1, 0, 0, 3, 2, 1, 0, 1, 3, 3, 1, 3, 1, 1, 3, 0] == fmap d4 q
-- [1, 1, 1, 0, 2, 4, 4, 0, 3, 2, 2, 3, 1, 2, 2, 3] == fmap d5 q
-- [5, 1, 4, 2, 1, 5, 3, 3, 3, 0, 5, 0, 4, 3, 2, 4] == fmap d6 q
-- [1, 6, 5, 4, 1, 0, 6, 5, 1, 3, 2, 6, 0, 6, 7, 2] == fmap d8 q

-- [1, 2, 4, 9, 3, 6, 13, 10, 5, 11, 7, 15, 14, 12, 8] == (take 15 $ iterate rand1 1)
-- [0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12, 5, 14, 7] == (take 16 $ iterate rand2 0)
-- q == (take 16 $ iterate rand 16)

-- drop 1 q == (fst $ flip runState 16 $ sequence $ take 15 $ repeat roll)

-- ds = cycle [d2, d3, d4, d5, d6, d8]
-- rolls = cycle [rollD2, rollD3, rollD4, rollD5, rollD6, rollD8]
-- zipWith ($) ds (drop 1 q) == (fst $ flip runState 16 $ sequence $ take 15 rolls)
