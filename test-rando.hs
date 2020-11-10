import Test.QuickCheck

import Rando

main :: IO ()
main = do
  putStrLn "`rand` should have a period of 240"
  let valid = elements [16..255] :: Gen Int in do
    quickCheck $ forAll valid $
      \x -> x `notElem` (drop 1 $ take 240 $ iterate rand x)
    quickCheck $ forAll valid $
      \x -> x == (last $ take 241 $ iterate rand x)
