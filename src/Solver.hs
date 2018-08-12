module Solver where

import Control.Monad

data Cell = B
          | W
          | I Int
  deriving (Eq, Show)

type Row = [Cell]
type Grid = [Row]

g1 :: Grid
g1 = [ [B, B, W, W, W, W]
     , [B, W, B, B, B, W]
     , [B, W, B, W, W, B]
     , [B, B, B, B, W, B]
     , [W, B, W, B, B, B]
     , [W, B, W, W, W, W]
     ]

g2 = [ [B, B, W, W, W, W]
     , [B, W, B, B, B, W]
     , [B, W, B, W, W, B]
     , [B, B, B, B, W, B]
     , [B, B, W, B, B, B]
     , [W, B, W, W, W, W]
     ]


valid :: Grid -> Bool
valid g = (islandCount g) &&
          (islandCell g) &&
          (connected g) &&
          (noPools g)

islandCount :: Grid -> Bool
islandCount = undefined

islandCell :: Grid -> Bool
islandCell = undefined

-- for there to be no pools, there has to be no discontinuations
-- row-wise, col-wise, or both, in the black cells
connected :: Grid -> Bool
--connected g = all (nodis $ rows g) || all (nodis $ cols g)
connected = undefined


noPools :: Grid -> Bool
noPools g = all noPools' (group 2 g)

noPools' :: [Row] -> Bool
noPools' [xs, ys] = all someWhite $ zipWith (++) (group 2 xs) (group 2 ys)
  where
    someWhite zs = not (all isBlack zs)
    isBlack B = True
    isBlack _ = False

noPools' _ = False

group :: Int -> [a] -> [[a]]
group n xs = go xs (length xs)
  where
    go xs len
      | n == 0 = error "Zero"
      | n > len = []
      | otherwise = l `seq` (take n xs : go (tail xs) l)
      where l = len - 1
