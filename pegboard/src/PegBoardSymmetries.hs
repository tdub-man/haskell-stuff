module PegBoardSymmetries
    ( zedSym
    ) where
import PegBoard
import PegBoardCriticalPoints(rows,concentricTrianglesExclusive)
import Helpers.Lists(middleElem)
-- import Data.List(zipWith)

--             (1,1)
--          (2,1),(2,2)
--       (3,1),(3,2),(3,3)
--    (4,1),(4,2),(4,3),(4,4)
-- (5,1),(5,2),(5,3),(5,4),(5,5)

boardSize :: Board -> Int
boardSize (Board ps hs) = x where
  (Coord x _) =  maximum $ ps ++ hs

-- flip full board, map holes/pegs to the flip
zedSym :: Board -> Board
zedSym b@(Board ps hs) = Board ps' hs' where
  isPeg x = (x,x `elem` ps)
  pRows = rows $ ps ++ hs
  isPegRows = map (map isPeg) pRows
  revRows = map reverse isPegRows
  swapCoords (_,p) c = (c,p)
  pshs = concat $ zipWith (zipWith swapCoords) revRows pRows
  extract (c,_) = c
  ps' = map extract . filter snd $ pshs
  hs' = map extract . filter (not . snd) $ pshs
  -- [[ (C1,True) ], [ (C22,False),(C21,True) ]]
  -- [[ C1        ], [ C21,        C22        ]]

-- rotate concentric triangles
-- rotate similar to circle type idea
clockSym :: Board -> Board
clockSym b = b where
  rings = concentricTrianglesExclusive b
