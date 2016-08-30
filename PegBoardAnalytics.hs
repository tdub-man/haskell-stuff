module PegBoardAnalytics
    ( endWith
    , shortestGame
    , endState
    , uniqueEndStates
    , numEndStates
    , innerTriangle
    , concentricTriangles
    , critPoints
    )where
import PegBoard
import Data.List(sortBy,nubBy,group,groupBy)
import Data.Function(on)
import Helpers.Lists(middle)
import Helpers.Math(ceilDiv)

-- ANALYTICS

endWith :: Int -> [BoardLog] -> [BoardLog]
endWith n = filter ((==n) . pegCount . _current)

shorterGame :: BoardLog -> BoardLog -> BoardLog
shorterGame a b = if gameLength a <= gameLength b then a else b where
  gameLength = length . collectLog

shortestGame :: [BoardLog] -> BoardLog
shortestGame = foldr1 shorterGame

endState :: BoardLog -> Board
endState = last . collectLog

uniqueEndStates :: [BoardLog] -> [BoardLog]
uniqueEndStates = nubBy ((==) `on` sortedEnd) where
  sortBoard (Board ps hs) = Board (sortBy compareCoord ps) (sortBy compareCoord hs)
  sortedEnd = sortBoard . endState

numEndStates :: [BoardLog] -> [(Int,Int)]
numEndStates = numberOfPegCounts . endPegCount . uniqueEndStates where
  endPegCount = map (pegCount . endState)
  numberOf xs = (head xs,length xs)
  numberOfPegCounts = map numberOf . group

-- CRITICAL POINTS
-- Assuming full board
-- Minimal unique starting coords,
-- others can be found by roatating/flipping these

rows :: Board -> [[Coord]]
rows (Board ps _) = groupBy (\(Coord a _) (Coord b _) -> a == b) ps

innerTriangle :: Board -> Board
innerTriangle b = Board mids [] where
  mids = concatMap middle . middle . rows $ b

concentricTriangles' :: (Board,[Board]) -> (Board,[Board])
concentricTriangles' (Board [] _,bs) = (Board [] [],bs)
concentricTriangles' (b,bs) = concentricTriangles' (b',bs') where
  b' = innerTriangle b
  bs' = b:bs

concentricTriangles :: Board -> [Board]
concentricTriangles b = snd . concentricTriangles' $ (b,[])

originShift :: [Coord] -> [Coord]
originShift = map (\(Coord x y) -> Coord (x-2) (y-1))

originShift' :: Board -> Board
originShift' (Board ps _) = Board ps' [] where
  ps' = originShift ps

insideShift :: [Coord] -> [Coord]
insideShift = map (\(Coord x y) -> Coord (x+2) (y+1))

insideShift' :: Board -> Board
insideShift' (Board ps _) = Board ps' [] where
  ps' = insideShift ps


topLeftRow :: Board -> [Coord]
topLeftRow (Board [] _) = []
topLeftRow (Board ps@(Coord _ y:_) _) = filter (\(Coord _ y') -> y' == y) ps

critPoints :: Board -> [Coord]
critPoints b = cs where
  inners = reverse . concentricTriangles $ b
  tlRows = map topLeftRow inners
  lenTLRows = map length tlRows
  ns = map (`ceilDiv` 2) lenTLRows
  cs = concat [ take n tlr | (tlr,n) <- zip tlRows ns ]
