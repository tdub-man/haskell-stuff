module PegBoardAnalytics
    ( endWith
    , shortestGame
    , endState
    , uniqueEndStates
    , numEndStates
    , critPoints
    )where
import PegBoard
import Data.List(sortBy,nubBy,group,groupBy)
import Data.Function(on)
import ListHelpers(middle)
import MathHelpers(ceilDiv)

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

originShift :: Board -> Board
originShift (Board ps _) = Board ps' [] where
  ps' = map (\(Coord x y) -> Coord (x-2) (y-1)) ps

topLeftRow :: Board -> [Coord]
topLeftRow (Board ps _) = filter (\(Coord _ y) -> y == 1) ps

critPoints' :: (Board,[Coord]) -> (Board,[Coord])
critPoints' (Board [] [],cs) = (Board [] [],cs)
critPoints' (b,cs) = critPoints' (b',cs') where
  b' = originShift . innerTriangle $ b
  tlRow = topLeftRow b
  lenTLRow = length tlRow
  n = lenTLRow `ceilDiv` 2
  cs' = cs ++ take n tlRow

critPoints :: Board -> [Coord]
critPoints (Board [] _) = []
critPoints b = cs where
  (_,cs) = critPoints' (b,[])

-- ALL GAMES ON 5 BOARD

-- tCoords :: [Coord]
-- tCoords = [(Coord 1 1),(Coord 2 1),(Coord 3 1),(Coord 3 2)]

tBoards :: [Board]
tBoards = [ removePeg c b | c <- coords ] where
  b = makeBoard 5
  coords = critPoints b

tAllGames :: [[BoardLog]]
tAllGames = map playGameLog tBoards

mapShow :: [[BoardLog]] -> IO ()
mapShow = mapM_ (print . numEndStates)

main :: IO ()
main = mapShow tAllGames
