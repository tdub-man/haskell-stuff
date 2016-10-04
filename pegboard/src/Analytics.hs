module Analytics
    ( endWith
    , shortestGame
    , endState
    , uniqueEndStates
    , numEndStates
    )where
import PegBoard
import PlayGame(BoardLog(BoardLog,_current),collectLog)
import CriticalPoints(concentricTriangles)
import Data.List(sort,nubBy,group,groupBy)
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
  sortBoard (Board ps hs) = Board (sort ps) (sort hs)
  sortedEnd = sortBoard . endState

numEndStates :: [BoardLog] -> [(Int,Int)]
numEndStates = numberOfPegCounts . endPegCount . uniqueEndStates where
  endPegCount = map (pegCount . endState)
  numberOf xs = (head xs,length xs)
  numberOfPegCounts = map numberOf . group
