module Analytics
    ( endWith
    , shortestGame
    , endState
    , uniqueEndStates
    , numEndStates
    , movePegRatio, movePegRatios
    , boardSymmetries
    ) where
import PegBoard
import PlayGame(BoardLog(BoardLog,_current),collectLog)
import CriticalPoints(concentricTriangles)
import PegBoardMove
import Symmetries

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

movePegRatio :: Board -> (Int,Int)
movePegRatio b@(Board ps _) = (length mvs,length ps) where
  mvs = nextMoves b

movePegRatios :: BoardLog -> [(Int,Int)]
movePegRatios = map movePegRatio . collectLog

boardSymmetries :: BoardLog -> [Symmetries]
boardSymmetries = map findSymmetries . collectLog
