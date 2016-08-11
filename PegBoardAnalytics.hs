module Main where

import PegBoard
import Data.List(sortBy,nubBy,group)
import Data.Function(on)

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

-- ALL GAMES ON 5 BOARD

-- Minimal unique starting coords
-- others can be found by roatating/flipping these
tCoords :: [Coord]
tCoords = [(Coord 1 1),(Coord 2 1),(Coord 3 1),(Coord 3 2)]

tBoards :: [Board]
tBoards = [ removePeg c b | c <- tCoords ] where
  b = makeBoard 5

tAllGames :: [[BoardLog]]
tAllGames = map playGameLog tBoards

mapShow :: [[BoardLog]] -> IO ()
mapShow = mapM_ (print . numEndStates)

main :: IO ()
main = mapShow tAllGames
