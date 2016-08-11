module Main where
import PegBoard
import PegBoardAnalytics

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
