module Main where
import PegBoard
import PegBoardAnalytics
import PegBoardCriticalPoints

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

board :: Board
board = removePeg (Coord 2 1) . makeBoard $ 4

games :: [BoardLog]
games = playGameLog board

main :: IO ()
main = mapM_ putStrLn . showBoardLog . head . endWith 1 $ games
-- main = mapShow tAllGames
