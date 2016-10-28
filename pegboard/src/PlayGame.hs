module PlayGame
    ( BoardLog(BoardLog,_current)
    , playGame
    , playGameLog
    , collectLog
    , showBoardLog
    ) where
import Data.List(partition)
import PegBoard(Board,Coord,showBoard)
import PegBoardMove(nextMoves,movePegs,movePegsAll)

data BoardLog = BoardLog { _current :: Board, _history :: [Board] } deriving (Eq)

instance Show BoardLog where
  show = show . collectLog

showBoardLog :: BoardLog -> [String]
showBoardLog = map showBoard . collectLog

changeBoard :: BoardLog -> Board -> BoardLog
changeBoard (BoardLog b bs) x = BoardLog x (b:bs)

collectLog :: BoardLog -> [Board]
collectLog (BoardLog b bl) = reverse $ b:bl

movePegsLog :: (Coord,Coord,Coord) -> BoardLog -> BoardLog
movePegsLog tri bLog = changeBoard bLog b where
  b = movePegs tri $ _current bLog

movePegsAllLog :: [(Coord,Coord,Coord)] -> BoardLog -> [BoardLog]
movePegsAllLog mvs bLog = [ movePegsLog mv bLog | mv <- mvs ]

partitionPlayable :: (Foldable t) => [(a, t b)] -> ( [(a, t b)],[(a, t b)] )
partitionPlayable = partition (\(_,m) -> null m)

play :: ([Board],[Board]) -> ([Board],[Board])
play ([],ended) = ([],ended)
play (playing,ended) = play (playing',ended') where
  boardAndMoves = zip playing $ map nextMoves playing
  -- (newlyEnded,playable) = partition (\(_,m) -> null m) boardAndMoves
  (newlyEnded,playable) = partitionPlayable boardAndMoves
  newlyEnded' = map fst newlyEnded
  ended' = ended ++ newlyEnded'
  playing' = concat [ movePegsAll mvs b | (b,mvs) <- playable ]

playLog :: ([BoardLog],[BoardLog]) -> ([BoardLog],[BoardLog])
playLog ([],ended) = ([],ended)
playLog (playing,ended) = playLog (playing',ended') where
  boardAndMoves = zip playing $ map (nextMoves . _current) playing
  -- (newlyEnded,playable) = partition (\(_,m) -> null m) boardAndMoves
  (newlyEnded,playable) = partitionPlayable boardAndMoves
  newlyEnded' = map fst newlyEnded
  ended' = ended ++ newlyEnded'
  playing' = concat [ movePegsAllLog mvs b | (b,mvs) <- playable ]

playGame :: Board -> [Board]
playGame b = endStates where
  (_,endStates) = play ([b],[])

playGameLog :: Board -> [BoardLog]
playGameLog b = games where
  bLog = BoardLog b []
  (_,games) = playLog ([bLog],[])
