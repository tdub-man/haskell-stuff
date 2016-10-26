module Solve
    (promptSolve
    ) where
import PegBoard
import PlayGame
import Analytics
import Graphics
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char

data Sequence = Seq   Picture            -- For the prompts
              | Seqs [Picture] [Picture] -- For the solving steps
              deriving (Eq)
type CoordCollect = (Coord,Maybe Int, Bool)
type Prompts = ([Sequence],[Sequence],CoordCollect)

solveFor :: Board -> Int -> [Board]
solveFor b n = collectLog . head . endWith n . playGameLog $ b

-- SEQUENCE STUFF
nextSeq :: Sequence -> Sequence
nextSeq (Seq p) = Seq p
nextSeq (Seqs [] bs) = Seqs [] bs
nextSeq (Seqs [a] bs) = Seqs [a] bs
nextSeq (Seqs (a:as) bs) = Seqs as (a:bs)

prevSeq :: Sequence -> Sequence
prevSeq (Seq p) = Seq p
prevSeq (Seqs as []) = Seqs as []
prevSeq (Seqs as (b:bs)) = Seqs (b:as) bs

seqRender :: Sequence -> Picture
seqRender (Seq  b)     = b
seqRender (Seqs as bs) = head as

renderBoardsSeq :: [Board] -> Sequence
renderBoardsSeq bs = Seqs (map renderBoard bs) []
--------------------------------------------------------------------------------

-- COORDCOLLECT STUFF
emptyCoordCol :: CoordCollect
emptyCoordCol = (Coord 0 0,Nothing,False)

coordInput :: CoordCollect -> Int -> CoordCollect
coordInput (c,Nothing,False) x = (c,Just x,False)
coordInput (_,Just x, False) y = (Coord x y,Nothing,True)
coordInput cc@(_,_,True)     _ = cc

getCoord :: CoordCollect -> Coord
getCoord (c,_,_) = c

coordMade :: CoordCollect -> Bool
coordMade (_,_,made) = made

onBoard :: Coord -> Board -> Bool
onBoard c (Board ps hs) = c `elem` (ps ++ hs)

validCoord :: CoordCollect -> Bool
validCoord (c,_,_) = onBoard c (makeBoard 5)

coordCheck :: CoordCollect -> Bool
coordCheck cc = coordMade cc && validCoord cc
--------------------------------------------------------------------------------

-- PROMPTS STUFF
nextPrompt :: Prompts -> Prompts
nextPrompt ([],  bs,cc) = ([],   bs,cc)
nextPrompt ([p], bs,cc) = ([p],  bs,cc)
nextPrompt (p:as,bs,cc) = (as, p:bs,cc)

prevPrompt :: Prompts -> Prompts
prevPrompt (as,[],cc) = (as,[],cc)
prevPrompt (as,p:bs,cc) = (p:as,bs,cc)

resetPrompt :: Prompts -> Prompts
resetPrompt (as,bs,_) = (ps,[],emptyCoordCol) where
  ps = reverse bs ++ as

getPrompt :: Prompts -> Picture
getPrompt ([],_,_)   = blank
getPrompt (p:as,_,_) = seqRender p

promptInput :: Prompts -> Int -> Prompts
promptInput ps@(as,bs,cc) n = let
  cc' = coordInput cc n
  in inputHelper (as,bs,cc')

inputHelper :: Prompts -> Prompts
inputHelper ps@(as,bs,cc)
  | coordCheck' ps = let
      start = removePeg (getCoord cc) (makeBoard 5)
      steps = solveFor start 1
      ps'   = addSteps (as,bs,cc) steps
      in nextPrompt ps'
  | not . coordMade' $ ps = nextPrompt ps
  | otherwise = resetPrompt ps

coordMade' :: Prompts -> Bool
coordMade' (_,_,cc) = coordMade cc

validCoordP :: Prompts -> Bool
validCoordP (_,_,cc) = validCoord cc

coordCheck' :: Prompts -> Bool
coordCheck' ps = coordMade' ps && validCoordP ps

promptCheck :: Prompts -> Prompts
promptCheck ps = if coordCheck' ps then ps else resetPrompt ps

onPrompt :: Prompts -> Bool
onPrompt ([],_,_)      = False
onPrompt (Seq _:_,_,_) = True
onPrompt _             = False

addSteps :: Prompts -> [Board] -> Prompts
addSteps (as,bs,cc) boards = (as ++ [renderBoardsSeq boards],bs,cc)
--------------------------------------------------------------------------------

-- Event handler
-- Num keys take numbers for coord
-- Invalid coord resets prompt
-- Valid coord moves to solving/diplaying board
eventHandler :: Event -> Prompts -> Prompts
eventHandler (EventKey (Char c) Down _ _) ps
  | onPrompt ps && isNumber c = promptInput ps (digitToInt c)
  | c == 'r' || c == 'R' = promptBase
  | otherwise = ps
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) ps@(a:as,bs,cc)
  | not (onPrompt ps) = (nextSeq a : as,bs,cc)
  | otherwise = ps
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) ps@(a:as,bs,cc)
  | not (onPrompt ps) = (prevSeq a : as,bs,cc)
  | otherwise = ps
eventHandler _ ps = ps

-- TODO: Add zoom and move like in regular display
-- TODO: Add prompt for desired number of end pegs

rowPrompt :: Sequence
rowPrompt = Seq labeledPrompt where
  board = renderBoard . makeBoard $ 5
  rowLabels = pictures . offset 0 16 $ [ scale 0.1 0.1 $ Text (show n) | n <- [5,4..1] ]
  rowLabels' = translate 0 (-4) . color white $ rowLabels
  rowLines  = pictures [ Line [(0,16*n),(l+16,16*n)] | (n,l) <- zip [1..5] [0,8..32] ]
  rowLines' = color white rowLines
  labeledPrompt = pictures . offset 16 0 $ [rowLabels',rowLines',board]

colPrompt :: Sequence
colPrompt = Seq labeledPrompt where
  board = renderBoard . makeBoard $ 5
  colLabels = [ translate 20 36 . scale 0.1 0.1 . Text $ show n | n <- [5,4..1] ]
  colLines = replicate 5 (Line [(0,0),(16,32)])
  colLabelLines = [ pictures [lb,ln] | (lb,ln) <- zip colLabels colLines ]
  colLabelLines' = color white . translate 104 8 . pictures . offset (-8) 16 $ colLabelLines
  labeledPrompt = pictures [board,colLabelLines']

promptBase :: Prompts
promptBase = ([rowPrompt,colPrompt],[],emptyCoordCol)

promptSolve :: IO ()
promptSolve = play
  (InWindow "SolvePrompted" (600,600) (0,0))
  black
  0
  promptBase
  getPrompt
  eventHandler
  (\_ x -> x)

--                            1 \
--                           / 2 \
--                          / / 3 \--- "Column"
--                         / / / 4 \
--        | 1 ---------- A  / / / 5 \
--        | 2 --------- X,X  / / /
-- Row ---| 3 -------- X,B,X  / /
--        | 4 ------- X,X,C,X  /
--        | 5 ------ X,X,X,X,X
--
-- Coordinates go by (row,column)
-- A in the diagram would be (1,1)
-- B would be (3,2) and C would be (4,3)

-- promptSolve :: IO ()
-- promptSolve = play
--   (InWindow "SolveBoard" (600,600) (0,0))
--   black
--   0
--   prompts
--   getPrompt
--   eventHandler
--   (\_ x -> x)
