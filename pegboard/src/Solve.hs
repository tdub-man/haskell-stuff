module Solve
    (
    ) where
import PegBoard
import PlayGame
import Analytics
import Graphics
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char

solveFor :: Board -> Int -> [Board]
solveFor b n = collectLog . head . endWith n . playGameLog $ b

data Sequence = Seq   Picture            -- For the prompts
              | Seqs [Picture] [Picture] -- For the solving steps
              deriving (Eq)
type CoordCollect = (Coord,Maybe Int, Bool)
type Prompts = ([Sequence],[Sequence],CoordCollect)

-- SEQUENCE STUFF
nextSeq :: Sequence -> Sequence
nextSeq (Seq p) = Seq p
nextSeq (Seqs [] bs) = Seqs [] bs
nextSeq (Seqs [a] bs) = Seqs [a] bs
nextSeq (Seqs (a:as) bs) = Seqs as (a:bs)

prevSeq :: Sequence -> Sequence
prevSeq (Seq p) = Seq p
prevSeq (Seqs as []) = Seqs as []
prevSeq (Seqs as (b:bs)) = Seqs (b:bs) as

seqRender :: Sequence -> Picture
seqRender (Seq  b)     = b
seqRender (Seqs as bs) = head as
--------------------------------------------------------------------------------

-- COORDCOLLECT STUFF
emptyCoordCol :: CoordCollect
emptyCoordCol = (Coord 0 0,Nothing, False)

coordInput :: CoordCollect -> Int -> CoordCollect
coordInput (c,Nothing,False) x = (c,Just x,False)
coordInput (_,Just x, False) y = (Coord x y,Nothing,True)
coordInput cc@(_,_,True)     _ = cc

onBoard :: Coord -> Board -> Bool
onBoard c (Board ps hs) = c `elem` (ps ++ hs)

validCoord :: CoordCollect -> Bool
validCoord (c,_,_) = onBoard c (makeBoard 5)
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
resetPrompt (as,bs,cc) = (ps,[],emptyCoordCol) where
  ps = reverse bs ++ as

getPrompt :: Prompts -> Picture
getPrompt ([],_,_)   = blank
getPrompt (p:as,_,_) = seqRender p

promptInput :: Prompts -> Int -> Prompts
promptInput (as,bs,cc) n = (as,bs,coordInput cc n)

coordMade :: Prompts -> Bool
coordMade (_,_,(_,_,b)) = b

validCoordP :: Prompts -> Bool
validCoordP (_,_,cc) = validCoord cc

coordCheck :: Prompts -> Prompts
coordCheck ps = if coordMade ps && validCoordP ps then ps else resetPrompt ps
--------------------------------------------------------------------------------

-- Event handler
-- Return key progresses to next prompt
-- Num keys take numbers for coord
-- Invalid coord resets prompt
-- Valid coord moves to solving/diplaying board
eventHandler :: Event -> Prompts -> Prompts
eventHandler (EventKey (SpecialKey KeyEnter) Down _ _) ps = nextPrompt ps
eventHandler (EventKey (Char c) Down _ _) ps =
  if isNumber c
  then promptInput ps (digitToInt c)
  else ps

-- Prompt for Row
rowPrompt :: Picture
rowPrompt = blank

-- Display board and first prompt
-- Get input
-- Display board and second prompt
-- Get input
-- Remove peg based on given inputs
-- solve and display interactive
getCoord :: Prompts -> Coord
getCoord (_,_,(c,_,_)) = c

startBoard :: Prompts -> Board
startBoard ps = removePeg (getCoord ps) (makeBoard 5)

-- Play
-- Giving input progresses to next steps

-- promptStart :: IO ()
-- promptStart = do
--   let b = renderBoard . makeBoard $ 5
--
--   let bs = collectLog . head . endWith 1 .  playGameLog $ b
--   displayInteractive bs

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
