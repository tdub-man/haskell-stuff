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

type MouseMove = (Bool,Float,Float)
type Scale = Float
type Translate = (Float,Float)
type WorldTrans = (Scale,Translate,MouseMove) -- Scale, Translate, MouseStart

type Prompts = ([Sequence],[Sequence],CoordCollect,WorldTrans)

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
nextPrompt ([],  bs,cc,wt) = ([],   bs,cc,wt)
nextPrompt ([p], bs,cc,wt) = ([p],  bs,cc,wt)
nextPrompt (p:as,bs,cc,wt) = (as, p:bs,cc,wt)

prevPrompt :: Prompts -> Prompts
prevPrompt (as,[],cc,wt) = (as,[],cc,wt)
prevPrompt (as,p:bs,cc,wt) = (p:as,bs,cc,wt)

resetPrompt :: Prompts -> Prompts
resetPrompt (as,bs,_,wt) = (ps,[],emptyCoordCol,wt) where
  ps = reverse bs ++ as

-- Add in transformations
getPrompt :: Prompts -> Picture
getPrompt ([],_,_,_)    = blank
getPrompt (p:as,_,_,wt) = transPicture wt . seqRender $ p

promptInput :: Prompts -> Int -> Prompts
promptInput ps@(as,bs,cc,wt) n = let
  cc' = coordInput cc n
  in inputHelper (as,bs,cc',wt)

inputHelper :: Prompts -> Prompts
inputHelper ps@(as,bs,cc,wt)
  | coordCheck' ps = let
      start = removePeg (getCoord cc) (makeBoard 5)
      steps = solveFor start 1
      ps'   = addSteps (as,bs,cc,wt) steps
      in nextPrompt ps'
  | not . coordMade' $ ps = nextPrompt ps
  | otherwise = resetPrompt ps

coordMade' :: Prompts -> Bool
coordMade' (_,_,cc,_) = coordMade cc

validCoordP :: Prompts -> Bool
validCoordP (_,_,cc,_) = validCoord cc

coordCheck' :: Prompts -> Bool
coordCheck' ps = coordMade' ps && validCoordP ps

promptCheck :: Prompts -> Prompts
promptCheck ps = if coordCheck' ps then ps else resetPrompt ps

onPrompt :: Prompts -> Bool
onPrompt ([],_,_,_)      = False
onPrompt (Seq _:_,_,_,_) = True
onPrompt _               = False

addSteps :: Prompts -> [Board] -> Prompts
addSteps (as,bs,cc,wt) boards = (as ++ [renderBoardsSeq boards],bs,cc,wt)
--------------------------------------------------------------------------------

-- World Transformation
-- Click the mouse to set mx and my
-- When mouse is moved, new position is subtracted from start
-- to find delta, which is added to translate


baseTrans :: WorldTrans
baseTrans = (baseScale,baseTranslate,baseMouse) where
  baseScale = 1
  baseTranslate = (0,0)
  baseMouse = (False,0,0)

setMouse :: WorldTrans -> (Float,Float) -> WorldTrans
setMouse (s,t,_) (mx,my) = (s,t,(True,mx,my))

resetMouse :: WorldTrans -> WorldTrans
resetMouse (s,t,_) = (s,t,(False,0,0))

moveMouse :: WorldTrans -> (Float,Float) -> WorldTrans
moveMouse wt@(s,(tx,ty),(ms,mx,my)) (x,y) =
  if ms
    then (s,(tx+x-mx,ty+y-my),(ms,x,y))
    else wt

scaleUp :: WorldTrans -> WorldTrans
scaleUp (s,t,m) = (s+0.1,t,m)

scaleDown :: WorldTrans -> WorldTrans
scaleDown (s,t,m) = if s <= 0 then (0,t,m) else (s-0.1,t,m)
-- Stop at zero to prevent flipping

transPicture :: WorldTrans -> Picture -> Picture
transPicture (s,(tx,ty),_) = scale s s . translate (tx/s) (ty/s)
-- Divide translate by scale to prevent "tracking",
-- moving more than it should when scaled greater than 1 and
-- moving less than it should when scaled less than 1

getTrans :: Prompts -> WorldTrans
getTrans (_,_,_,wt) = wt

setTrans :: Prompts -> WorldTrans -> Prompts
setTrans (as,bs,cc,_) wt = (as,bs,cc,wt)
--------------------------------------------------------------------------------

-- Event handler
-- Num keys take numbers for coord
-- Invalid coord resets prompt
-- Valid coord moves to solving/diplaying board
eventHandler :: Event -> Prompts -> Prompts
eventHandler (EventKey (Char c) Down _ _) ps
  | onPrompt ps && isNumber c = promptInput ps (digitToInt c)
  | c == 'r' || c == 'R' = setTrans promptBase . getTrans $ ps
  | otherwise = ps
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) ps@(a:as,bs,cc,wt)
  | not (onPrompt ps) = (nextSeq a : as,bs,cc,wt)
  | otherwise = ps
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) ps@(a:as,bs,cc,wt)
  | not (onPrompt ps) = (prevSeq a : as,bs,cc,wt)
  | otherwise = ps
eventHandler (EventKey (MouseButton LeftButton) Down _ m) (as,bs,cc,wt) =
  (as,bs,cc,setMouse wt m)
eventHandler (EventKey (MouseButton LeftButton) Up _ _) (as,bs,cc,wt) =
  (as,bs,cc,resetMouse wt)
eventHandler (EventKey (MouseButton WheelUp) _ _ _) (as,bs,cc,wt) =
  (as,bs,cc,scaleUp wt)
eventHandler (EventKey (MouseButton WheelDown) _ _ _) (as,bs,cc,wt) =
  (as,bs,cc,scaleDown wt)
eventHandler (EventMotion m) (as,bs,cc,wt) = (as,bs,cc,moveMouse wt m)
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
  labeledPrompt = translate (-48) 0 . pictures . offset 16 0 $ [rowLabels',rowLines',board]

colPrompt :: Sequence
colPrompt = Seq labeledPrompt where
  board = renderBoard . makeBoard $ 5
  colLabels = [ translate 20 36 . scale 0.1 0.1 . Text $ show n | n <- [5,4..1] ]
  colLines = replicate 5 (Line [(0,0),(16,32)])
  colLabelLines = [ pictures [lb,ln] | (lb,ln) <- zip colLabels colLines ]
  colLabelLines' = color white . translate 104 8 . pictures . offset (-8) 16 $ colLabelLines
  labeledPrompt = pictures [board,colLabelLines']

promptBase :: Prompts
promptBase = ([rowPrompt,colPrompt],[],emptyCoordCol,baseTrans)

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
