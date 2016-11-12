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
type CountCollect = (Int,Bool) -- Count, count selected flag
type CoordCollect = (Coord,Maybe Int,Bool) -- Coord, x holder, coord made flag
type InputCollect = (CountCollect,CoordCollect,[Int]) -- [int] for holding inut digits
-- PegCount,CountMade,StartCoord,PartialCoord,CoordMade

type MouseMove = (Bool,Float,Float)
type Scale = Float
type Translate = (Float,Float)
type WorldTrans = (Scale,Translate,MouseMove) -- Scale, Translate, MouseStart

type Prompts = ([Sequence],[Sequence],InputCollect,WorldTrans)

-- TODO Take input greater than one digit
-- put numbers onto a list
-- process list on RETURN

solveFor :: Board -> Int -> Maybe [Board]
solveFor b n = solution where
  ends = endWith n . playGameLog $ b
  solution = case ends of
    [] -> Nothing
    _  -> Just . collectLog . head $ ends

--------------------------------------------------------------------------------

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

-- INPUTCOLLECT STUFF
emptyInputCol :: InputCollect
emptyInputCol = ((0,False),(Coord 0 0,Nothing,False),[])

getCount :: InputCollect -> Int
getCount ((c,_),_,_) = c

countSelected :: InputCollect -> Bool
countSelected ((_,cs),_,_) = cs

digitInput :: InputCollect -> Int -> InputCollect
digitInput (ct,cd,ds) x = (ct,cd,x:ds)

-- Least significant digit first
makeInt :: [Int] -> Int
makeInt = sum . zipWith (*) (iterate (*10) 1)

-- Call when return is hit
processCount :: InputCollect -> InputCollect
-- We've already processed count
processCount ic@((_,True),_,_) = ic
-- There is nothing to process
processCount ic@(_,_,[]) = ic
-- Collect digits into a count
processCount (_,cd,ds) = ((makeInt ds,True),cd,[])

-- Call when return is hit
processCoord :: InputCollect -> InputCollect
-- We've already processed the coord
processCoord ic@(_,(_,_,True),_) = ic
-- There is nothing to process
processCoord ic@(_,_,[]) = ic
-- Collect digits into the x coord
processCoord (ct,(c,Nothing,_),ds) = (ct,(c,Just $ makeInt ds,False),[])
-- Collect digits into the y coord
processCoord (ct,(_,Just x,_),ds) = (ct,(Coord x . makeInt $ ds,Nothing,True),[])

getCoord :: InputCollect -> Coord
getCoord (_,(c,_,_),_) = c

rowSelected :: InputCollect -> Bool
rowSelected (_,(_,Nothing,_),_) = False
rowSelected (_,(_,Just _,_),_) = True

coordMade :: InputCollect -> Bool
coordMade (_,(_,_,made),_) = made

validCount :: InputCollect -> Bool
validCount ((c,_),_,_) = c >= 1 && c <= 9
-- What is the max you can end with on a 5 board?

validCoord :: InputCollect -> Bool
validCoord (_,(c,_,_),_) = onBoard c (makeBoard 5) where
  -- Can just do greater than/less than compare of x and y
  onBoard c (Board ps hs) = c `elem` (ps ++ hs)

coordCheck :: InputCollect -> Bool
coordCheck ic = coordMade ic && validCoord ic
--------------------------------------------------------------------------------

-- PROMPTS STUFF
nextPrompt :: Prompts -> Prompts
nextPrompt ([],  bs,ic,wt) = ([],   bs,ic,wt)
-- Keep at least one prompt in the current list
nextPrompt ([p], bs,ic,wt) = ([p],  bs,ic,wt)
nextPrompt (p:as,bs,ic,wt) = (as, p:bs,ic,wt)

prevPrompt :: Prompts -> Prompts
prevPrompt (as,[],ic,wt) = (as,[],ic,wt)
prevPrompt (as,p:bs,ic,wt) = (p:as,bs,ic,wt)

resetPrompt :: Prompts -> Prompts
-- Hard reset everything, but keep WorldTrans
resetPrompt = setTrans promptBase . getTrans
-- -- Reverses the past prompts to put them at the beginning (zipper)
-- -- Resets InputCollect to its empty state
-- -- Preserves WorldTrans
-- resetPrompt (as,bs,_,wt) = (ps,[],emptyInputCol,wt) where
--   ps = reverse bs ++ as

getPrompt :: Prompts -> Picture
getPrompt ([],_,_,_)    = blank
getPrompt (p:as,_,_,wt) = transPicture wt . seqRender $ p

-- Call on valid digit input
digitInputP :: Prompts -> Int -> Prompts
digitInputP (as,bs,ic,wt) n = (as,bs,digitInput ic n,wt)

-- Call on ENTER
processInputP :: Prompts -> Prompts
processInputP (as,bs,ic,wt)
  -- We want to select a count first
  -- The count must not be made (flag not set) in order to make it
  -- Making the count will set the flag
  -- We then go on to the next prompt
  | not (countSelected ic) = nextPrompt (as,bs,processCount ic,wt)
  -- After count is selected, make a coord
  -- The coord must not be made (flag not set) in order to make it
  -- We must processCoord TWICE, once for the row, once for the col

  -- We haven't selected the row
  -- Process the input, but don't try to solve,just prompt col
  | not (coordMade ic) && not (rowSelected ic) = nextPrompt (as,bs,processCoord ic,wt)
  -- We have selected the row, but not the col
  -- Process the input, then solve
  -- Solve helper will move to next
  | not (coordMade ic) = solveHelper (as,bs,processCoord ic,wt)
  -- Why are you hitting ENTER? There is nothing more to make
  | otherwise = (as,bs,ic,wt) -- Maybe move next here?
-- Maybe add intermediate prompt to start solving?

solveHelper :: Prompts -> Prompts
solveHelper ps@(as,bs,ic,wt)
  | inputCheck ps = let -- Everything's good to go
      start = removePeg (getCoord ic) (makeBoard 5)
      steps = solveFor start (getCount ic)
      ps'   = addSteps (as,bs,ic,wt) steps
      in nextPrompt ps'
  | otherwise = resetPrompt ps -- You messed up somewhere

inputCheck :: Prompts -> Bool
inputCheck (_,_,ic,_) =
  countSelected ic && validCount ic -- Do we have a good count?
  && coordMade ic && validCoord ic  -- Do we have a good coord?

-- Seq will include countPrompt, rowPrompt, and colPrompt
-- Seqs is reserved for the solving steps
onPrompt :: Prompts -> Bool
onPrompt ([],_,_,_)      = False
onPrompt (Seq _:_,_,_,_) = True
onPrompt _               = False

addSteps :: Prompts -> Maybe [Board] -> Prompts
addSteps (as,bs,ic@((ct,_),(cd,_,_),_),wt) Nothing = (as ++ failed,bs,ic,wt) where
  msg = "No results for starting from " ++ show cd
        ++ " and ending with " ++ show ct
  failed = (:[]) . Seq . color white . scale 0.1 0.1 $ Text msg
addSteps (as,bs,ic,wt) (Just boards) = (as ++ [renderBoardsSeq boards],bs,ic,wt)
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
setTrans (as,bs,ic,_) wt = (as,bs,ic,wt)
--------------------------------------------------------------------------------

-- Event handler
-- Num keys take numbers for coord
-- Invalid coord resets prompt
-- Valid coord moves to solving/diplaying board
eventHandler :: Event -> Prompts -> Prompts
eventHandler (EventKey (Char c) Down _ _) ps
  -- We are looking for numbers and we got one
  | onPrompt ps && isNumber c = digitInputP ps (digitToInt c)
  -- Wherever we are, we want to start over
  | c == 'r' || c == 'R' = resetPrompt ps
  -- Character pressed is no good
  | otherwise = ps
eventHandler (EventKey (SpecialKey KeyEnter) Down _ _) ps
  -- We are still looking for input, sowe can advance to the next stage
  | onPrompt ps = processInputP ps
  | otherwise = ps
eventHandler (EventKey (SpecialKey KeyPadEnter) Down _ _) ps
  | onPrompt ps = processInputP ps
  | otherwise = ps
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) ps@(a:as,bs,ic,wt)
  | not (onPrompt ps) = (nextSeq a : as,bs,ic,wt)
  | otherwise = ps
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) ps@(a:as,bs,ic,wt)
  | not (onPrompt ps) = (prevSeq a : as,bs,ic,wt)
  | otherwise = ps
eventHandler (EventKey (MouseButton LeftButton) Down _ m) (as,bs,ic,wt) =
  (as,bs,ic,setMouse wt m)
eventHandler (EventKey (MouseButton LeftButton) Up _ _) (as,bs,ic,wt) =
  (as,bs,ic,resetMouse wt)
eventHandler (EventKey (MouseButton WheelUp) _ _ _) (as,bs,ic,wt) =
  (as,bs,ic,scaleUp wt)
eventHandler (EventKey (MouseButton WheelDown) _ _ _) (as,bs,ic,wt) =
  (as,bs,ic,scaleDown wt)
eventHandler (EventMotion m) (as,bs,ic,wt) = (as,bs,ic,moveMouse wt m)
eventHandler _ ps = ps
-- Can hit enter without entering numbers
-- Giving invalid coord resets to row prompt
--   Accepts inputs in proper order though (count,row,col)

pegCountPrompt :: Sequence
pegCountPrompt = Seq msg where
  msg = color white . scale 0.1 0.1 $ Text "Enter number of pegs to end with"

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
promptBase = ([pegCountPrompt,rowPrompt,colPrompt],[],emptyInputCol,baseTrans)

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
