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

type Digits = [Int]

data Inputs = InputCount { count :: Int
                         , digits :: Digits
                         }
            | InputCoord { coord :: Coord
                         , xCoord :: Maybe Int
                         , digits :: Digits
                         }
            | InputSolve
            | InputSteps
            | None deriving (Eq)

defaultInputCount :: Inputs
defaultInputCount = InputCount 0 []

defaultInputCoord :: Inputs
defaultInputCoord = InputCoord (Coord 0 0) Nothing []

data PSequence = PSeq   Picture            -- For the prompts
               | PSeqs [Picture] [Picture] -- For the solving steps
               deriving (Eq)

data Prompt = Prompt {
    input :: Inputs -- Holds the input stuff
  , handle :: Prompt -> Event -> Prompt -- Pass numbers to input, change seqDisp, process, check
  , process :: Inputs -> Inputs -- Processes input values into an output value
  , seqDisp :: PSequence -- The prompt/seqDisp, these should really be one thing for the prompts, so nothing to reset
  , check :: Inputs -> Bool -- Check if input gathered
  , completed :: Bool -- Completion flag
  }

--------------------------------------------------------------------------------

-- Prompts
-- Should a prompt reset after invalid input? Better than waiting til the end?
-- If we dont advance to next prompt until current is completed, we have to

coordSeq :: PSequence
coordSeq = PSeq labeledPrompt' where
  board = renderBoard . makeBoard $ 5
  rowLabels = pictures . offset 0 16 $ [ scale 0.1 0.1 $ Text (show n) | n <- [5,4..1] ]
  rowLabels' = translate 0 (-4) . color white $ rowLabels
  rowLines  = pictures [ Line [(0,16*n),(l+16,16*n)] | (n,l) <- zip [1..5] [0,8..32] ]
  rowLines' = color white rowLines
  colLabels = [ translate 20 36 . scale 0.1 0.1 . Text $ show n | n <- [5,4..1] ]
  colLines = replicate 5 (Line [(0,0),(16,32)])
  colLabelLines = [ pictures [lb,ln] | (lb,ln) <- zip colLabels colLines ]
  colLabelLines' = color white . translate 104 8 . pictures . offset (-8) 16 $ colLabelLines
  labeledPrompt = pictures [board,colLabelLines']
  labeledPrompt' = translate (-48) 0 . pictures . offset 16 0 $ [rowLabels',rowLines',labeledPrompt]

countPrompt :: Prompt
countPrompt =
  Prompt baseInput handler proc disp chck comp where
    baseInput = defaultInputCount
    handler p (EventKey (Char c) Down _ _)
      | not (completed p) && isDigit c = p { input = i' } where
          i = input p
          i' = i { digits = digitToInt c : (digits i) }
    handler p (EventKey (SpecialKey enter) Down _ _)
      | enter == KeyEnter || enter == KeyPadEnter = p { input = i', completed = cp } where
          i = (process p) (input p)
          cp = (check p) i
          i' = if cp then i else defaultInputCount
    handler pt _ = pt
    proc (InputCount _ ds) = InputCount (makeInt ds) []
    disp = pegCountPrompt
    chck (InputCount 0 []) = False
    chck (InputCount ct _) = ct >= 1 && ct <= 13 -- Maximum end pegs?
    comp = False

coordPrompt :: Prompt
coordPrompt =
  Prompt baseInput handler proc disp chck comp where
    baseInput = defaultInputCoord
    handler p (EventKey (Char c) Down _ _)
      | not (completed p) && isDigit c = p { input = i' } where
          i = input p
          i' = i { digits = digitToInt c : (digits i) }
    handler p (EventKey (SpecialKey enter) Down _ _)
      | enter == KeyEnter || enter == KeyPadEnter = p { input = i', completed = cp } where
          i = (process p) (input p)
          cp = (check p) i'
          i' = case (i,cp) of
                  (InputCoord _ (Just _) _,_) -> i' -- In the middle of getting coord, don't reset
                  (_,False) -> defaultInputCoord -- The coord is "completed", but no good, reset
                  _ -> i' -- The coord is completed and good, don't reset
    handler pt _ = pt
    proc (InputCoord cd Nothing ds) = InputCoord cd (Just $ makeInt ds) []
    proc (InputCoord _ (Just x) ds) = InputCoord (Coord x $ makeInt ds) Nothing []
    disp = coordSeq
    chck (InputCoord (Coord 0 0) _ _) = False
    chck (InputCoord c _ _) = c `elem` (ps ++ hs) where
      (Board ps hs) = makeBoard 5
    comp = False

-- The purpose of this isn't to solve the board, only to notify
-- the user that it is ready to be solved (and so it doesn't look
-- stuck at the last prompt)
solvePrompt :: Prompt
solvePrompt =
  Prompt baseInput handler proc disp chck comp where
    baseInput = InputSolve
    handler p (EventKey (SpecialKey enter) Down _ _)
      | enter == KeyEnter || enter == KeyPadEnter = p { completed = True }
    handler pt _ = pt
    proc = id
    disp = coordSeq -- Change to intermediate "solve" prompt
    chck _ = True
    comp = False

-- Have a list of the prompts
-- Move from one to the next when completed
-- After solve prompt, extract info from other prompts
-- Solve board and seqDisp results in steps prompt
stepsPrompt :: Prompt
stepsPrompt =
  Prompt baseInput handler proc sdisp chck comp where
    baseInput = InputSteps
    handler pt (EventKey (SpecialKey KeyLeft) Down _ _) = pt { seqDisp = prevSeq (seqDisp pt) }
    handler pt (EventKey (SpecialKey KeyRight) Down _ _) = pt { seqDisp = nextSeq (seqDisp pt) }
    handler pt _ = pt
    proc = id
    sdisp = PSeqs [] [] -- Change to steps
    chck _ = False
    comp = False

nonePrompt :: Prompt
nonePrompt =
  Prompt baseInput handler proc sdisp chck comp where
    baseInput = None
    handler pt _ = pt
    proc = id
    sdisp = PSeq failPic where
      failPic = color white . scale 0.1 0.1 $ Text "Failed to find solution"
    chck _ = False
    comp = False
--------------------------------------------------------------------------------
type MouseMove = (Bool,Float,Float)
type Scale = Float
type Translate = (Float,Float)
type WorldTrans = (Scale,Translate,MouseMove) -- Scale, Translate, MouseStart

solveFor :: Board -> Int -> Maybe [Board]
solveFor b n = solution where
  ends = endWith n . playGameLog $ b
  solution = case ends of
    [] -> Nothing
    _  -> Just . collectLog . head $ ends

--------------------------------------------------------------------------------

-- SEQUENCE STUFF
nextSeq :: PSequence -> PSequence
nextSeq (PSeq p) = PSeq p
nextSeq (PSeqs [] bs) = PSeqs [] bs
nextSeq (PSeqs [a] bs) = PSeqs [a] bs
nextSeq (PSeqs (a:as) bs) = PSeqs as (a:bs)

prevSeq :: PSequence -> PSequence
prevSeq (PSeq p) = PSeq p
prevSeq (PSeqs as []) = PSeqs as []
prevSeq (PSeqs as (b:bs)) = PSeqs (b:as) bs

seqRender :: PSequence -> Picture
seqRender (PSeq  b)     = b
seqRender (PSeqs as bs) = head as

renderBoardsSeq :: [Board] -> PSequence
renderBoardsSeq bs = PSeqs (map renderBoard bs) []
--------------------------------------------------------------------------------
-- Least significant digit first
makeInt :: [Int] -> Int
makeInt [] = 0
makeInt ds = sum . zipWith (*) (iterate (*10) 1) $ ds


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
--------------------------------------------------------------------------------

pegCountPrompt :: PSequence
pegCountPrompt = PSeq msg where
  msg = color white . scale 0.1 0.1 $ Text "Enter number of pegs to end with"

rowPrompt :: PSequence
rowPrompt = PSeq labeledPrompt where
  board = renderBoard . makeBoard $ 5
  rowLabels = pictures . offset 0 16 $ [ scale 0.1 0.1 $ Text (show n) | n <- [5,4..1] ]
  rowLabels' = translate 0 (-4) . color white $ rowLabels
  rowLines  = pictures [ Line [(0,16*n),(l+16,16*n)] | (n,l) <- zip [1..5] [0,8..32] ]
  rowLines' = color white rowLines
  labeledPrompt = translate (-48) 0 . pictures . offset 16 0 $ [rowLabels',rowLines',board]

colPrompt :: PSequence
colPrompt = PSeq labeledPrompt where
  board = renderBoard . makeBoard $ 5
  colLabels = [ translate 20 36 . scale 0.1 0.1 . Text $ show n | n <- [5,4..1] ]
  colLines = replicate 5 (Line [(0,0),(16,32)])
  colLabelLines = [ pictures [lb,ln] | (lb,ln) <- zip colLabels colLines ]
  colLabelLines' = color white . translate 104 8 . pictures . offset (-8) 16 $ colLabelLines
  labeledPrompt = pictures [board,colLabelLines']

-- promptBase :: Prompts
-- promptBase = ([pegCountPrompt,rowPrompt,colPrompt],[],emptyInputCol,baseTrans)

-- Will need special type to hold prompt zipper and worldTrans

-- type PromptZip = ([Prompt],[Prompt])
data PromptZip = PromptZip { current :: Prompt
                           , next :: [Prompt]
                           , prev :: [Prompt]
                           , worldT :: WorldTrans
                           }

promptBase :: PromptZip
promptBase = PromptZip countPrompt [coordPrompt,solvePrompt,stepsPrompt,nonePrompt] [] baseTrans

promptNext :: PromptZip -> PromptZip
promptNext pz
  | null . next $ pz = pz
  | otherwise = PromptZip (head nxt) (tail nxt) (current pz : prev pz) (worldT pz) where
      nxt = next pz

promptPrev :: PromptZip -> PromptZip
promptPrev pz
  | null . prev $ pz = pz
  | otherwise = PromptZip (head prv) (current pz : next pz) (tail prv) (worldT pz) where
      prv = prev pz

resetPromptZip :: PromptZip -> PromptZip
resetPromptZip pz = PromptZip cur nxt' [] (worldT pz) where
  prv = reverse . prev $ pz
  nxt = prv ++ [current pz] ++ next pz
  cur = head nxt -- Impossible for nxt to be empty, there must be something in old current
  nxt' = tail nxt

-- Not doing anything to transform renders
eventHandler' :: Event -> PromptZip -> PromptZip
eventHandler' (EventKey (Char 'r') Down _ _) pz = promptBase { worldT = worldT pz }
eventHandler' (EventKey (Char 'R') Down _ _) pz = resetPromptZip pz
eventHandler' (EventKey (MouseButton LeftButton) Down _ m) pz = pz { worldT = setMouse (worldT pz) m }
eventHandler' (EventKey (MouseButton LeftButton) Up _ m) pz = pz { worldT = resetMouse (worldT pz) }
eventHandler' (EventKey (MouseButton WheelUp) _ _ _) pz = pz { worldT = scaleUp (worldT pz) }
eventHandler' (EventKey (MouseButton WheelDown) _ _ _) pz = pz { worldT = scaleDown (worldT pz) }
eventHandler' (EventMotion m) pz = pz { worldT = moveMouse (worldT pz) m }
eventHandler' event pz -- guard promptSolve on ENTER keys
  | (input . current $ pz) == InputSolve = let
      crd = coord . input . current . promptPrev $ pz
      cnt = count . input . current . promptPrev . promptPrev $ pz
      brd = makeBoard 5
      boards = (removePeg crd brd) `solveFor` cnt
      in case boards of
              Nothing -> promptNext . promptNext $ pz -- Skip to nonePrompt
              Just bs -> pnxt { current = cur' } where
                pnxt = promptNext pz -- move zipper to stepsPrompt
                cur = current pnxt -- get stepsPrompt
                cur' = cur { seqDisp = PSeqs (map renderBoard bs) [] } -- Updated current
  | otherwise = pz'' where
      cur = current pz
      cur' = (handle cur) cur event
      pz' = pz { current = cur' }
      pz'' = if (completed cur')
            then promptNext pz'
            else pz'

promptSolve :: IO ()
promptSolve = play
  (InWindow "SolvePrompted" (600,600) (0,0))
  black
  0
  promptBase -- world
  (\pz -> transPicture (worldT pz) . seqRender . seqDisp . current $ pz)
  eventHandler' -- Event -> world -> world
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
