{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CrossFrac
    (
    --   CFCoord(CFCoord)
    -- , CFLine(CFLine)
    -- , midCoord
    -- , lineBetween
    -- , splitLine
    -- , shiftCoord
    -- , shiftLine
    -- , flipV
    -- , flipH
    -- , flipLine
    -- , flipLine'
    -- , crossLine
    -- , crossFractal
    -- , recCrossFractal
      -- cfFractal
    --   crossFractal
    -- , recCrossFractal
    -- , startFractal
    -- , cfFractal
    dispFrac
    ) where
-- import Helpers.Math((.^))
-- import Helpers.Classes
import Graphics.Gloss

-- All references to lines are line segments

data CFCoord = CFCoord { _x :: Float, _y :: Float} deriving (Eq,Show)
data CFLine = CFLine { _a :: CFCoord, _mid :: CFCoord, _b :: CFCoord} deriving (Eq,Show)

-- instance Ord CFCoord where
--   compare a b = hyp a `compare` hyp b

-- instance DistA CFCoord Double where
--   multA = (*)
--   distA (CFCoord x1 y1) (CFCoord x2 y2) = sqrt $ dx + dy where
--     dx = (x2 - x1) .^ 2
--     dy = (y2 - y1) .^ 2

-- instance Ord CFLine where
--   compare a b = lengthLine a `compare` lengthLine b

-- instance DistA CFLine Double where
--   multA = (*)
--   distA ()

-- hyp :: CFCoord -> Double
-- hyp (CFCoord x y) = sqrt $ (x .^ 2) + (y .^ 2)

midCoord :: CFCoord -> CFCoord -> CFCoord
midCoord (CFCoord x1 y1) (CFCoord x2 y2) = CFCoord x y where
  x = (x1 + x2) / 2
  y = (y1 + y2) / 2

-- lengthLine :: CFLine -> Double
-- lengthLine (CFLine a _ b) = distA a b

-- Make a line between A and B
lineBetween :: CFCoord -> CFCoord -> CFLine
lineBetween a b = CFLine a (midCoord a b) b

lineBetween' :: Float -> Float -> Float -> Float -> CFLine
lineBetween' a b c d = lineBetween (CFCoord a b) (CFCoord c d)

splitLine :: CFLine -> [CFLine]
splitLine (CFLine a m b) = [la,lb] where
  la = CFLine a (midCoord a m) m
  lb = CFLine m (midCoord m b) b

shiftCoord :: CFCoord -> CFCoord -> CFCoord
shiftCoord (CFCoord x1 y1) (CFCoord x2 y2) = CFCoord x y where
  x = x1 + x2
  y = y1 + y2

shiftLine :: CFLine -> CFCoord -> CFLine
shiftLine (CFLine a m b) c = CFLine a' m' b' where
  a' = shiftCoord a c
  m' = shiftCoord m c
  b' = shiftCoord b c

-- Flip A about horizontal line through B
flipV :: CFCoord -> CFCoord -> CFCoord
flipV (CFCoord x y1) (CFCoord _ y2) = CFCoord x y where
  y = (2*y2) - y1

-- Flip A about vertical line through B
flipH :: CFCoord -> CFCoord -> CFCoord
flipH (CFCoord x1 y) (CFCoord x2 _) = CFCoord x y where
  x = (2*x2) - x1

-- Flip a line about its midpoint
flipLine :: CFLine -> CFLine
flipLine (CFLine a m b) = CFLine a' m b' where
  a' = flipV a m
  b' = flipV b m

flipLine' :: CFLine -> CFLine
flipLine' (CFLine (CFCoord x1 y1) m (CFCoord x2 y2)) = CFLine a m b where
  a = CFCoord x1 y2
  b = CFCoord x2 y1

crossLine :: CFLine -> [CFLine]
crossLine l = [l,flipLine' l]

crossFractal :: [CFLine] -> [CFLine]
crossFractal ls = crosses where
  splits = concatMap splitLine ls
  crosses = concatMap crossLine splits

recCrossFractal :: [CFLine] -> Int -> [CFLine]
recCrossFractal ls 0 = ls
recCrossFractal ls n =
  recCrossFractal ls' (n-1) where
    ls' = crossFractal ls

startFractal :: Int -> [CFLine]
startFractal n = rcf where
  ls = crossLine $ lineBetween' 0 0 256 256
  rcf = recCrossFractal ls n

-- Graphics?

lineToPicture :: CFLine -> Picture
lineToPicture (CFLine (CFCoord a b) _ (CFCoord c d)) =
  Line [(a,b),(c,d)]

cfFractal :: Int -> Picture
cfFractal n = Color white frac' where
  frac = startFractal n
  frac' = Pictures $ map lineToPicture frac

dispFrac :: Int -> IO ()
dispFrac n = display
             (InWindow "CrossFrac" (600,600) (20,20))
             black
             (Rotate 45 (cfFractal n))
