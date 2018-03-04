module ThueMorse
    ( LR, notLR
    , tm, binTM
    , toBin
    , tm'
    ) where

data LR = L | R deriving (Eq,Show,Enum,Bounded)

notLR :: LR -> LR
notLR L = R
notLR R = L

thueMorse :: [LR] -> Int-> [LR]
thueMorse x 0 = x
thueMorse [] n = thueMorse [L] (n-1)
thueMorse x n = thueMorse x' (n-1) where
  complement L = [L,R]
  complement R = [R,L]
  x' = concatMap complement x

tm :: Int -> [LR]
tm = thueMorse []

toBin :: LR -> Int
toBin L = 0
toBin R = 1

binThueMorse :: [LR] -> Int -> [Int]
binThueMorse x = map toBin . thueMorse x

binTM :: Int -> [Int]
binTM = binThueMorse []

thueMorse' :: [LR] -> [LR]
thueMorse' [] = thueMorse' [L]
thueMorse' x = thueMorse' x' where
  x' = x ++ map notLR x

tm' :: [LR]
tm' = thueMorse' []
