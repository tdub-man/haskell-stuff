-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE KindSignatures #-}

module Kinematics
    ( Motion(Motion,_di,_df,_vi,_vf,_a,_t)
    , XDim, YDim, ZDim
    , setDi, setDf, setVi, setVf, setA, setT
    , sets
    , calcVf, calcVi
    , calcDi, calcDf
    , calcA , calcT
    , findPossible
    , ex
    ) where
import Helpers.Lists(generalPerms,generalNPerms,foldlBind)
import Data.List(nub)

data SigDig n = Null | SigDig n deriving (Eq,Show)
data Motion n = Motion { _di :: SigDig n, _df :: SigDig n
                       , _vi :: SigDig n, _vf :: SigDig n
                       , _a  :: SigDig n, _t  :: SigDig n} deriving (Eq,Show)
type XDim = Motion
type YDim = Motion
type ZDim = Motion

-- For testing
ex :: Motion Double
ex = Motion di df vi vf a t where
  di = SigDig 0
  df = SigDig 56.25
  vi = SigDig 0
  vf = SigDig 30
  a  = SigDig 8.0
  t  = SigDig 3.75

setDi :: Motion n -> SigDig n -> Motion n
setDi (Motion _ df vi vf a t) di = Motion di df vi vf a t
setDf :: Motion n -> SigDig n -> Motion n
setDf (Motion di _ vi vf a t) df = Motion di df vi vf a t
setVi :: Motion n -> SigDig n -> Motion n
setVi (Motion di df _ vf a t) vi = Motion di df vi vf a t
setVf :: Motion n -> SigDig n -> Motion n
setVf (Motion di df vi _ a t) vf = Motion di df vi vf a t
setA  :: Motion n -> SigDig n -> Motion n
setA  (Motion di df vi vf _ t) a = Motion di df vi vf a t
setT  :: Motion n -> SigDig n -> Motion n
setT  (Motion di df vi vf a _) t = Motion di df vi vf a t

-- For testing
sets :: [Motion n -> SigDig n -> Motion n]
sets = [setDi,setDf,setVi,setVf,setA,setT]

-- For testing
nSets :: [Motion n -> Motion n]
nSets = map papp sets where
  papp f = (`f` Null)

-- For testing
cPNSets :: Int -> [Motion n -> Motion n]
cPNSets n = conses where
  pns = generalNPerms n nSets
  conses = map (foldl1 (.)) pns

-- For testing
fExs :: Int -> [Motion Double]
fExs n = fs <*> [ex] where
  fs = cPNSets n

nfExs :: Int -> [Motion Double]
nfExs = nub . fExs

calcVf :: (Floating n) => Motion n -> Maybe (Motion n)
calcVf (Motion di df vi'@(SigDig vi) _ a'@(SigDig a) t'@(SigDig t)) =
  Just $ Motion di df vi' vf a' t' where
    vf = SigDig $ vi + a*t
calcVf (Motion di'@(SigDig di) df'@(SigDig df) vi'@(SigDig vi) _ a t'@(SigDig t)) =
  Just $ Motion di' df' vi' vf a t' where
    vf = SigDig $ ((df-di) * 2 / t) - vi
calcVf (Motion di'@(SigDig di) df'@(SigDig df) vi'@(SigDig vi) _ a'@(SigDig a) t) =
  Just $ Motion di' df' vi' vf a' t where
    vf = SigDig . sqrt $ (vi*vi) + 2*a*(df-di)
calcVf _ = Nothing

calcVi :: (Floating n) => Motion n -> Maybe (Motion n)
calcVi (Motion di df _ vf'@(SigDig vf) a'@(SigDig a) t'@(SigDig t)) =
  Just $ Motion di df vi vf' a' t' where
    vi = SigDig $ vf - a*t
calcVi (Motion di'@(SigDig di) df'@(SigDig df) _ vf'@(SigDig vf) a t'@(SigDig t)) =
  Just $ Motion di' df' vi vf' a t' where
    vi = SigDig $ ((df-di) * 2 / t) - vf
calcVi (Motion di'@(SigDig di) df'@(SigDig df) _ vf'@(SigDig vf) a'@(SigDig a) t) =
  Just $ Motion di' df' vi vf' a' t where
    vi = SigDig . sqrt $ (vf*vf) - 2*a*(df-di)
calcVi _ = Nothing

calcDi :: (Floating n) => Motion n -> Maybe (Motion n)
calcDi (Motion _ df'@(SigDig df) vi'@(SigDig vi) vf'@(SigDig vf) a t'@(SigDig t)) =
  Just $ Motion di df' vi' vf' a t' where
    di = SigDig $ df - 0.5*t*(vi + vf)
calcDi (Motion _ df'@(SigDig df) vi'@(SigDig vi) vf a'@(SigDig a) t'@(SigDig t)) =
  Just $ Motion di df' vi' vf a' t' where
    di = SigDig $ df - vi*t - 0.5*a*t*t
calcDi (Motion _ df'@(SigDig df) vi'@(SigDig vi) vf'@(SigDig vf) a'@(SigDig a) t) =
  Just $ Motion di df' vi' vf' a' t where
    di = SigDig $ df - ((vf*vf - vi*vi)/(2*a))
calcDi _ = Nothing

calcDf :: (Floating n) => Motion n -> Maybe (Motion n)
calcDf (Motion di'@(SigDig di) _ vi'@(SigDig vi) vf'@(SigDig vf) a t'@(SigDig t)) =
  Just $ Motion di' df vi' vf' a t' where
    df = SigDig $ di + 0.5*t*(vi + vf)
calcDf (Motion di'@(SigDig di) _ vi'@(SigDig vi) vf a'@(SigDig a) t'@(SigDig t)) =
  Just $ Motion di' df vi' vf a' t' where
    df = SigDig $ di + vi*t + 0.5*a*t*t
calcDf (Motion di'@(SigDig di) _ vi'@(SigDig vi) vf'@(SigDig vf) a'@(SigDig a) t) =
  Just $ Motion di' df vi' vf' a' t where
    df = SigDig $ di + ((vf*vf - vi*vi)/(2*a))
calcDf _ = Nothing

calcA :: (Floating n) => Motion n -> Maybe (Motion n)
calcA (Motion di df vi'@(SigDig vi) vf'@(SigDig vf) _ t'@(SigDig t)) =
  Just $ Motion di df vi' vf' a t' where
    a = SigDig $ (vf - vi)/t
calcA (Motion di'@(SigDig di) df'@(SigDig df) vi'@(SigDig vi) vf _ t'@(SigDig t)) =
  Just $ Motion di' df' vi' vf a t' where
    a = SigDig $ 2*(df - di - vi*t)/(t*t)
calcA (Motion di'@(SigDig di) df'@(SigDig df) vi'@(SigDig vi) vf'@(SigDig vf) _ t) =
  Just $ Motion di' df' vi' vf' a t where
    a = SigDig $ (vf*vf - vi*vi)/(2*(df-di))
calcA _ = Nothing

calcT :: (Floating n) => Motion n -> Maybe (Motion n)
calcT (Motion di df vi'@(SigDig vi) vf'@(SigDig vf) a'@(SigDig a) _) =
  Just $ Motion di df vi' vf' a' t where
    t = SigDig $ (vf - vi)/a
calcT (Motion di'@(SigDig di) df'@(SigDig df) vi'@(SigDig vi) vf'@(SigDig vf) a _) =
  Just $ Motion di' df' vi' vf' a t where
    t = SigDig $ 2*(df - di)/(vi + vf)
calcT (Motion di'@(SigDig di) df'@(SigDig df) vi'@(SigDig vi) vf a'@(SigDig a) _) =
  Just $ Motion di' df' vi' vf a' t where
    t = SigDig $ (disc - vi)/a
    disc = sqrt $ vi*vi - 2*a*(di-df)
calcT _ = Nothing

allValues :: Motion n -> Bool
allValues (Motion (SigDig _) (SigDig _) (SigDig _) (SigDig _) (SigDig _) (SigDig _)) = True
allValues _ = False

eqs :: (Floating n) => [Motion n -> Maybe (Motion n)]
eqs = [calcDi,calcDf,calcVi,calcVf,calcA,calcT]

findPossible :: (Floating n,Eq n) => Motion n -> Motion n
findPossible m
    | allValues m = m
    | otherwise = m'' where
          eqs' = generalPerms eqs
          res = map (foldlBind m) eqs'
          resF = filter (/=Nothing) res
          exJust (Just x) = x
          resF' = filter (allValues . exJust) resF
          m'' = if null resF'
            then m
            else head . map exJust $ resF
