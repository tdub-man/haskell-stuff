module Circle
    ( Circle, toCircle
    , isClosed, isOpen
    , rotateR, rotateL
    , cycleR, cycleL
    , openCircle, closeCircle
    , lengthC, circleFoldR
    ) where
import Control.Applicative
import Data.Monoid

-- Circle (Just _) _ = closed
-- Circle (Nothing) _ = open
data Circle a = Circle { point :: Maybe a, body :: [a] } deriving (Show)

emptyCircle :: Circle a
emptyCircle = Circle Nothing []

instance Functor Circle where
  fmap f (Circle Nothing xs) = Circle Nothing (map f xs)
  fmap f (Circle (Just x) xs) = Circle (Just . f $ x) (map f xs)

instance Applicative Circle where
  pure x = Circle (Just x) []
  (Circle Nothing []) <*> _ = emptyCircle
  cf <*> ca = toCircle bs where
    (Circle _ fs) = openCircle cf
    (Circle _ as) = openCircle ca
    bs = fs <*> as

instance Monoid (Circle m) where
  mempty = emptyCircle
  mappend = joinCircles

instance Monad Circle where
  return = pure
  (Circle Nothing []) >>= _ = mempty
  ca >>= f = let
    cb = fmap f ca
    in circleFoldR mappend mempty cb

toCircle :: [a] -> Circle a
toCircle []     = Circle Nothing []
toCircle (x:xs) = Circle (Just x) xs

isClosed :: Circle a -> Bool
isClosed (Circle (Just _) _) = True
isClosed _ = False

isOpen :: Circle a -> Bool
isOpen = not . isClosed

rotateR :: Circle a -> Circle a
rotateR c@(Circle _ [])      = c
rotateR (Circle Nothing xs)  = Circle x xs' where
  x   = Just . last $ xs
  xs' = init xs
rotateR (Circle (Just x) xs) = Circle x' xs' where
  x'  = Just . last $ xs
  xs' = x:init xs

rotateL :: Circle a -> Circle a
rotateL c@(Circle _ [])           = c
rotateL (Circle Nothing (x:xs))   = Circle (Just x) xs
rotateL (Circle (Just x) (x':xs)) = Circle (Just x') xs' where
  xs' = xs ++ [x]

cycleR :: Circle a -> Int -> Circle a
cycleR c 0 = c
cycleR c i = cycleR c' (i-1) where
  c' = rotateR c

cycleL :: Circle a -> Int -> Circle a
cycleL c 0 = c
cycleL c i = cycleL c' (i-1) where
  c' = rotateL c

openCircle :: Circle a -> Circle a
openCircle c@(Circle Nothing _) = c
openCircle (Circle (Just x) xs) = Circle Nothing (x:xs)

closeCircle :: Circle a -> Circle a
closeCircle c@(Circle (Just _) _) = c
closeCircle c@(Circle _ [])       = c
closeCircle (Circle _ (x:xs))     = Circle (Just x) xs

lengthC :: Circle a -> Int
lengthC (Circle Nothing xs) = length xs
lengthC (Circle _ xs) = 1 + length xs

joinCircles :: Circle a -> Circle a -> Circle a
joinCircles ca cb = toCircle ab where
  (Circle _ as) = openCircle ca
  (Circle _ bs) = openCircle cb
  ab = as ++ bs

circleFoldR :: (a -> b -> b) -> b -> Circle a -> b
circleFoldR f b ca = foldr f b as where
  (Circle _ as) = openCircle ca
