module Wrapper
    ( Wrapper
    , unwrap
    , arrUnwrap
    , wrapBasic
    , wrapWraps
    , arrWrapBasic
    , arrWrapWraps
    , arrPairBasic
    , arrPairWraps
    , select
    ) where
import Control.Applicative
import Data.Monoid()

data Wrapper a = Empty
               | Val a
               | Pair (Wrapper a) (Wrapper a)
               | Wrap (Wrapper a) deriving (Eq)

instance (Show a) => Show (Wrapper a) where
  show Empty = ""
  show (Val a) = show a
  show (Pair a Empty) = show a
  show (Pair Empty b) = show b
  show (Pair a b) = show a ++ "," ++ show b
  show (Wrap x) = "{" ++ show x ++ "}"

instance Functor Wrapper where
  fmap _ Empty = Empty
  fmap f (Val x) = Val (f x)
  fmap f (Pair a b) = Pair (fmap f a) (fmap f b)
  fmap f (Wrap x) = Wrap (fmap f x)

instance Applicative Wrapper where
  pure = Val
  Empty <*> _ = Empty
  (Val f) <*> x = fmap f x
  (Wrap a) <*> x = Wrap (a <*> x)
  (Pair a b) <*> x = Pair (a <*> x) (b <*> x)
  -- (Pair (Val (+1)) (Val (+2)))

instance Monad Wrapper where
  return = Val
  Empty >>= _ = Empty
  (Val a) >>= f = f a
  (Wrap a) >>= f = Wrap (a >>= f)
  (Pair a b) >>= f = Pair (a >>= f) (b >>= f)
  fail _ = Empty


unwrap :: Wrapper a -> [a]
unwrap Empty = []
unwrap (Val x) = [x]
unwrap (Pair a b) = unwrap a ++ unwrap b
unwrap (Wrap x) = unwrap x

arrUnwrap :: [Wrapper a] -> [a]
arrUnwrap = concatMap unwrap

wrapBasic :: a -> Int -> Wrapper a
wrapBasic x 0 = Val x
wrapBasic x n = Wrap (wrapBasic x (n-1))

wrapWraps :: Wrapper a -> Int -> Wrapper a
wrapWraps x 0 = x
wrapWraps x n = Wrap (wrapWraps x (n-1))

arrWrapBasic :: [a] -> Int -> [Wrapper a]
arrWrapBasic xs n = map (`wrapBasic` n) xs

arrWrapWraps :: [Wrapper a] -> Int -> [Wrapper a]
arrWrapWraps xs n = map (`wrapWraps` n) xs

arrPairBasic :: [a] -> Wrapper a
arrPairBasic = foldr (\x -> Pair (wrapBasic x 0)) Empty

arrPairWraps :: [Wrapper a] -> Wrapper a
arrPairWraps = foldr (\x -> Pair (wrapWraps x 0)) Empty

select :: Int -> Wrapper a -> Wrapper a
select _ Empty = Empty
select _ (Val a) = Val a
select n (Wrap a) = select n a
select 0 (Pair a _) = a
select n (Pair _ b) = select (n-1) b
