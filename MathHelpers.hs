module MathHelpers
    ( squareRoot
    ) where

(.^) :: Num a => a -> Int -> a
(.^) x n = x^n

squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (.^2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r.^2 <= n && n < (r+1).^2
  in  head $ dropWhile (not . isRoot) iters
