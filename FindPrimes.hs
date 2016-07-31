module Main where
import System.IO
import Primes(sieveForm)

main :: IO ()
main = do
  withFile "prime-numbers.txt" WriteMode (\handle -> do
    putStrLn "Find primes to:"
    num <- getLine
    let
      n = read num :: Integer
      ps = sieveForm n
    hPutStrLn handle $ unlines . map show $ ps)
  putStrLn "Done"
