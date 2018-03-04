module Main where
import System.IO
import TriangleSquares

last' :: [a] -> Maybe a
last' [] = Nothing
last' x = Just $ last x

maybeLine :: Maybe String -> String
maybeLine Nothing = "None"
maybeLine (Just x) = x

intRead :: String -> Integer
intRead = read

main :: IO ()
main = do
  contents <- readFile "triangle-squares.txt"
  let
    lastLine = last' . lines $ contents
  putStrLn $ "Last Number: " ++ maybeLine lastLine ++
    "\nHow many new numbers would you like to find? "
  num <- getLine
  let
    results = calcTriangleSquaresFrom n i [] where
      n = intRead num
      i = case lastLine of
        Nothing -> 1
        Just x -> intRead x
  appendFile "triangle-squares.txt" $ unlines . map show $ results
  putStrLn "Done"
