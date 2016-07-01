import Control.Monad

main = do
  line <- getLine
  unless (null line) $ do
    putStrLn $ reverseWords line
    main

reverseWords :: String -> String
reverseWords = unwords . map reverse . reverse . words
