import Data.Char

main = do
    line <- getLine
    if null line
        then return ()
    else do
        putStrLn $ reverseWords line
        main

-- reverses
reverseWords :: String -> String
reverseWords = unwords . map reverse . words
