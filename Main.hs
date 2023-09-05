module Main where

import Chunks

main :: IO ()
main = do
    n <- characterCount "greetings.txt"
    putStrLn $ show n