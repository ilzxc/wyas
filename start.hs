module Main where
import System.Environment

main :: IO ()
main = do
    putStrLn ("Enter your name: > ")
    args <- getLine
    putStrLn ("Hello, " ++ args)

test x = show (read (x !! 0) + read (x !! 1))