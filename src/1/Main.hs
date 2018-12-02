module Main where

import System.IO
import qualified Data.Set as S
import Data.Set (member)


changes :: String -> [Integer]
changes = fmap readInteger . words
  where readInteger :: String -> Integer
        readInteger ('-':xs) = negate (read xs)
        readInteger ('+':xs) = read xs
        readInteger xs = read xs

frequencies :: [Integer] -> [Integer]
frequencies = scanl (+) 0 

firstRepetition :: Ord a => [a] -> a
firstRepetition xs = go xs S.empty
  where go (x:xs) seen
          | x `member` seen = x
          | otherwise = go xs (S.insert x seen)
        go _ _ = error "reached end of infinite list!"

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "Part one: " <> (show . last . frequencies . changes $ input)
  putStrLn $ "Part two: " <> (show . firstRepetition . frequencies . cycle . changes $ input)

