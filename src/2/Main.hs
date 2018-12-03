module Main where


import Data.List
import Data.Maybe
import System.IO


checksum :: String -> Int
checksum str = count 3 * count 2
  where  hasExactly n = not . null . filter ((==n) . snd) 
         count n = sum . fmap (fromEnum . hasExactly n . occurences) . words $ str

occurences :: String -> [(Char, Integer)]
occurences = fmap ((,) <$> head <*> fromIntegral . length) . group . sort 

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = ((,) x <$> xs) <> pairs xs 

differences :: Eq a => [a] -> [a] -> Int
differences as bs = sum . fmap (fromEnum . not) $ zipWith (==) as bs

main :: IO ()
main = do
  input <- getContents
  print $ checksum input
  print $ catMaybes . uncurry (zipWith (\a b -> if a == b then Just a else Nothing))  . head . filter (\(a,b) -> differences a b == 1 ) . pairs . words $ input 
