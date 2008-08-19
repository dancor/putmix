-- for each word argument, print all anagrams the preserve consonant/vowel
-- structure (e.g. "putnam" yields both "manput" and "tampun" among others)

import Data.List
import Util
import qualified Data.Map as M
import System.Environment

sectionOn :: (Ord b) => (a -> b) -> [a] -> ([b], M.Map b [a])
sectionOn f xs = (map f xs, M.fromListWith (++) [(f x, [x]) | x <- xs])

unsection [] sectMap = []
unsection (i:is) sectMap = let
  Just (x:xs) = M.lookup i sectMap
  in x:unsection is (M.insert i xs sectMap)

shufs :: [Char] -> [[Char]]
shufs wd = let
  (sectOrd, sectMap) = sectionOn (`elem` "aeiou") wd
  -- we could actually write a more efficient nub . perms, but who cares
  sectLs = sequence . M.elems $ M.map (nub . perms) sectMap
  in map (unsection sectOrd . M.fromList . zip [False, True]) sectLs

main :: IO ()
main = getArgs >>= mapM_ (putStr . unlines . shufs)
