module Main (main) where

import Criterion.Main
import Data.List (unfoldr)
import Data.Word (Word64)

import qualified Data.Tree as T
import qualified System.Random as R
import qualified System.Random.TF as TF
import qualified System.Random.SplitMix as SM

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

-- infinite list
genList :: R.RandomGen g => g -> [Int]
genList = unfoldr (Just . R.next)

-- truncated
genListN :: R.RandomGen g => g -> [Int]
genListN = take 2048 . genList

randomList :: Int -> [Int]
randomList = genListN . R.mkStdGen

tfRandomList :: Word64 -> [Int]
tfRandomList w64 = genListN $ TF.seedTFGen (w64, w64, w64, w64)

splitMixList :: Word64 -> [Int]
splitMixList w64 = genListN $ SM.mkSMGen w64

-------------------------------------------------------------------------------
-- Tree
-------------------------------------------------------------------------------

genTree :: R.RandomGen g => g -> T.Tree Int
genTree g = case R.next g of
    ~(i, g') -> T.Node i $ case R.split g' of
        (ga, gb) -> [genTree ga, genTree gb]

genTreeN :: R.RandomGen g => g -> T.Tree Int
genTreeN = cutTree 9 . genTree
  where
    cutTree :: Int -> T.Tree a -> T.Tree a
    cutTree n (T.Node x forest)
        | n <= 0    = T.Node x []
        | otherwise = T.Node x (map (cutTree (n - 1)) forest)

randomTree :: Int -> T.Tree Int
randomTree = genTreeN . R.mkStdGen

tfRandomTree :: Word64 -> T.Tree Int
tfRandomTree w64 = genTreeN $ TF.seedTFGen (w64, w64, w64, w64)

splitMixTree :: Word64 -> T.Tree Int
splitMixTree w64 = genTreeN $ SM.mkSMGen w64

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ bgroup "list"
        [ bench "random"    $ nf randomList 42
        , bench "tf-random" $ nf tfRandomList 42
        , bench "splitmix"  $ nf splitMixList 42
        ]
    , bgroup "tree"
        [ bench "random"    $ nf randomTree 42
        , bench "tf-random" $ nf tfRandomTree 42
        , bench "splitmix"  $ nf splitMixTree 42
        ]
    ]
