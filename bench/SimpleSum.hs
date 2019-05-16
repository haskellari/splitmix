{-# LANGUAGE CPP #-}
module Main (main) where

import System.Environment (getArgs)
import Data.List (foldl')
import Data.Word (Word32)

import qualified System.Random as R
import qualified System.Random.SplitMix as SM
import qualified System.Random.SplitMix32 as SM32

newGen :: a -> (a -> g) -> IO g -> IO g
#if 0
newGen _ _ new = new
#else
newGen seed mk _ = return (mk seed)
#endif

main :: IO ()
main = do
    putStrLn "Summing randoms..."
    getArgs >>= \args -> case args of
        "splitmix"   : _ -> newGen 33 SM.mkSMGen   SM.newSMGen   >>= \g -> print $ benchSum g SM.nextTwoWord32
        "splitmix32" : _ -> newGen 33 SM32.mkSMGen SM32.newSMGen >>= \g -> print $ benchSum g SM32.nextTwoWord32
        "random"     : _ -> R.newStdGen   >>= \g -> print $ benchSum g randomNextTwoWord32

        -- after Closure Compiler getArgs return [] always?
        -- _ -> newGen 33 SM.mkSMGen   SM.newSMGen   >>= \g -> print $ benchSum g SM.nextTwoWord32
        _ -> newGen 33 SM32.mkSMGen SM32.newSMGen >>= \g -> print $ benchSum g SM32.nextTwoWord32


benchSum :: g -> (g -> (Word32, Word32, g)) -> Word32
benchSum g next = foldl' (+) 0 $ take 10000000 $ unfoldr2 next g

-- | Infinite unfoldr with two element generator
unfoldr2 :: (s -> (a, a, s)) -> s -> [a]
unfoldr2 f = go where
    go s = let (x, y, s') = f s in x : y : go s'

randomNextTwoWord32 :: R.StdGen -> (Word32, Word32, R.StdGen)
randomNextTwoWord32 s0 = (x, y, s2) where
    (x, s1) = R.random s0
    (y, s2) = R.random s1
