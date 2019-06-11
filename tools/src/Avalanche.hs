{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Bits
import System.Random.SplitMix
import Control.Monad (when)
import Data.Word
import Data.List (unfoldr)
import Data.Foldable (for_)
import Control.Monad.ST
import Text.Printf

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import SimulatedAnnealing

-- |
--
-- @
-- uint32_t murmurmix32( uint32_t h )
-- {
--   h ^= h >> 16;
--   h *= 0x85ebca6b;
--   h ^= h >> 13;
--   h *= 0xc2b2ae35;
--   h ^= h >> 16;
--
--   return h;
-- }
-- @
--
murmurmix32 :: Word32 -> Word32
murmurmix32 x0 =
    let x1 = x0 `xor` (x0 `shiftR` 16)
        x2 = x1 * 0x85ebca6b
        x3 = x2 `xor` (x1 `shiftR` 13)
        x4 = x3 * 0xc2b2ae35
        x5 = x4 `xor` (x4 `shiftR` 16)
    in x5

paramMurMurMix
    :: Int -> Int -> Int
    -> Word32 -> Word32
    -> Word32 -> Word32
paramMurMurMix s1 s2 s3 m1 m2 x0 =
    let x1 = x0 `xor` (x0 `shiftR` s1)
        x2 = x1 * m1
        x3 = x2 `xor` (x1 `shiftR` s2)
        x4 = x3 * m2
        x5 = x4 `xor` (x4 `shiftR` s3)
    in x5

avalancheStep :: (Word32 -> Word32) -> U.MVector s Word32 -> Word32 -> ST s ()
avalancheStep f vec x =
    for_ [0..31] $ \i -> do
        let y  = x `xor` mask i
            y' = f y

        for_ [0..31] $ \j ->
            when (((x' `xor` y') .&. mask j) /= 0) $
                MU.unsafeModify vec succ (i * 32 + j)
  where
    x' = f x
    mask k = 1 `shiftL` k

-- | Calculate avalanche energy for given function.
avalanche :: U.Vector Word32 -> (Word32 -> Word32) -> Double
avalanche inputs f =
    let matrix = runST $ do
            vec <- MU.replicate (32 * 32) (0 :: Word32)

            U.foldM_ (\_ -> avalancheStep f vec) () inputs

            U.freeze vec

        matrix1 = U.map (\n -> sq $ (fromIntegral n - halfsize) / size) matrix
        matrix2 = U.sum matrix1

    in matrix2 / (32 * 32) * 4.0

  where
    sq x = x * x

    size :: Double
    size = fromIntegral $ U.length inputs

    halfsize :: Double
    halfsize = size /2

getInput :: Int -> U.Vector Word32
getInput size =
    U.fromList $ map fromIntegral $ take size $ unfoldr (Just . nextWord64) smgen
  where
    smgen  = mkSMGen 0xdeadbeef

main :: IO ()
main = do
    -- test avalanche of murmurmix32
    do
        let input = getInput $ 1024 * 1024
        print $ avalanche input murmurmix32

    -- test avalanche of paramMurMurMix
    do
        let input = getInput $ 1024 * 1024
        print $ avalanche input $ paramMurMurMix 16 13 16 0x69ad6ccb 0xcd9ab5b3

    -- test avalanche of bad paramMurMurMix
    do
        let input = getInput $ 1024 * 1024
        print $ avalanche input $ paramMurMurMix 16 13 16 0x00000001 0x00010000

    -- simulated-annealing
    -- https://en.wikipedia.org/wiki/Simulated_annealing
    optimise (mkSMGen 0xfeedbacc) 0 0
    -- optimise (mkSMGen 0xfeedbacc) 0xa9b1c34d 0x6ca4ba2d

-- | MurMurMixer parameters.
data MurMurData = MurMurData !Int !Int !Int !Word32 !Word32

instance Pretty MurMurData where
    pretty (MurMurData _ _ _ m1 m2) = printf "0x%08x 0x%08x" m1 m2

optimise :: SMGen -> Word32 -> Word32 -> IO ()
optimise initGen i1 i2 = do
    _ <- simulatedAnnealing problem initGen (MurMurData 16 13 16 i1 i2) genStep
    return ()
  where
    problem :: Problem MurMurData Int
    problem = Problem
        { pPerturb = perturb
        , pEnergy  = energy
        }
      where
        energy :: MurMurData -> Double
        energy (MurMurData s1 s2 s3 m1 m2) =
            avalanche input $ paramMurMurMix s1 s2 s3 m1 m2

        input = getInput $ 1024 * 256

    perturb :: MurMurData -> Int -> MurMurData
    perturb (MurMurData s1 s2 s3 m1 m2) w = MurMurData s1 s2 s3 m1' m2'
      where
        (m1', m2') | w >= 32  = (m1 `xor` mask (w - 32), m2)
                   | otherwise = (m1, m2 `xor` mask w)

    genStep :: SMGen -> Int
    genStep g =
        let (w64, _) = nextWord64 g
        in fromIntegral $ w64 `mod` 64

    mask :: Int -> Word32
    mask k = 1 `shiftL` k

