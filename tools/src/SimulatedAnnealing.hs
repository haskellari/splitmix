{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module SimulatedAnnealing (
    simulatedAnnealing,
    Problem (..),
    Pretty (..),
    ) where

import System.Random.SplitMix
import Text.Printf

class Pretty a where
    pretty :: a -> String

data Problem state step = Problem
    { pPerturb :: state -> step -> state
    , pEnergy  :: state -> Double
    }

simulatedAnnealing
    :: forall state step. Pretty state
    => Problem state step  -- ^ problem definition
    -> SMGen               -- ^ initial generator
    -> state               -- ^ initial state
    -> (SMGen -> step)     -- ^ generate step
    -> IO (state, Double)
simulatedAnnealing (Problem perturb energy) g0 state0 genStep
    = go 0 [] startT g0 state0 (energy state0)
  where
    -- params
    innerSteps :: Int
    innerSteps = 128

    startT = 1e-2

    prettySE :: state -> Double -> String
    prettySE s e = pretty s ++ printf " E=%.09f" e

    -- loop
    go :: Int -> [Double] -> Double -> SMGen -> state -> Double -> IO (state, Double)
    go  !n !deltaEs !t !g !s !e | n > innerSteps = do
        let t' = clamp (0.5 * t) (0.8 * t) (abs $ average deltaEs)
        if null deltaEs
        then do
            putStrLn $ "END: " ++ prettySE s e
            return (s, e)
        else do
            putStrLn $ printf "Temperature drop %5.02e -> %5.02e" t t'
            go 0 [] t' g s e

    go n deltaEs t g s e = do
        let (g1, g2) = splitSMGen g
        let step     = genStep g1
        let s'       = perturb s step
        let e'       = energy s'
        let deltaE   = e' - e
        let prob     | deltaE < 0 = 1
                     | otherwise  = exp (negate deltaE / t)
        let (x, g3)  = nextDouble g2

        let takeStep = x < prob

        putStrLn $ printf "% 5d | curr: %s | next: %s | ΔE=%+.09f T=%5.02e P=%.09f X=%.09f | %s"
            n
            (prettySE s e)
            (prettySE s' e')
            deltaE
            t
            prob
            x
            (if takeStep then "take" else "stay")

        let deltaEs' | deltaE < 0 = deltaE : deltaEs
                     | otherwise  = deltaEs

        if takeStep
        then go (succ n) deltaEs' t g3 s' e'
        else go (succ n) deltaEs' t g3 s  e

average :: [Double] -> Double
average [] = 0
average xs = sum xs / fromIntegral (length xs)

clamp :: Ord a => a -> a -> a -> a
clamp mi ma x | x < mi    = mi
              | x > ma    = ma
              | otherwise = x
