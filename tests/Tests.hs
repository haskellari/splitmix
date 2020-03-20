module Main (main) where

import Test.HUnit (assertBool)
import Data.Bits ((.&.))

import qualified System.Random.SplitMix as SM
import qualified System.Random.SplitMix32 as SM32

main :: IO ()
main = do
    g0 <- SM.newSMGen
    g32 <- SM32.newSMGen

    _ <- run64 g0 $ \g1 -> do
        let (w', g2) = SM.nextWord64 g1
        let w = w' .&. 0xff
        let w1 = w + 1

        g3 <- run64 g2 $ \g -> do
            let (x, _) = SM.bitmaskWithRejection64 w1 g
            assertBool ("64-64 " ++ show x ++ " <= " ++ show w) (x < w1)

        g4 <- run64 g3 $ \g -> do
            let (x, _) = SM.bitmaskWithRejection64' w g
            assertBool ("64-64 " ++ show x ++ " < " ++ show w) (x <= w)

        g5 <- run64 g4 $ \g -> do
            let u1 = fromIntegral w1
            let (x, _) = SM.bitmaskWithRejection32 u1 g
            assertBool ("64-32 " ++ show x ++ " <= " ++ show w) (x < u1)

        _ <- run64 g5 $ \g -> do
            let u = fromIntegral w
            let (x, _) = SM.bitmaskWithRejection32' u g
            assertBool ("64-32 " ++ show x ++ " < " ++ show w) (x <= u)

        return ()

    _ <- run32 g32 $ \g1 -> do
        let (w', g2) = SM32.nextWord64 g1
        let w = w' .&. 0xff
        let w1 = w + 1

        g3 <- run32 g2 $ \g -> do
            let (x, _) = SM32.bitmaskWithRejection64 w1 g
            assertBool ("32-64 " ++ show x ++ " <= " ++ show w) (x < w1)

        g4 <- run32 g3 $ \g -> do
            let (x, _) = SM32.bitmaskWithRejection64' w g
            assertBool ("32-64 " ++ show x ++ " < " ++ show w) (x <= w)

        g5 <- run32 g4 $ \g -> do
            let u1 = fromIntegral w1
            let (x, _) = SM32.bitmaskWithRejection32 u1 g
            assertBool ("32-32 " ++ show x ++ " <= " ++ show w) (x < u1)

        _ <- run32 g5 $ \g -> do
            let u = fromIntegral w
            let (x, _) = SM32.bitmaskWithRejection32' u g
            assertBool ("32-32 " ++ show x ++ " < " ++ show w) (x <= u)

        return ()

    return ()

run32 :: Monad m => SM32.SMGen -> (SM32.SMGen -> m ()) -> m SM32.SMGen
run32 = run SM32.splitSMGen    
    
run64 :: Monad m => SM.SMGen -> (SM.SMGen -> m ()) -> m SM.SMGen
run64 = run SM.splitSMGen    

run :: Monad m => (g -> (g, g)) -> g -> (g -> m ()) -> m g
run split g0 action = go (3000 :: Int) g0 where
    go n g | n <= 0    = return g
           | otherwise = do
        let (g1, g2) = split g
        action g1
        go (pred n) g2
