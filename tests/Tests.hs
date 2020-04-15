module Main (main) where

import Data.Bits ((.&.))
import Data.Word (Word64)
import Test.Framework (defaultMain, testGroup)

import qualified System.Random.SplitMix as SM
import qualified System.Random.SplitMix32 as SM32

import Uniformity
import MiniQC (Gen (..), Arbitrary (..), testMiniProperty, counterexample)

main :: IO ()
main = defaultMain
    [ testUniformity "SM64 uniformity" (arbitrary :: Gen Word64) (.&. 0xf) 16
    , testUniformity "SM64 uniformity" (arbitrary :: Gen Word64) (.&. 0xf0) 16

    , testUniformity "bitmaskWithRejection uniformity" (arbitrary :: Gen Word64mod7) id 7

    , testGroup "SM bitmaskWithRejection"
        [ testMiniProperty "64" $ \w' seed -> do
            let w = w' .&. 0xff
            let w1 = w + 1
            let g = SM.mkSMGen seed
            let (x, _) = SM.bitmaskWithRejection64 w1 g
            counterexample ("64-64 " ++ show x ++ " <= " ++ show w) (x < w1)
        , testMiniProperty "64'" $ \w' seed -> do 
            let w = w' .&. 0xff
            let g = SM.mkSMGen seed
            let (x, _) = SM.bitmaskWithRejection64' w g
            counterexample ("64-64 " ++ show x ++ " < " ++ show w) (x <= w)
        , testMiniProperty "32" $ \w' seed -> do 
            let w = w' .&. 0xff
            let u1 = w'
            let g = SM.mkSMGen seed
            let (x, _) = SM.bitmaskWithRejection32 u1 g
            counterexample ("64-32 " ++ show x ++ " <= " ++ show w) (x < u1)
        , testMiniProperty "32'" $ \w' seed -> do 
            let w = w' .&. 0xff
            let u = w
            let g = SM.mkSMGen seed
            let (x, _) = SM.bitmaskWithRejection32' u g
            counterexample ("64-32 " ++ show x ++ " < " ++ show w) (x <= u)
        ]
    , testGroup "SM32 bitmaskWithRejection"
        [ testMiniProperty "64" $ \w' seed -> do
            let w = w' .&. 0xff
            let w1 = w + 1
            let g = SM32.mkSMGen seed
            let (x, _) = SM32.bitmaskWithRejection64 w1 g
            counterexample ("64-64 " ++ show x ++ " <= " ++ show w) (x < w1)
        , testMiniProperty "64'" $ \w' seed -> do 
            let w = w' .&. 0xff
            let g = SM32.mkSMGen seed
            let (x, _) = SM32.bitmaskWithRejection64' w g
            counterexample ("64-64 " ++ show x ++ " < " ++ show w) (x <= w)
        , testMiniProperty "32" $ \w' seed -> do 
            let w = w' .&. 0xff
            let u1 = w'
            let g = SM32.mkSMGen seed
            let (x, _) = SM32.bitmaskWithRejection32 u1 g
            counterexample ("64-32 " ++ show x ++ " <= " ++ show w) (x < u1)
        , testMiniProperty "32'" $ \w' seed -> do 
            let w = w' .&. 0xff
            let u = w
            let g = SM32.mkSMGen seed
            let (x, _) = SM32.bitmaskWithRejection32' u g
            counterexample ("64-32 " ++ show x ++ " < " ++ show w) (x <= u)
        ]
    ]

newtype Word64mod7 = W7 Word64 deriving (Eq, Ord, Show)

instance Arbitrary Word64mod7 where
    arbitrary = Gen $ \g -> W7 $ fst $ SM.bitmaskWithRejection64' 6 g
