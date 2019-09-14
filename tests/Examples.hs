module Main (main) where

import Test.HUnit ((@?=))

import qualified System.Random as R
import qualified System.Random.SplitMix as SM
import qualified System.Random.SplitMix32 as SM32

main :: IO ()
main = do
    let g = SM32.mkSMGen 42
    show g @?= "SMGen 142593372 1604540297"
    print g

    let (w32, g') = SM32.nextWord32 g
    w32     @?= 1296549791
    show g' @?= "SMGen 1747133669 1604540297"

    issue23

issue23 :: IO ()
issue23 = do
    let seed = (10000, 10000)
    let (txt, g) = populateRandomString (SM.seedSMGen' seed)
    txt @?= "QH"
    print g

  where
    populateRandomString :: SM.SMGen -> (String, SM.SMGen)
    populateRandomString smGen =
        let (char, ngen)  = R.randomR ('A', 'Z') smGen
            (char2, gen2) = R.randomR ('A', 'Z') ngen
        in ([char, char2], gen2)
