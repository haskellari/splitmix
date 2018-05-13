module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.List (unfoldr)
import System.Environment (getArgs)
import System.IO (stdout)
import Data.Word (Word64)

import qualified Data.ByteString.Builder as B
import qualified System.Random.SplitMix as SM
import qualified System.Random.TF as TF
import qualified System.Random.TF.Gen as TF
import qualified System.Random.TF.Init as TF

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["splitmix"]      -> splitmix Nothing
        ["splitmix", arg] -> splitmix (Just (read arg))
        ["tfrandom"]      -> tfrandom
        ["splitmix-tree"] -> splitmixTree
        _ -> return ()

splitmix :: Maybe Word64 -> IO ()
splitmix seed = smgen >>= B.hPutBuilder stdout . go
  where
    smgen = maybe SM.initSMGen (return . SM.mkSMGen) seed

    go :: SM.SMGen -> B.Builder
    go g = case SM.nextWord64 g of 
        ~(w64, g') -> B.word64LE w64 `mappend` go g'

splitmixTree :: IO ()
splitmixTree = SM.initSMGen >>= B.hPutBuilder stdout . go
  where
    go :: SM.SMGen -> B.Builder
    go g = case SM.splitSMGen g of
        ~(ga, gb) -> builder 8 ga `mappend` go gb

    builder :: Int -> SM.SMGen -> B.Builder
    builder n = mconcat . take n . map B.word64LE . unfoldr (Just . SM.nextWord64)

tfrandom :: IO ()
tfrandom = TF.initTFGen >>= B.hPutBuilder stdout . go
  where
    go :: TF.TFGen -> B.Builder
    go g = case TF.next g of 
        ~(w32, g') -> B.word32LE w32 `mappend` go g'
