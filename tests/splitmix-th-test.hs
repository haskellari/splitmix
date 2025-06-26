{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.Haskell.TH.Syntax

import System.Random.SplitMix

main :: IO ()
main = print val where
    val :: Double
    val = $(runIO (newSMGen >>= \g -> return (fst (nextDouble g))) >>= lift)
